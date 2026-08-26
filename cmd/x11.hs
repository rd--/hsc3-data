import qualified Control.Exception {- base -}
import qualified Control.Monad {- base -}
import qualified Data.Bits {- base -}
import qualified System.Environment {- base -}
import qualified System.Exit {- base -}
import qualified Text.Printf {- base -}

import qualified Graphics.X11.Xlib as Xlib {- xlib -}

import qualified Sound.Osc.Fd as Osc {- hosc -}
import qualified Sound.Osc.Transport.Fd.Udp as Fd.Udp {- hosc -}

import qualified Sound.Sc3.Data.X11.Ptr as X11 {- hsc3-data -}

import qualified Sound.Midi.Osc as Midi.Osc {- midi-osc -}
import qualified Sound.Midi.Type as Midi {- midi-osc -}

-- * Ptr Midi-osc

to_cc_msg :: Int -> Int -> Double -> Osc.Message
to_cc_msg ch cc r = Midi.Osc.cvm_to_osc 0xFF (Midi.Control_Change ch cc (floor (r * 127)))

-- > ptr_midi_osc (0.01,0,1,0,2)
ptr_midi_osc :: (Double, Int, Int, Int, Int) -> IO ()
ptr_midi_osc (dt, x_ch, x_cc, y_ch, y_cc) = do
  x11 <- X11.x11_init ":0"
  fd <- Fd.Udp.openUdp "127.0.0.1" 57150 -- midi-osc
  let recur (rx', ry') = do
        ((rx, ry), _, (x, y), _) <- X11.x11_ptr_raw x11
        Control.Monad.when (rx /= rx') (Osc.sendMessage fd (to_cc_msg x_ch x_cc x))
        Control.Monad.when (ry /= ry') (Osc.sendMessage fd (to_cc_msg y_ch y_cc y))
        Osc.pauseThread dt
        recur (rx, ry)
  Control.Exception.finally (recur (0, 0)) (X11.x11_close x11 >> Osc.close fd)

-- * Ptr Trace

entry :: Double -> Double -> Double -> String
entry tm x y = Text.Printf.printf "%.3f,%.3f,%.3f" tm x y

has_mask :: Data.Bits.Bits a => a -> a -> Bool
has_mask k m = (m Data.Bits..&. k) == k

-- > ptr_trace False 0.01
ptr_trace :: Bool -> Double -> IO ()
ptr_trace u dt = do
  putStrLn "tm,x,y"
  x11 <- X11.x11_init ":0"
  t0 <- Osc.time
  let recur (rx', ry') = do
        tm <- Osc.time
        ((rx, ry), c1, c2, m) <- X11.x11_ptr_raw x11
        let (x, y) = if u then c1 else c2
        Control.Monad.when (has_mask Xlib.button3Mask m) System.Exit.exitSuccess
        Control.Monad.when (rx /= rx' || ry /= ry') (putStrLn (entry (tm - t0) x y))
        Osc.pauseThread dt
        recur (rx, ry)
  Control.Exception.finally (recur (0, 0)) (X11.x11_close x11)

-- * Main

help :: [String]
help =
  [ "hsc3-x11"
  , ""
  , "  ptr midi-osc dt:float x-ch:int x-cc:int y-ch:int y-cc:int"
  , "  ptr trace u|n delta-time:float"
  ]

main :: IO ()
main = do
  a <- System.Environment.getArgs
  case a of
    ["ptr", "midi-osc", dt, x_ch, x_cc, y_ch, y_cc] ->
      ptr_midi_osc (read dt, read x_ch, read x_cc, read y_ch, read y_cc)
    ["ptr", "trace", nrm, dt] -> ptr_trace (nrm == "u") (read dt)
    _ -> putStrLn (unlines help)
