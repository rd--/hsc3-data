import Control.Exception {- base -}
import Control.Monad {- base -}
import Data.Bits {- base -}
import Data.Word {- base -}
import System.Environment {- base -}
import System.Exit {- base -}
import Text.Printf {- base -}

import qualified Graphics.X11.Xlib as X {- xlib -}

import Sound.OSC.FD {- hosc -}

import Sound.SC3.Data.X11.Ptr {- hsc3-data -}

import qualified Sound.Midi.Type as M {- midi-osc -}
import qualified Sound.Midi.OSC as M {- midi-osc -}

type R = Double

-- * PTR MIDI-OSC

to_cc_msg :: Word8 -> Word8 -> Double -> Message
to_cc_msg ch cc r = M.cvm_to_osc 0xFF (M.Control_Change ch cc (floor (r * 127)))

-- > ptr_midi_osc (0.01,0,1,0,2)
ptr_midi_osc :: (R, Word8, Word8, Word8, Word8) -> IO ()
ptr_midi_osc (dt,x_ch,x_cc,y_ch,y_cc) = do
  x11 <- x11_init ":0"
  fd <- openUDP "127.0.0.1" 57150 -- midi-osc
  let recur (rx',ry') = do
        ((rx,ry),_,(x,y),_) <- x11_ptr_raw x11
        when (rx /= rx') (sendMessage fd (to_cc_msg x_ch x_cc x))
        when (ry /= ry') (sendMessage fd (to_cc_msg y_ch y_cc y))
        pauseThread dt
        recur (rx,ry)
  finally (recur (0,0)) (x11_close x11 >> close fd)

-- * PTR TRACE

entry :: R -> R -> R -> String
entry tm x y = printf "%.3f,%.3f,%.3f" tm x y

has_mask :: Bits a => a -> a -> Bool
has_mask k m = (m .&. k) == k

-- > ptr_trace False 0.01
ptr_trace :: Bool -> R -> IO ()
ptr_trace u dt = do
  putStrLn "tm,x,y"
  x11 <- x11_init ":0"
  t0 <- time
  let recur (rx',ry') = do
        tm <- time
        ((rx,ry),c1,c2,m) <- x11_ptr_raw x11
        let (x,y) = if u then c1 else c2
        when (has_mask X.button3Mask m) exitSuccess
        when (rx /= rx' || ry /= ry') (putStrLn (entry (tm - t0) x y))
        pauseThread dt
        recur (rx,ry)
  finally (recur (0,0)) (x11_close x11)

-- * MAIN

help :: [String]
help =
  ["hsc3-x11"
  ,""
  ,"  ptr midi-osc dt:float x-ch:int x-cc:int y-ch:int y-cc:int"
  ,"  ptr trace u|n delta-time:float"]

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["ptr","midi-osc",dt,x_ch,x_cc,y_ch,y_cc] ->
      ptr_midi_osc (read dt,read x_ch,read x_cc,read y_ch,read y_cc)
    ["ptr","trace",nrm,dt] -> ptr_trace (nrm == "u") (read dt)
    _ -> putStrLn (unlines help)
