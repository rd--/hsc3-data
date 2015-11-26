import Control.Exception {- base -}
import Control.Monad {- base -}
import System.Environment {- base -}

import Sound.OSC.FD {- hosc -}
import Sound.SC3.Data.X11.Ptr {- hsc3-data -}
import qualified Sound.Midi.Type as M {- midi-osc -}
import qualified Sound.Midi.OSC as M {- midi-osc -}

type R = Double

to_cc_msg :: Int -> Int -> Double -> Message
to_cc_msg ch cc r = M.m_message (-1) (M.Control_Change ch cc (floor (r * 127)))

-- > x11_ptr_to_midi_osc (0.01,0,1,0,2)
x11_ptr_to_midi_osc :: (R, Int, Int, Int, Int) -> IO ()
x11_ptr_to_midi_osc (dt,x_ch,x_cc,y_ch,y_cc) = do
  x11 <- x11_init ":0"
  fd <- openUDP "127.0.0.1" 57150 -- midi-osc
  let recur (rx',ry') = do
        (rx,ry,x,y,_) <- x11_ptr_raw x11
        when (rx /= rx') (sendMessage fd (to_cc_msg x_ch x_cc x))
        when (ry /= ry') (sendMessage fd (to_cc_msg y_ch y_cc y))
        pauseThread dt
        recur (rx,ry)
  finally (recur (0,0)) (x11_close x11 >> close fd)

main :: IO ()
main = do
  a <- getArgs
  case a of
    [dt,x_ch,x_cc,y_ch,y_cc] ->
        x11_ptr_to_midi_osc (read dt,read x_ch,read x_cc,read y_ch,read y_cc)
    _ -> putStrLn "x11-ptr-to-midi-osc dt:float x-ch:int x-cc:int y-ch:int y-cc:int"
