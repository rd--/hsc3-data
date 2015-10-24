import Control.Exception {- base -}
import Control.Monad {- base -}
import Data.Bits {- base -}
import System.Exit {- base -}
import Text.Printf {- base -}

import Sound.OSC {- hosc -}
import Sound.SC3.Data.X11.Ptr {- hsc3-data -}

import qualified Graphics.X11.Xlib as X {- xlib -}

type R = Double

entry :: R -> R -> R -> String
entry tm x y = printf "%.3f,%.3f,%.3f" tm x y

has_mask :: Bits a => a -> a -> Bool
has_mask k m = (m .&. k) == k

trace :: X11 R -> (R,R,Int,Int) -> IO ()
trace x11 (t0,dt,rx',ry') = do
  tm <- time
  (rx,ry,x,y,m) <- x11_ptr_raw x11
  when (has_mask X.button3Mask m) exitSuccess
  when (rx /= rx' || ry /= ry') (putStrLn (entry (tm - t0) x y))
  pauseThread dt
  trace x11 (t0,dt,rx,ry)
  return ()

main :: IO ()
main = do
  putStrLn "tm,x,y"
  x11 <- x11_init ":0"
  t0 <- time
  finally (trace x11 (t0,0.001,0,0)) (x11_close x11)


