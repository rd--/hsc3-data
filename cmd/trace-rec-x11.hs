import Control.Exception {- base -}
import Control.Monad {- base -}
import Data.Bits {- base -}
import System.Environment {- base -}
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

-- > trace_rec_x11 False 0.01
trace_rec_x11 :: Bool -> R -> IO ()
trace_rec_x11 u dt = do
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

main :: IO ()
main = do
  a <- getArgs
  case a of
    [nrm,dt] -> trace_rec_x11 (nrm == "u") (read dt)
    _ -> putStrLn "trace-rec-x11 u|n delta-time:float"
