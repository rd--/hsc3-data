-- | X11 pointer access
module Sound.SC3.Data.X11.Ptr where

import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xlib.Extras as X

-- | X11 connection state.
type X n = (X.Display,X.Window,n,n)

-- | Initialize X11 connection.
x_init :: Fractional n => String -> IO (X n)
x_init n = do
  d <- X.openDisplay n
  let r = X.defaultRootWindow d
  a <- X.getWindowAttributes d r
  let rw = 1.0 / fromIntegral (X.wa_width a)
      rh = 1.0 / fromIntegral (X.wa_height a)
  return (d, r, rw, rh)

-- | Close X11 connection.
x_close :: X n -> IO ()
x_close (d, _, _, _) = X.closeDisplay d

-- | Read pointer location relative to root window.
x_ptr :: Fractional n => X n -> IO (n,n)
x_ptr (d, r, rw, rh) = do
  (_, _, _, _, _, dx, dy, _) <- X.queryPointer d r
  let x' = fromIntegral dx * rw
      y' = 1.0 - (fromIntegral dy * rh)
  return (x', y')
