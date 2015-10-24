-- | X11 pointer access
module Sound.SC3.Data.X11.Ptr where

import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xlib.Extras as E

-- | X11 connection state.
type X11 n = (X.Display,X.Window,n,n)

-- | Initialize X11 connection.
x11_init :: Fractional n => String -> IO (X11 n)
x11_init n = do
  d <- X.openDisplay n
  let r = X.defaultRootWindow d
  a <- E.getWindowAttributes d r
  let rw = 1.0 / fromIntegral (E.wa_width a)
      rh = 1.0 / fromIntegral (E.wa_height a)
  return (d, r, rw, rh)

-- | Close X11 connection.
x11_close :: X11 n -> IO ()
x11_close (d, _, _, _) = X.closeDisplay d

x11_ptr_raw :: Fractional n => X11 n -> IO (Int,Int,n,n,X.Modifier)
x11_ptr_raw (d, r, rw, rh) = do
  (_, _, _, _, _, x, y, m) <- X.queryPointer d r
  return (fromIntegral x,fromIntegral y
         ,fromIntegral x * rw,1.0 - (fromIntegral y * rh)
         ,fromIntegral m)

-- | Read pointer location relative to root window.
x11_ptr :: Fractional n => X11 n -> IO (n,n)
x11_ptr x11 = do
  (_,_,x,y,_) <- x11_ptr_raw x11
  return (x, y)
