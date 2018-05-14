-- | X11 pointer access
module Sound.SC3.Data.X11.Ptr where

import Foreign.C.Types {- base -}
import qualified Graphics.X11.Xlib as X {- X11 -}
import qualified Graphics.X11.Xlib.Extras as E {- X11 -}

-- | X11 connection state.
type X11 n = (X.Display,X.Window,CInt,n)

-- | Initialize X11 connection.
x11_init :: Fractional n => String -> IO (X11 n)
x11_init n = do
  d <- X.openDisplay n
  let r = X.defaultRootWindow d
  a <- E.getWindowAttributes d r
  let h = E.wa_height a
      mul = 1.0 / fromIntegral (max (E.wa_width a) h)
  return (d, r, h, mul)

-- | Close X11 connection.
x11_close :: X11 n -> IO ()
x11_close (d, _, _, _) = X.closeDisplay d

x11_ptr_raw :: Fractional n => X11 n -> IO (Int,Int,n,n,X.Modifier)
x11_ptr_raw (d, r, h, mul) = do
  (_, _, _, _, _, x, y, m) <- X.queryPointer d r
  return (fromIntegral x,fromIntegral y
         ,fromIntegral x * mul,fromIntegral (h - y) * mul
         ,fromIntegral m)

-- | Read pointer location relative to root window.
x11_ptr :: Fractional n => X11 n -> IO (n,n)
x11_ptr x11 = do
  (_,_,x,y,_) <- x11_ptr_raw x11
  return (x, y)
