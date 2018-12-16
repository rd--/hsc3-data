-- | X11 pointer access
module Sound.SC3.Data.X11.Ptr where

import Foreign.C.Types {- base -}

import qualified Graphics.X11.Xlib as X {- X11 -}
import qualified Graphics.X11.Xlib.Extras as E {- X11 -}

-- | X11 connection state.
type X11 n = (X.Display,X.Window,(CInt,CInt),(n,n))

-- | Initialize X11 connection.
x11_init :: Fractional n => String -> IO (X11 n)
x11_init n = do
  d <- X.openDisplay n
  let r = X.defaultRootWindow d
  a <- E.getWindowAttributes d r
  let w = E.wa_width a
      h = E.wa_height a
      w_mul = 1.0 / fromIntegral w
      h_mul = 1.0 / fromIntegral h
  return (d, r, (w,h), (w_mul,h_mul))

-- | Close X11 connection.
x11_close :: X11 n -> IO ()
x11_close (d, _, _, _) = X.closeDisplay d

x11_ptr_raw :: (Ord n,Fractional n) => X11 n -> IO ((Int,Int),(n,n),(n,n),X.Modifier)
x11_ptr_raw (d, r, (_w,h), (w_mul,h_mul)) = do
  (_, _, _, _, _, x, y, mdf) <- X.queryPointer d r
  let u_mul = min w_mul h_mul
  return ((fromIntegral x,fromIntegral y)
         ,(fromIntegral x * u_mul,fromIntegral (h - y) * u_mul)
         ,(fromIntegral x * w_mul,fromIntegral (h - y) * h_mul)
         ,mdf)

-- | Read pointer location relative to root window.
x11_ptr :: (Ord n,Fractional n) => X11 n -> IO (n,n)
x11_ptr x11 = do
  (_,c,_,_) <- x11_ptr_raw x11
  return c
