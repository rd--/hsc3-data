-- | X11 pointer access
module Sound.Sc3.Data.X11.Ptr where

import qualified Foreign.C.Types {- base -}

import qualified Graphics.X11.Xlib as Xlib {- X11 -}
import qualified Graphics.X11.Xlib.Extras as Xlib.Extras {- X11 -}

-- | X11 connection state, (display,root-window,(width,height),1/(width,height))
type X11 n = (Xlib.Display, Xlib.Window, (Foreign.C.Types.CInt, Foreign.C.Types.CInt), (n, n))

-- | Initialize X11 connection.
x11_init :: Fractional n => String -> IO (X11 n)
x11_init n = do
  d <- Xlib.openDisplay n
  let r = Xlib.defaultRootWindow d
  a <- Xlib.Extras.getWindowAttributes d r
  let w = Xlib.Extras.wa_width a
      h = Xlib.Extras.wa_height a
      w_mul = 1.0 / fromIntegral w
      h_mul = 1.0 / fromIntegral h
  return (d, r, (w, h), (w_mul, h_mul))

-- | Close X11 connection.
x11_close :: X11 n -> IO ()
x11_close (d, _, _, _) = Xlib.closeDisplay d

-- | Read pointer as (raw,location relative to root window.
x11_ptr_raw :: (Ord n, Fractional n) => X11 n -> IO ((Int, Int), (n, n), (n, n), Xlib.Modifier)
x11_ptr_raw (d, r, (_w, h), (w_mul, h_mul)) = do
  (_, _, _, _, _, x, y, mdf) <- Xlib.queryPointer d r
  let u_mul = min w_mul h_mul
  return
    ( (fromIntegral x, fromIntegral y)
    , (fromIntegral x * u_mul, fromIntegral (h - y) * u_mul)
    , (fromIntegral x * w_mul, fromIntegral (h - y) * h_mul)
    , mdf
    )

-- | Read pointer location relative to root window.
x11_ptr :: (Ord n, Fractional n) => X11 n -> IO (n, n)
x11_ptr x11 = do
  (_, c, _, _) <- x11_ptr_raw x11
  return c
