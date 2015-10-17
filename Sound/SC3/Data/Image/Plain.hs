module Sound.SC3.Data.Image.Plain where

import qualified Data.ByteString as B {- bytestring -}

import qualified Codec.Picture as I {- JuicyPixels -}
import qualified Codec.Picture.Types as I {- JuicyPixels -}

import qualified Sound.SC3.Data.Bitmap.PBM as D {- hsc3-data -}
import qualified Sound.SC3.Data.Bitmap.Type as D {- hsc3-data -}

import qualified Sound.File.NeXT as SF {- hsc3-sf -}

-- * IMAGE

type RGB8 = I.PixelRGB8
type IMAGE = I.Image RGB8

img_load :: FilePath -> IO IMAGE
img_load fn = do
  b <- B.readFile fn
  case I.decodeImage b of
    Left err -> error err
    Right (I.ImageY8 img) -> return (I.promoteImage img)
    Right (I.ImageRGB8 img) -> return img
    Right (I.ImageCMYK8 img) -> return (I.convertImage img)
    Right (I.ImageYCbCr8 img) -> return (I.convertImage img)
    Right _ -> error "img_load: not Y8|RGB8|CMYK8|YCbCr8 image"

-- | Dimensions as (width,height) pair.
img_dimensions :: IMAGE -> (Int,Int)
img_dimensions i = (I.imageWidth i,I.imageHeight i)

img_row :: IMAGE -> Int -> [RGB8]
img_row i r =
    let (w,h) = img_dimensions i
    in if r >= h
       then error "img_row: domain error"
       else map (\c -> I.pixelAt i c r) [0 .. w - 1]

img_column :: IMAGE -> Int -> [RGB8]
img_column i c =
    let (w,h) = img_dimensions i
    in if c >= w
       then error "img_column: domain error"
       else map (\r -> I.pixelAt i c r) [0 .. h - 1]

img_column_order :: IMAGE -> [[RGB8]]
img_column_order i = map (img_column i) [0 .. I.imageWidth i - 1]

img_row_order :: IMAGE -> [[RGB8]]
img_row_order i = map (img_row i) [0 .. I.imageHeight i - 1]

-- * RGB

type T3 a = (a,a,a)
type RGB = T3 Double

w8_to_f :: I.Pixel8 -> Double
w8_to_f = (/ 255) . fromIntegral

rgb8_to_rgb :: RGB8 -> RGB
rgb8_to_rgb (I.PixelRGB8 r g b) = (w8_to_f r,w8_to_f g,w8_to_f b)

img_row_rgb :: IMAGE -> Int -> [RGB]
img_row_rgb i = map rgb8_to_rgb . img_row i

img_column_rgb :: IMAGE -> Int -> [RGB]
img_column_rgb i = map rgb8_to_rgb . img_column i

img_column_order_rgb :: IMAGE -> [[RGB]]
img_column_order_rgb i = map (img_column_rgb i) [0 .. I.imageWidth i - 1]

img_row_order_rgb :: IMAGE -> [[RGB]]
img_row_order_rgb i = map (img_row_rgb i) [0 .. I.imageHeight i - 1]

-- * Greyscale

-- | Grey value, 0.0 is black & 1.0 is white.
type GREY = Double

-- | Require R G and B values to be equal.
rgb8_to_gs_eq :: RGB8 -> Either RGB8 GREY
rgb8_to_gs_eq px =
    let (I.PixelRGB8 r g b) = px
    in if r == g && r == b then Right (w8_to_f r) else Left px

-- | 'error' variant.
rgb8_to_gs_eq' :: RGB8 -> GREY
rgb8_to_gs_eq' = either_err "rgb8_to_gs_eq" . rgb8_to_gs_eq

-- | 'GREY' to 'RGB8'.
--
-- > map gs_to_rgb8 [0.0,1.0] == [I.PixelRGB8 0 0 0,I.PixelRGB8 255 255 255]
gs_to_rgb8 :: GREY -> RGB8
gs_to_rgb8 x = let x' = floor (x * 255) in I.PixelRGB8 x' x' x'

-- | Write greyscale image as audio file.  Each row is stored as a channel.
img_gs_write_sf :: FilePath -> IMAGE -> IO ()
img_gs_write_sf fn i =
    let (w,h) = img_dimensions i
        ro = img_row_order i
        ro' = map (map rgb8_to_gs_eq') ro
        hdr = SF.Header w SF.Float 44100 h
    in SF.write fn hdr ro'

img_from_gs :: (Int,Int) -> [[GREY]] -> IMAGE
img_from_gs (w,h) ro =
    let ro' = map (map gs_to_rgb8) ro
        f x y = (ro' !! y) !! x
    in I.generateImage f w h

ro_derive_dimensions :: [[a]] -> (Int,Int)
ro_derive_dimensions ro =
    let w = length (head ro)
        h = length ro
    in (w,h)

-- | Read NeXT audio file as image, channels are rows.
img_gs_read_sf :: FilePath -> IO IMAGE
img_gs_read_sf fn = do
  (hdr,ro) <- SF.read fn
  let SF.Header nf _ _ nc = hdr
  return (img_from_gs (nf,nc) ro)

img_write_png :: FilePath -> IMAGE -> IO ()
img_write_png = I.writePng

-- * Black & white

-- | Black & white (black is 'True', white is 'False').
type BW = Bool

-- | Translates (0,0,0) to Black/True and (1,1,1) to White/False.
rgb8_to_bw_eq :: RGB8 -> Either RGB8 BW
rgb8_to_bw_eq c =
    let f x = if x == 0 then Right True else if x == 1 then Right False else Left c
    in either Left f (rgb8_to_gs_eq c)

rgb8_to_bw_eq' :: I.PixelRGB8 -> BW
rgb8_to_bw_eq' = either_err "rgb8_to_bw_eq" . rgb8_to_bw_eq

-- | Black & white image to 'D.Bitarray'.
img_bw_to_bitarray :: IMAGE -> D.Bitarray
img_bw_to_bitarray i =
    let (w,h) = img_dimensions i
        ro = img_row_order i
        ro' = map (map rgb8_to_bw_eq') ro
    in ((h,w),ro')

img_bw_write_pbm :: FilePath -> IMAGE -> IO ()
img_bw_write_pbm fn = writeFile fn . D.bitarray_pbm1 . img_bw_to_bitarray

-- * Miscellaneous

either_err :: Show e => String -> Either e c -> c
either_err nm = either (\err -> error (show (nm,err))) id

{-

let fn = "/home/rohan/cvs/uc/uc-26/daily-practice/2014-12-13/06.le.png"
let fn = "/home/rohan/uc/sp-id/jpeg/eliminator.jpeg"

i <- img_load fn
img_dimensions i == (120,36)

img_row i 0
let ro = img_row_order i
let gs = map (map rgb8_to_gs_eq') ro

img_bw_write_pbm "/tmp/t.pbm" i

img_gs_write_sf "/tmp/t.au" i

i' <- img_gs_read_sf "/tmp/t.au"
img_dimensions i'

import Sound.SC3
withSC3 (async (b_allocRead 0 "/tmp/t.au" 0 0))

import Sound.SC3.Plot
withSC3 (plot_buffer_displace 0)

-}
