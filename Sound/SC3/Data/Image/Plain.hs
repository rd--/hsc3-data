module Sound.SC3.Data.Image.Plain where

import qualified Data.ByteString as B {- bytestring -}

import qualified Codec.Picture as I {- JuicyPixels -}
import qualified Codec.Picture.Types as I {- JuicyPixels -}

import qualified Sound.SC3.Data.Bitmap.PBM as D {- hsc3-data -}
import qualified Sound.SC3.Data.Bitmap.Type as D {- hsc3-data -}

import qualified Sound.File.NeXT as AU {- hsc3-sf -}
import qualified Sound.File.NeXT.Vector as AU {- hsc3-data -}
import qualified Sound.File.HSndFile as SF {- hsc3-sf-hsndfile -}

import qualified Data.Vector.Storable as V {- vector -}

import qualified Data.CG.Minus.Colour.Grey as C {- hcg-minus -}

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

type RGB n = (n,n,n)

w8_to_fractional :: Fractional n => I.Pixel8 -> n
w8_to_fractional = (/ 255) . fromIntegral

w8_to_f32 :: I.Pixel8 -> Float
w8_to_f32 = w8_to_fractional

w8_to_f64 :: I.Pixel8 -> Double
w8_to_f64 = w8_to_fractional

rgb8_to_rgb :: Fractional n => RGB8 -> RGB n
rgb8_to_rgb (I.PixelRGB8 r g b) = let f = w8_to_fractional in (f r,f g,f b)

img_row_rgb :: Fractional n => IMAGE -> Int -> [RGB n]
img_row_rgb i = map rgb8_to_rgb . img_row i

img_column_rgb :: Fractional n => IMAGE -> Int -> [RGB n]
img_column_rgb i = map rgb8_to_rgb . img_column i

img_column_order_rgb :: Fractional n => IMAGE -> [[RGB n]]
img_column_order_rgb i = map (img_column_rgb i) [0 .. I.imageWidth i - 1]

img_row_order_rgb :: Fractional n => IMAGE -> [[RGB n]]
img_row_order_rgb i = map (img_row_rgb i) [0 .. I.imageHeight i - 1]

-- * Greyscale

-- | Grey value, 0.0 is black & 1.0 is white.
type GREY = Double

-- | Channel selector
data CHANNEL = RED | GREEN | BLUE

-- | Extract channel.
rgb8_ch :: CHANNEL -> RGB8 -> I.Pixel8
rgb8_ch ch (I.PixelRGB8 r g b) =
    case ch of
      RED -> r
      GREEN -> g
      BLUE -> b

rgb8_to_gs_ch :: Fractional n => CHANNEL -> RGB8 -> n
rgb8_to_gs_ch ch = w8_to_fractional . rgb8_ch ch

rgb_to_gs_rec_709 ::  Fractional n => RGB8 -> n
rgb_to_gs_rec_709 = C.rgb_to_gs_luminosity C.luminosity_coef_rec_709 . rgb8_to_rgb

-- | Require R G and B values to be equal.
rgb8_to_gs_eq :: Fractional n => RGB8 -> Either RGB8 n
rgb8_to_gs_eq px =
    let (I.PixelRGB8 r g b) = px
    in if r == g && r == b then Right (w8_to_fractional r) else Left px

-- | 'error' variant.
rgb8_to_gs_eq' :: Fractional n => RGB8 -> n
rgb8_to_gs_eq' = either_err "rgb8_to_gs_eq" . rgb8_to_gs_eq

-- | 'GREY' to 'RGB8'.
--
-- > map gs_to_rgb8 [0.0,1.0] == [I.PixelRGB8 0 0 0,I.PixelRGB8 255 255 255]
gs_to_rgb8 :: RealFrac n => n -> RGB8
gs_to_rgb8 x = let x' = floor (x * 255) in I.PixelRGB8 x' x' x'

-- | Column order vector.
img_gs_vec_co :: (V.Storable n,RealFrac n) => (RGB8 -> n) -> IMAGE -> V.Vector n
img_gs_vec_co to_gs i =
    let (w,h) = img_dimensions i
        f n = let (x,y) = n `divMod` h in to_gs (I.pixelAt i x y)
    in V.generate (w * h) f

img_from_vec_co :: (V.Storable n,RealFrac n) => (Int,Int) -> V.Vector n -> IMAGE
img_from_vec_co (w,h) v =
    let f x y = gs_to_rgb8 (v V.! (x * h + y))
    in I.generateImage f w h

-- | Write greyscale image as NeXT audio file.  Each row is stored as a channel.
img_gs_write_sf :: (RGB8 -> Double) -> FilePath -> IMAGE -> IO ()
img_gs_write_sf to_gs fn i =
    let (w,h) = img_dimensions i
        v = img_gs_vec_co to_gs i
        hdr = SF.Header h w 44100
    in SF.write_au_f32_vec fn hdr v

-- | Write greyscale image as audio file.  Each row is stored as a channel.
img_gs_write_au :: (RGB8 -> Float) -> FilePath -> IMAGE -> IO ()
img_gs_write_au to_gs fn i =
    let (w,h) = img_dimensions i
        v = img_gs_vec_co to_gs i
        hdr = AU.Header w AU.Float 44100 h
    in AU.write_f32_vec fn (hdr,v)

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
  let SF.Header nc nf _ = hdr
  return (img_from_gs (nf,nc) (map (map realToFrac) ro))

-- | Read NeXT audio file as image, channels are rows.
img_gs_read_au :: FilePath -> IO IMAGE
img_gs_read_au fn = do
  (hdr,ro) <- AU.read fn
  let AU.Header nf _ _ nc = hdr
  return (img_from_gs (nf,nc) ro)

img_write_png :: FilePath -> IMAGE -> IO ()
img_write_png = I.writePng

-- * Black & white

-- | Black & white (black is 'True', white is 'False').
type BW = Bool

gs_to_bw_eq :: (Eq n, Num n) => e -> n -> Either e Bool
gs_to_bw_eq err x = if x == 0 then Right True else if x == 1 then Right False else Left err

-- | gs < 0.5 = True, gs > 0.5 = False.
--
-- > map gs_to_bw_mp [0,0.25,0.5,0.75,1]
gs_to_bw_mp :: (Fractional n,Ord n) => n -> Bool
gs_to_bw_mp = (< 0.5)

-- | Translates (0,0,0) to Black/True and (1,1,1) to White/False.
rgb8_to_bw_eq :: RGB8 -> Either RGB8 BW
rgb8_to_bw_eq c = either Left (gs_to_bw_eq c) (rgb8_to_gs_eq c)

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

rgb8_bw_inverse :: RGB8 -> RGB8
rgb8_bw_inverse (I.PixelRGB8 r g b) =
    case (r,g,b) of
      (255,255,255) -> I.PixelRGB8 0 0 0
      (0,0,0) -> I.PixelRGB8 255 255 255
      _ -> error "rgb8_bw_inverse: not B or W"

img_bw_inverse :: IMAGE -> IMAGE
img_bw_inverse = I.pixelMap rgb8_bw_inverse

img_bw_write_sf :: FilePath -> IMAGE -> IO ()
img_bw_write_sf fn = img_gs_write_sf (rgb8_to_gs_ch RED) fn . img_bw_inverse

-- * Miscellaneous

either_err :: Show e => String -> Either e c -> c
either_err nm = either (\err -> error (show (nm,err))) id

{-

let fn = "/home/rohan/cvs/uc/uc-26/daily-practice/2014-12-13/06.le.png"
let fn = "/home/rohan/uc/sp-id/jpeg/eliminator.jpeg"
let fn = "/home/rohan/cvs/uc/uc-26/daily-practice/2015-10-15.png"

i <- img_load fn
img_dimensions i == (120,36)

img_row i 0
let ro = img_row_order i
let gs = map (map rgb8_to_gs_eq') ro

img_bw_write_pbm "/tmp/t.pbm" i

let v = img_gs_vec_co (rgb8_to_gs RED) i
V.length v

img_gs_write_sf (rgb8_to_gs_ch RED) "/tmp/t.au" i

img_gs_write_au (rgb8_to_gs_ch RED) "/tmp/t.au" i

(img_bw_inverse i)

i' <- img_gs_read_sf "/tmp/t.au"
img_dimensions i'

import Sound.SC3
withSC3 (async (b_allocRead 0 "/tmp/t.au" 0 0))
withSC3 (async (b_allocReadChannel 0 "/tmp/t.au" 0 0 [200 .. 220]))
withSC3 (b_query1_unpack 0)

import Sound.SC3.Plot
withSC3 (plot_buffer_displace 0)
plot_sf "/tmp/t_00.au"
plot_sf "/tmp/m.au"

import Data.Bits
let gen n = map (fromIntegral . fromEnum . testBit n) [0..11]
let dat = map gen [0 .. 1999]
let hdr = AU.Header 12 AU.Float 44100 2000
AU.write "/tmp/t.au" hdr dat

-}
