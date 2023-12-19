module Sound.Sc3.Data.Image.Plain where

import Data.Function {- base -}
import Data.List {- base -}
import Data.Word {- base -}

import qualified Data.Array.Unboxed as Array {- array -}
import qualified Data.ByteString as ByteString {- bytestring -}
import qualified Data.Vector.Storable as Vector {- vector -}

import qualified Codec.Picture as I {- JuicyPixels -}
import qualified Codec.Picture.Types as I {- JuicyPixels -}

import qualified Music.Theory.List as List {- hmt-base -}

import qualified Sound.File.HSndFile as Sf.SndFile {- hsc3-sf-hsndfile -}
import qualified Sound.File.Next as Sf.Au {- hsc3-sf -}

import qualified Data.Cg.Minus.Colour.Grey as Colour.Grey {- hcg-minus -}

import qualified Sound.Sc3.Data.Bitmap.Pbm as Pbm {- hsc3-data -}
import qualified Sound.Sc3.Data.Bitmap.Type as Bitmap {- hsc3-data -}
import qualified Sound.Sc3.Data.Image.Pgm as Pgm {- hsc3-data -}
import qualified Sound.Sc3.Data.Image.Type as Image {- hsc3-data -}

-- * Image

-- | Packed 24-bit RGByteString.
type Rgb24 = I.PixelRGB8

-- | Array of Rgb24.
type Image = I.Image Rgb24

img_load :: FilePath -> IO Image
img_load fn = do
  b <- ByteString.readFile fn
  case I.decodeImage b of
    Left err -> error err
    Right (I.ImageY8 img) -> return (I.promoteImage img)
    Right (I.ImageRGB8 img) -> return img
    Right (I.ImageCMYK8 img) -> return (I.convertImage img)
    Right (I.ImageYCbCr8 img) -> return (I.convertImage img)
    Right _ -> error "img_load: not Y8|RGB8|CMYK8|YCbCr8 image"

img_write_png :: FilePath -> Image -> IO ()
img_write_png = I.writePng

-- | Dimensions as (width,height) pair.
img_dimensions :: Image -> Image.Dimensions
img_dimensions i = (I.imageWidth i, I.imageHeight i)

img_index_safe :: Image -> Image.Ix -> Rgb24
img_index_safe i (c, r) =
  let (w, h) = img_dimensions i
  in if c < 0 || c >= w || r < 0 || r >= h
      then error "img_index_safe: domain error"
      else I.pixelAt i c r

img_index :: Image -> Image.Ix -> Rgb24
img_index i (c, r) = I.pixelAt i c r

img_row :: Image -> Int -> [Rgb24]
img_row i r =
  let (w, h) = img_dimensions i
  in if r >= h
      then error "img_row: domain error"
      else map (\c -> I.pixelAt i c r) [0 .. w - 1]

img_column :: Image -> Int -> [Rgb24]
img_column i c =
  let (w, h) = img_dimensions i
  in if c >= w
      then error "img_column: domain error"
      else map (I.pixelAt i c) [0 .. h - 1]

img_column_order :: Image -> [[Rgb24]]
img_column_order i = map (img_column i) [0 .. I.imageWidth i - 1]

img_row_order :: Image -> [[Rgb24]]
img_row_order i = map (img_row i) [0 .. I.imageHeight i - 1]

-- * Rgb

type Rgb n = (n, n, n)

rgb24_unpack :: Rgb24 -> Rgb Word8
rgb24_unpack (I.PixelRGB8 r g b) = (r, g, b)

rgb24_pack :: Rgb Word8 -> Rgb24
rgb24_pack (r, g, b) = I.PixelRGB8 r g b

w8_to_fractional :: Fractional n => Word8 -> n
w8_to_fractional = (/ 255) . fromIntegral

w8_to_f32 :: Word8 -> Float
w8_to_f32 = w8_to_fractional

w8_to_f64 :: Word8 -> Double
w8_to_f64 = w8_to_fractional

rgb24_to_rgb :: Fractional n => Rgb24 -> Rgb n
rgb24_to_rgb (I.PixelRGB8 r g b) = let f = w8_to_fractional in (f r, f g, f b)

img_row_rgb :: Fractional n => Image -> Int -> [Rgb n]
img_row_rgb i = map rgb24_to_rgb . img_row i

img_column_rgb :: Fractional n => Image -> Int -> [Rgb n]
img_column_rgb i = map rgb24_to_rgb . img_column i

img_column_order_rgb :: Fractional n => Image -> [[Rgb n]]
img_column_order_rgb i = map (img_column_rgb i) [0 .. I.imageWidth i - 1]

img_row_order_rgb :: Fractional n => Image -> [[Rgb n]]
img_row_order_rgb i = map (img_row_rgb i) [0 .. I.imageHeight i - 1]

-- * Greyscale

-- | Grey value, 0.0 is black & 1.0 is white.
type Grey = Double

-- | Channel selector
data Channel = Red | Green | Blue

-- | Extract channel.
rgb24_ch :: Channel -> Rgb24 -> Word8
rgb24_ch ch (I.PixelRGB8 r g b) =
  case ch of
    Red -> r
    Green -> g
    Blue -> b

rgb24_to_gs_ch :: Fractional n => Channel -> Rgb24 -> n
rgb24_to_gs_ch ch = w8_to_fractional . rgb24_ch ch

rgb_to_gs_rec_709 :: Fractional n => Rgb24 -> n
rgb_to_gs_rec_709 = Colour.Grey.rgb_to_gs_luminosity Colour.Grey.luminosity_coef_rec_709 . rgb24_to_rgb

-- | Require R G and B values to be equal.
rgb24_to_gs_eq :: Fractional n => Rgb24 -> Either Rgb24 n
rgb24_to_gs_eq px =
  let (I.PixelRGB8 r g b) = px
  in if r == g && r == b then Right (w8_to_fractional r) else Left px

-- | 'error' variant.
rgb24_to_gs_eq' :: Fractional n => Rgb24 -> n
rgb24_to_gs_eq' = either_err "rgb24_to_gs_eq" . rgb24_to_gs_eq

{- | 'Grey' to 'Rgb24'.

>>> map gs_to_rgb24 [0.0,1.0]
[PixelRGB8 0 0 0,PixelRGB8 255 255 255]
-}
gs_to_rgb24 :: RealFrac n => n -> Rgb24
gs_to_rgb24 x = let x' = floor (x * 255) in I.PixelRGB8 x' x' x'

-- | Column order vector.
img_gs_vec_co :: Vector.Storable n => (Rgb24 -> n) -> Image -> Vector.Vector n
img_gs_vec_co to_gs i =
  let (w, h) = img_dimensions i
      f n = let (x, y) = n `divMod` h in to_gs (I.pixelAt i x y)
  in Vector.generate (w * h) f

-- | Construct GS 'Image' from column order 'Vector.Vector'.
img_from_vec_co :: (Vector.Storable n, RealFrac n) => Image.Dimensions -> Vector.Vector n -> Image
img_from_vec_co (w, h) v =
  let f x y = gs_to_rgb24 (v Vector.! Image.ix_to_linear_co (w, h) (x, y))
  in I.generateImage f w h

-- | Write greyscale image as NeXT audio file.  Each row is stored as a channel.
img_gs_write_sf :: (Rgb24 -> Double) -> FilePath -> Image -> IO ()
img_gs_write_sf to_gs fn i =
  let (w, h) = img_dimensions i
      v = img_gs_vec_co to_gs i
      hdr = Sf.SndFile.Sf_Header h w 44100 Sf.SndFile.fmt_au_f32_be
  in Sf.SndFile.write_vec fn hdr v

-- | Write greyscale image as NeXT audio file.  Each row is stored as a channel.
img_gs_write_au :: (Rgb24 -> Float) -> FilePath -> Image -> IO ()
img_gs_write_au to_gs fn i =
  let (w, h) = img_dimensions i
      v = img_gs_vec_co to_gs i
      hdr = Sf.Au.Sf_Header w Sf.Au.Float 44100 h
  in Sf.Au.au_write_f32_vec fn (hdr, v)

img_from_gs :: Image.Dimensions -> [[Grey]] -> Image
img_from_gs (w, h) ro =
  let ro' = map (map gs_to_rgb24) ro
      f x y = (ro' !! y) !! x
  in I.generateImage f w h

-- | Derive dimesions from row-order regular list array.
ro_derive_dimensions :: [[a]] -> Image.Dimensions
ro_derive_dimensions ro =
  let w = length (List.head_err ro)
      h = length ro
  in (w, h)

-- | Read NeXT audio file as image, channels are rows.
img_gs_read_sf :: FilePath -> IO Image
img_gs_read_sf fn = do
  (hdr, ro) <- Sf.SndFile.read fn
  let Sf.SndFile.Sf_Header nc nf _ _ = hdr
  return (img_from_gs (nf, nc) (map (map realToFrac) ro))

-- | Read NeXT audio file as image, channels are rows.
img_gs_read_au :: FilePath -> IO Image
img_gs_read_au fn = do
  (hdr, ro) <- Sf.Au.au_read fn
  let Sf.Au.Sf_Header nf _ _ nc = hdr
  return (img_from_gs (nf, nc) ro)

-- | Write 8-bit or 16-bit Pgm5 file.
img_write_pgm5 :: Pgm.Depth -> (Rgb24 -> Grey) -> FilePath -> Image -> IO ()
img_write_pgm5 d to_gs fn i =
  let (w, h) = img_dimensions i
      z = case d of
        8 -> 255
        16 -> 65535
        _ -> error "img_write_pgm5: depth not 8 or 16"
      f = round . (* z) . to_gs
      l = [((r, c), f (I.pixelAt i c r)) | r <- [0 .. h - 1], c <- [0 .. w - 1]]
      a = Array.array ((0, 0), (h - 1, w - 1)) l
  in Pgm.pgm5_save_0 fn (d, a)

-- * Black & white

-- | Black & white (black is 'True', white is 'False').
type Bw = Bool

gs_to_bw_eq :: (Eq n, Num n) => e -> n -> Either e Bool
gs_to_bw_eq err x = if x == 0 then Right True else if x == 1 then Right False else Left err

{- | gs < 0.5 = True, gs > 0.5 = False.

>>> map gs_to_bw_mp [0,0.25,0.5,0.75,1]
[True,True,False,False,False]
-}
gs_to_bw_mp :: (Fractional n, Ord n) => n -> Bool
gs_to_bw_mp = (< 0.5)

{- | Translates (0,0,0) to Black/True and (1,1,1) to White/False, any
other inputs are errors.
-}
rgb24_to_bw_eq :: Rgb24 -> Either Rgb24 Bw
rgb24_to_bw_eq c = either Left (gs_to_bw_eq c) (rgb24_to_gs_eq c :: Either Rgb24 Double)

-- | Error variant.
rgb24_to_bw_eq' :: I.PixelRGB8 -> Bw
rgb24_to_bw_eq' = either_err "rgb24_to_bw_eq" . rgb24_to_bw_eq

-- | Black & white image to 'Bitmap.Bitindices' using given reduction function.
img_bw_to_bitindices' :: (Rgb24 -> Bool) -> Image -> Bitmap.Bitindices
img_bw_to_bitindices' to_bw i =
  let (w, h) = img_dimensions i
      f ix (x, y) =
        if y >= h
          then ix
          else
            if x >= w
              then f ix (0, y + 1)
              else
                let ix' = if to_bw (I.pixelAt i x y) then (y, x) : ix else ix
                in f ix' (x + 1, y)
  in ((h, w), f [] (0, 0))

-- | 'img_bw_to_bitindices'' of 'rgb24_to_bw_eq''.
img_bw_to_bitindices :: Image -> Bitmap.Bitindices
img_bw_to_bitindices = img_bw_to_bitindices' rgb24_to_bw_eq'

-- | Black & white image to 'Bitmap.Bitarray' using given reduction function.
img_bw_to_bitarray' :: (Rgb24 -> Bw) -> Image -> Bitmap.Bitarray
img_bw_to_bitarray' f i =
  let (w, h) = img_dimensions i
      ro = img_row_order i
      ro' = map (map f) ro
  in ((h, w), ro')

-- | 'img_bw_to_bitarray'' of 'rgb24_to_bw_eq''.
img_bw_to_bitarray :: Image -> Bitmap.Bitarray
img_bw_to_bitarray = img_bw_to_bitarray' rgb24_to_bw_eq'

img_bw_write_pbm1 :: FilePath -> Image -> IO ()
img_bw_write_pbm1 fn = writeFile fn . Pbm.bitarray_pbm1 . img_bw_to_bitarray

img_bw_write_pbm4 :: (Rgb24 -> Bw) -> FilePath -> Image -> IO ()
img_bw_write_pbm4 f pbm_fn =
  Pbm.pbm4_write pbm_fn
    . Pbm.bitindices_to_pbm
    . img_bw_to_bitindices' f

rgb24_bw_inverse :: Rgb24 -> Rgb24
rgb24_bw_inverse (I.PixelRGB8 r g b) =
  case (r, g, b) of
    (255, 255, 255) -> I.PixelRGB8 0 0 0
    (0, 0, 0) -> I.PixelRGB8 255 255 255
    _ -> error "rgb24_bw_inverse: not B or W"

img_bw_inverse :: Image -> Image
img_bw_inverse = I.pixelMap rgb24_bw_inverse

img_bw_write_sf :: FilePath -> Image -> IO ()
img_bw_write_sf fn = img_gs_write_sf (rgb24_to_gs_ch Red) fn . img_bw_inverse

-- * Query

-- | List of uniqe colours with location of initial occurence, in row-order.
img_uniq_colours :: Image -> [(Image.Ix, Rgb24)]
img_uniq_colours i =
  let dm = img_dimensions i
      f r u l =
        case l of
          [] -> reverse r
          (ix : l') ->
            let c = img_index i ix
            in if c `elem` u
                then f r u l'
                else f ((ix, c) : r) (c : u) l'
  in f [] [] (Image.img_indices dm)

-- | Grouped by row.
img_uniq_colours_gr :: Image -> [[(Image.Ix, Rgb24)]]
img_uniq_colours_gr = groupBy ((==) `on` (snd . fst)) . img_uniq_colours

-- * Miscellaneous

either_err :: Show e => String -> Either e c -> c
either_err nm = either (\err -> error (show (nm, err))) id
