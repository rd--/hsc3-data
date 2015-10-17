-- | PBM image files.
module Sound.SC3.Data.Bitmap.PBM where

import Data.Bits {- base -}
import qualified Data.ByteString as B {- bytestring -}
import Data.List {- base -}
import Data.Word {- base -}

import qualified Codec.Image.PBM as I {- bitwise -}
import qualified Data.Array.BitArray as A {- bitwise -}

import Sound.SC3.Data.Bitmap.Type {- hsc3-data -}

-- * PBM1

-- | Portable Bit Map (format 1, netpbm standard)
type PBM1 = String

-- | 'PBM1' of 'Bitarray', note that black is written @1@ and white @0@.
bitarray_pbm1 :: Bitarray -> PBM1
bitarray_pbm1 ((h,w),a) =
    let ty = "P1"
        dm = show w ++ " " ++ show h
        f = intersperse ' ' . map (bit_to_char ('1','0'))
    in unlines ([ty,dm] ++ map f a)

-- | 'PBM1' of 'Bitmap'.
bitmap_pbm1 :: FiniteBits b => Bitmap b -> PBM1
bitmap_pbm1 = bitarray_pbm1 . bitmap_to_bitarray

-- | 'PBM1' of 'Bitindices'.
bitindices_pbm1 :: Bitindices -> PBM1
bitindices_pbm1 = bitarray_pbm1 . bitindices_to_bitarray

-- * PBM 1 & 4

type PBM = I.PBM

w8_char :: Word8 -> Char
w8_char = toEnum . fromIntegral

bs_string :: B.ByteString -> String
bs_string = map w8_char . B.unpack

decode_pbm1 :: B.ByteString -> Either String I.PBM
decode_pbm1 = either (Left . show) (Right . fst) . I.decodePlainPBM . bs_string

decode_pbm4 :: B.ByteString -> Either String I.PBM
decode_pbm4 = either (Left . show) (Right . fst) . I.decodePBM

-- | Load one image from a PBM(1) or PBM(4) file.
read_pbm :: FilePath -> IO PBM
read_pbm nm = do
  b <- B.readFile nm
  let df = case B.index b 1 of
             49 -> decode_pbm1
             52 -> decode_pbm4
             _ -> error "read_pbm: not P1 or P4?"
  case df b of
    Left err -> error ("read_pbm: " ++ err)
    Right i -> return i

pbm_ascii :: PBM -> String
pbm_ascii = bitindices_show . pbm_to_bitindices

-- | 'pbm_ascii' of 'read_pbm'
pbm_print_ascii :: FilePath -> IO ()
pbm_print_ascii nm = do
  pbm <- read_pbm nm
  putStrLn ("\n" ++ pbm_ascii pbm)

pbm_to_bitindices :: PBM -> Bitindices
pbm_to_bitindices i =
    let a = I.pbmPixels i
        w = I.pbmWidth i
        b = case A.bounds a of
              ((0,0),(r,_)) -> (r+1,w)
              _ -> error "pbm_to_bitindices: non zero indices?"
    in (b,map fst (filter snd (A.assocs a)))

