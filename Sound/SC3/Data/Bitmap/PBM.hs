module Sound.SC3.Data.Bitmap.PBM where

import Data.Bits {- base -}
import qualified Data.ByteString as B {- bytestring -}
import Data.List {- base -}

import qualified Codec.Image.PBM as I {- bitwise -}
import qualified Data.Array.BitArray as A {- bitwise -}

import Sound.SC3.Data.Bitmap.Type {- hsc3-data -}

-- * PBM1

-- | Portable Bit Map (format 1, netpbm standard)
type PBM1 = String

-- | 'PBM1' of 'Bitarray'.
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

-- * PBM4

type PBM4 = I.PBM

-- | Load one image from a PBM(4) file.
read_pbm4_1 :: FilePath -> IO PBM4
read_pbm4_1 nm = do
  b <- B.readFile nm
  case I.decodePBM b of
    Left _ -> error "read_pbm1"
    Right (i,_) -> return i

pbm4_ascii :: PBM4 -> String
pbm4_ascii = bitindices_show . pbm4_to_bitindices

-- | 'pbm4_ascii' of 'read_pbm4_1'
pbm4_print_ascii :: FilePath -> IO ()
pbm4_print_ascii nm = do
  pbm <- read_pbm4_1 nm
  putStrLn ("\n" ++ pbm4_ascii pbm)

pbm4_to_bitindices :: PBM4 -> Bitindices
pbm4_to_bitindices i =
    let a = I.pbmPixels i
        w = I.pbmWidth i
        b = case A.bounds a of
              ((0,0),(r,_)) -> (r+1,w)
              _ -> error "pbm_to_bitindices: non zero indices?"
    in (b,map fst (filter snd (A.assocs a)))

