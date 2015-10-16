module Sound.SC3.Data.Bitmap.PBM where

import qualified Codec.Image.PBM as I {- bitwise -}
import qualified Data.Array.BitArray as A {- bitwise -}
import qualified Data.ByteString as B {- bytestring -}

import Sound.SC3.Data.Bitmap.Type {- hsc3-data -}

-- | Load one image from a PBM(4) file.
read_pbm4_1 :: FilePath -> IO I.PBM
read_pbm4_1 nm = do
  b <- B.readFile nm
  case I.decodePBM b of
    Left _ -> error "read_pbm1"
    Right (i,_) -> return i

pbm_ascii :: I.PBM -> String
pbm_ascii = bitindices_show . pbm_to_bitindices

-- > pbm_print_ascii (prj_fn "02.pbm")
pbm_print_ascii :: FilePath -> IO ()
pbm_print_ascii nm = do
  pbm <- read_pbm4_1 nm
  putStrLn ("\n" ++ pbm_ascii pbm)

pbm_to_bitindices :: I.PBM -> Bitindices
pbm_to_bitindices i =
    let a = I.pbmPixels i
        w = I.pbmWidth i
        b = case A.bounds a of
              ((0,0),(r,_)) -> (r+1,w)
              _ -> error "pbm_to_bitindices: non zero indices?"
    in (b,map fst (filter snd (A.assocs a)))

