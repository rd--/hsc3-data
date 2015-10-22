module Sound.File.NeXT.Vector where

import Control.Monad {- base -}
import qualified Data.ByteString.Lazy as B {- bytestring -}
import Data.Int {- base -}
import qualified Data.Vector.Storable as V {- vector -}
import System.IO {- base -}

import qualified Sound.OSC.Coding.Byte as C {- hosc -}
import qualified Sound.File.NeXT as AU {- hsc3-sf -}

bs_section :: (Int64, Int64) -> B.ByteString -> B.ByteString
bs_section (st,nb) b = B.take nb (B.drop st b)

bs_f32 :: Integral a => B.ByteString -> a -> Float
bs_f32 b n = C.decode_f32 (bs_section (fromIntegral n * 4,4) b)

-- | Read a 32 bit floating point NeXT sound file.
--
-- > (hdr,vec) <- read_vec_f32 "/home/rohan/sw/hsc3-sf/au/mc-4-16.au"
-- > V.length vec == 64
read_vec_f32 :: FilePath -> IO (AU.Header, V.Vector Float)
read_vec_f32 fn = do
  h <- openFile fn ReadMode
  (hdr,off) <- AU.read_header h
  when (AU.encoding hdr /= AU.Float) (error "read_vec_f32: not float data")
  let ne = AU.frameCount hdr * AU.channelCount hdr
  b <- B.hGet h (ne * 4)
  hClose h
  print (off,ne)
  let vec = V.generate ne (bs_f32 b)
  return (hdr,vec)
