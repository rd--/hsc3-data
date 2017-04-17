module Sound.SC3.Data.URandom where

import Control.Monad {- base -}
import qualified Data.ByteString.Lazy as B {- bytestring -}
import Data.Int {- base -}
import qualified Data.Binary as Binary {- binary -}
import System.IO {- base -}

-- | Read /n/ bytes from @/dev/urandom@.
ur_bytes :: Int -> IO B.ByteString
ur_bytes n = withFile "/dev/urandom" ReadMode (flip B.hGet n)

-- | urandom 'Int32'.
ur_int32 :: IO Int32
ur_int32 = fmap Binary.decode (ur_bytes 4)

-- | /k/-element list of /n/ byte sequences from urandom.
ur_byte_set :: Int -> Int -> IO [B.ByteString]
ur_byte_set n k =
    let f h = replicateM k (B.hGet h n)
    in withFile "/dev/urandom" ReadMode f

-- | /k/-element list of urandom 'Int32'.
--
-- > ur_int32_set 3
ur_int32_set :: Int -> IO [Int32]
ur_int32_set = fmap (map Binary.decode) . ur_byte_set 4

-- | Convert 'Int32' into range (0,1).
--
-- > fmap (map int32_to_normal) (ur_int32_set 3)
int32_to_normal :: Fractional n => Int32 -> n
int32_to_normal n =
    let m = fromIntegral (maxBound::Int32)
    in (fromIntegral n + m) / (m * 2)

-- | 'int32_to_normal' of 'ur_int32_set'.
--
-- > ur_real_set 3
ur_real_set :: Fractional n => Int -> IO [n]
ur_real_set = fmap (map int32_to_normal) . ur_int32_set

