{- | /dev/urandom

"The random number generator gathers environmental noise from device drivers and other sources into an entropy pool."

This is an appropriate source of a seed for a psuedo random number generator, c.f. "Sound.Sc3.Common.Random".
-}
module Sound.Sc3.Data.Urandom where

import Control.Monad {- base -}
import Data.Int {- base -}
import System.IO {- base -}

import qualified Data.Binary as Binary {- binary -}
import qualified Data.ByteString.Lazy as ByteString {- bytestring -}

-- | Read /n/ bytes from @/dev/urandom@.
ur_bytes :: Int -> IO ByteString.ByteString
ur_bytes n = withFile "/dev/urandom" ReadMode (`ByteString.hGet` n)

-- | urandom 'Int32'.
ur_int32 :: IO Int32
ur_int32 = fmap Binary.decode (ur_bytes 4)

-- | /k/-element list of /n/ byte sequences from urandom.
ur_byte_set :: Int -> Int -> IO [ByteString.ByteString]
ur_byte_set n k =
  let f h = replicateM k (ByteString.hGet h n)
  in withFile "/dev/urandom" ReadMode f

{- | /k/-element list of urandom 'Int32'.

> ur_int32_set 3
-}
ur_int32_set :: Int -> IO [Int32]
ur_int32_set = fmap (map Binary.decode) . ur_byte_set 4

{- | Convert 'Int32' into range (0,1).

> fmap (map int32_to_normal) (ur_int32_set 3)
-}
int32_to_normal :: Fractional n => Int32 -> n
int32_to_normal n =
  let m = fromIntegral (maxBound :: Int32)
  in (fromIntegral n + m) / (m * 2)

{- | 'int32_to_normal' of 'ur_int32_set'.

> ur_real_set 3
-}
ur_real_set :: Fractional n => Int -> IO [n]
ur_real_set = fmap (map int32_to_normal) . ur_int32_set
