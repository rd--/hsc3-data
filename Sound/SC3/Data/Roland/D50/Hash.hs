-- | D50 / Hash
module Sound.SC3.Data.Roland.D50.Hash where

import Data.Word {- base -}
import Text.Printf {- base -}

import Sound.SC3.Data.Roland.D50 {- hsc3-data -}

import qualified Data.ByteString as B {- bytestring -}
import qualified Data.Digest.Murmur32 as Hash {- murmur-hash -}

-- | D50 parameter data hashes are 32-bit words.
type D50_Hash = Word32

-- | Parse 'D50_Hash' from 8-character hex string.
--
-- > d50_hash_parse "8C7F8CB0" == 0x8C7F8CB0
d50_hash_parse :: String -> D50_Hash
d50_hash_parse = read . ("0x" ++)

-- | Print 'D50_Hash' as 8-character hex string.
d50_hash_pp :: D50_Hash -> String
d50_hash_pp = printf "%08X"

-- | Hash D50_Param parameter sequence to a 32-bit word.
d50_param_hash :: D50_Param -> D50_Hash
d50_param_hash = Hash.asWord32 . Hash.hash32 . B.pack . concat

-- | 'd50_param_hash' of 'd50_patch_param'.
d50_patch_hash :: D50_Patch -> D50_Hash
d50_patch_hash = d50_param_hash . d50_patch_param
