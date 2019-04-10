-- | DX7 / Hash
module Sound.SC3.Data.Yamaha.DX7.Hash where

import Data.Word {- base -}
import Text.Printf {- base -}

import qualified Data.ByteString as B {- bytestring -}
import qualified Data.Digest.Murmur32 as Hash {- murmur-hash -}

import Sound.SC3.Data.Yamaha.DX7 {- hsc3-data -}

-- | DX7 parameter data hashes are 32-bit words.
type DX7_Hash = Word32

-- | Parse 'DX7_Hash' from 8-character hex string.
--
-- > dx7_hash_parse "8C7F8CB0" == 0x8C7F8CB0
dx7_hash_parse :: String -> DX7_Hash
dx7_hash_parse = read . ("0x" ++)

-- | Print 'DX7_Hash' as 8-character hex string.
dx7_hash_pp :: DX7_Hash -> String
dx7_hash_pp = printf "%08X"

-- | Hash 145-element parameter sequence to a 32-bit word.
dx7_param_hash :: DX7_Param -> DX7_Hash
dx7_param_hash = Hash.asWord32 . Hash.hash32 . B.pack

-- | 'dx7_param_hash' of 'dx7_voice_param'.
dx7_voice_hash :: DX7_Voice -> DX7_Hash
dx7_voice_hash = dx7_param_hash . dx7_voice_param
