-- | Dx7 / Hash
module Sound.Sc3.Data.Yamaha.Dx7.Hash where

import Data.Word {- base -}
import Text.Printf {- base -}

import qualified Data.ByteString as B {- bytestring -}
import qualified Data.Digest.Murmur32 as Hash {- murmur-hash -}

import Sound.Sc3.Data.Yamaha.Dx7 {- hsc3-data -}

-- | Dx7 parameter data hashes are 32-bit words.
type Dx7_Hash = Word32

-- | Parse 'Dx7_Hash' from 8-character hex string.
--
-- > dx7_hash_parse "8C7F8CB0" == 0x8C7F8CB0
dx7_hash_parse :: String -> Dx7_Hash
dx7_hash_parse = read . ("0x" ++)

-- | Print 'Dx7_Hash' as 8-character hex string.
dx7_hash_pp :: Dx7_Hash -> String
dx7_hash_pp = printf "%08X"

-- | Hash 145-element parameter sequence to a 32-bit word.
dx7_param_hash :: Dx7_Param -> Dx7_Hash
dx7_param_hash = Hash.asWord32 . Hash.hash32 . B.pack . map fromIntegral

-- | 'dx7_param_hash' of 'dx7_voice_param'.
dx7_voice_hash :: Dx7_Voice -> Dx7_Hash
dx7_voice_hash = dx7_param_hash . dx7_voice_param
