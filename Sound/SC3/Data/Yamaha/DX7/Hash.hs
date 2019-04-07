-- | DX7 / Hash
module Sound.SC3.Data.Yamaha.DX7.Hash where

import Data.Word {- base -}
import Text.Printf {- base -}

import qualified Data.ByteString as B {- bytestring -}
import qualified Data.Digest.Murmur32 as Hash {- murmur-hash -}

import qualified Music.Theory.Array as T {- hmt -}
import qualified Music.Theory.Array.CSV as T {- hmt -}
import qualified Music.Theory.Byte as T {- hmt -}
import qualified Music.Theory.List as T {- hmt -}

import Sound.SC3.Data.Yamaha.DX7 {- hsc3-data -}

-- | Hash 145-element parameter sequence to a 23-bit word.
dx7_param_hash32 :: DX7_Param -> Word32
dx7_param_hash32 = Hash.asWord32 . Hash.hash32 . B.pack

-- | Print 'Word32' as 8-character hex string.
word32_hex_pp :: Word32 -> String
word32_hex_pp = printf "%08X"

-- | Make CSV data of (PARAM-HASH,PARAM).
dx7_param_hash32_csv :: [DX7_Param] -> String
dx7_param_hash32_csv x =
  let opt = (True,',',False,T.CSV_No_Align)
      tbl = map (\p -> [word32_hex_pp (dx7_param_hash32 p),T.byte_seq_hex_pp False p]) x
  in T.csv_table_pp id opt (Nothing,tbl)

-- | (PARAM-HASH,(PARAM,VOICE-NAME))
type DX7_Voice_Hash = (Word32,(DX7_Param,String))

-- | Derive 'DX7_Voice_Hash'.
dx7_voice_hash :: DX7_Voice -> DX7_Voice_Hash
dx7_voice_hash v =
  let p = dx7_voice_param v
  in (dx7_param_hash32 p,(p,dx7_voice_name '?' v))

-- | (PARAM-HASH,[(PARAM,VOICE-NAME)])
type DX7_Voice_Hash_Set = (Word32,[(DX7_Param,String)])

-- | Derive 'DX7_Voice_Hash_Set'.
dx7_voice_hash_set :: [DX7_Voice] -> [DX7_Voice_Hash_Set]
dx7_voice_hash_set = T.collate . map dx7_voice_hash

-- | Locate any collisions in the hash set.
dx7_voice_hash_set_collisions :: [DX7_Voice_Hash_Set] -> Maybe [DX7_Voice_Hash_Set]
dx7_voice_hash_set_collisions x =
  case filter (not . T.all_eq . map fst . snd) x of
    [] -> Nothing
    r -> Just r

-- | Make regular table of names at hash set.
dx7_voice_hash_set_names_tbl :: [DX7_Voice_Hash_Set] -> T.Table String
dx7_voice_hash_set_names_tbl =
  let f (h,x) = map (\(_,nm) -> [word32_hex_pp h,nm]) x
  in concatMap f

-- | Make CSV file string of names at hash set.
dx7_voice_hash_set_names_csv :: [DX7_Voice_Hash_Set] -> String
dx7_voice_hash_set_names_csv x =
  let opt = (True,',',False,T.CSV_No_Align)
      tbl = dx7_voice_hash_set_names_tbl x
  in T.csv_table_pp id opt (Nothing,tbl)

-- | Make CSV file string of param at hash set.
dx7_voice_hash_set_param_csv :: [DX7_Voice_Hash_Set] -> String
dx7_voice_hash_set_param_csv = dx7_param_hash32_csv . map (fst . head . snd)
