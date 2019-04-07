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
import qualified Music.Theory.Tuple as T {- hmt -}

import Sound.SC3.Data.Yamaha.DX7 {- hsc3-data -}

-- | DX7 parameter data hashes are 32-bit words.
type DX7_Hash = Word32

-- | Hash 145-element parameter sequence to a 32-bit word.
dx7_param_hash32 :: DX7_Param -> DX7_Hash
dx7_param_hash32 = Hash.asWord32 . Hash.hash32 . B.pack

-- | Print 'DX7_Hash' as 8-character hex string.
dx7_hash_pp :: DX7_Hash -> String
dx7_hash_pp = printf "%08X"

-- | Make hex strings for (PARAM-HASH,PARAM).
dx7_param_hash32_str :: DX7_Param -> (String,String)
dx7_param_hash32_str p = (dx7_hash_pp (dx7_param_hash32 p),T.byte_seq_hex_pp False p)

-- | Make CSV data of (PARAM-HASH,PARAM).
dx7_param_hash32_csv :: [DX7_Param] -> String
dx7_param_hash32_csv x =
  let opt = (True,',',False,T.CSV_No_Align)
      tbl = map (T.t2_to_list . dx7_param_hash32_str) x
  in T.csv_table_pp id opt (Nothing,tbl)

-- | (PARAM-HASH,(PARAM,VOICE-NAME))
type DX7_Voice_Hash = (DX7_Hash,(DX7_Param,String))

-- | Derive 'DX7_Voice_Hash'.
dx7_voice_hash :: DX7_Voice -> DX7_Voice_Hash
dx7_voice_hash v =
  let p = dx7_voice_param v
  in (dx7_param_hash32 p,(p,dx7_voice_name '?' v))

-- | (PARAM-HASH,[(PARAM,VOICE-NAME)])
type DX7_Voice_Hash_Set = (DX7_Hash,[(DX7_Param,String)])

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
  let f (h,x) = map (\(_,nm) -> [dx7_hash_pp h,nm]) x
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
