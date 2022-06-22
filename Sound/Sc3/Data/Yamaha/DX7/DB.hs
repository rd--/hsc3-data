-- | DX7 / DB
module Sound.Sc3.Data.Yamaha.DX7.DB where

import Control.Monad {- base-}
import Data.List {- base-}
import System.Directory {- directory -}
import System.FilePath {- filepath -}

import qualified Music.Theory.Array.CSV as T {- hmt -}
import qualified Music.Theory.Byte as T {- hmt -}
import qualified Music.Theory.Directory.Find as T {- hmt -}
import qualified Music.Theory.List as T {- hmt -}

import Sound.Sc3.Data.Yamaha.DX7 {- hsc3-data -}
import Sound.Sc3.Data.Yamaha.DX7.Hash {- hsc3-data -}

-- * SYX-DB-TREE

-- | (SYX-NAME,FILE-NAME,[(VOICE-INDEX/ONE-INDEXED,VOICE)])
type DX7_SYX_DAT = (String, FilePath, [(Int, DX7_Voice)])

-- | Set of DX7_SYX_DAT
type DX7_SYX_DB_TREE = [DX7_SYX_DAT]

-- | Scan /dir/ for ".syx" files and make DB tree.
--   Ignore files that are not 4104-BYTES.
--
-- > dir = "/home/rohan/sw/hsc3-data/data/yamaha/"
-- > db <- dx7_syx_db_tree dir
-- > length db == 213
-- > map (\(nm,_,_) -> nm) db
dx7_syx_db_tree :: FilePath -> IO DX7_SYX_DB_TREE
dx7_syx_db_tree dir = do
  fn <- fmap sort . filterM (fmap (== 4104) . getFileSize) =<< T.dir_find_ext ".syx" dir
  let nm = map takeBaseName fn
  p <- mapM dx7_load_fmt9_sysex_err fn
  return (zip3 nm fn (map (zip [1..]) p))

-- | Given SYX-NAME lookup voice data.
dx7_syx_db_tree_get :: DX7_SYX_DB_TREE -> String -> [(Int,DX7_Voice)]
dx7_syx_db_tree_get t nm =
  let f (x,_,l) = if x == nm then l else []
  in concatMap f t

-- * SYX-DB

-- | (SYX-NAME,FILE-NAME,INDEX,VOICE,NAME,HASH,PARAM)
type DX7_SYX_VC = (String, FilePath, Int, DX7_Voice, String, DX7_Hash, DX7_Param)

-- | Get voice.
dx7_syx_vc_voice :: DX7_SYX_VC -> DX7_Voice
dx7_syx_vc_voice (_,_,_,v,_,_,_) = v

-- | Get name.
dx7_syx_vc_name :: DX7_SYX_VC -> String
dx7_syx_vc_name (_,_,_,_,nm,_,_) = nm

-- | Get hash.
dx7_syx_vc_hash :: DX7_SYX_VC -> DX7_Hash
dx7_syx_vc_hash (_,_,_,_,_,h,_) = h

-- | CSV entry for (HASH,PARAM)
vc_hash_param_csv :: DX7_SYX_VC -> [String]
vc_hash_param_csv (_,_,_,_,_,h,r) = [dx7_hash_pp h,T.byte_seq_hex_pp False r]

-- | CSV entry for (HASH,NAME)
vc_hash_name_csv :: DX7_SYX_VC -> [String]
vc_hash_name_csv (_,_,_,_,n,h,_) = [dx7_hash_pp h,n]

-- | SYX DB.
type DX7_SYX_DB = [DX7_SYX_VC]

-- | Flatten DX7_SYX_DAT.
dx7_syx_dat_seq :: DX7_SYX_DAT -> [DX7_SYX_VC]
dx7_syx_dat_seq (nm,fn,vc) =
  let (ix,v) = unzip vc
      n = map (dx7_voice_name '?') v
      h = map dx7_voice_hash v
      r = map dx7_voice_param v
  in zip7 (repeat nm) (repeat fn) ix v n h r

-- | Scan /dir/ for ".syx" files and make DB.
dx7_syx_db :: FilePath -> IO DX7_SYX_DB
dx7_syx_db dir = do
  t <- dx7_syx_db_tree dir
  return (concatMap dx7_syx_dat_seq t)

-- * DB-HASH

-- | (HASH,PARAM,NAME)
type DX7_HASH_VC = (DX7_Hash,DX7_Param,String)

-- | Derive 'DX7_HASH_VC'.
dx7_hash_vc :: DX7_Voice -> DX7_HASH_VC
dx7_hash_vc v =
  let p = dx7_voice_param v
  in (dx7_param_hash p,p,dx7_voice_name '?' v)

-- | Make CSV data of (HASH,PARAM).
dx7_hash_vc_param_csv :: DX7_HASH_VC -> [String]
dx7_hash_vc_param_csv (h,p,_) = [dx7_hash_pp h,T.byte_seq_hex_pp False p]

-- | Make CSV data of (HASH,NAME)
dx7_hash_vc_name_csv :: DX7_HASH_VC -> [String]
dx7_hash_vc_name_csv (h,_,n) = [dx7_hash_pp h,n]

-- | Write HASH-DB files to /dir/.
--   Files are not sorted or uniqed.
--
-- > db <- dx7_load_hex "/home/rohan/DX7/dexed.uniq.text"
-- > dx7_hash_db db "/home/rohan/rd/j/2019-04-07"
dx7_hash_db_store :: [DX7_Voice] -> FilePath -> IO ()
dx7_hash_db_store db dir = do
  let u = map dx7_hash_vc db
  T.csv_table_write_def id (dir </> "dx7-names.csv") (map dx7_hash_vc_name_csv u)
  T.csv_table_write_def id (dir </> "dx7-param.csv") (map dx7_hash_vc_param_csv u)

-- * HASH-DB

-- | HASH-DB
type DX7_Hash_DB = ([(DX7_Hash,String)],[(DX7_Hash,DX7_Param)])

-- | Load HASH-DB from /dir/.
--   The DB is permitted to have unnamed parameters.
--
-- > dir = "/home/rohan/rd/j/2019-04-07"
-- > db <- dx7_hash_db_load dir
-- > (length (fst db),length (snd db)) == (38041,30270)
dx7_hash_db_load :: FilePath -> IO DX7_Hash_DB
dx7_hash_db_load dir = do
  nm <- T.csv_table_read_def id (dir </> "dx7-names.csv")
  pr <- T.csv_table_read_def id (dir </> "dx7-param.csv")
  let nm_f [h,n] = (dx7_hash_parse h,n)
      nm_f _ = error "dx7_hash_db_load: nm_f?"
      pr_f [h,r] = (dx7_hash_parse h,T.read_hex_byte_seq r)
      pr_f _ = error "dx7_hash_db_load: pr_f?"
  return (map nm_f nm,map pr_f pr)

-- | Get NAMES (perhaps empty) and PARAM given HASH.
dx7_hash_db_get :: DX7_Hash_DB -> DX7_Hash -> ([String],DX7_Param)
dx7_hash_db_get (nm,pr) h =
  (map snd (filter ((== h) . fst) nm)
  ,T.lookup_err h pr)

-- | Extract voices from DB.  First name is applied. Un-named voices are given default.
dx7_hash_db_vc :: String -> DX7_Hash_DB -> [DX7_Voice]
dx7_hash_db_vc df (nm,pr) =
  let f (h,p) = dx7_param_to_dx7_voice (T.lookup_def h df nm) p
  in map f pr

-- | Get subset of DB matching NAME using given equality function and case-fold function.
--   ie. eq_f could be (==), 'isInfixOf', 'isPrefixOf' &etc.
--       cf_f could be 'id' or 'toLower'
--
-- > lc = dx7_hash_db_locate (isInfixOf,Data.Char.toLower) db "FAIR"
-- > vc = dx7_hash_db_vc "----------" lc
dx7_hash_db_locate :: (String -> String -> Bool,Char -> Char) -> DX7_Hash_DB -> String -> DX7_Hash_DB
dx7_hash_db_locate (eq_f,cf_f) (nm,pr) x =
  let f p q = map cf_f p `eq_f` map cf_f q
      nm' = filter (f x . snd) nm
      pr' = filter ((`elem` map fst nm') . fst) pr
  in (nm',pr')

-- | Get all PARAM having NAME.
dx7_hash_db_search :: DX7_Hash_DB -> String -> [DX7_Voice]
dx7_hash_db_search (nm,pr) x =
 let h = map fst (filter ((== x) . snd) nm)
 in map (dx7_param_to_dx7_voice x . snd) (filter ((`elem` h) . fst) pr)
