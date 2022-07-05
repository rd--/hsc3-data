-- | Dx7 / Db
module Sound.Sc3.Data.Yamaha.Dx7.Db where

import Control.Monad {- base-}
import Data.List {- base-}
import System.Directory {- directory -}
import System.FilePath {- filepath -}

import qualified Music.Theory.Array.Csv as T {- hmt -}
import qualified Music.Theory.Byte as T {- hmt -}
import qualified Music.Theory.Directory.Find as T {- hmt -}
import qualified Music.Theory.List as T {- hmt -}

import Sound.Sc3.Data.Yamaha.Dx7 {- hsc3-data -}
import Sound.Sc3.Data.Yamaha.Dx7.Hash {- hsc3-data -}

-- * Syx-Db-Tree

-- | (Syx-Name,File-Name,[(Voice-Index/One-Indexed,Voice)])
type Dx7_Syx_Dat = (String, FilePath, [(Int, Dx7_Voice)])

-- | Set of Dx7_Syx_Dat
type Dx7_Syx_Db_TREE = [Dx7_Syx_Dat]

-- | Scan /dir/ for ".syx" files and make DB tree.
--   Ignore files that are not 4104-BYTES.
--
-- > dir = "/home/rohan/sw/hsc3-data/data/yamaha/"
-- > db <- dx7_syx_db_tree dir
-- > length db == 213
-- > map (\(nm,_,_) -> nm) db
dx7_syx_db_tree :: FilePath -> IO Dx7_Syx_Db_TREE
dx7_syx_db_tree dir = do
  fn <- fmap sort . filterM (fmap (== 4104) . getFileSize) =<< T.dir_find_ext ".syx" dir
  let nm = map takeBaseName fn
  p <- mapM dx7_load_fmt9_sysex_err fn
  return (zip3 nm fn (map (zip [1..]) p))

-- | Given SYX-NAME lookup voice data.
dx7_syx_db_tree_get :: Dx7_Syx_Db_TREE -> String -> [(Int,Dx7_Voice)]
dx7_syx_db_tree_get t nm =
  let f (x,_,l) = if x == nm then l else []
  in concatMap f t

-- * Syx-Db

-- | (Syx-Name,File-Name,Index,Voice,Name,Hash,Param)
type Dx7_Syx_Vc = (String, FilePath, Int, Dx7_Voice, String, Dx7_Hash, Dx7_Param)

-- | Get voice.
dx7_syx_vc_voice :: Dx7_Syx_Vc -> Dx7_Voice
dx7_syx_vc_voice (_,_,_,v,_,_,_) = v

-- | Get name.
dx7_syx_vc_name :: Dx7_Syx_Vc -> String
dx7_syx_vc_name (_,_,_,_,nm,_,_) = nm

-- | Get hash.
dx7_syx_vc_hash :: Dx7_Syx_Vc -> Dx7_Hash
dx7_syx_vc_hash (_,_,_,_,_,h,_) = h

-- | CSV entry for (HASH,PARAM)
vc_hash_param_csv :: Dx7_Syx_Vc -> [String]
vc_hash_param_csv (_,_,_,_,_,h,r) = [dx7_hash_pp h,T.byte_seq_hex_pp False r]

-- | CSV entry for (HASH,NAME)
vc_hash_name_csv :: Dx7_Syx_Vc -> [String]
vc_hash_name_csv (_,_,_,_,n,h,_) = [dx7_hash_pp h,n]

-- | SYX DB.
type Dx7_Syx_Db = [Dx7_Syx_Vc]

-- | Flatten Dx7_Syx_Dat.
dx7_syx_dat_seq :: Dx7_Syx_Dat -> [Dx7_Syx_Vc]
dx7_syx_dat_seq (nm,fn,vc) =
  let (ix,v) = unzip vc
      n = map (dx7_voice_name '?') v
      h = map dx7_voice_hash v
      r = map dx7_voice_param v
  in zip7 (repeat nm) (repeat fn) ix v n h r

-- | Scan /dir/ for ".syx" files and make DB.
dx7_syx_db :: FilePath -> IO Dx7_Syx_Db
dx7_syx_db dir = do
  t <- dx7_syx_db_tree dir
  return (concatMap dx7_syx_dat_seq t)

-- * Db-Hash

-- | (Hash,Param,Name)
type Dx7_Hash_Vc = (Dx7_Hash, Dx7_Param, String)

-- | Derive 'Dx7_Hash_Vc'.
dx7_hash_vc :: Dx7_Voice -> Dx7_Hash_Vc
dx7_hash_vc v =
  let p = dx7_voice_param v
  in (dx7_param_hash p,p,dx7_voice_name '?' v)

-- | Make Csv data of (Hash,Param).
dx7_hash_vc_param_csv :: Dx7_Hash_Vc -> [String]
dx7_hash_vc_param_csv (h,p,_) = [dx7_hash_pp h,T.byte_seq_hex_pp False p]

-- | Make Csv data of (Hash,Name)
dx7_hash_vc_name_csv :: Dx7_Hash_Vc -> [String]
dx7_hash_vc_name_csv (h,_,n) = [dx7_hash_pp h,n]

{- | Write Hash-Db files to /dir/.
Files are not sorted or uniqed.

> db <- dx7_load_hex "/home/rohan/Dx7/dexed.uniq.text"
> dx7_hash_db db "/home/rohan/rd/j/2019-04-07"
-}
dx7_hash_db_store :: [Dx7_Voice] -> FilePath -> IO ()
dx7_hash_db_store db dir = do
  let u = map dx7_hash_vc db
  T.csv_table_write_def id (dir </> "dx7-names.csv") (map dx7_hash_vc_name_csv u)
  T.csv_table_write_def id (dir </> "dx7-param.csv") (map dx7_hash_vc_param_csv u)

-- * Hash-Db

-- | Hash-Db
type Dx7_Hash_DB = ([(Dx7_Hash,String)],[(Dx7_Hash,Dx7_Param)])

{- | Load Hash-Db from /dir/.
The DB is permitted to have unnamed parameters.

> dir = "/home/rohan/rd/j/2019-04-07"
> db <- dx7_hash_db_load dir
> (length (fst db),length (snd db)) == (38041,30270)
-}
dx7_hash_db_load :: FilePath -> IO Dx7_Hash_DB
dx7_hash_db_load dir = do
  nm <- T.csv_table_read_def id (dir </> "dx7-names.csv")
  pr <- T.csv_table_read_def id (dir </> "dx7-param.csv")
  let nm_f [h,n] = (dx7_hash_parse h,n)
      nm_f _ = error "dx7_hash_db_load: nm_f?"
      pr_f [h,r] = (dx7_hash_parse h,T.read_hex_byte_seq r)
      pr_f _ = error "dx7_hash_db_load: pr_f?"
  return (map nm_f nm,map pr_f pr)

-- | Get NAMES (perhaps empty) and PARAM given HASH.
dx7_hash_db_get :: Dx7_Hash_DB -> Dx7_Hash -> ([String],Dx7_Param)
dx7_hash_db_get (nm,pr) h =
  (map snd (filter ((== h) . fst) nm)
  ,T.lookup_err h pr)

-- | Extract voices from DB.  First name is applied. Un-named voices are given default.
dx7_hash_db_vc :: String -> Dx7_Hash_DB -> [Dx7_Voice]
dx7_hash_db_vc df (nm,pr) =
  let f (h,p) = dx7_param_to_dx7_voice (T.lookup_def h df nm) p
  in map f pr

{- | Get subset of DB matching NAME using given equality function and case-fold function.
ie. eq_f could be (==), 'isInfixOf', 'isPrefixOf' &etc.
cf_f could be 'id' or 'toLower'

> lc = dx7_hash_db_locate (isInfixOf,Data.Char.toLower) db "FAIR"
> vc = dx7_hash_db_vc "----------" lc
-}
dx7_hash_db_locate :: (String -> String -> Bool,Char -> Char) -> Dx7_Hash_DB -> String -> Dx7_Hash_DB
dx7_hash_db_locate (eq_f,cf_f) (nm,pr) x =
  let f p q = map cf_f p `eq_f` map cf_f q
      nm' = filter (f x . snd) nm
      pr' = filter ((`elem` map fst nm') . fst) pr
  in (nm',pr')

-- | Get all param having name.
dx7_hash_db_search :: Dx7_Hash_DB -> String -> [Dx7_Voice]
dx7_hash_db_search (nm,pr) x =
 let h = map fst (filter ((== x) . snd) nm)
 in map (dx7_param_to_dx7_voice x . snd) (filter ((`elem` h) . fst) pr)
