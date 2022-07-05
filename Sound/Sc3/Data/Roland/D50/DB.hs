-- | D50 / DB
module Sound.Sc3.Data.Roland.D50.DB where

import Data.List {- base -}
import System.FilePath {- filepath -}

import qualified Music.Theory.Array.Csv as T {- hmt-base -}
import qualified Music.Theory.Byte as T {- hmt-base -}
import qualified Music.Theory.Directory.Find as T {- hmt-base -}
import qualified Music.Theory.List as T {- hmt-base -}

import Sound.Sc3.Data.Roland.D50 {- hsc3-data -}
import Sound.Sc3.Data.Roland.D50.Hash {- hsc3-data -}

-- * SYX-DB-TREE

-- | (SYX-NAME,FILE-NAME,[(PATCH-INDEX/ONE-INDEXED,PATCH)])
type D50_SYX_DAT = (String, FilePath, [(Int, D50_Patch)])

-- | Sequence of SYX-DAT
type D50_SYX_DB_TREE = [D50_SYX_DAT]

-- | Scan /dir/ for ".syx" files and make DB tree.
d50_syx_db_tree :: FilePath -> IO D50_SYX_DB_TREE
d50_syx_db_tree dir = do
  fn <- fmap sort (T.dir_find_ext ".syx" dir)
  let nm = map takeBaseName fn
  p <- mapM (fmap fst . d50_load_sysex) fn
  return (zip3 nm fn (map (zip [1..]) p))

-- | Given SYX-NAME lookup patch data.
d50_syx_db_tree_get :: D50_SYX_DB_TREE -> String -> [(Int,D50_Patch)]
d50_syx_db_tree_get t nm =
  let f (x,_,l) = if x == nm then l else []
  in concatMap f t

-- * SYX-DB

-- | (SYX-NAME,FILE-NAME,PATCH-INDEX,PATCH,PATCH-NAME-SET,PATCH-HASH,PATCH-PARAM)
type D50_SYX_VC = (String, FilePath, Int, D50_Patch, D50_Patch_Name_Set, D50_Hash, D50_Param)

-- | Get SYSEX name.
d50_syx_vc_syx_name :: D50_SYX_VC -> String
d50_syx_vc_syx_name (nm,_,_,_,_,_,_) = nm

-- | Get SYSEX file-name.
d50_syx_vc_syx_file_name :: D50_SYX_VC -> String
d50_syx_vc_syx_file_name (_,fn,_,_,_,_,_) = fn

-- | Get patch index (1-64).
d50_syx_vc_ix :: D50_SYX_VC -> Int
d50_syx_vc_ix (_,_,ix,_,_,_,_) = ix

-- | Get patch.
d50_syx_vc_patch :: D50_SYX_VC -> D50_Patch
d50_syx_vc_patch (_,_,_,p,_,_,_) = p

-- | Get name set.
d50_syx_vc_name_set :: D50_SYX_VC -> D50_Patch_Name_Set
d50_syx_vc_name_set (_,_,_,_,nm,_,_) = nm

-- | Get patch-name.
d50_syx_vc_patch_name :: D50_SYX_VC -> String
d50_syx_vc_patch_name (_,_,_,_,(_,_,nm),_,_) = nm

-- | Get hash.
d50_syx_vc_hash :: D50_SYX_VC -> D50_Hash
d50_syx_vc_hash (_,_,_,_,_,h,_) = h

-- | CSV entry for (PATCH-HASH,PATCH-PARAM)
vc_hash_param_csv :: D50_SYX_VC -> [String]
vc_hash_param_csv (_,_,_,_,_,h,r) = [d50_hash_pp h,T.byte_seq_hex_pp False (concat r)]

-- | CSV entry for (PATCH-HASH,PATCH-NAME-SET)
vc_hash_names_csv :: D50_SYX_VC -> [String]
vc_hash_names_csv (_,_,_,_,(u,l,p),h,_) = [d50_hash_pp h,u,l,p]

-- | SYX DB.
type D50_SYX_DB = [D50_SYX_VC]

-- | Flatten D50_SYX_DAT.
d50_syx_dat_seq :: D50_SYX_DAT -> [D50_SYX_VC]
d50_syx_dat_seq (nm,fn,vc) =
  let (ix,p) = unzip vc
      n = map d50_patch_name_set p
      h = map d50_patch_hash p
      r = map d50_patch_param p
  in zip7 (repeat nm) (repeat fn) ix p n h r

-- | Scan /dir/ for ".syx" files and make DB.
d50_syx_db :: FilePath -> IO D50_SYX_DB
d50_syx_db dir = do
  t <- d50_syx_db_tree dir
  return (concatMap d50_syx_dat_seq t)

-- | Write SYX-DB to files at /dir/.
--   The DB is stored as two CSV files, d50-names and d50-param.
db_store :: FilePath -> D50_SYX_DB -> IO ()
db_store dir db = do
  T.csv_table_write_def id (dir </> "d50-names.csv") (map vc_hash_names_csv db)
  T.csv_table_write_def id (dir </> "d50-param.csv") (map vc_hash_param_csv db)

-- | Select entried from DB by hash.
d50_syx_db_get :: D50_SYX_DB -> D50_Hash -> [D50_SYX_VC]
d50_syx_db_get db h = filter ((== h) . d50_syx_vc_hash) db

-- | Variant requiring unique match.
d50_syx_db_get1 :: D50_SYX_DB -> D50_Hash -> D50_SYX_VC
d50_syx_db_get1 db = T.unlist1_err . d50_syx_db_get db

-- | Select entries from DB by name, flag is 'True' for case-sensitive matching.
d50_syx_db_match :: Bool -> D50_SYX_DB -> D50_Patch_Name_Set -> [D50_SYX_VC]
d50_syx_db_match cs db nm = filter (d50_patch_name_match cs nm . d50_syx_vc_name_set) db

-- | Variant requiring unique match.
d50_syx_db_match1 :: Bool -> D50_SYX_DB -> D50_Patch_Name_Set -> D50_SYX_VC
d50_syx_db_match1 cs db = T.unlist1_err . d50_syx_db_match cs db

-- * HASH-DB

-- | HASH-DB
type D50_Hash_DB = ([(D50_Hash,D50_Patch_Name_Set)],[(D50_Hash,D50_Param)])

-- | Load HASH-DB from /dir/.
d50_hash_db_load :: FilePath -> IO D50_Hash_DB
d50_hash_db_load dir = do
  nm <- T.csv_table_read_def id (dir </> "d50-names.csv")
  pr <- T.csv_table_read_def id (dir </> "d50-param.csv")
  let nm_f [h,u,l,n] = (d50_hash_parse h,(u,l,n))
      nm_f _ = error "d50_hash_db_load: nm_f?"
      pr_f [h,r] = (d50_hash_parse h,d50_param_segment (T.read_hex_byte_seq r))
      pr_f _ = error "d50_hash_db_load: pr_f?"
  return (map nm_f nm,map pr_f pr)

-- | Lookup DB given HASH.
d50_hash_db_get :: D50_Hash_DB -> D50_Hash -> (D50_Patch_Name_Set,D50_Param)
d50_hash_db_get (nm,pr) h =
  let x = T.lookup_def h (d50_patch_name_set_nil '-') nm
  in case lookup h pr of
       Nothing -> error "d50_hash_db_get?"
       Just y -> (x,y)

{-

> syx_dir = "/home/rohan/sw/hsc3-data/data/roland/d50"
> db <- d50_syx_db syx_dir
> length db == 2112
> db_dir = "/home/rohan/rd/j/2019-04-09"
> db_store db_dir db

> db <- d50_hash_db_load db_dir
> (n,r) = d50_hash_db_get db 0x31EDB7E6
> p = d50_patch_gen n r
> putStrLn $ unlines $ d50_patch_group_pp p

-}
