import Control.Monad {- base -}
import System.Environment {- base -}
import Text.Printf {- base -}

import qualified Sound.Osc.Core as Osc {- hosc -}

import qualified Sound.Midi.Pm as Pm {- midi-osc -}

import Sound.Sc3.Data.Math.Types {- hsc3-data -}
import qualified Sound.Sc3.Data.Roland.D50 as D50 {- hsc3-data -}
import qualified Sound.Sc3.Data.Roland.D50.Db as D50.Db {- hsc3-data -}
import qualified Sound.Sc3.Data.Roland.D50.Hash as D50.Hash {- hsc3-data -}
import qualified Sound.Sc3.Data.Roland.D50.Pm as D50.Pm {- hsc3-data -}
import qualified Sound.Sc3.Data.Roland.D50.Pp as D50.Pp {- hsc3-data -}

-- * Common

-- > map ms_to_sec [1,10,50,100,1000] == [0.001,0.01,0.05,0.1,1]
ms_to_sec :: Int -> Double
ms_to_sec n = fromIntegral n / 1000

sleep_ms :: Int -> IO ()
sleep_ms = Osc.pauseThread . ms_to_sec

-- > send_sysex_def [D50.d50_ack_gen 0]
send_sysex_def :: [[U8]] -> IO ()
send_sysex_def x = void (Pm.pm_with_default_output (\fd -> Pm.pm_sysex_write_seq 10 fd x))

pm_run_proc :: Int -> Pm.Pm_Fd -> Pm.Proc_F -> IO ()
pm_run_proc dt fd proc_f =
    let recur = do
          r <- Pm.pm_process_events proc_f fd
          when (not r) (sleep_ms dt)
          recur
    in recur

-- * Load on program change (lpc)

lpc_recv_midi :: ([D50.D50_Patch], Pm.Pm_Fd) -> Pm.Proc_F
lpc_recv_midi (p,fd) m =
  let pp = putStrLn. unlines . D50.Pp.d50_patch_group_pp
  in case m of
       Left (0xC0,n,0) -> pp (u8_at p n) >> D50.Pm.d50_send_patch_tmp_fd (u8_at p n) fd
       _ -> return ()

-- > let fn = "/home/rohan/sw/hsc3-data/data/roland/d50/PN-D50-00.syx"
-- > lpc_run fn
lpc_run :: FilePath -> IO ()
lpc_run fn = do
  (p,_) <- D50.d50_load_sysex fn
  in_fd <- Pm.pm_open_input_def
  out_fd <- Pm.pm_open_output_def
  pm_run_proc 10 in_fd (lpc_recv_midi (p,out_fd))

-- * Print

d50_print_dat :: Maybe Int -> ((Int,[U8]) -> [String]) -> [D50.D50_Patch] -> IO ()
d50_print_dat m_ix pp v =
  case m_ix of
    Nothing -> putStr . unlines . concatMap pp $ zip [0..] v
    Just ix -> putStr . unlines . pp . (!! ix) $ zip [0..] v

print_name :: Bool -> (Int,[U8]) -> String
print_name bnk (k,p) =
  let nm = D50.Pp.d50_patch_name_set_pp p
  in if bnk
     then let (b,n) = D50.d50_ix_to_bank k
          in printf "%d%d %s" b n nm
     else nm

dat_print :: Bool -> Maybe Int -> String -> [D50.D50_Patch] -> IO ()
dat_print bnk ix ty v =
  let f pp = d50_print_dat ix pp v
  in case ty of
       "pp-area" -> f (D50.Pp.d50_patch_area_pp . snd)
       "csv" -> f (D50.Pp.d50_patch_csv True . snd)
       "hex" -> f (return . D50.Pp.d50_sysex_pp . snd)
       "name" -> f (return . print_name bnk)
       "pp-group" -> f (D50.Pp.d50_patch_group_pp . snd)
       _ -> error "dat_print?"

-- > sysex_print Nothing "name" [fn]
sysex_print :: Maybe Int -> String -> [FilePath] -> IO ()
sysex_print m_ix ty fn = mapM D50.d50_load_sysex fn >>= mapM_ (dat_print True m_ix ty) . map fst

hex_print :: Maybe Int -> String -> [FilePath] -> IO ()
hex_print m_ix ty fn = mapM D50.d50_load_hex fn >>= mapM_ (dat_print False m_ix ty)

-- * Send

parse_d50_ix :: (Integral t, Read t) => String -> String -> Maybe t
parse_d50_ix nil s = if s == nil then Nothing else Just (read s)

-- > let fn = "/home/rohan/sw/hsc3-data/data/roland/d50/PN-D50-00.syx"
-- > send_patch Nothing 0 fn
-- > send_patch (Just 10) 0 fn
send_patch :: Maybe U8 -> U8 -> FilePath -> IO ()
send_patch d50_ix sysex_ix fn = do
  (p,_) <- D50.d50_load_sysex fn
  case d50_ix of
    Nothing -> D50.Pm.d50_send_patch_tmp_def (u8_at p sysex_ix)
    _ -> error "send_patch?"

hex_send :: Int -> FilePath -> IO ()
hex_send k fn = D50.d50_load_hex fn >>= D50.Pm.d50_send_patch_tmp_def . (!! k)

-- * Set

-- > set_wg_pitch_kf (1/8)
set_wg_pitch_kf :: (Eq n, Fractional n) => n -> IO ()
set_wg_pitch_kf r = send_sysex_def (map D50.d50_dsc_gen (D50.d50_wg_pitch_kf_dt1 r))

-- * Data transfer

-- > transfer_recv_bulk_hex "/tmp/d50.hex.text" "/tmp/rvb.hex.text"
transfer_recv_bulk_hex :: FilePath -> FilePath -> IO ()
transfer_recv_bulk_hex p_fn r_fn = do
  (p,r) <- D50.Pm.d50_recv_bulk_data 0
  D50.d50_store_hex p_fn p
  D50.d50_store_hex r_fn r

transfer_recv_bulk_sysex :: FilePath -> IO ()
transfer_recv_bulk_sysex fn = do
  d <- D50.Pm.d50_recv_dat_def 0
  D50.d50_store_binary_u8 fn (concatMap D50.d50_dsc_gen d)

-- > let fn = "/home/rohan/sw/hsc3-data/data/roland/d50/PN-D50-01.syx"
-- > transfer_send_bulk_sysex fn
transfer_send_bulk_sysex :: FilePath -> IO ()
transfer_send_bulk_sysex fn = do
  dsc <- D50.d50_load_sysex_dsc fn
  D50.Pm.d50_send_bulk_data_def 0 dsc

syx_vc_pp :: D50.Db.D50_Syx_Vc -> String
syx_vc_pp (syx_nm,_,ix,p,_,hsh,_) =
  printf "%-20s - %02d - %s - %08X" syx_nm ix (D50.Pp.d50_patch_summary p) hsh

-- > sysex_db_search_name False "/home/rohan/sw/hsc3-data/data/roland/d50" ("-","-","FAIRLIGHT")
sysex_db_search_name :: Bool -> FilePath -> D50.D50_Patch_Name_Set -> IO ()
sysex_db_search_name cs dir nm_nil = do
  db <- D50.Db.d50_syx_db dir
  let from_nil x = if x == "-" then "" else x
      nm = let (l_nm,u_nm,p_nm) = nm_nil in (from_nil l_nm,from_nil u_nm,from_nil p_nm)
      vc = D50.Db.d50_syx_db_match cs db nm
  putStrLn (unlines (map syx_vc_pp vc))

-- > sysex_db_search_hash "/home/rohan/sw/hsc3-data/data/roland/d50" ["31EDB7E6","DB726918"]
sysex_db_search_hash :: FilePath -> [String] -> IO ()
sysex_db_search_hash dir h = do
  db <- D50.Db.d50_syx_db dir
  let vc = concatMap (D50.Db.d50_syx_db_get db) (map D50.Hash.d50_hash_parse h)
  putStrLn (unlines (map syx_vc_pp vc))

-- * Main

usage :: [String]
usage =
  ["hex print pp-select pp-type text-file..."
  ,"hex send ix text-file"
  ,"set wg-pitch-kf ratio"
  ,"sysex load-on-program-change sysex-file"
  ,"sysex print pp-select pp-type sysex-file..."
  ,"sysex send {tmp | d50-ix} sysex-ix sysex-file"
  ,"sysex-db search hash sysex-dir hash..."
  ,"sysex-db search name cs|ci sysex-dir lower-name|- upper-name|- patch-name|-"
  ,"transfer receive bulk {hex | sysex} {patch-file reverb-file | sysex-file}"
  ,"transfer send bulk {hex | sysex} {patch-file reverb-file | sysex-file}"
  ,""
  ,"  pp-select = {ix | all}"
  ,"  pp-type = {csv | hex | name | pp-area | pp-group}"
  ]

usage_wr :: IO ()
usage_wr = putStrLn (unlines usage)

main :: IO ()
main = do
  a <- getArgs
  case a of
    "hex":"print":ix:ty:fn_seq -> hex_print (parse_d50_ix "all" ix) ty fn_seq
    ["hex","send",ix,fn] -> hex_send (read ix) fn
    ["set","wg-pitch-kf",r] -> set_wg_pitch_kf (read r :: Double)
    ["sysex","load-on-program-change",fn] -> Pm.pm_with_midi (lpc_run fn)
    "sysex":"print":ix:ty:fn_seq -> sysex_print (parse_d50_ix "all" ix) ty fn_seq
    ["sysex","send",d50_ix,sysex_ix,fn] -> send_patch (parse_d50_ix "tmp" d50_ix) (read sysex_ix) fn
    "sysex-db":"search":"hash":dir:h -> sysex_db_search_hash dir h
    ["sysex-db","search","name",cr,dir,l,u,p] -> sysex_db_search_name (cr == "cs") dir (l,u,p)
    ["transfer","receive","bulk","hex",p_fn,r_fn] -> transfer_recv_bulk_hex p_fn r_fn
    ["transfer","receive","bulk","sysex",fn] -> transfer_recv_bulk_sysex fn
    ["transfer","send","bulk","sysex",fn] -> transfer_send_bulk_sysex fn
    _ -> usage_wr
