import Control.Monad {- base -}
import Data.List {- base -}
import System.Environment {- base -}

import qualified Data.List.Split as Split {- split -}

import Sound.OSC {- hosc -}

import qualified Sound.Midi.Common as M {- midi-osc -}
import qualified Sound.Midi.Type as M {- midi-osc -}

import qualified Sound.Midi.PM as PM {- midi-osc -}

import qualified Sound.SC3.Data.Roland.D50 as D50 {- hsc3-data -}
import qualified Sound.SC3.Data.Roland.D50.PM as D50 {- hsc3-data -}

-- * COMMON

type U8 = M.U8

-- > map ms_to_sec [1,10,50,100,1000] == [0.001,0.01,0.05,0.1,1]
ms_to_sec :: Int -> Double
ms_to_sec n = fromIntegral n / 1000

sleep_ms :: Int -> IO ()
sleep_ms = pauseThread . ms_to_sec

-- > send_sysex_def [D50.d50_ack_gen 0]
send_sysex_def :: [[U8]] -> IO ()
send_sysex_def x = void (PM.pm_with_default_output (\fd -> PM.pm_sysex_write_set 10 fd x))

send_patch_fd :: PM.PM_FD -> Maybe U8 -> [U8] -> IO ()
send_patch_fd fd d50_ix p =
    let d = case d50_ix of
              Nothing -> D50.d50_dsc_gen_seq (D50.DT1_CMD,0,0,p)
              Just i -> let a = D50.patch_memory_base i
                        in D50.d50_wsd_gen 0 a (genericLength p) :
                           D50.d50_dsc_gen_seq (D50.DAT_CMD,0,a,p)
    in void (PM.pm_sysex_write_set 10 fd d)

send_patch_def :: Maybe U8 -> [U8] -> IO ()
send_patch_def ix p = PM.pm_with_default_output (\fd -> send_patch_fd fd ix p)

patch_pp :: [U8] -> IO ()
patch_pp = putStrLn. unlines . D50.d50_patch_group_pp

pm_run_proc :: Int -> PM.PM_FD -> PM.PROC_F -> IO ()
pm_run_proc dt fd proc_f =
    let recur = do
          r <- PM.pm_process_events proc_f fd
          when (not r) (sleep_ms dt)
          recur
    in recur

-- * LOAD ON PROGRAM CHANGE (LPC)

u8_index :: [t] -> U8 -> t
u8_index l n = l !! (fromIntegral n)

lpc_recv_midi :: ([D50.D50_Patch], PM.PM_FD) -> Either (M.CVM3 U8) (M.SYSEX U8) -> IO ()
lpc_recv_midi (p,fd) m =
    case m of
      Left (0xC0,n,0) -> print n >> patch_pp (u8_index p n) >> send_patch_fd fd Nothing (u8_index p n)
      _ -> return ()

-- > let fn = "/home/rohan/sw/hsc3-data/data/roland/d50/PN-D50-00.syx"
-- > lpc_run fn
lpc_run :: FilePath -> IO ()
lpc_run fn = do
  p <- D50.d50_load_sysex fn
  in_fd <- PM.pm_open_input_def
  out_fd <- PM.pm_open_output_def
  pm_run_proc 10 in_fd (lpc_recv_midi (p,out_fd))

-- * PRINT

-- > let fn = "/home/rohan/sw/hsc3-data/data/roland/d50/PN-D50-01.syx"
-- > d50_print_sysex 0 D50.d50_patch_group_pp fn
-- > d50_print_sysex Nothing (return . D50.patch_name) fn
-- > d50_print_sysex Nothing (return . intercalate " | " . D50.patch_name_set) fn
d50_print_sysex :: Maybe Int -> ([U8] -> [String]) -> FilePath -> IO ()
d50_print_sysex m_ix pp fn =
  case m_ix of
    Nothing -> D50.d50_load_sysex fn >>= putStr . unlines . concatMap pp
    Just ix -> D50.d50_load_sysex fn >>= putStr . unlines . pp . (!! ix)

-- > let fn = "/home/rohan/uc/invisible/light/d50/d50.hex.text"
-- > d50_print_patch_text D50.d50_patch_group_pp fn
d50_print_patch_text :: ([U8] -> [String]) -> FilePath -> IO ()
d50_print_patch_text pp fn = D50.d50_load_hex fn >>= mapM_ putStrLn . map (unlines . pp)

-- * SEND

parse_d50_ix :: (Integral t, Read t) => String -> String -> Maybe t
parse_d50_ix nil s = if s == nil then Nothing else Just (read s)

-- > send_patch Nothing 0 "/home/rohan/sw/hsc3-data/data/roland/d50/PN-D50-00.syx"
-- > send_patch (Just 10) 0 "/home/rohan/sw/hsc3-data/data/roland/d50/PN-D50-00.syx"
send_patch :: Maybe U8 -> U8 -> FilePath -> IO ()
send_patch d50_ix sysex_ix fn = do
  p <- D50.d50_load_sysex fn
  send_patch_def d50_ix (u8_index p sysex_ix)

-- * SET

-- > set_wg_pitch_kf (1/8)
set_wg_pitch_kf :: (Eq n, Fractional n) => n -> IO ()
set_wg_pitch_kf r = send_sysex_def (map D50.d50_dsc_gen (D50.d50_wg_pitch_kf_dt1 r))

-- * MAIN

usage :: [String]
usage =
  ["hex print {csv|pp-group} text-file..."
  ,"set wg-pitch-kf ratio"
  ,"sysex load-on-program-change sysex-file"
  ,"sysex print {ix|all} {name|pp-group} sysex-file..."
  ,"sysex send {tmp|d50-ix} sysex-ix sysex-file"
  ]

usage_wr :: IO ()
usage_wr = putStrLn (unlines usage)

hex_print :: String -> [FilePath] -> IO ()
hex_print ty fn =
  case ty of
    "csv" -> mapM_ (d50_print_patch_text D50.d50_patch_csv) fn
    "pp-group" -> mapM_ (d50_print_patch_text D50.d50_patch_group_pp) fn
    _ -> error "hex_print?"

sysex_print :: Maybe Int -> String -> [FilePath] -> IO ()
sysex_print ix ty fn =
  case ty of
    "name" -> mapM_ (d50_print_sysex ix (return . intercalate " | " . D50.patch_name_set)) fn
    "pp-group" -> mapM_ (d50_print_sysex ix D50.d50_patch_group_pp) fn
    _ -> error "sysex_print?"

main :: IO ()
main = do
  a <- getArgs
  case a of
    "hex":"print":ty:fn_seq -> hex_print ty fn_seq
    ["set","wg-pitch-kf",r] -> set_wg_pitch_kf (read r :: Double)
    ["sysex","load-on-program-change",fn] -> PM.pm_with_midi (lpc_run fn)
    "sysex":"print":ix:ty:fn_seq -> sysex_print (parse_d50_ix "all" ix) ty fn_seq
    ["sysex","send",d50_ix,sysex_ix,fn] -> send_patch (parse_d50_ix "tmp" d50_ix) (read sysex_ix) fn
    _ -> usage_wr

data_transfer_recv_bulk :: IO ([D50.D50_Patch],[D50.D50_Reverb])
data_transfer_recv_bulk = do
  let err str = error ("data_transfer_recv_bulk: " ++ str)
  d <- D50.d50_recv_dat_def 0
  when (length d /= 136) (err "#d?")
  let b = concatMap D50.dsc_data d
  when (length b /= 34688) (err "#b?")
  let k = D50.d50_parameter_n * 64
      (p_dat,r_dat) = splitAt k b
  return (Split.chunksOf D50.d50_parameter_n p_dat
         ,Split.chunksOf D50.d50_reverb_data_segment_n r_dat)

{-

(p,r) <- data_transfer_recv_bulk
D50.d50_store_hex "/tmp/d50.hex.text" p
D50.d50_store_hex "/tmp/rvb.hex.text" r

-}
