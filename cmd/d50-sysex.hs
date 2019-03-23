import Control.Monad {- base -}
import Data.List {- base -}
import System.Environment {- base -}

import qualified Sound.PortMidi as M {- portmidi -}

import Sound.OSC {- hosc -}

import qualified Sound.Midi.Common as M {- midi-osc -}
import qualified Sound.Midi.PM as M {- midi-osc -}

import qualified Sound.SC3.Data.Roland.D50 as D50 {- hsc3-data -}

-- * COMMON

type U8 = M.U8

-- > map ms_to_sec [1,10,50,100,1000] == [0.001,0.01,0.05,0.1,1]
ms_to_sec :: Int -> Double
ms_to_sec n = fromIntegral n / 1000

sleep_ms :: Int -> IO ()
sleep_ms = pauseThread . ms_to_sec

with_default_output :: (M.PMStream -> IO r) -> IO r
with_default_output f = M.pm_default_output >>= \k -> M.pm_with_output_device k f

send_sysex_def :: [[U8]] -> IO ()
send_sysex_def x = void (with_default_output (\fd -> M.pm_sysex_write_set 10 fd x))

send_patch_fd :: M.PMStream -> Maybe U8 -> [U8] -> IO ()
send_patch_fd fd d50_ix p =
    let d = case d50_ix of
              Nothing -> D50.d50_dsc_gen_seq (D50.DTI_CMD,0,0,p)
              Just i -> let a = D50.patch_memory_base i
                        in D50.d50_wsd_gen 0 a (genericLength p) :
                           D50.d50_dsc_gen_seq (D50.DAT_CMD,0,a,p)
    in void (M.pm_sysex_write_set 10 fd d)

send_patch_def :: Maybe U8 -> [U8] -> IO ()
send_patch_def ix p = with_default_output (\fd -> send_patch_fd fd ix p)

patch_pp :: [U8] -> IO ()
patch_pp = putStrLn. unlines . D50.d50_patch_group_pp

pm_run_proc :: Int -> M.PMStream -> M.PROC_F U8 -> IO ()
pm_run_proc dt fd proc_f =
    let recur = do
          r <- M.pm_process_events proc_f fd
          when (not r) (sleep_ms dt)
          recur
    in recur

-- * LOAD ON PROGRAM CHANGE (LPC)

u8_index :: [t] -> U8 -> t
u8_index l n = l !! (fromIntegral n)

lpc_recv_midi :: ([[U8]], M.PMStream) -> (U8,U8,U8,U8) -> IO ()
lpc_recv_midi (p,fd) m =
    case m of
      (0xC0,n,0,0) -> print n >> patch_pp (u8_index p n) >> send_patch_fd fd Nothing (u8_index p n)
      _ -> return ()

-- > let fn = "/home/rohan/sw/hsc3-data/data/roland/d50/PN-D50-00.syx"
-- > lpc_run fn
lpc_run :: FilePath -> IO ()
lpc_run fn = do
  p <- D50.d50_load_sysex fn
  in_k <- M.pm_default_input
  out_k <- M.pm_default_output
  in_fd <- M.pm_open_input in_k
  out_fd <- M.pm_open_output out_k
  pm_run_proc 10 in_fd (lpc_recv_midi (p,out_fd))

-- * PRINT

-- > let fn = "/home/rohan/sw/hsc3-data/data/roland/d50/PN-D50-01.syx"
-- > d50_print_sysex "0" D50.d50_patch_group_pp fn
-- > d50_print_sysex "all" (return . D50.patch_name) fn
-- > d50_print_sysex "all" (return . intercalate " | " . D50.patch_name_set) fn
d50_print_sysex :: String -> ([U8] -> [String]) -> FilePath -> IO ()
d50_print_sysex ix pp fn =
    if ix == "all"
    then D50.d50_load_sysex fn >>= putStr . unlines . concatMap pp
    else D50.d50_load_sysex fn >>= putStr . unlines . pp . (!! (read ix))

-- > let fn = "/home/rohan/uc/invisible/light/d50/d50.hex.text"
-- > d50_print_patch_text D50.d50_patch_group_pp fn
d50_print_patch_text :: ([U8] -> [String]) -> FilePath -> IO ()
d50_print_patch_text pp fn = D50.load_d50_text fn >>= putStr . unlines . pp

-- * SEND

parse_d50_ix :: String -> Maybe U8
parse_d50_ix s = if s == "tmp" then Nothing else Just (read s)

-- > send_patch Nothing 0 "/home/rohan/sw/hsc3-data/data/roland/d50/PN-D50-00.syx"
-- > send_patch (Just 10) 0 "/home/rohan/sw/hsc3-data/data/roland/d50/PN-D50-00.syx"
send_patch :: Maybe U8 -> U8 -> FilePath -> IO ()
send_patch d50_ix sysex_ix fn = do
  p <- D50.d50_load_sysex fn
  send_patch_def d50_ix (u8_index p sysex_ix)

-- * SET

-- > set_wg_pitch_kf (1/8)
set_wg_pitch_kf :: (Eq n, Fractional n) => n -> IO ()
set_wg_pitch_kf r = send_sysex_def (map D50.d50_dsc_gen (D50.d50_wg_pitch_kf_dti r))

-- * MAIN

usage :: IO ()
usage =
    let h = ["load-on-program-change sysex-file"
            ,"print-patch text {csv|pp-group} text-file..."
            ,"print-sysex {ix|all} {name|pp-group} sysex-file..."
            ,"send patch {tmp|d50-ix} sysex-ix sysex-file"
            ,"set wg-pitch-kf ratio"]
    in putStrLn (unlines h)

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["load-on-program-change",fn] -> M.pm_with_midi (lpc_run fn)
    "print-patch":"text":"csv":fn_seq -> mapM_ (d50_print_patch_text D50.d50_patch_csv) fn_seq
    "print-patch":"text":"pp-group":fn_seq -> mapM_ (d50_print_patch_text D50.d50_patch_group_pp) fn_seq
    "print-sysex":ix:"name":fn_seq -> mapM_ (d50_print_sysex ix (return . intercalate " | " . D50.patch_name_set)) fn_seq
    "print-sysex":ix:"pp-group":fn_seq -> mapM_ (d50_print_sysex ix D50.d50_patch_group_pp) fn_seq
    ["send","patch",d50_ix,sysex_ix,fn] -> send_patch (parse_d50_ix d50_ix) (read sysex_ix) fn
    ["set","wg-pitch-kf",r] -> set_wg_pitch_kf (read r :: Double)
    _ -> usage
