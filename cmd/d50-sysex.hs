import Control.Monad {- base -}
import System.Environment {- base -}

import Sound.OSC {- hosc -}
import qualified Sound.PortMidi as M {- portmidi -}
import qualified Sound.Midi.PM as M {- midi-osc -}
import Sound.SC3.Data.Roland.D50 {- hsc3-data -}

-- * COMMON

-- > map ms_to_sec [1,10,50,100,1000] == [0.001,0.01,0.05,0.1,1]
ms_to_sec :: Int -> Double
ms_to_sec n = fromIntegral n / 1000

sleep_ms :: Int -> IO ()
sleep_ms = pauseThread . ms_to_sec

with_default_output :: (M.PMStream -> IO r) -> IO r
with_default_output f = M.pm_default_output >>= \k -> M.pm_with_output_device k f

send_sysex :: M.PMStream -> [[U8]] -> IO ()
send_sysex fd = mapM_ (\x -> M.pm_sysex_write fd x >> sleep_ms 50)

send_sysex_def :: [[U8]] -> IO ()
send_sysex_def x = with_default_output (\fd -> send_sysex fd x)

send_patch_fd :: M.PMStream -> Maybe U8 -> [U8] -> IO ()
send_patch_fd fd d50_ix p =
    let d = case d50_ix of
              Nothing -> d50_dsc_gen_seq (DTI_CMD,0,0,p)
              Just i -> let a = patch_memory_base i
                        in d50_wsd_gen 0 a (length p) : d50_dsc_gen_seq (DAT_CMD,0,a,p)
    in send_sysex fd d

send_patch_def :: Maybe U8 -> [U8] -> IO ()
send_patch_def ix p = with_default_output (\fd -> send_patch_fd fd ix p)

patch_pp :: [U8] -> IO ()
patch_pp = putStrLn. unlines . d50_patch_group_pp

pm_run_proc :: Int -> M.PMStream -> M.PROC_F U8 -> IO ()
pm_run_proc dt fd proc_f =
    let recur = do
          r <- M.pm_process_events proc_f fd
          when (not r) (sleep_ms dt)
          recur
    in recur

-- * LOAD ON PROGRAM CHANGE (LPC)

lpc_recv_midi :: ([[U8]], M.PMStream) -> (U8,U8,U8,U8) -> IO ()
lpc_recv_midi (p,fd) m =
    case m of
      (0xC0,n,0,0) -> print n >> patch_pp (p !! n) >> send_patch_fd fd Nothing (p !! n)
      _ -> return ()

-- > lpc_run "/home/rohan/data/roland-d50/PND50-00.syx"
lpc_run :: FilePath -> IO ()
lpc_run fn = do
  p <- d50_load_sysex fn
  in_k <- M.pm_default_input
  out_k <- M.pm_default_output
  in_fd <- M.pm_open_input in_k
  out_fd <- M.pm_open_output out_k
  pm_run_proc 10 in_fd (lpc_recv_midi (p,out_fd))

-- * PRINT

d50_print :: String -> ([U8] -> [String]) -> FilePath -> IO ()
d50_print ix pp fn =
    if ix == "all"
    then d50_load_sysex fn >>= putStrLn . unlines . concatMap pp
    else d50_load_sysex fn >>= putStrLn . unlines . pp . (!! (read ix))

-- * SEND

parse_d50_ix :: String -> Maybe U8
parse_d50_ix s = if s == "tmp" then Nothing else Just (read s)

-- > send_patch Nothing 0 "/home/rohan/data/roland-d50/PND50-00.syx"
-- > send_patch (Just 10) 0 "/home/rohan/data/roland-d50/PND50-00.syx"
send_patch :: Maybe U8 -> U8 -> FilePath -> IO ()
send_patch d50_ix sysex_ix fn = do
  p <- d50_load_sysex fn
  send_patch_def d50_ix (p !! sysex_ix)

-- * SET

-- > set_wg_pitch_kf (1/8)
set_wg_pitch_kf :: (Eq n, Fractional n) => n -> IO ()
set_wg_pitch_kf r = send_sysex_def (map d50_dsc_gen (d50_wg_pitch_kf_dti r))

-- * MAIN

usage :: IO ()
usage =
    let h = ["load-on-program-change sysex-file"
            ,"print {ix|all} {name|pp-group} sysex-file..."
            ,"send patch {tmp|d50-ix} sysex-ix sysex-file"
            ,"set wg-pitch-kf ratio"]
    in putStrLn (unlines h)

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["load-on-program-change",fn] -> M.pm_with_midi (lpc_run fn)
    "print":ix:"name":fn_seq -> mapM_ (d50_print ix (return . patch_name)) fn_seq
    "print":ix:"pp-group":fn_seq -> mapM_ (d50_print ix d50_patch_group_pp) fn_seq
    ["send","patch",d50_ix,sysex_ix,fn] -> send_patch (parse_d50_ix d50_ix) (read sysex_ix) fn
    ["set","wg-pitch-kf",r] -> set_wg_pitch_kf (read r)
    _ -> usage
