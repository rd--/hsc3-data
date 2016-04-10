import Control.Monad {- base -}
import System.Environment {- base -}

import Sound.OSC {- hosc -}
import qualified Sound.PortMidi as M {- portmidi -}
import qualified Sound.Midi.PM as M {- midi-osc -}
import Sound.SC3.Data.Roland.D50 {- hsc3-data -}

-- * LOAD ON PROGRAM CHANGE (LPC)

-- > map ms_to_sec [1,10,50,100,1000] == [0.001,0.01,0.05,0.1,1]
ms_to_sec :: Int -> Double
ms_to_sec n = fromIntegral n / 1000

sleep_ms :: Int -> IO ()
sleep_ms = pauseThread . ms_to_sec

send_sysex :: M.PMStream -> [[U8]] -> IO ()
send_sysex fd = mapM_ (\x -> M.pm_sysex_write fd x >> sleep_ms 50)

send_patch :: M.PMStream -> [U8] -> IO ()
send_patch fd p = let d = d50_dti_gen_seq (0,0,p) in send_sysex fd d

patch_pp :: [U8] -> IO ()
patch_pp = putStrLn. unlines . d50_patch_group_pp

pm_run_proc :: Int -> M.PMStream -> M.PROC_F U8 -> IO ()
pm_run_proc dt fd proc_f =
    let recur = do
          r <- M.pm_process_events proc_f fd
          when (not r) (sleep_ms dt)
          recur
    in recur

lpc_recv_midi :: ([[U8]], M.PMStream) -> (U8,U8,U8,U8) -> IO ()
lpc_recv_midi (p,fd) m =
    case m of
      (0xC0,n,0,0) -> print n >> patch_pp (p !! n) >> send_patch fd (p !! n)
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

d50_print :: String -> FilePath -> ([U8] -> [String]) -> IO ()
d50_print ix fn pp =
    if ix == "all"
    then d50_load_sysex fn >>= putStrLn . unlines . concatMap pp
    else d50_load_sysex fn >>= putStrLn . unlines . pp . (!! (read ix))

usage :: IO ()
usage =
    let h = ["load-on-program-change sysex-file"
            ,"print ix|all name|pp-group file-name"]
    in putStrLn (unlines h)

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["load-on-program-change",fn] -> M.pm_with_midi (lpc_run fn)
    ["print",ix,"name",fn] -> d50_print ix fn (return . patch_name)
    ["print",ix,"pp-group",fn] -> d50_print ix fn d50_patch_group_pp
    _ -> usage
