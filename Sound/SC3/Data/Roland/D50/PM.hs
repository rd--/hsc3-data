-- | D50 midi communication, using PortMidi bindings.
module Sound.SC3.Data.Roland.D50.PM where

import qualified Sound.Midi.PM as PM {- midi-osc -}

import Sound.SC3.Data.Roland.D50 {- hsc3-data -}

-- | Send ACK sysex.
--
-- > PM.pm_with_default_output (d50_send_ack 0)
d50_send_ack :: U8 -> PM.PM_FD -> IO ()
d50_send_ack ch fd = PM.pm_sysex_write fd (d50_ack_gen ch)

-- | Send EOD sysex.
d50_send_eod :: U8 -> PM.PM_FD -> IO ()
d50_send_eod ch fd = PM.pm_sysex_write fd (d50_eod_gen ch)

-- | Send WSD sysex.
d50_send_wsd :: U8 -> D50_ADDRESS -> U24 -> PM.PM_FD -> IO ()
d50_send_wsd ch addr sz fd = PM.pm_sysex_write fd (d50_wsd_gen ch addr sz)

-- | Send DSC sysex.
d50_send_dsc :: D50_DSC -> PM.PM_FD -> IO ()
d50_send_dsc dsc fd = PM.pm_sysex_write fd (d50_dsc_gen dsc)

-- | Receive ACK sysex, else error.
d50_recv_ack :: PM.PM_FD -> IO ()
d50_recv_ack fd = do
  syx <- PM.pm_sysex_read fd
  case d50_cmd_parse syx of
    Just (_,ACK_CMD,[],0) -> return ()
    _ -> error "d50_recv_ack?"

-- | Receive RQD sysex, else error.
d50_recv_rqd :: PM.PM_FD -> IO (D50_ADDRESS, U24)
d50_recv_rqd fd = do
  syx <- PM.pm_sysex_read fd
  case d50_addr_sz_cmd_parse syx of
    Just (_,RQD_CMD,addr,sz) -> return (addr,sz)
    _ -> error ("d50_recv_rqd? : " ++ d50_sysex_pp syx)

-- | Read sequence of DAT sysex, send ACK for each, until EOD sysex arrives.
d50_recv_dat_seq :: U8 -> (PM.PM_FD,PM.PM_FD) -> IO [D50_DSC]
d50_recv_dat_seq ch (in_fd,out_fd) =
  let recur st = do
        syx <- PM.pm_sysex_read in_fd
        case d50_cmd_parse syx of
          Just (_,DAT_CMD,_,_) ->
            d50_send_ack ch out_fd >>
            recur (d50_dsc_parse_err syx : st)
          Just (_,EOD_CMD,[],0) -> return (reverse st)
          _ -> error "d50_recv_dat_seq?"
  in recur []

-- | Read WSD sysex, send ACK, then run 'd50_recv_dat_seq'.
d50_recv_dat :: U8 -> (PM.PM_FD,PM.PM_FD) -> IO [D50_DSC]
d50_recv_dat ch (in_fd,out_fd) =  do
  syx <- PM.pm_sysex_read in_fd
  case d50_cmd_parse syx of
    Just (_,WSD_CMD,_,6) -> do
      d50_send_ack ch out_fd
      d50_recv_dat_seq ch (in_fd,out_fd)
    _ -> error "d50_recv_dat?"

-- | 'd50_recv_dat' with default MIDI I/O.
d50_recv_dat_def :: U8 -> IO [D50_DSC]
d50_recv_dat_def ch = PM.pm_with_io_def (d50_recv_dat ch)

-- | Run 'd50_recv_dat_def' then 'd50_bulk_data_transfer_parse'.
--
-- > (p,r) <- d50_recv_bulk_data 0
-- > map d50_patch_name p
d50_recv_bulk_data :: U8 -> IO ([D50_Patch],[D50_Reverb])
d50_recv_bulk_data ch = do
  d <- d50_recv_dat_def ch
  case d50_bulk_data_transfer_parse (d50_dsc_seq_join d) of
    Nothing -> error "d50_recv_bulk_data?"
    Just r -> return r

-- | Send each 'D50_DSC' then wait for 'ACK'.
d50_send_dat_seq :: [D50_DSC] -> (PM.PM_FD,PM.PM_FD) -> IO ()
d50_send_dat_seq dsc (in_fd,out_fd) =
  let f e = d50_send_dsc (dsc_set_cmd DAT_CMD e) out_fd >>
            d50_recv_ack in_fd
  in mapM_ f dsc

-- | Send WSD, recv ACK, run 'd50_send_dat_seq', send EOD.
d50_send_bulk_data :: U8 -> [D50_DSC] -> (PM.PM_FD,PM.PM_FD) -> IO ()
d50_send_bulk_data ch dsc (in_fd,out_fd) = do
  d50_send_wsd ch 0x8000 0x8780 out_fd
  d50_recv_ack in_fd
  d50_send_dat_seq dsc (in_fd,out_fd)
  d50_send_eod ch out_fd

-- | 'PM.pm_with_io_def' of 'd50_send_bulk_data'
d50_send_bulk_data_def :: U8 -> [D50_DSC] -> IO ()
d50_send_bulk_data_def ch dsc = PM.pm_with_io_def (d50_send_bulk_data ch dsc)

-- | Send patch data to temporary memory area, as DT1 command sequence.
d50_send_patch_tmp_fd :: D50_Patch -> PM.PM_FD -> IO ()
d50_send_patch_tmp_fd p fd = do
  let d = d50_dsc_gen_seq (DT1_CMD,0,0,p)
  -- mapM_ (putStrLn . d50_sysex_pp) d
  PM.pm_sysex_write_seq 20 fd d

{- | 'PM.pm_with_default_output' of 'd50_send_patch_tmp_fd'

> let fn = "/home/rohan/sw/hsc3-data/data/roland/d50/PN-D50-00.syx"
> (p,r) <- d50_load_sysex fn
> d50_send_patch_tmp_def (p !! 0)

-}
d50_send_patch_tmp_def :: D50_Patch -> IO ()
d50_send_patch_tmp_def = PM.pm_with_default_output . d50_send_patch_tmp_fd

{-

send_patch_work_area :: U8 -> [U8] -> (PM.PM_FD,PM.PM_FD) -> IO ()
send_patch_work_area ix p (in_fd,out_fd) = do
  let a = D50.d50_patch_memory_base ix
  PM.pm_sysex_write out_fd (D50.d50_wsd_gen 0 a (D50.u24_length p))
  D50.d50_recv_ack in_fd
  PM.pm_sysex_write_seq 10 out_fd (D50.d50_dsc_gen_seq (D50.DAT_CMD,0,a,p))

-}
