-- | D50 midi communication, using PortMidi bindings.
module Sound.SC3.Data.Roland.D50.PM where

import qualified Sound.Midi.PM as PM {- midi-osc -}

import Sound.SC3.Data.Roland.D50 {- hsc3-data -}

-- | Send ACK sysex.
--
-- > PM.pm_with_default_output (d50_send_ack 0)
d50_send_ack :: U8 -> PM.PM_FD -> IO ()
d50_send_ack ch fd = PM.pm_sysex_write fd (d50_ack_gen ch)

-- | Receive ACK sysex, else error.
d50_recv_ack :: PM.PM_FD -> IO ()
d50_recv_ack fd = do
  syx <- PM.pm_sysex_read fd
  case d50_cmd_parse syx of
    Just (_,ACK_CMD,[],0) -> return ()
    _ -> error "d50_recv_ack?"

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
