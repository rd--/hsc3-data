module Sound.SC3.Data.Roland.D50.PM where

import qualified Sound.Midi.PM as PM {- midi-osc -}

import Sound.SC3.Data.Roland.D50 {- hsc3-data -}

d50_send_ack :: U8 -> PM.PM_FD -> IO ()
d50_send_ack ch fd = PM.pm_sysex_write_err fd (d50_ack_gen ch)

d50_recv_dat_seq :: U8 -> (PM.PM_FD,PM.PM_FD) -> IO [DSC]
d50_recv_dat_seq ch (in_fd,out_fd) =
  let recur st = do
        syx <- PM.pm_read_sysex in_fd
        case d50_cmd_parse syx of
          Just (_,0x42,_,_) ->
            d50_send_ack ch out_fd >>
            recur (d50_dsc_parse_err syx : st)
          Just (_,0x45,[],0) -> return (reverse st)
          _ -> error "d50_recv_dat_seq?"
  in recur []

d50_recv_dat :: U8 -> (PM.PM_FD,PM.PM_FD) -> IO [DSC]
d50_recv_dat ch (in_fd,out_fd) =  do
  syx <- PM.pm_read_sysex in_fd
  case d50_cmd_parse syx of
    Just (_,0x40,_,6) ->
      PM.pm_sysex_write out_fd (d50_ack_gen ch) >>
      d50_recv_dat_seq ch (in_fd,out_fd)
    _ -> error "d50_recv_dat?"

d50_recv_dat_def :: U8 -> IO [DSC]
d50_recv_dat_def ch = PM.pm_with_io_def (d50_recv_dat ch)
