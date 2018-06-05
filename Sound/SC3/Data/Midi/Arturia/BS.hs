module Sound.SC3.Data.Midi.Arturia.BS where

-- pp = parameter number (1-6)
-- cc = controller number (0x20-0x2F=encoders,0x70-0x7F=pads,0x30=level,)

import Data.Word {- base -}

type U8 = Word8

set_sysex :: U8 -> U8 -> U8 -> [U8]
set_sysex pp cc vv = [0xF0,0x00,0x20,0x6B,0x7F,0x42,0x02,0x00,pp,cc,vv,0xF7]

-- > request_sysex 0x01 0x20
request_sysex :: U8 -> U8 -> [U8]
request_sysex pp cc = [0xF0,0x00,0x20,0x6B,0x7F,0x42,0x01,0x00,pp,cc,0xF7]

pad_mode_dsc :: [(U8,String)]
pad_mode_dsc =
  [(0x00,"OFF")
  ,(0x01,"CC_SWITCH/SILENT")
  ,(0x07,"MMC")
  ,(0x08,"CC_Switch")
  ,(0x09,"NOTE")
  ,(0x0B,"PROGRAM_CHANGE")]

enc_mode_dsc :: [(U8,String)]
enc_mode_dsc =
  [(0x00,"OFF")
  ,(0x01,"MIDI_CC")
  ,(0x04,"RPN/NRPN")]

enc_param_dsc :: [(U8,String)]
enc_param_dsc =
  [(0x02,"MIDI_CHANNEL")
  ,(0x03,"CC_NUMBER")
  ,(0x04,"MINIMA")
  ,(0x05,"MAXIMA")
  ,(0x03,"BEHAVIOUR")]

enc_behaviour_dsc :: [(U8,String)]
enc_behaviour_dsc =
  [(0x00,"ABSOLUTE")
  ,(0x01,"RELATIVE_1")
  ,(0x02,"RELATIVE_2")
  ,(0x03,"RELATIVE_3")]

control_key_codes :: [(String,U8)]
control_key_codes =
  [("STOP",0x58)
  ,("START",0x59)
  ,("CNTRL/SEQ",0x5A)
  ,("EXTSYNC",0x5B)
  ,("RECALL",0x5C)
  ,("STORE",0x5D)
  ,("SHIFT",0x5E)
  ,("CHAN",0x5F)]
