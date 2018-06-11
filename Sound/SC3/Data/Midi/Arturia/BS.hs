module Sound.SC3.Data.Midi.Arturia.BS where

import Data.Word {- base -}

-- | Unsigned 8-bit integer (ie. Word8).
type U8 = Word8

enum_to_u8 :: Enum e => e -> U8
enum_to_u8 = fromIntegral . fromEnum

-- | System-exclusive data.
type SYSEX = [U8]

-- | General form of "SET" messages.
--
-- pp = parameter number,
-- cc = control ID (see 'control_id_dsc'),
-- vv = value
set_sysex :: U8 -> U8 -> U8 -> SYSEX
set_sysex pp cc vv = [0xF0,0x00,0x20,0x6B,0x7F,0x42,0x02,0x00,pp,cc,vv,0xF7]

{- | General form of "GET" messages.

pp = parameter number,
cc = contol ID

> request_sysex 0x01 0x20 == [0xF0,0x00,0x20,0x6B,0x7F,0x42,0x01,0x00,0x01,0x20,0xF7]

-}
request_sysex :: U8 -> U8 -> SYSEX
request_sysex pp cc = [0xF0,0x00,0x20,0x6B,0x7F,0x42,0x01,0x00,pp,cc,0xF7]

-- * Global

set_global_midi_channel :: U8 -> SYSEX
set_global_midi_channel ch = set_sysex 0x50 0x0B ch

set_cv_gate_interface_receive_channel :: U8 -> SYSEX
set_cv_gate_interface_receive_channel ch = set_sysex 0x50 0x0C ch

-- | There are three encoder acceleration modes.
--
-- > map toEnum [0,1,2] == [Slow,Medium,Fast]
data Acceleration = Slow | Medium | Fast deriving (Eq,Enum,Show)

-- 0=slow, 1=medium, 2=fast
set_encoder_acceleration :: U8 -> SYSEX
set_encoder_acceleration x = set_sysex 0x41 0x04 x

-- > map toEnum [0 .. 3] == [Linear,Logarithmic,Exponential,Full]
data Curve = Linear | Logarithmic | Exponential | Full deriving (Eq,Enum,Show)

-- 0=linear, 1=logarithmic, 2=exponential, 3=full
pad_velocity_curve :: U8 -> SYSEX
pad_velocity_curve x = set_sysex 0x41 0x03 x

-- * ENC = encoder

enc_cm_set_cc_sysex :: U8 -> U8 -> SYSEX
enc_cm_set_cc_sysex k cc = set_sysex 0x03 k cc

enc_cm_set_ch_sysex :: U8 -> U8 -> SYSEX
enc_cm_set_ch_sysex k ch = set_sysex 0x02 k ch

enc_cm_set_abs_sysex :: U8 -> (U8,U8,(U8,U8)) -> [SYSEX]
enc_cm_set_abs_sysex k (ch,cc,(l,r)) =
  [set_sysex 0x01 k 0x01 -- control-change mode
  ,set_sysex 0x02 k ch
  ,set_sysex 0x03 k cc
  ,set_sysex 0x04 k l
  ,set_sysex 0x05 k r
  ,set_sysex 0x06 k 0x00 -- absolute-value mode
  ]

enc_cm_set_basic_sysex :: U8 -> U8 -> [SYSEX]
enc_cm_set_basic_sysex ch cc0 = concatMap (\k -> enc_cm_set_abs_sysex (0x20 + k) (ch,cc0 + k,(0,127))) [0 .. 15]

-- * DSC = description

pad_mode_dsc :: [(U8,String)]
pad_mode_dsc =
  [(0x00,"OFF")
  ,(0x01,"CC_SWITCH/SILENT") -- cc = control-change
  ,(0x02,"UNKNOWN")
  ,(0x03,"UNKNOWN")
  ,(0x04,"UNKNOWN")
  ,(0x05,"UNKNOWN")
  ,(0x06,"UNKNOWN")
  ,(0x07,"MMC")
  ,(0x08,"CC_SWITCH")
  ,(0x09,"MNN") -- mnn = midi-note-number
  ,(0x0A,"UNKNOWN")
  ,(0x0B,"PROGRAM_CHANGE")]

-- | nm = NOTE MODE
pad_nm_param_dsc :: [(U8,String)]
pad_nm_param_dsc =
  [(0x02,"MIDI_CHANNEL")
  ,(0x03,"MNN")
  ,(0x06,"BEHAVIOUR")]

pad_nm_behaviour_dsc :: [(U8,String)]
pad_nm_behaviour_dsc =
  [(0x00,"TOGGLE")
  ,(0x01,"GATE")]

enc_mode_dsc :: [(U8,String)]
enc_mode_dsc =
  [(0x00,"OFF")
  ,(0x01,"CC") -- CC = CONTROL-CHANGE
  ,(0x02,"UNKNOWN")
  ,(0x03,"UNKNOWN")
  ,(0x04,"RPN/NRPN")]

-- cm = CONTROL-CHANGE MODE
enc_cm_param_dsc :: [(U8,String)]
enc_cm_param_dsc =
  [(0x01,"MODE")
  ,(0x02,"MIDI_CHANNEL")
  ,(0x03,"CC_NUMBER")
  ,(0x04,"MINIMA")
  ,(0x05,"MAXIMA")
  ,(0x06,"BEHAVIOUR")]

enc_cm_behaviour_dsc :: [(U8,String)]
enc_cm_behaviour_dsc =
  [(0x00,"ABSOLUTE")
  ,(0x01,"RELATIVE_1")
  ,(0x02,"RELATIVE_2")
  ,(0x03,"RELATIVE_3")]

-- ENCODER 1-16 = 0x20-0x2F
-- PAD 1-16 = 0x70-0x7F
control_id_dsc :: [(U8,String)]
control_id_dsc =
  [(0x30,"LEVEL/RATE")
  ,(0x58,"STOP")
  ,(0x59,"START")
  ,(0x5A,"CNTRL/SEQ")
  ,(0x5B,"EXTSYNC")
  ,(0x5C,"RECALL")
  ,(0x5D,"STORE")
  ,(0x5E,"SHIFT")
  ,(0x5F,"CHAN")]

{-

import qualified Sound.Midi.PM as PM

k <- PM.pm_output_by_name "Arturia BeatStep MIDI 1"
run = PM.pm_with_output_device k

run (\fd -> PM.pm_sysex_write fd (set_encoder_acceleration (enum_to_u8 Slow)))

run (\fd -> PM.pm_sysex_write fd (enc_cm_set_ch_sysex 0x20 0x00))
run (\fd -> PM.pm_sysex_write fd (enc_cm_set_cc_sysex 0x20 0x00))

run (\fd -> PM.pm_sysex_write_set fd (enc_cm_set_abs_sysex 0x20 (0x00,0x00,(0x00,0x7F))))
run (\fd -> PM.pm_sysex_write_set fd (enc_cm_set_basic_sysex 0 0))

PM.pm_with_output_device k (\fd -> PM.pm_cvm3_write fd (0x90,0x2C,0x7F))
PM.pm_with_output_device k (\fd -> PM.pm_cvm3_write fd (0x90,0x2C,0x00))

-}
