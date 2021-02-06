-- | Arturia Beatstep Controller utilities.
--
-- <https://www.untergeek.de/2014/11/taming-arturias-beatstep-sysex-codes-for-programming-via-ipad>
module Sound.SC3.Data.Midi.Arturia.BS where

import Data.Word {- base -}

-- | Unsigned 8-bit integer (ie. Word8).
type U8 = Word8

-- | Type-specialised 'fromEnum'.
enum_to_u8 :: Enum e => e -> U8
enum_to_u8 = fromIntegral . fromEnum

-- | System-exclusive data, bracketed by 0xF0 and 0xF7.
type SYSEX = [U8]

-- | Sysex prefix common to all packets.
common_sysex_prefix :: SYSEX
common_sysex_prefix = [0xF0,0x00,0x20,0x6B,0x7F,0x42]

-- | Add common_sysex_prefix to U8 sequence.
with_common_sysex_prefix :: [U8] -> SYSEX
with_common_sysex_prefix = (++) common_sysex_prefix

-- | General form of SET messages.
--
-- pp = parameter number,
-- cc = control ID (see 'control_id_dsc'),
-- vv = value
--
-- > set_sysex 0x00 0x21 0x40 == with_common_sysex_prefix [0x02,0x00,0x00,0x21,0x40,0xF7]
set_sysex :: U8 -> U8 -> U8 -> SYSEX
set_sysex pp cc vv = with_common_sysex_prefix [0x02,0x00,pp,cc,vv,0xF7]

-- | Identifier for control, ie. an encoder or a pad (0-15)
type Ctl_Id = U8

-- | 0x20-0x2F address the sixteen encoders
ctl_enc_ix0 :: U8
ctl_enc_ix0 = 0x20

-- | 0x70-0x7F address the sixteen pads.
ctl_pad_ix0 :: U8
ctl_pad_ix0 = 0x70

{- | General form of GET (REQUEST) messages.

pp = parameter number,
cc = control ID

> request_sysex 0x01 ctl_enc_ix0 == with_common_sysex_prefix [0x01,0x00,0x01,0x20,0xF7]

-}
request_sysex :: U8 -> U8 -> SYSEX
request_sysex pp cc = with_common_sysex_prefix [0x01,0x00,pp,cc,0xF7]

-- * Global

-- | Global midi channel (0-15).
--
-- > global_midi_channel_set 0x15 == with_common_sysex_prefix [0x02,0x00,0x40,0x06,0x15,0xF7]
global_midi_channel_set :: U8 -> SYSEX
global_midi_channel_set ch = set_sysex 0x40 0x06 ch -- corrected from 0x50 0x0B, see comments

-- | Global CV/Gate interface receive channel (0-15).
global_cv_gate_interface_receive_channel_set :: U8 -> SYSEX
global_cv_gate_interface_receive_channel_set ch = set_sysex 0x50 0x0C ch

-- | Enumeration of encoder acceleration modes.
--
-- > map toEnum [0,1,2] == [Slow,Medium,Fast]
data Acceleration = Slow | Medium | Fast deriving (Eq,Enum,Show)

-- | Global encoder acceleration (0-2)
global_encoder_acceleration_set :: Acceleration -> SYSEX
global_encoder_acceleration_set = set_sysex 0x41 0x04 . enum_to_u8

-- | Enumeration of pad velocity curves.
--
-- > map toEnum [0 .. 3] == [Linear,Logarithmic,Exponential,Full]
data Curve = Linear | Logarithmic | Exponential | Full deriving (Eq,Enum,Show)

-- | Set global pad velocity curve
global_pad_velocity_curve_set :: Curve -> SYSEX
global_pad_velocity_curve_set = set_sysex 0x41 0x03 . enum_to_u8

-- * Preset

-- | Store current settings to memory location (0-15)
preset_store :: U8 -> SYSEX
preset_store mm = with_common_sysex_prefix [0x06,mm,0xF7]

-- | Recall settings from memory location (0-15)
preset_recall :: U8 -> SYSEX
preset_recall mm = with_common_sysex_prefix [0x05,mm,0xF7]

-- * ENC = encoder, cm = CC-mode

data Encoder_Mode = Absolute | Relative_X40 | Relative_X00 | Relative_X10 deriving (Eq,Enum,Show)

{- | Encoder control-change mode.

k = control-id (0-15) (translated to 0x20-0x2F),
ch = channel, cc = control-change-number,
l = left, r = right,
md = mode
-}
enc_ccm_set :: Ctl_Id -> (U8,U8,(U8,U8),Encoder_Mode) -> [SYSEX]
enc_ccm_set j (ch,cc,(l,r),md) =
  let k = ctl_enc_ix0 + j
  in [set_sysex 0x01 k 0x01 -- control-change-mode
     ,set_sysex 0x02 k ch -- channel (setting to 0x41 uses the global midi channel?)
     ,set_sysex 0x03 k cc -- control-change-number
     ,set_sysex 0x04 k l -- absolute mode only
     ,set_sysex 0x05 k r -- absolute mode only
     ,set_sysex 0x06 k (enum_to_u8 md) -- 0=absolute-value 1-3=relative
     ]

-- | Set 16 encoders, only CC value differs per encoder.
enc_ccm_set_16 :: (U8,[U8],(U8,U8),Encoder_Mode) -> [SYSEX]
enc_ccm_set_16 (ch,cc_seq,rng,md) =
  let f j cc = enc_ccm_set j (ch,cc,rng,md)
  in concat (zipWith f [0 .. 15] cc_seq)

-- | Set only CC value (not channel or range or mode)
enc_ccm_set_cc :: Ctl_Id -> U8 -> SYSEX
enc_ccm_set_cc j cc = set_sysex 0x03 (ctl_enc_ix0 + j) cc

enc_ccm_set_cc_16 :: [U8] -> [SYSEX]
enc_ccm_set_cc_16 = let f j cc = enc_ccm_set_cc j cc in zipWith f [0 .. 15]

enc_ccm_set_ch :: Ctl_Id -> U8 -> SYSEX
enc_ccm_set_ch j ch = set_sysex 0x02 (ctl_enc_ix0 + j) ch

enc_ccm_set_value :: Ctl_Id -> U8 -> SYSEX
enc_ccm_set_value j n = set_sysex 0x00 (ctl_enc_ix0 + j) n

-- * PAD, nm = note-mode

pad_off_set :: U8 -> SYSEX
pad_off_set k = set_sysex 0x01 k 0x00

-- | Enumeration of pad switch modes.
data Switch_Mode = Toggle | Gate deriving (Eq,Enum,Show)

-- | Set pad to midi-note mode (mnm).
pad_mnm_set :: Ctl_Id -> (U8, U8, Switch_Mode) -> [SYSEX]
pad_mnm_set j (ch,mnn,md) =
  let k = ctl_pad_ix0 + j
  in [set_sysex 0x01 k 0x09 -- midi-note-number mode
     ,set_sysex 0x02 k ch
     ,set_sysex 0x03 k mnn
     ,set_sysex 0x06 k (enum_to_u8 md) -- 0=toggle,1=gate
     ]

-- | 'pad_mnm_set' for all pads.
pad_mnm_set_16 :: (U8, [U8], Switch_Mode) -> [SYSEX]
pad_mnm_set_16 (ch,mnn_seq,md) =
  let f j mnn = pad_mnm_set j (ch,mnn,md)
  in concat (zipWith f [0 .. 15] mnn_seq)

-- | Set 16 encoders, only CC value differs per encoder.
pad_mnm_set_16_mpe :: ([U8],Switch_Mode) -> [SYSEX]
pad_mnm_set_16_mpe (mnn_seq,md) =
  let f j mnn = pad_mnm_set j (j,mnn,md)
  in concat (zipWith f [0 .. 15] mnn_seq)

{- | The three CCM behaviors for pads are:

1. SWITCH-TOGGLE (0x01=0x08 + 0x06=0x00)
2. SWITCH-GATE (0x01=0x08 + 0x06=0x01)
3. PRESSURE(-GATE) (0x01=0x01)

The first two send the MINIMA value for off and the MAXIMA value for ON
(respectively at consecutive presses, and at press/release).

The third sends continuous pressure data in the range (MINIMA,MAXIMA) while pressed.

-}
data Pad_CCM_Mode = Pad_Switch Switch_Mode | Pad_Pressure deriving (Eq,Show)

pad_ccm_mode_ty :: Pad_CCM_Mode -> U8
pad_ccm_mode_ty x = case x of {Pad_Switch _ -> 0x08;Pad_Pressure -> 0x01}

pad_ccm_mode_sw :: Pad_CCM_Mode -> U8
pad_ccm_mode_sw x = case x of {Pad_Switch md -> enum_to_u8 md;Pad_Pressure -> 0x00}

-- | Set pad to control-change mode (ccm).
pad_ccm_set :: Ctl_Id -> (Pad_CCM_Mode, U8, U8, (U8, U8)) -> [SYSEX]
pad_ccm_set j (md,ch,cc,(off,on)) =
  let k = ctl_pad_ix0 + j
  in [set_sysex 0x01 k (pad_ccm_mode_ty md) -- control-change sub-type
     ,set_sysex 0x02 k ch
     ,set_sysex 0x03 k cc
     ,set_sysex 0x04 k off
     ,set_sysex 0x05 k on
     ,set_sysex 0x06 k (pad_ccm_mode_sw md) -- 0=toggle,1=gate
     ]

pad_ccm_set_16 :: (Pad_CCM_Mode, U8, [U8], (U8, U8)) -> [SYSEX]
pad_ccm_set_16 (md,ch,cc_seq,off_on) =
  let f j cc = pad_ccm_set j (md,ch,cc,off_on)
  in concat (zipWith f [0 .. 15] cc_seq)

-- | There are two LEDs for each pad, each can be on or off.
data LED_Status = LED_Off | LED_Red | LED_Blue | LED_Magenta deriving (Eq,Show)

-- | LED_Status to control byte.
led_status_to_u8 :: LED_Status -> U8
led_status_to_u8 x = case x of {LED_Off -> 0x00 ; LED_Red -> 0x01 ; LED_Blue -> 0x10 ; LED_Magenta -> 0x11}

-- | Set status of LED at pad /j/ (0-15).
pad_led_set :: Ctl_Id -> LED_Status -> SYSEX
pad_led_set j x = set_sysex 0x10 (ctl_pad_ix0 + j) (led_status_to_u8 x)

-- * DSC = description

-- | Descriptions of pad colour arguments.  There are two LEDS, red and blue.
pad_clr_dsc :: [(U8,String)]
pad_clr_dsc = [(0x00,"OFF"),(0x01,"RED"),(0x10,"BLUE"),(0x11,"MAGENTA")]

pad_mode_dsc :: [(U8,String)]
pad_mode_dsc =
  [(0x00,"OFF")
  ,(0x01,"CC_PRESSURE") -- cc = control-change
  ,(0x02,"UNKNOWN")
  ,(0x03,"UNKNOWN")
  ,(0x04,"UNKNOWN")
  ,(0x05,"UNKNOWN")
  ,(0x06,"UNKNOWN")
  ,(0x07,"MMC")
  ,(0x08,"CC_SWITCH")
  ,(0x09,"MNN") -- mnn = midi-note-number (ie. NOTE MODE)
  ,(0x0A,"UNKNOWN")
  ,(0x0B,"PROGRAM_CHANGE")]

-- | MMC command descriptions, <https://en.wikipedia.org/wiki/MIDI_Machine_Control>
pad_mmc_dsc :: [(U8,String)]
pad_mmc_dsc =
  [(0x01,"Stop")
  ,(0x02,"Play")
  ,(0x03,"Deferred Play")
  ,(0x04,"Fast Forward")
  ,(0x05,"Rewind")
  ,(0x06,"Record Strobe")
  ,(0x07,"Record Exit")
  ,(0x08,"Record Pause")
  ,(0x09,"Pause")
  ,(0x0A,"Eject")
  ,(0x0B,"Chase")
  ,(0x0D,"MMC Reset")
  ,(0x40,"Write") -- PARAM
  ,(0x44,"Goto") -- PARAM
  ,(0x47,"Shuttle") -- PARAM
  ]

-- | mnm = MIDI-NOTE MODE
pad_mnm_param_dsc :: [(U8,String)]
pad_mnm_param_dsc =
  [(0x02,"MIDI_CHANNEL")
  ,(0x03,"MNN")
  ,(0x06,"BEHAVIOUR")]

pad_behaviour_dsc :: [(U8,String)]
pad_behaviour_dsc =
  [(0x00,"TOGGLE")
  ,(0x01,"GATE")]

enc_mode_dsc :: [(U8,String)]
enc_mode_dsc =
  [(0x00,"OFF")
  ,(0x01,"CC") -- CC = CONTROL-CHANGE
  ,(0x02,"UNKNOWN")
  ,(0x03,"UNKNOWN")
  ,(0x04,"RPN/NRPN")]

-- ccm = CONTROL-CHANGE MODE
enc_ccm_param_dsc :: [(U8,String)]
enc_ccm_param_dsc =
  [(0x01,"MODE")
  ,(0x02,"MIDI_CHANNEL")
  ,(0x03,"CC_NUMBER")
  ,(0x04,"MINIMA")
  ,(0x05,"MAXIMA")
  ,(0x06,"BEHAVIOUR")]

enc_behaviour_dsc :: [(U8,String)]
enc_behaviour_dsc =
  [(0x00,"ABSOLUTE")
  ,(0x01,"RELATIVE_1")
  ,(0x02,"RELATIVE_2")
  ,(0x03,"RELATIVE_3")]

-- | The LHS controls are refered to by unique identifiers.
lhs_control_id_dsc :: [(U8,String)]
lhs_control_id_dsc =
  [(0x30,"Level/Rate")
  ,(0x58,"Stop")
  ,(0x59,"Start")
  ,(0x5A,"Cntrl/Seq")
  ,(0x5B,"Ext Sync")
  ,(0x5C,"Recall")
  ,(0x5D,"Store")
  ,(0x5E,"Shift")
  ,(0x5F,"Chan")]

{-

import qualified Sound.Midi.PM as PM

k <- PM.pm_output_by_name "Arturia BeatStep MIDI 1"
run = PM.pm_with_output_device k
msg x = run (\fd -> PM.pm_sysex_write_seq 5 fd x)

msg [pad_led_set 0 LED_Red]
msg [pad_led_set 0 LED_Blue]
msg [pad_led_set 0 LED_Off]

msg [set_sysex 0x00 0x21 0x40]

msg [global_midi_channel_set 0x15]
msg [global_encoder_acceleration_set Slow]

msg [enc_ccm_set_ch 0x00 0x00]
msg [enc_ccm_set_cc 0x00 0x00]

msg (enc_ccm_set 0x01 (0x00,0x00,(0x00,0x7F),Absolute))
msg (enc_ccm_set 0x01 (0x00,0x00,(0x00,0x7F),Relative_X00))

msg (enc_ccm_set_16 (0x00,[0x00 .. 0x0F],(0x00,0x7F),0x00))
msg (enc_ccm_set_cc_16 [0 .. 15])

msg (pad_mnm_set_16 (0x00,[0x3C .. ],Toggle))
msg (pad_mnm_set_16 (0x00,[0x3C .. ],Gate))
msg (pad_mnm_set_16_mpe ([0x3C ..],Gate))

msg (pad_ccm_set_16 (Pad_Switch Toggle,0,[0 .. 16],(0x00,0x7F)))
msg (pad_ccm_set_16 (Pad_Switch Gate,0,[0 .. 16],(0x00,0x7F)))

msg [global_pad_velocity_curve_set Linear]
msg [global_pad_velocity_curve_set Logarithmic]
msg [global_pad_velocity_curve_set Exponential]

msg (pad_ccm_set_16 (Pad_Pressure,0,[0 .. 16],(0x00,0x7F)))
msg (pad_ccm_set_16 (Pad_Pressure,0,[0 .. 16],(0x30,0x57)))

PM.pm_with_output_device k (\fd -> PM.pm_cvm3_write fd (0x90,0x3C,0x7F))
PM.pm_with_output_device k (\fd -> PM.pm_cvm3_write fd (0x90,0x00,0x00))

msg [request_sysex 0x01 ctl_enc_ix0]

-}
