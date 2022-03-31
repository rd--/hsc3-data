-- | Arturia Beatstep Controller utilities.
--
-- <https://www.untergeek.de/2014/11/taming-arturias-beatstep-sysex-codes-for-programming-via-ipad>
-- <https://downloads.arturia.net/products/beatstep/manual/BeatStep_Manual_1_0_1_EN.pdf>
module Sound.SC3.Data.Midi.Arturia.BeatStep where

import Data.Char {- base -}
import System.FilePath {- filepath -}
import Text.Printf {- base -}

import Sound.Midi.Common {- midi-osc -}
import Sound.Midi.Type {- midi-osc -}

-- | Type-generalised 'fromEnum'.
enum_to_num :: (Enum e, Num n) => e -> n
enum_to_num = fromIntegral . fromEnum

-- | Type-specialised 'fromEnum'.
enum_to_u8 :: Enum e => e -> Byte
enum_to_u8 = enum_to_num

-- | Sysex prefix common to all packets.
common_sysex_prefix :: Num n => Sysex n
common_sysex_prefix = [0xF0,0x00,0x20,0x6B,0x7F,0x42]

-- | Add common_sysex_prefix to byte sequence.
with_common_sysex_prefix :: Num n => [n] -> Sysex n
with_common_sysex_prefix = (++) common_sysex_prefix

-- | General form of SET messages.
--
-- pp = parameter number,
-- cc = control ID (see 'control_id_dsc'),
-- vv = value
--
-- > set_sysex 0x00 0x21 0x40 == with_common_sysex_prefix [0x02,0x00,0x00,0x21,0x40,0xF7]
set_sysex :: Num n => n -> n -> n -> Sysex n
set_sysex pp cc vv = with_common_sysex_prefix [0x02,0x00,pp,cc,vv,0xF7]

-- | Identifier for control, ie. an encoder or a pad (0-15)
type Ctl_Id = Nibble

-- | 0x20-0x2F address the sixteen encoders
ctl_enc_ix0 :: Num n => n
ctl_enc_ix0 = 0x20

-- | 0x70-0x7F address the sixteen pads.
ctl_pad_ix0 :: Num n => n
ctl_pad_ix0 = 0x70

{- | General form of GET (REQUEST) messages.

pp = parameter number,
cc = control ID

> request_sysex 0x01 ctl_enc_ix0 == with_common_sysex_prefix [0x01,0x00,0x01,0x20,0xF7]

-}
request_sysex :: Num n => n -> n -> Sysex n
request_sysex pp cc = with_common_sysex_prefix [0x01,0x00,pp,cc,0xF7]

-- * Global

-- | Global midi channel (0-15) [untested] (pressing CHAN and PAD-N sets the global channel to N)
--
-- > global_midi_channel_set 0x0F == [0xF0,0x00,0x20,0x6B,0x7F,0x42,0x02,0x00,0x40,0x06,0x0F,0xF7]
global_midi_channel_set :: Num n => n -> Sysex n
global_midi_channel_set = set_sysex 0x40 0x06 -- corrected from 0x50 0x06 & also 0x50 0x0B, see comments

-- | Global CV/Gate interface receive channel (0-15) [untested]
global_cv_gate_interface_receive_channel_set :: Num n => n -> Sysex n
global_cv_gate_interface_receive_channel_set = set_sysex 0x50 0x0C

-- | Enumeration of encoder acceleration modes.
--
-- > map toEnum [0,1,2] == [Slow,Medium,Fast]
data Acceleration = Slow | Medium | Fast deriving (Eq,Enum,Show)

-- | Global encoder acceleration (0-2)
global_encoder_acceleration_set :: Num n => Acceleration -> Sysex n
global_encoder_acceleration_set = set_sysex 0x41 0x04 . enum_to_num

-- | Enumeration of pad velocity curves.
--
-- > map toEnum [0 .. 3] == [Linear,Logarithmic,Exponential,Full]
data Curve = Linear | Logarithmic | Exponential | Full deriving (Eq,Enum,Show)

-- | Set global pad velocity curve
global_pad_velocity_curve_set :: Num n => Curve -> Sysex n
global_pad_velocity_curve_set = set_sysex 0x41 0x03 . enum_to_num

-- * Preset

-- | Store current settings to memory location (0-15)
preset_store :: Num n => n -> Sysex n
preset_store mm = with_common_sysex_prefix [0x06,mm,0xF7]

-- | Recall settings from memory location (0-15)
preset_recall :: Num n => n -> Sysex n
preset_recall mm = with_common_sysex_prefix [0x05,mm,0xF7]

-- * ENC = encoder, cc = CC-mode

data Encoder_Mode = Absolute | Relative_X40 | Relative_X00 | Relative_X10 deriving (Eq,Enum,Show)

encoder_mode_str :: Encoder_Mode -> String
encoder_mode_str x = ["ABS","REL","REL_00","REL_10"] !! fromEnum x

{- | Encoder control-change mode.

k = control-id (0-15) (translated to 0x20-0x2F),
ch = channel, cc = control-change-number,
l = left, r = right,
md = mode
-}
enc_cc_set :: Ctl_Id -> (Byte,Byte,(Byte,Byte),Encoder_Mode) -> [Sysex Byte]
enc_cc_set j (ch,cc,(l,r),md) =
  let k = ctl_enc_ix0 + j
  in [set_sysex 0x01 k 0x01 -- control-change-mode
     ,set_sysex 0x02 k ch -- channel (setting to 0x41 uses the global midi channel?)
     ,set_sysex 0x03 k cc -- control-change-number
     ,set_sysex 0x04 k l -- absolute mode only
     ,set_sysex 0x05 k r -- absolute mode only
     ,set_sysex 0x06 k (enum_to_u8 md) -- 0=absolute-value 1-3=relative
     ]

-- | Set 16 encoders, only CC value differs per encoder.
enc_cc_set_16 :: (Byte,[Byte],(Byte,Byte),Encoder_Mode) -> [Sysex Byte]
enc_cc_set_16 (ch,cc_seq,rng,md) =
  let f j cc = enc_cc_set j (ch,cc,rng,md)
  in concat (zipWith f [0 .. 15] cc_seq)

-- | Set only CC value (not channel or range or mode)
enc_cc_set_cc :: Ctl_Id -> Byte -> Sysex Byte
enc_cc_set_cc j = set_sysex 0x03 (ctl_enc_ix0 + j)

enc_cc_set_cc_16 :: [Byte] -> [Sysex Byte]
enc_cc_set_cc_16 = let f j cc = enc_cc_set_cc j cc in zipWith f [0 .. 15]

enc_cc_set_ch :: Ctl_Id -> Byte -> Sysex Byte
enc_cc_set_ch j = set_sysex 0x02 (ctl_enc_ix0 + j)

enc_cc_set_value :: Ctl_Id -> Byte -> Sysex Byte
enc_cc_set_value j = set_sysex 0x00 (ctl_enc_ix0 + j)

-- * ENC PN MODE

-- | (N/R)PN Mode
data PN_Mode = NRPN | RPN deriving (Eq,Enum,Show)

-- | (N/R)PN Type
data PN_Type = Coarse | Fine deriving (Eq,Show)

-- | 'PN_Type' to byte code.
pn_type_to_u8 :: Num n => PN_Type -> n
pn_type_to_u8 x = case x of {Coarse -> 0x06 ; Fine -> 0x26}

{- | Encoder (N/R)PN mode.

k = control-id (0-15) (translated to 0x20-0x2F),
ch = channel, ty = coarse/fine,
msb = most-significant-byte-index, lsb = least-significant-byte-index,
md = mode

The device only sends either the LSB or the MSB value, ie. this does not send 14-bit encoder data.
-}
enc_pn_set :: Ctl_Id -> (Byte,PN_Type,(Byte,Byte),PN_Mode) -> [Sysex Byte]
enc_pn_set j (ch,ty,(msb,lsb),md) =
  let k = ctl_enc_ix0 + j
  in [set_sysex 0x01 k 0x04 -- nrpn/rpn-mode
     ,set_sysex 0x02 k ch -- channel
     ,set_sysex 0x03 k (pn_type_to_u8 ty) -- coarse/fine 0x06=coarse 0x26=fine
     ,set_sysex 0x04 k msb -- MSB index
     ,set_sysex 0x05 k lsb -- LSB index
     ,set_sysex 0x06 k (enum_to_u8 md) -- 0=NRPN 1=RPN
     ]

-- * PAD-NM (note-mode)

-- | Enumeration of pad switch modes.
data Switch_Mode = Toggle | Gate deriving (Eq,Enum,Show)

-- | cs=consecutive, rl=release
switch_mode_str :: Switch_Mode -> String
switch_mode_str x = case x of {Toggle -> "CS" ; Gate -> "RL"}

-- | Set pad to midi-note mode (mn).
pad_mn_set :: Ctl_Id -> (Byte, Byte, Switch_Mode) -> [Sysex Byte]
pad_mn_set j (ch,mnn,md) =
  let k = ctl_pad_ix0 + j
  in [set_sysex 0x01 k 0x09 -- midi-note-number mode
     ,set_sysex 0x02 k ch
     ,set_sysex 0x03 k mnn
     ,set_sysex 0x06 k (enum_to_u8 md) -- 0=toggle,1=gate
     ]

-- | 'pad_mn_set' for all pads.
pad_mn_set_16 :: (Byte, [Byte], Switch_Mode) -> [Sysex Byte]
pad_mn_set_16 (ch,mnn_seq,md) =
  let f j mnn = pad_mn_set j (ch,mnn,md)
  in concat (zipWith f [0 .. 15] mnn_seq)

-- | Set 16 encoders, only CC value differs per encoder.
pad_mn_set_16_mpe :: ([Byte],Switch_Mode) -> [Sysex Byte]
pad_mn_set_16_mpe (mnn_seq,md) =
  let f j mnn = pad_mn_set j (j,mnn,md)
  in concat (zipWith f [0 .. 15] mnn_seq)

-- * PAD-CC (control-change)

{- | The three CC behaviors for pads are:

1. SWITCH-TOGGLE (0x01=0x08 + 0x06=0x00)
2. SWITCH-GATE (0x01=0x08 + 0x06=0x01)
3. PRESSURE(-GATE) (0x01=0x01)

The first two send the MINIMA value for off and the MAXIMA value for ON
(respectively at consecutive presses, and at press/release).

The third sends continuous pressure data in the range (MINIMA,MAXIMA) while pressed.

-}
data Pad_CC_Mode = Pad_Switch Switch_Mode | Pad_Pressure deriving (Eq,Show)

pad_cc_mode_ty :: Num n => Pad_CC_Mode -> n
pad_cc_mode_ty x = case x of {Pad_Switch _ -> 0x08;Pad_Pressure -> 0x01}

pad_cc_mode_sw :: Num n => Pad_CC_Mode -> n
pad_cc_mode_sw x = case x of {Pad_Switch md -> enum_to_num md;Pad_Pressure -> 0x00}

pad_cc_mode_str :: Pad_CC_Mode -> String
pad_cc_mode_str x = case x of {Pad_Switch Toggle -> "SW1" ; Pad_Switch Gate -> "SW2" ; Pad_Pressure -> "PRS"}

-- | Set pad to control-change mode (ccm).
pad_cc_set :: Ctl_Id -> (Pad_CC_Mode, Byte, Byte, (Byte, Byte)) -> [Sysex Byte]
pad_cc_set j (md,ch,cc,(off,on)) =
  let k = ctl_pad_ix0 + j
  in [set_sysex 0x01 k (pad_cc_mode_ty md) -- control-change sub-type
     ,set_sysex 0x02 k ch
     ,set_sysex 0x03 k cc
     ,set_sysex 0x04 k off
     ,set_sysex 0x05 k on
     ,set_sysex 0x06 k (pad_cc_mode_sw md) -- 0=toggle,1=gate
     ]

pad_cc_set_16 :: (Pad_CC_Mode, Byte, [Byte], (Byte, Byte)) -> [Sysex Byte]
pad_cc_set_16 (md,ch,cc_seq,off_on) =
  let f j cc = pad_cc_set j (md,ch,cc,off_on)
  in concat (zipWith f [0 .. 15] cc_seq)

-- * PAD-PC (program-change)

-- | Set pad /j/ to send a program (/prg/) & bank select (/lsb/,/msb/) message on channel /ch/
pad_pc_set :: Ctl_Id -> (Byte, Byte, (Byte, Byte)) -> [Sysex Byte]
pad_pc_set j (ch,prg,(lsb,msb)) =
  let k = ctl_pad_ix0 + j
  in [set_sysex 0x01 k 0x0B -- program-change
     ,set_sysex 0x02 k ch -- channel
     ,set_sysex 0x03 k prg -- program-number
     ,set_sysex 0x04 k lsb -- bank-number (lsb)
     ,set_sysex 0x05 k msb -- bank-number (msb)
     ]

-- | Set pads to select program-change within indicated bank.
pad_pc_set_16 :: (Byte, [Byte], (Byte, Byte)) -> [Sysex Byte]
pad_pc_set_16 (ch,pc_seq,bank) =
  let f j pc = pad_pc_set j (ch,pc,bank)
  in concat (zipWith f [0 .. 15] pc_seq)

-- * LED

-- | There are two LEDs, on red and one blue, and each can be on or off.
data LED_Status = LED_Off | LED_Red | LED_Blue | LED_Magenta deriving (Eq,Show)

-- | PP LED_Status
led_status_str :: LED_Status -> String
led_status_str = map toUpper . drop 4 . show

-- | LED_Status to control byte.
led_status_to_u8 :: LED_Status -> Byte
led_status_to_u8 x = case x of {LED_Off -> 0x00 ; LED_Red -> 0x01 ; LED_Blue -> 0x10 ; LED_Magenta -> 0x11}

-- | Sequence of all control indices that have LEDs, the LHS controls (except STOP) and the pads.
led_ix_set :: [Byte]
led_ix_set = [0x59 .. 0x5F] ++ [0x70 .. 0x7F]

-- | Set status of LED at index /k/ (either a PAD index or a LHS control index)
led_set :: Byte -> LED_Status -> Sysex Byte
led_set k x = set_sysex 0x10 k (led_status_to_u8 x)

-- | Set status of LED at pad /j/ (0-15).
led_pad_set :: Ctl_Id -> LED_Status -> Sysex Byte
led_pad_set j = led_set (ctl_pad_ix0 + j)

-- | The Cntrl-Seq light is ordinarily on, turn it off.
led_cntrl_seq_off :: Sysex Byte
led_cntrl_seq_off = led_set 0x5A LED_Off

-- * DSC = description

-- | Descriptions of pad colour arguments.  There are two LEDS, red and blue.
pad_clr_dsc :: Num n => [(n, String)]
pad_clr_dsc = [(0x00,"OFF"),(0x01,"RED"),(0x10,"BLUE"),(0x11,"MAGENTA")]

pad_mode_dsc :: Num n => [(n, String)]
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
pad_mmc_dsc :: Num n => [(n, String)]
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
pad_mn_param_dsc :: Num n => [(n, String)]
pad_mn_param_dsc =
  [(0x02,"MIDI_CHANNEL")
  ,(0x03,"MNN")
  ,(0x06,"BEHAVIOUR")]

pad_behaviour_dsc :: Num n => [(n, String)]
pad_behaviour_dsc =
  [(0x00,"TOGGLE")
  ,(0x01,"GATE")]

enc_mode_dsc :: Num n => [(n, String)]
enc_mode_dsc =
  [(0x00,"OFF")
  ,(0x01,"CC") -- CC = CONTROL-CHANGE
  ,(0x02,"UNKNOWN")
  ,(0x03,"UNKNOWN")
  ,(0x04,"RPN/NRPN")]

-- cc = CONTROL-CHANGE MODE
enc_cc_param_dsc :: Num n => [(n, String)]
enc_cc_param_dsc =
  [(0x01,"MODE")
  ,(0x02,"MIDI_CHANNEL")
  ,(0x03,"CC_NUMBER")
  ,(0x04,"MINIMA")
  ,(0x05,"MAXIMA")
  ,(0x06,"BEHAVIOUR")]

enc_behaviour_dsc :: Num n => [(n, String)]
enc_behaviour_dsc =
  [(0x00,"ABSOLUTE")
  ,(0x01,"RELATIVE_1")
  ,(0x02,"RELATIVE_2")
  ,(0x03,"RELATIVE_3")]

-- | The LHS controls are refered to by unique identifiers.
lhs_control_id_dsc :: Num n => [(n, String)]
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

-- * LIBRARY

type Sysex_LIB = [(String, [Sysex Byte])]

sysex_lib_accel :: Sysex_LIB
sysex_lib_accel =
  [("enc-acceleration-SLOW",[global_encoder_acceleration_set Slow])
  ,("enc-acceleration-MEDIUM",[global_encoder_acceleration_set Medium])
  ,("enc-acceleration-FAST",[global_encoder_acceleration_set Fast])]

sysex_lib_curve :: Sysex_LIB
sysex_lib_curve =
  [("pad-velocity-curve-LINEAR",[global_pad_velocity_curve_set Linear])
  ,("pad-velocity-curve-LOGARITHMIC",[global_pad_velocity_curve_set Logarithmic])
  ,("pad-velocity-curve-EXPONENTIAL",[global_pad_velocity_curve_set Exponential])
  ,("pad-velocity-curve-FULL",[global_pad_velocity_curve_set Full])]

sysex_lib_channel :: Sysex_LIB
sysex_lib_channel =
  [("midi-channel-C00",[global_midi_channel_set 0])
  ,("cv-midi-channel-C00",[global_cv_gate_interface_receive_channel_set 0])]

sysex_lib_enc_ch :: Sysex_LIB
sysex_lib_enc_ch =
  let mk_enc_cc_ch ch ix md =
        (printf "enc-cc-seq-C%02X-I%02X-%s" ch ix (encoder_mode_str md)
        ,enc_cc_set_16 (ch,map (+ ix) [0x00 .. 0x0F],(0x00,0x7F),md))
  in [mk_enc_cc_ch ch ix md | ch <- [0..1]
                            , ix <- [0x00,0x10 .. 0x70]
                            , md <- [Absolute,Relative_X40]]

sysex_lib_enc :: Sysex_LIB
sysex_lib_enc =
  let mk_enc_cc ix md =
        (printf "enc-cc-seq-I%02X-%s" ix (encoder_mode_str md)
        ,enc_cc_set_16 (0x41,map (+ ix) [0x00 .. 0x0F],(0x00,0x7F),md))
  in [mk_enc_cc ix md | ix <- [0x00,0x10 .. 0x70]
                      , md <- [Absolute,Relative_X40]]

sysex_lib_led :: Sysex_LIB
sysex_lib_led =
  let mk_led_all x = map (\k -> set_sysex 0x10 k (led_status_to_u8 x)) led_ix_set
  in [("led-all-OFF",mk_led_all LED_Off)
     ,("led-all-BLUE",mk_led_all LED_Blue)
     ,("led-all-MAGENTA",mk_led_all LED_Magenta)
     ,("led-all-RED",mk_led_all LED_Red)
     ,("led-cntrl-seq-OFF",[led_cntrl_seq_off])]

sysex_lib_led_pad :: Sysex_LIB
sysex_lib_led_pad =
  let mk_led_pad x =
        map (\j -> (printf "led-pad-%02X-%s" j (led_status_str x)
                   ,[led_pad_set j x])) [0 .. 15]
  in concatMap mk_led_pad [LED_Off,LED_Red,LED_Blue,LED_Magenta]

sysex_lib_pad_cc_ch :: Sysex_LIB
sysex_lib_pad_cc_ch =
  let mk_pad_cc ch ix md =
        (printf "pad-cc-seq-C%02X-I%02X-%s" ch ix (pad_cc_mode_str md)
        ,pad_cc_set_16 (md,ch,map (+ ix) [0x00 .. 0x0F],(0x00,0x7F)))
  in [mk_pad_cc ch ix md | ch <- [0..3]
                         , ix <- [0, 0x10 .. 0x70]
                         , md <- [Pad_Switch Toggle,Pad_Switch Gate,Pad_Pressure]]

sysex_lib_pad_cc :: Sysex_LIB
sysex_lib_pad_cc =
  let mk_pad_cc ix md =
        (printf "pad-cc-seq-I%02X-%s" ix (pad_cc_mode_str md)
        ,pad_cc_set_16 (md,0x41,map (+ ix) [0x00 .. 0x0F],(0x00,0x7F)))
  in [mk_pad_cc ix md | ix <- [0, 0x10 .. 0x70]
                      , md <- [Pad_Switch Toggle,Pad_Switch Gate,Pad_Pressure]]

sysex_lib_pad_mn :: Sysex_LIB
sysex_lib_pad_mn =
  let mk_pad_mn ix md =
        (printf "pad-mn-seq-I%02X-%s" ix (switch_mode_str md)
        ,pad_mn_set_16 (0x41,map (+ ix) [0x00 .. 0x0F],md))
  in [mk_pad_mn ix md | ix <- [0x30,0x3C,0x48]
                      , md <- [Toggle,Gate]]

-- | Library of named Sysex message sequences.
--
-- > map fst sysex_library
sysex_library :: [(String, [Sysex Byte])]
sysex_library =
  concat [sysex_lib_accel,sysex_lib_curve
         ,sysex_lib_enc
         ,sysex_lib_led
         ,sysex_lib_pad_cc,sysex_lib_pad_mn]

-- | Write 'sysex_library' to indicated directory.
--
-- > sysex_library_store "/home/rohan/sw/hsc3-data/data/midi/arturia/beatstep/"
sysex_library_store :: FilePath -> IO ()
sysex_library_store dir = do
  let wr (nm,syx) = bytes_store (dir </> nm <.> "syx") (concat syx)
  mapM_ wr sysex_library

{-

import qualified Sound.Midi.Pm as Pm

k <- Pm.pm_output_by_name "Arturia BeatStep MIDI 1"
run = Pm.pm_with_output_device k
msg x = run (\fd -> Pm.pm_sysex_write_seq 5 fd x)

msg (map (\k -> set_sysex 0x10 k (led_status_to_u8 LED_Red)) led_ix_set)
msg (map (\k -> set_sysex 0x10 k (led_status_to_u8 LED_Blue)) led_ix_set)
msg (map (\k -> set_sysex 0x10 k (led_status_to_u8 LED_Magenta)) led_ix_set)
msg (map (\k -> set_sysex 0x10 k (led_status_to_u8 LED_Off)) led_ix_set)

msg [led_cntrl_seq_off]

msg [global_midi_channel_set 0x0A] -- ?
msg [global_encoder_acceleration_set Slow]

msg (enc_cc_set_16 (0x41,[0x00 .. 0x0F],(0x00,0x7F),Absolute))
msg (enc_cc_set_16 (0x41,[0x00 .. 0x0F],(0x00,0x7F),Relative_X40))

msg (enc_pn_set 0x00 (0x00,Coarse,(0x20,0x40),NRPN))
msg (enc_pn_set 0x00 (0x00,Fine,(0x20,0x40),NRPN))

msg (pad_mn_set_16 (0x00,[0x3C .. ],Toggle))
msg (pad_mn_set_16 (0x00,[0x3C .. ],Gate))
msg (pad_mn_set_16_mpe ([0x3C ..],Gate))

msg (pad_cc_set_16 (Pad_Switch Toggle,0,[0 .. 16],(0x00,0x7F)))
msg (pad_cc_set_16 (Pad_Switch Gate,0,[0 .. 16],(0x00,0x7F)))

msg (pad_pc_set_16 (0x00,[0x10 ..],(0x10,0x01)))

msg [global_pad_velocity_curve_set Linear]
msg [global_pad_velocity_curve_set Logarithmic]
msg [global_pad_velocity_curve_set Exponential]

msg (pad_cc_set_16 (Pad_Pressure,0,[0 .. 16],(0x00,0x7F)))
msg (pad_cc_set_16 (Pad_Pressure,0,[0 .. 16],(0x30,0x57)))

Pm.pm_with_output_device k (\fd -> Pm.pm_cvm3_write fd (0x90,0x3C,0x7F))
Pm.pm_with_output_device k (\fd -> Pm.pm_cvm3_write fd (0x90,0x00,0x00))

msg [request_sysex 0x01 ctl_enc_ix0]

-}
