-- | Arturia Beatstep Controller utilities.
--
-- <https://www.untergeek.de/2014/11/taming-arturias-beatstep-sysex-codes-for-programming-via-ipad>
-- <https://downloads.arturia.net/products/beatstep/manual/BeatStep_Manual_1_0_1_EN.pdf>
module Sound.Sc3.Data.Midi.Arturia.BeatStep where

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

{- | General form of set messages.

pp = parameter number,
cc = control id (see 'control_id_dsc'),
vv = value

>>> set_sysex 0x00 0x21 0x40 == with_common_sysex_prefix [0x02,0x00,0x00,0x21,0x40,0xF7]
True
-}
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

{- | General form of Get (Request) messages.

pp = parameter number,
cc = control id

>>> request_sysex 0x01 ctl_enc_ix0 == with_common_sysex_prefix [0x01,0x00,0x01,0x20,0xF7]
True
-}
request_sysex :: Num n => n -> n -> Sysex n
request_sysex pp cc = with_common_sysex_prefix [0x01,0x00,pp,cc,0xF7]

-- * Global

{- | Global midi channel (0-15) [untested] (pressing chan and pad-n sets the global channel to n)

>>> global_midi_channel_set 0x0F == [0xF0,0x00,0x20,0x6B,0x7F,0x42,0x02,0x00,0x40,0x06,0x0F,0xF7]
True
-}
global_midi_channel_set :: Num n => n -> Sysex n
global_midi_channel_set = set_sysex 0x40 0x06 -- corrected from 0x50 0x06 & also 0x50 0x0B, see comments

-- | Global Cv/Gate interface receive channel (0-15) [untested]
global_cv_gate_interface_receive_channel_set :: Num n => n -> Sysex n
global_cv_gate_interface_receive_channel_set = set_sysex 0x50 0x0C

{- | Enumeration of encoder acceleration modes.

>>> map toEnum [0,1,2] == [Slow,Medium,Fast]
-}
data Acceleration = Slow | Medium | Fast deriving (Eq,Enum,Show)

-- | Global encoder acceleration (0-2)
global_encoder_acceleration_set :: Num n => Acceleration -> Sysex n
global_encoder_acceleration_set = set_sysex 0x41 0x04 . enum_to_num

{- | Enumeration of pad velocity curves.

>>> map (toEnum :: Int -> Curve) [0 .. 3]
[Linear,Logarithmic,Exponential,Full]
-}
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

-- * Enc = encoder, cc = Cc-mode

data Encoder_Mode = Absolute | Relative_X40 | Relative_X00 | Relative_X10 deriving (Eq,Enum,Show)

encoder_mode_str :: Encoder_Mode -> String
encoder_mode_str x = ["abs","rel","rel_00","rel_10"] !! fromEnum x

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

-- | Set 16 encoders, only Cc value differs per encoder.
enc_cc_set_16 :: (Byte,[Byte],(Byte,Byte),Encoder_Mode) -> [Sysex Byte]
enc_cc_set_16 (ch,cc_seq,rng,md) =
  let f j cc = enc_cc_set j (ch,cc,rng,md)
  in concat (zipWith f [0 .. 15] cc_seq)

-- | Set only Cc value (not channel or range or mode)
enc_cc_set_cc :: Ctl_Id -> Byte -> Sysex Byte
enc_cc_set_cc j = set_sysex 0x03 (ctl_enc_ix0 + j)

enc_cc_set_cc_16 :: [Byte] -> [Sysex Byte]
enc_cc_set_cc_16 = let f j cc = enc_cc_set_cc j cc in zipWith f [0 .. 15]

enc_cc_set_ch :: Ctl_Id -> Byte -> Sysex Byte
enc_cc_set_ch j = set_sysex 0x02 (ctl_enc_ix0 + j)

enc_cc_set_value :: Ctl_Id -> Byte -> Sysex Byte
enc_cc_set_value j = set_sysex 0x00 (ctl_enc_ix0 + j)

-- * Enc Pn Mode

-- | (N/R)Pn Mode
data Pn_Mode = Nrpn | Rpn deriving (Eq,Enum,Show)

-- | (N/R)Pn Type
data Pn_Type = Coarse | Fine deriving (Eq,Show)

-- | 'Pn_Type' to byte code.
pn_type_to_u8 :: Num n => Pn_Type -> n
pn_type_to_u8 x = case x of {Coarse -> 0x06 ; Fine -> 0x26}

{- | Encoder (N/R)Pn mode.

k = control-id (0-15) (translated to 0x20-0x2F),
ch = channel, ty = coarse/fine,
msb = most-significant-byte-index, lsb = least-significant-byte-index,
md = mode

The device only sends either the Lsb or the Msb value, ie. this does not send 14-bit encoder data.
-}
enc_pn_set :: Ctl_Id -> (Byte,Pn_Type,(Byte,Byte),Pn_Mode) -> [Sysex Byte]
enc_pn_set j (ch,ty,(msb,lsb),md) =
  let k = ctl_enc_ix0 + j
  in [set_sysex 0x01 k 0x04 -- nrpn/rpn-mode
     ,set_sysex 0x02 k ch -- channel
     ,set_sysex 0x03 k (pn_type_to_u8 ty) -- coarse/fine 0x06=coarse 0x26=fine
     ,set_sysex 0x04 k msb -- msb index
     ,set_sysex 0x05 k lsb -- lsb index
     ,set_sysex 0x06 k (enum_to_u8 md) -- 0=Nrpn 1=Rpn
     ]

-- * Pad-Nm (note-mode)

-- | Enumeration of pad switch modes.
data Switch_Mode = Toggle | Gate deriving (Eq,Enum,Show)

-- | cs=consecutive, rl=release
switch_mode_str :: Switch_Mode -> String
switch_mode_str x = case x of {Toggle -> "cs" ; Gate -> "rl"}

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

-- | Variant that places each pad on a separate channel.
pad_mn_set_16_mpe :: ([Byte],Switch_Mode) -> [Sysex Byte]
pad_mn_set_16_mpe (mnn_seq,md) =
  let f j mnn = pad_mn_set j (j,mnn,md)
  in concat (zipWith f [0 .. 15] mnn_seq)

-- * Pad-Cc (control-change)

{- | The three Cc behaviors for pads are:

1. Switch-Toggle (0x01=0x08 + 0x06=0x00)
2. Switch-Gate (0x01=0x08 + 0x06=0x01)
3. Pressure(-Gate) (0x01=0x01)

The first two send the minima value for off and the maxima value for on
(respectively at consecutive presses, and at press/release).

The third sends continuous pressure data in the range (minima, maxima) while pressed.

-}
data Pad_Cc_Mode = Pad_Switch Switch_Mode | Pad_Pressure deriving (Eq,Show)

pad_cc_mode_ty :: Num n => Pad_Cc_Mode -> n
pad_cc_mode_ty x = case x of {Pad_Switch _ -> 0x08;Pad_Pressure -> 0x01}

pad_cc_mode_sw :: Num n => Pad_Cc_Mode -> n
pad_cc_mode_sw x = case x of {Pad_Switch md -> enum_to_num md;Pad_Pressure -> 0x00}

pad_cc_mode_str :: Pad_Cc_Mode -> String
pad_cc_mode_str x = case x of {Pad_Switch Toggle -> "sw1" ; Pad_Switch Gate -> "sw2" ; Pad_Pressure -> "prs"}

-- | Set pad to control-change mode (ccm).
pad_cc_set :: Ctl_Id -> (Pad_Cc_Mode, Byte, Byte, (Byte, Byte)) -> [Sysex Byte]
pad_cc_set j (md,ch,cc,(off,on)) =
  let k = ctl_pad_ix0 + j
  in [set_sysex 0x01 k (pad_cc_mode_ty md) -- control-change sub-type
     ,set_sysex 0x02 k ch
     ,set_sysex 0x03 k cc
     ,set_sysex 0x04 k off
     ,set_sysex 0x05 k on
     ,set_sysex 0x06 k (pad_cc_mode_sw md) -- 0=toggle,1=gate
     ]

pad_cc_set_16 :: (Pad_Cc_Mode, Byte, [Byte], (Byte, Byte)) -> [Sysex Byte]
pad_cc_set_16 (md,ch,cc_seq,off_on) =
  let f j cc = pad_cc_set j (md,ch,cc,off_on)
  in concat (zipWith f [0 .. 15] cc_seq)

-- * Pad-Pc (program-change)

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

-- * Led

-- | There are two Leds, on red and one blue, and each can be on or off.
data Led_Status = Led_Off | Led_Red | Led_Blue | Led_Magenta deriving (Eq,Show)

-- | Pp Led_Status
led_status_str :: Led_Status -> String
led_status_str = map toUpper . drop 4 . show

-- | Led_Status to control byte.
led_status_to_u8 :: Led_Status -> Byte
led_status_to_u8 x = case x of {Led_Off -> 0x00 ; Led_Red -> 0x01 ; Led_Blue -> 0x10 ; Led_Magenta -> 0x11}

-- | Sequence of all control indices that have Leds, the Lhs controls (except Stop) and the pads.
led_ix_set :: [Byte]
led_ix_set = [0x59 .. 0x5F] ++ [0x70 .. 0x7F]

-- | Set status of Led at index /k/ (either a pad index or a Lhs control index)
led_set :: Byte -> Led_Status -> Sysex Byte
led_set k x = set_sysex 0x10 k (led_status_to_u8 x)

-- | Set status of Led at pad /j/ (0-15).
led_pad_set :: Ctl_Id -> Led_Status -> Sysex Byte
led_pad_set j = led_set (ctl_pad_ix0 + j)

-- | The Cntrl-Seq light is ordinarily on, turn it off.
led_cntrl_seq_off :: Sysex Byte
led_cntrl_seq_off = led_set 0x5A Led_Off

-- * Dsc = description

-- | Descriptions of pad colour arguments.  There are two Leds, red and blue.
pad_clr_dsc :: Num n => [(n, String)]
pad_clr_dsc = [(0x00,"Off"),(0x01,"Red"),(0x10,"Blue"),(0x11,"Magenta")]

pad_mode_dsc :: Num n => [(n, String)]
pad_mode_dsc =
  [(0x00,"Off")
  ,(0x01,"Cc_Pressure") -- cc = control-change
  ,(0x02,"Unknown")
  ,(0x03,"Unknown")
  ,(0x04,"Unknown")
  ,(0x05,"Unknown")
  ,(0x06,"Unknown")
  ,(0x07,"Mmc")
  ,(0x08,"Cc_Switch")
  ,(0x09,"Mnn") -- mnn = midi-note-number (ie. note mode)
  ,(0x0A,"Unknown")
  ,(0x0B,"Program_Change")]

-- | Mmc command descriptions, <https://en.wikipedia.org/wiki/MIDI_Machine_Control>
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
  ,(0x0D,"Mmc Reset")
  ,(0x40,"Write") -- param
  ,(0x44,"Goto") -- param
  ,(0x47,"Shuttle") -- param
  ]

-- | mnm = midi-note mode
pad_mn_param_dsc :: Num n => [(n, String)]
pad_mn_param_dsc =
  [(0x02,"Midi_Channel")
  ,(0x03,"Mnn")
  ,(0x06,"Behaviour")]

pad_behaviour_dsc :: Num n => [(n, String)]
pad_behaviour_dsc =
  [(0x00,"Toggle")
  ,(0x01,"Gate")]

enc_mode_dsc :: Num n => [(n, String)]
enc_mode_dsc =
  [(0x00,"Off")
  ,(0x01,"Cc") -- cc = control-change
  ,(0x02,"Unknown")
  ,(0x03,"Unknown")
  ,(0x04,"Rpn/Nrpn")]

-- | cc = control-change mode
enc_cc_param_dsc :: Num n => [(n, String)]
enc_cc_param_dsc =
  [(0x01,"Mode")
  ,(0x02,"Midi_Channel")
  ,(0x03,"Cc_Number")
  ,(0x04,"Minima")
  ,(0x05,"Maxima")
  ,(0x06,"Behaviour")]

enc_behaviour_dsc :: Num n => [(n, String)]
enc_behaviour_dsc =
  [(0x00,"Absolute")
  ,(0x01,"Relative_1")
  ,(0x02,"Relative_2")
  ,(0x03,"Relative_3")]

-- | The lhs controls are refered to by unique identifiers.
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

-- * Library

type Sysex_Lib = [(String, [Sysex Byte])]

sysex_lib_accel :: Sysex_Lib
sysex_lib_accel =
  [("enc-acceleration-slow",[global_encoder_acceleration_set Slow])
  ,("enc-acceleration-medium",[global_encoder_acceleration_set Medium])
  ,("enc-acceleration-fast",[global_encoder_acceleration_set Fast])]

sysex_lib_curve :: Sysex_Lib
sysex_lib_curve =
  [("pad-velocity-curve-linear",[global_pad_velocity_curve_set Linear])
  ,("pad-velocity-curve-logarithmic",[global_pad_velocity_curve_set Logarithmic])
  ,("pad-velocity-curve-exponential",[global_pad_velocity_curve_set Exponential])
  ,("pad-velocity-curve-full",[global_pad_velocity_curve_set Full])]

sysex_lib_channel :: Sysex_Lib
sysex_lib_channel =
  [("midi-channel-c00",[global_midi_channel_set 0])
  ,("cv-midi-channel-c00",[global_cv_gate_interface_receive_channel_set 0])]

sysex_lib_enc_ch :: Sysex_Lib
sysex_lib_enc_ch =
  let mk_enc_cc_ch ch ix md =
        (printf "enc-cc-seq-c%02x-i%02x-%s" ch ix (encoder_mode_str md)
        ,enc_cc_set_16 (ch,map (+ ix) [0x00 .. 0x0F],(0x00,0x7F),md))
  in [mk_enc_cc_ch ch ix md | ch <- [0..1]
                            , ix <- [0x00,0x10 .. 0x70]
                            , md <- [Absolute,Relative_X40]]

sysex_lib_enc :: Sysex_Lib
sysex_lib_enc =
  let mk_enc_cc ix md =
        (printf "enc-cc-seq-i%02x-%s" ix (encoder_mode_str md)
        ,enc_cc_set_16 (0x41,map (+ ix) [0x00 .. 0x0F],(0x00,0x7F),md))
  in [mk_enc_cc ix md | ix <- [0x00,0x10 .. 0x70]
                      , md <- [Absolute,Relative_X40]]

sysex_lib_led :: Sysex_Lib
sysex_lib_led =
  let mk_led_all x = map (\k -> set_sysex 0x10 k (led_status_to_u8 x)) led_ix_set
  in [("led-all-off",mk_led_all Led_Off)
     ,("led-all-blue",mk_led_all Led_Blue)
     ,("led-all-magenta",mk_led_all Led_Magenta)
     ,("led-all-red",mk_led_all Led_Red)
     ,("led-cntrl-seq-off",[led_cntrl_seq_off])]

sysex_lib_led_pad :: Sysex_Lib
sysex_lib_led_pad =
  let mk_led_pad x =
        map (\j -> (printf "led-pad-%02x-%s" j (led_status_str x)
                   ,[led_pad_set j x])) [0 .. 15]
  in concatMap mk_led_pad [Led_Off,Led_Red,Led_Blue,Led_Magenta]

sysex_lib_pad_cc_ch :: Sysex_Lib
sysex_lib_pad_cc_ch =
  let mk_pad_cc ch ix md =
        (printf "pad-cc-seq-c%02x-i%02x-%s" ch ix (pad_cc_mode_str md)
        ,pad_cc_set_16 (md,ch,map (+ ix) [0x00 .. 0x0F],(0x00,0x7F)))
  in [mk_pad_cc ch ix md | ch <- [0..3]
                         , ix <- [0, 0x10 .. 0x70]
                         , md <- [Pad_Switch Toggle,Pad_Switch Gate,Pad_Pressure]]

sysex_lib_pad_cc :: Sysex_Lib
sysex_lib_pad_cc =
  let mk_pad_cc ix md =
        (printf "pad-cc-seq-i%02x-%s" ix (pad_cc_mode_str md)
        ,pad_cc_set_16 (md,0x41,map (+ ix) [0x00 .. 0x0F],(0x00,0x7F)))
  in [mk_pad_cc ix md | ix <- [0, 0x10 .. 0x70]
                      , md <- [Pad_Switch Toggle,Pad_Switch Gate,Pad_Pressure]]

sysex_lib_pad_mn :: Sysex_Lib
sysex_lib_pad_mn =
  let mk_pad_mn ix md =
        (printf "pad-mn-seq-i%02x-%s" ix (switch_mode_str md)
        ,pad_mn_set_16 (0x41,map (+ ix) [0x00 .. 0x0F],md))
  in [mk_pad_mn ix md | ix <- [0x30,0x3C,0x48]
                      , md <- [Toggle,Gate]]

{- | Library of named Sysex message sequences.

>>> putStr $ unlines $ take 5 (map fst sysex_library)
enc-acceleration-slow
enc-acceleration-medium
enc-acceleration-fast
pad-velocity-curve-linear
pad-velocity-curve-logarithmic
-}
sysex_library :: [(String, [Sysex Byte])]
sysex_library =
  concat [sysex_lib_accel,sysex_lib_curve
         ,sysex_lib_enc
         ,sysex_lib_led
         ,sysex_lib_pad_cc,sysex_lib_pad_mn]

{- | Write 'sysex_library' to indicated directory.

> sysex_library_store "/home/rohan/sw/hsc3-data/data/midi/arturia/beatstep/"
-}
sysex_library_store :: FilePath -> IO ()
sysex_library_store dir = do
  let wr (nm,syx) = bytes_store (dir </> nm <.> "syx") (concat syx)
  mapM_ wr sysex_library

{-

import qualified Sound.Midi.Pm as Pm

k <- Pm.pm_output_by_name "Arturia BeatStep MIDI 1"
run = Pm.pm_with_output_device k
msg x = run (\fd -> Pm.pm_sysex_write_seq 5 fd x)

msg (map (\k -> set_sysex 0x10 k (led_status_to_u8 Led_Red)) led_ix_set)
msg (map (\k -> set_sysex 0x10 k (led_status_to_u8 Led_Blue)) led_ix_set)
msg (map (\k -> set_sysex 0x10 k (led_status_to_u8 Led_Magenta)) led_ix_set)
msg (map (\k -> set_sysex 0x10 k (led_status_to_u8 Led_Off)) led_ix_set)

msg [led_cntrl_seq_off]

msg [global_midi_channel_set 0x00]
msg [global_encoder_acceleration_set Slow] -- Slow Medium Fast

msg (enc_cc_set_16 (0x41,[0x00 .. 0x0F],(0x00,0x7F),Absolute))
msg (enc_cc_set_16 (0x41,[0x00 .. 0x0F],(0x00,0x7F),Relative_X40))

msg (enc_pn_set 0x00 (0x00,Coarse,(0x20,0x40),Nrpn))
msg (enc_pn_set 0x00 (0x00,Fine,(0x20,0x40),Nrpn))
msg (enc_cc_set 0x00 (0x00,0x00,(0x00,0x7f),Absolute))

msg (pad_mn_set_16 (0x00,[0x3C .. ],Toggle))
msg (pad_mn_set_16 (0x00,[0x3C .. ],Gate))
msg (pad_mn_set_16_mpe ([0x3C ..],Gate))

msg (pad_cc_set_16 (Pad_Switch Toggle,0,[0 .. 16],(0x00,0x7F)))
msg (pad_cc_set_16 (Pad_Switch Gate,0,[0 .. 16],(0x00,0x7F)))

msg (pad_pc_set_16 (0x00,[0x00 ..],(0x10,0x01)))

msg [global_pad_velocity_curve_set Linear]
msg [global_pad_velocity_curve_set Logarithmic]
msg [global_pad_velocity_curve_set Exponential]

msg (pad_cc_set_16 (Pad_Pressure,0,[0 .. 16],(0x00,0x7F))) -- full range (0, 127)
msg (pad_cc_set_16 (Pad_Pressure,0,[0 .. 16],(0x30,0x57))) -- limit range (48, 87)

Pm.pm_with_output_device k (\fd -> Pm.pm_cvm3_write fd (0x90,0x3C,0x7F))
Pm.pm_with_output_device k (\fd -> Pm.pm_cvm3_write fd (0x90,0x00,0x00))

msg [request_sysex 0x01 ctl_enc_ix0]

-}
