-- | Supperware HeadTracker
module Sound.Sc3.Data.Supperware.HeadTracker where

import Data.Bits {- base -}

import qualified Data.List.Split as Split {- split -}

import Music.Theory.Geometry.Matrix {- hmt-base -}

import qualified Sound.Midi.Constant as Constant {- midi-osc -}
import qualified Sound.Midi.Type as Midi {- midi-osc -}

-- | Two element (zero-prefixed) manufacturer id.
supperwareManufacturerId :: Num i => (i, i, i)
supperwareManufacturerId = (0x00, 0x21, 0x42)

-- | Enclose bytes in SysEx pre and post ambles.
supperwareSysEx :: Num i => [i] -> Midi.SysEx i
supperwareSysEx contents = [Constant.k_SysEx_Status, 0x00, 0x21, 0x42] ++ contents ++ [Constant.k_SysEx_End]

{- | Decode 14-bit fixed-point numbers in Q2.11 format.

>>> decodeQ2Dot11 (0x19,0x22) -- pi/2
1.5791015625

>>> decodeQ2Dot11 (0x19,0x22) / pi * 180 -- 90
90.47585495376379
-}
decodeQ2Dot11 :: (Integral i, Bits i, Fractional r) => (i, i) -> r
decodeQ2Dot11 (b1, b2) =
  let w = (shiftL b1 7) + b2
      w' = if (w >= 0x2000) then w - 0x4000 else w
  in fromIntegral w' / 2048

-- | Form a single valued sysex message.
singleValueSysEx :: Num i => (i, i, i) -> Midi.SysEx i
singleValueSysEx (message, parameter, value) = supperwareSysEx [message, parameter, value]

-- | The head tracker can report angle values in three formats.
data AngleMode = YprMode | QuaternionMode | MatrixMode

angleModeByte :: Num a => AngleMode -> a
angleModeByte angleMode =
  case angleMode of
    YprMode -> 0x01
    QuaternionMode -> 0x05
    MatrixMode -> 0x09

{- | Turn head tracker on.

>>> turnOnMessage MatrixMode False
[240,0,33,66,0,0,8,1,9,3,64,247]
-}
turnOnMessage :: Num i => AngleMode -> Bool -> Midi.SysEx i
turnOnMessage angleMode use100Hz =
  supperwareSysEx [0x00, 0x00, if use100Hz then 0x28 else 0x08, 0x01, angleModeByte angleMode, 0x03, 0x40]

{- | Turn head tracker off.

>>> turnOffMessage
[240,0,33,66,0,0,64,247]
-}
turnOffMessage :: Num i => Midi.SysEx i
turnOffMessage = singleValueSysEx (0x00, 0x00, 0x40)

{- | Set most recent stable data point as zero (level, straight ahead).

>>> zeroMessage
[240,0,33,66,1,0,1,247]
-}
zeroMessage :: Num i => Midi.SysEx i
zeroMessage = singleValueSysEx (0x01, 0x00, 0x01)

{- | Set chirality.

>>> chiralityMessage False
[240,0,33,66,0,4,2,247]
-}
chiralityMessage :: Num i => Bool -> Midi.SysEx i
chiralityMessage isRightEarChirality = singleValueSysEx (0x00, 0x04, if isRightEarChirality then 0x03 else 0x02)

compassMessage :: (Bits i, Num i) => Bool -> Bool -> Midi.SysEx i
compassMessage compassShouldBeOn compassShouldApplyYawCorrection =
  let onOff = if compassShouldBeOn then 0x60 else 0x60 .|. 0x10
      corrected = if compassShouldApplyYawCorrection then onOff else onOff .|. 0x08
  in singleValueSysEx (0x00, 0x03, corrected)

calibrateCompassMessage :: Num i => Midi.SysEx i
calibrateCompassMessage = singleValueSysEx (0x00, 0x03, 0x44)

readbackMessage :: Num i => Midi.SysEx i
readbackMessage =
  supperwareSysEx
    [ 0x02 -- Message 2 : Readback
    , 0x03 -- Magnetometer
    , 0x04 -- Gesture and chirality
    , 0x11 -- Travel mode
    ]

data Angle t = Ypr (t, t, t) | Quaternion (t, t, t, t) | Matrix (M33 t)
  deriving (Eq, Show)

decodeYpr :: (Integral i, Bits i, Fractional r) => (i, i, i, i, i, i) -> Angle r
decodeYpr (y1, y2, p1, p2, r1, r2) =
  Ypr
    ( decodeQ2Dot11 (y1, y2)
    , decodeQ2Dot11 (p1, p2)
    , decodeQ2Dot11 (r1, r2)
    )

decodeQuaternion :: (Integral i, Bits i, Fractional r) => (i, i, i, i, i, i, i, i) -> Angle r
decodeQuaternion (w1, w2, x1, x2, y1, y2, z1, z2) =
  Quaternion
    ( decodeQ2Dot11 (w1, w2)
    , decodeQ2Dot11 (x1, x2)
    , decodeQ2Dot11 (y1, y2)
    , decodeQ2Dot11 (z1, z2)
    )

decodeMatrix :: (Integral i, Bits i, Fractional r) => [i] -> Angle r
decodeMatrix bytes =
  let list = map (\each -> decodeQ2Dot11 (each !! 0, each !! 1)) (Split.chunksOf 2 bytes)
  in Matrix (m33_from_list_err list)

parseSysEx :: (Integral i, Bits i, Fractional r) => [i] -> Maybe (Angle r)
parseSysEx sysEx =
  case sysEx of
    [0xF0, 0x00, 0x21, 0x42, 0x40, 0x00, y1, y2, p1, p2, r1, r2, 0xF7] ->
      Just (decodeYpr (y1, y2, p1, p2, r1, r2))
    [0xF0, 0x00, 0x21, 0x42, 0x40, 0x01, w1, w2, x1, x2, y1, y2, z1, z2, 0xF7] ->
      Just (decodeQuaternion (w1, w2, x1, x2, y1, y2, z1, z2))
    0xF0 : 0x00 : 0x21 : 0x42 : 0x40 : 0x02 : rest ->
      Just (decodeMatrix (take (9 * 2) rest))
    _ -> Nothing

{-

import Sound.Midi.Pm {- midi-osc -}
pm_enumerate_devices
o <- pm_output_by_name "Head Tracker MIDI 1"
pm_with_output_device o (\fd -> pm_sysex_write fd turnOffMessage)
pm_with_output_device o (\fd -> pm_sysex_write fd (turnOnMessage YprMode False)) -- YprMode QuaternionMode MatrixMode
pm_with_output_device o (\fd -> pm_sysex_write fd zeroMessage)
i <- pm_input_by_name "Head Tracker MIDI 1"
pm_with_input_device i (\fd -> pm_sysex_read fd >>= print)
pm_with_input_device i (\fd -> pm_sysex_read fd >>= (print . parseSysEx))
import Control.Monad
printYpr (Just (Ypr (y,p,r))) = let f x = round (x * 180) in print (f y,f p,f r)
pm_with_input_device i (\fd -> forever (pm_sysex_read fd >>= (printYpr . parseSysEx)))

-}
