-- | Supperware HeadTracker
module Sound.Sc3.Data.Supperware.HeadTracker where

import Data.Bits {- base -}

--import qualified Sound.Midi.Common as Midi {- midi-osc -}
import qualified Sound.Midi.Constant as Constant {- midi-osc -}
--import qualified Sound.Midi.SysEx as SysEx {- midi-osc -}
import qualified Sound.Midi.Type as Midi {- midi-osc -}

supperwareManufacturerId :: Num i => (i,i,i)
supperwareManufacturerId = (0x00, 0x21, 0x42)

supperwareSysEx :: Num i => [i] -> Midi.SysEx i
supperwareSysEx contents = [Constant.k_SysEx_Status, 0x00, 0x21, 0x42] ++ contents ++ [Constant.k_SysEx_End]

singleValueSysEx :: Num i => (i, i, i) -> Midi.SysEx i
singleValueSysEx (message, parameter, value) = supperwareSysEx [message, parameter, value]

data AngleMode = Ypr | Quaternion | Matrix

angleModeByte :: Num a => AngleMode -> a
angleModeByte angleMode =
  case angleMode of
    Ypr -> 0x01
    Quaternion -> 0x05
    Matrix -> 0x09

turnOnMessage :: Num i => AngleMode -> Bool -> Midi.SysEx i
turnOnMessage angleMode use100Hz =
  supperwareSysEx [0x00, 0x00, if use100Hz then 0x28 else 0x08, 0x01, angleModeByte angleMode, 0x03, 0x40]

turnOffMessage :: Num i => Midi.SysEx i
turnOffMessage = singleValueSysEx (0x00, 0x00, 0x40)

zeroMessage :: Num i => Midi.SysEx i
zeroMessage = singleValueSysEx (0x01, 0x00, 0x01)

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
  [0x02 -- Message 2 : Readback
  ,0x03 -- Magnetometer
  ,0x04 -- Gesture and chirality
  ,0x11 -- Travel mode
  ]
