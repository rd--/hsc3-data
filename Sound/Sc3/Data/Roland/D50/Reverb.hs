-- | D50 / Reverb
module Sound.Sc3.Data.Roland.D50.Reverb where

import Data.Char {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}

import Sound.Sc3.Data.Math.Types {- hsc3-data -}

-- | (REVERB-INDEX,NAME)
type D50_REVERB_NAME = (U8, String)

-- | Reverbs 1-16 are shared (common) reverb types, 17-32 are user (bank) reverb types.
d50_reverb_type_shared :: [D50_REVERB_NAME]
d50_reverb_type_shared =
  [ (01, "Small Hall")
  , (02, "Medium Hall")
  , (03, "Large Hall")
  , (04, "Chapel")
  , (05, "Box")
  , (06, "Small Metal Room")
  , (07, "Small Room")
  , (08, "Medium Room")
  , (09, "Medium Large Room")
  , (10, "Large Room")
  , (11, "Single Delay (102 ms)")
  , (12, "Cross Delay (180 ms)")
  , (13, "Cross Delay (224 ms)")
  , (14, "Cross Delay (148-296 ms)")
  , (15, "Short Gate (200 ms)")
  , (16, "Long Gate (480 ms)")
  ]

-- | USR string variant of 'd50_reverb_type_shared', with indices for 17-32.
d50_reverb_type_usr :: String
d50_reverb_type_usr =
  let rw c =
        case c of
          ' ' -> Just '-'
          '(' -> Nothing
          ')' -> Nothing
          _ -> Just (toUpper c)
  in intercalate ";" (map (mapMaybe rw . snd) d50_reverb_type_shared ++ map show [17 :: U8 .. 32])

-- | Reverb from PN-D50-00 (ie. FACTORY PRESET)
d50_reverb_type_PN_D50_00 :: [D50_REVERB_NAME]
d50_reverb_type_PN_D50_00 =
  [ (17, "Bright Hall")
  , (18, "Large Cave")
  , (19, "Steel Pan")
  , (20, "Delay (248 ms)")
  , (21, "Delay (338 ms)")
  , (22, "Cross Delay (157 ms)")
  , (23, "Cross Delay (252 ms)")
  , (24, "Cross Delay (274-137 ms)")
  , (25, "Gate Reverb")
  , (26, "Reverse Gate (360 ms)")
  , (27, "Reverse Gate (480 ms)")
  , (28, "Slap Back")
  , (29, "Slap Back")
  , (30, "Slap Back")
  , (31, "Twisted Space")
  , (32, "Space")
  ]

-- | Reverb from PN-D50-01
d50_reverb_type_PN_D50_01 :: [D50_REVERB_NAME]
d50_reverb_type_PN_D50_01 =
  [ (17, "Very Small Hall")
  , (18, "Medium Small Hall")
  , (19, "Medium Large Hall")
  , (20, "Very Large Hall")
  , (21, "Slap Back (short)")
  , (22, "Slap Back (medium)")
  , (23, "Slap Back (long)")
  , (24, "Cross Delay (34-102 ms)")
  , (25, "Cross Delay ,(103-206 ms)")
  , (26, "Cross Delay ,(130 ms)")
  , (27, "Cross Delay ,(306-153 ms)")
  , (28, "Cross Delay ,(191 ms)")
  , (29, "Cross Delay ,(220 ms)")
  , (30, "Cross Delay ,(22.5-284 ms)")
  , (31, "Cross Delay ,(382-11 ms)")
  , (32, "Cross Delay ,(28-426 ms)")
  ]

-- | Reverb from PN-D50-02
d50_reverb_type_PN_D50_02 :: [D50_REVERB_NAME]
d50_reverb_type_PN_D50_02 =
  [ (17, "Very Small Room")
  , (18, "Medium Small Room")
  , (19, "Medium Room")
  , (20, "Large Room")
  , (21, "Metal Can")
  , (22, "Short Gate (140 ms)")
  , (23, "Medium Gate (250 ms)")
  , (24, "Long Gate (390 ms)")
  , (25, "Reverse Gate (270 ms)")
  , (26, "Reverse Gate (440 ms)")
  , (27, "Delay (94 ms)")
  , (28, "Delay (122 ms)")
  , (29, "Delay (142 ms)")
  , (30, "Cross Delay (168 ms)")
  , (31, "Delay (212 ms)")
  , (32, "Delay (290 ms)")
  ]

-- | Reverb from PN-D50-03
d50_reverb_type_PN_D50_03 :: [D50_REVERB_NAME]
d50_reverb_type_PN_D50_03 =
  [ (17, "Cross Delay (114 ms)")
  , (18, "Cross Delay (165 ms)")
  , (19, "Cross Delay (198 ms)")
  , (20, "Cross Delay (240 ms)")
  , (21, "Tap Delay (110-38 ms)")
  , (22, "Tap Delay (50-180 ms)")
  , (23, "Tap Delay (250-500 ms)")
  , (24, "Pan Delay (250-500 ms)")
  , (25, "Single Delay (136 ms)")
  , (26, "Single Delay (205 ms)")
  , (27, "Single Delay (270 ms)")
  , (28, "Single Delay (355 ms)")
  , (29, "Single Delay (430 ms)")
  , (30, "Single Delay (500 ms)")
  , (31, "Rolling (short)")
  , (32, "Rolling (long)")
  ]

-- | Reverb from PN-D50-04
d50_reverb_type_PN_D50_04 :: [D50_REVERB_NAME]
d50_reverb_type_PN_D50_04 =
  [ (17, "Small Hall")
  , (18, "Medium Hall")
  , (19, "Large Hall")
  , (20, "Cave")
  , (21, "Small Room")
  , (22, "Medium Room")
  , (23, "Large Room")
  , (24, "Garage")
  , (25, "Slap Back")
  , (26, "Small Can")
  , (27, "Metal Box")
  , (28, "Medium Gate (320 ms)")
  , (29, "Long Gate (430 ms)")
  , (30, "Gate Reverse (370 ms)")
  , (31, "Space")
  , (32, "Flange Space")
  ]
