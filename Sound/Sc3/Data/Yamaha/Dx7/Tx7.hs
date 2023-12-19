-- | Yamaha Tx7
module Sound.Sc3.Data.Yamaha.Dx7.Tx7 where

-- * 4-6. 1 Performance Bulk Data (f = 1)

tx7_perf_voice_tbl :: Num n => [(n, String, String, (n, n))]
tx7_perf_voice_tbl =
  [ (2, "POLY/MONO                    ", "MONO", (0, 1))
  , (3, "PITCH BEND RANGE             ", "PBR ", (0, 12))
  , (4, "PITCH BEND STEP              ", "PBS ", (0, 12))
  , (5, "PORTAMENTO TIME              ", "PTIM", (0, 99))
  , (6, "PORTAMENTO/GLISSANDO         ", "GLIS", (0, 1))
  , (7, "PORTAMENTO MODE              ", "PORM", (0, 1))
  , (9, "MODULATION WHEEL SENSITIVITY ", "MWS ", (0, 15))
  , (10, "MODULATION WHEEL ASSIGN      ", "MWA ", (0, 7))
  , (11, "FOOT CONTROLLER SENSITIVITY  ", "FCS ", (0, 15))
  , (12, "FOOT CONTROLLER ASSIGN       ", "FCA ", (0, 7))
  , (13, "AFTER TOUCH SENSITIVITY      ", "ATS ", (0, 15))
  , (14, "AFTER TOUCH ASSIGN           ", "ATA ", (0, 7))
  , (15, "BREATH CONTROLLER SENSITIVITY", "BCS ", (0, 15))
  , (16, "BREATH CONTROLLER ASSIGN     ", "BCA ", (0, 7))
  , (26, "AUDIO OUTPUT LEVEL ATTENUATOR", "ATN ", (0, 7))
  ]
