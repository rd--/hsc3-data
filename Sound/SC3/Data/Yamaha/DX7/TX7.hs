-- | Yamaha TX7
module Sound.SC3.Data.Yamaha.DX7.TX7 where

-- * 4-6. 1 Performance Bulk Data (f = 1)

tx7_perf_voice_tbl :: Num n => [(n,String,(n,n))]
tx7_perf_voice_tbl =
  [( 2,"POLY/MONO                    ",(0, 1))
  ,( 3,"PITCH BEND RANGE             ",(0,12))
  ,( 4,"PITCH BEND STEP              ",(0,12))
  ,( 5,"PORTAMENTO TIME              ",(0,99))
  ,( 6,"PORTAMENTO/GLISSANDO         ",(0, 1))
  ,( 7,"PORTAMENTO MODE              ",(0, 1))
  ,( 9,"MODULATION WHEEL SENSITIVITY ",(0,15))
  ,(10,"MODULATION WHEEL ASSIGN      ",(0, 7))
  ,(11,"FOOT CONTROLLER SENSITIVITY  ",(0,15))
  ,(12,"FOOT CONTROLLER ASSIGN       ",(0, 7))
  ,(13,"AFTER TOUCH SENSITIVITY      ",(0,15))
  ,(14,"AFTER TOUCH ASSIGN           ",(0, 7))
  ,(15,"BREATH CONTROLLER SENSITIVITY",(0,15))
  ,(16,"BREATH CONTROLLER ASSIGN     ",(0, 7))
  ,(26,"AUDIO OUTPUT LEVEL ATTENUATOR",(0, 7))]
