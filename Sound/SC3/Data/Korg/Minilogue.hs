-- | Korg Minilogue
module Sound.SC3.Data.Korg.Minilogue where

import Data.Word {- base -}

-- | (AREA-NAME,[(CTL-NAME,CTL-CC,CTL-N)])
ml_cc_tbl :: [(String, [(String, Word8, Word8)])]
ml_cc_tbl =
  [("VCO 1"
   ,[("OCTAVE",0x30,0x04),("WAVE",0x32,0x03),("PITCH",0x22,0x80),("SHAPE",0x24,0x80)])
  ,("VCO 2"
   ,[("OCTAVE",0x31,0x04),("WAVE",0x33,0x03),("PITCH",0x23,0x80),("SHAPE",0x25,0x80)])
  ,("VCO 2 MODULATION"
   ,[("CROSS MOD DEPTH",0x29,0x80),("PITCH EG INT",0x2A,0x80),("SYNC",0x50,0x02),("RING",0x51,0x02)])
  ,("MIXER"
   ,[("VCO1",0x27,0x80),("VCO2",0x28,0x80),("NOISE",0x21,0x80)])
  ,("FILTER"
   ,[("CUTOFF",0x2B,0x80),("RESONANCE",0x2C,0x80),("EG INT",0x2D,0x80)
    ,("N-POLE",0x54,0x02),("KEY TRACK",0x53,0x03),("VELOCITY",0x52,0x03)])
  ,("AMP EG"
   ,[("ATTACK",0x10,0x80),("DECAY",0x11,0x80),("SUSTAIN",0x12,0x80),("RELEASE",0x13,0x80)])
  ,("EG"
   ,[("ATTACK",0x14,0x80),("DECAY",0x15,0x80),("SUSTAIN",0x16,0x80),("RELEASE",0x17,0x80)])
  ,("LFO"
   ,[("WAVE",0x3A,0x03),("EG MOD",0x39,0x03),("RATE",0x18,0x80),("INT",0x1A,0x80)
    ,("TARGET",0x38,0x03)])
  ,("DELAY"
   ,[("HI PASS CUTOFF",0x1D,0x80),("TIME",0x1E,0x80),("FEEDBACK",0x1D,0x80)
    ,("OUTPUT ROUTING",0x58,0x03)])
  ,(""
   ,[("VOICE MODE DEPTH",0x1B,0x80)])
  ]

-- | Enumerate data-values given CTL-N.
--
-- > map ml_ctl_enum [2,3,4,128] == [[0,127],[0,64,127],[0,42,84,127],[0 .. 127]]
ml_ctl_enum :: Int -> [Word8]
ml_ctl_enum n =
  case n of
    0x02 -> [0x00,0x7F]
    0x03 -> [0x00,0x40,0x7F]
    0x04 -> [0x00,0x2A,0x54,0x7F]
    0x80 -> [0x00 .. 0x7F]
    _ -> error "ml_ctl_enum?"

ml_cc_seq_grp :: [[[Word8]]]
ml_cc_seq_grp =
  [[[34,36],[39],[43]   ,[16..19],[20..31],[]]
  ,[[35,37],[40],[44,45],[20..23],[]      ,[27]]
  ,[[41,42],[33],[24,34],[]      ,[]      ,[]]]

ml_cc_seq_ln :: [[Word8]]
ml_cc_seq_ln = map concat ml_cc_seq_grp

mk_cc_seq :: [Word8]
mk_cc_seq = concat ml_cc_seq_ln
