-- | Korg Minilogue
module Sound.SC3.Data.Korg.Minilogue where

-- | (AREA-NAME,[(CTL-NAME,CTL-CC)])
ml_cc_tbl :: [(String, [(String, Int)])]
ml_cc_tbl =
  [("VCO 1"
   ,[("OCTAVE",0x30),("WAVE",0x32),("PITCH",0x22),("SHAPE",0x24)])
  ,("VCO 2"
   ,[("OCTAVE",0x31),("WAVE",0x33),("PITCH",0x23),("SHAPE",0x25)])
  ,("VCO 2 MODULATION"
   ,[("CROSS MOD DEPTH",0x29),("PITCH EG INT",0x2A),("SYNC",0x50),("RING",0x51)])
  ,("MIXER"
   ,[("VCO1",0x27),("VCO2",0x28),("NOISE",0x21)])
  ,("FILTER"
   ,[("CUTOFF",0x2B),("RESONANCE",0x2C),("EG INT",0x2D)
    ,("N-POLE",0x54),("KEY TRACK",0x53),("VELOCITY",0x52)])
  ,("AMP EG"
   ,[("ATTACK",0x10),("DECAY",0x11),("SUSTAIN",0x12),("RELEASE",0x13)])
  ,("EG"
   ,[("ATTACK",0x14),("DECAY",0x15),("SUSTAIN",0x16),("RELEASE",0x17)])
  ,("LFO"
   ,[("WAVE",0x3A),("EG MOD",0x39),("RATE",0x18),("INT",0x1A),("TARGET",0x38)])
  ,("DELAY"
   ,[("HI PASS CUTOFF",0x1D),("TIME",0x1E),("FEEDBACK",0x1D),("OUTPUT ROUTING",0x58)])
  ,(""
   ,[("VOICE MODE DEPTH",0x1B)])
  ]
