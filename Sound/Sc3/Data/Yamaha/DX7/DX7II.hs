-- | Yamaha DX7II
module Sound.Sc3.Data.Yamaha.DX7.DX7II where

import Data.List {- base -}

import qualified Data.List.Split as Split {- split -}

import qualified Music.Theory.Show as Show {- hmt -}

import Sound.Sc3.Data.Yamaha.DX7 {- hsc3-data -}

-- * UTIL

-- | Error if /e/ is false else /r/.
dx7ii_assert :: Bool -> t -> t
dx7ii_assert e r = if e then r else error "dx7ii_assert?"

-- * VCED

-- | DX7 OP param names (short,long).
--   The naming in the DX7II manual is niced than the DX7 manual.
--
-- > length dx7ii_op_parameter_names == dx7_op_nparam
dx7ii_op_parameter_names :: [(String,String)]
dx7ii_op_parameter_names =
  [("R1","EG RATE 1")
  ,("R2","EG RATE 2")
  ,("R3","EG RATE 3")
  ,("R4","EG RATE 4")
  ,("L1","EG LEVEL 1")
  ,("L2","EG LEVEL 2")
  ,("L3","EG LEVEL 3")
  ,("L4","EG LEVEL 4")
  ,("BP","BREAK POINT")
  ,("LD","LEFT DEPTH")
  ,("RD","RIGHT DEPTH")
  ,("LC","LEFT CURVE")
  ,("RC","RIGHT CURVE")
  ,("RS","RATE SCALING")
  ,("AMS","MODULATION SENSITIVITY")
  ,("TS","TOUCH SENSITIVITY")
  ,("TL","TOTAL LEVEL")
  ,("PM","FREQUENCY MODE")
  ,("PC","FREQUENCY COURSE")
  ,("PF","FREQUENCY FINE")
  ,("PD","DETUNE")]

-- | DX7 SH param names (short,long).
--
-- > length dx7ii_sh_parameter_names == dx7_sh_nparam
dx7ii_sh_parameter_names :: [(String,String)]
dx7ii_sh_parameter_names =
  [("PR1" ,"PEG RATE 1")
  ,("PR2" ,"PEG RATE 2")
  ,("PR3" ,"PEG RATE 3")
  ,("PR4" ,"PEG RATE 4")
  ,("PL1" ,"PEG LEVEL 1")
  ,("PL2" ,"PEG LEVEL 2")
  ,("PL3" ,"PEG LEVEL 3")
  ,("PL4" ,"PEG LEVEL 4")
  ,("ALS" ,"ALGORITHM SELECTOR")
  ,("FBL" ,"FEED BACK LEVEL")
  ,("OPI" ,"OSC.PHASE INIT")
  ,("LFS" ,"LFO SPEED")
  ,("LFD" ,"LFO DELAY TIME")
  ,("LPMD","PITCH MODULATION DEPTH")
  ,("LAMD","AMPLITUDE MODULATION DEPTH")
  ,("LFKS","LFO KEY SYNC")
  ,("LFW" ,"LFO WAVE")
  ,("LPMS","LFO PITCH MODULATION SENSITIVITY")
  ,("TRNP","TRANSPOSE")]

-- * Microtune Parameter Change Message (DX7s DX7II)

-- | Constant for per-octave mode (ie. tune all pitch-classes equally).
--
-- > import Music.Theory.Bits {- hmt -}
-- > gen_bitseq_pp 8 dx7ii_microtune_octave == "01111101"
dx7ii_microtune_octave :: U8
dx7ii_microtune_octave = 0x7D

-- | Constant for full gamut mode (ie. tune all notes distinctly).
--
-- > gen_bitseq_pp 8 dx7ii_microtune_full == "01111110"
dx7ii_microtune_full :: U8
dx7ii_microtune_full = 0x7E

-- | DX7 tuning units are 64 steps per semi-tone (100 cents).
--
-- > map dx7ii_tuning_units_to_cents [0x00,0x10,0x20,0x30,0x40] == [0,25,50,75,100]
-- > map dx7ii_tuning_units_to_cents [0..8] == [0,1.5625,3.125,4.6875,6.25,7.8125,9.375,10.9375,12.5]
dx7ii_tuning_units_to_cents :: U8 -> Double
dx7ii_tuning_units_to_cents x = fromIntegral x * (100 / 64)

{- | Microtune parameter change sysex

md = 0x7D | 0x7E
d1 = key number (0-11 for Octave, 0-127 for Full)
d2 = midi note number (13 - 108)
d3 = fine tuning (0 - 63)

The fine tuning parameter is in Yamaha tuning units away from note d2 in 12 ET.
There are 64 tuning units per half step, 12 * 64 = 768 per octave.

If d3 < 33, it is displayed on the LCD as a positive offset from note d2.
Otherwise, it is be displayed as a negative offset -31 to -1 from note d2 + 1.

> let r = [0xF0,0x43,0x10,0x18,0x7E,0x3C,0x3C,0x00,0xF7]
> dx7ii_microtune_parameter_change_sysex 0 dx7ii_microtune_full 60 60 0 == r

-}
dx7ii_microtune_parameter_change_sysex :: U8 -> U8 -> U8 -> U8 -> U8 -> [U8]
dx7ii_microtune_parameter_change_sysex ch md d1 d2 d3 =
  dx7_parameter_change_sysex ch (6,0) md [d1,d2,d3]

-- * 5-2. Additional Parameters (ACED format)

-- | ACED short parameter names.
--
-- > length dx7ii_aced_param_nm == 49
dx7ii_aced_param_nm :: [String]
dx7ii_aced_param_nm =
  concat [map (\n -> "SCM" ++ show n) [6::Int,5 .. 1]
         ,map (\n -> "AMS" ++ show n) [6::Int,5 .. 1]
         ,["PEGR", "LTRG", "VPSW", "PMOD"
          ,"PBR", "PBS", "PBM", "RNDP"
          ,"PORM", "PQNT", "POS"
          ,"MWPM", "MWAM", "MWEB"
          ,"FC1PM", "FC1AM", "FC1EB", "FC1VL"
          ,"BCPM", "BCAM", "BCEB", "BCPB"
          ,"ATPM", "ATAM", "ATEB", "ATPB"
          ,"PGRS"
          ,"FC2PM", "FC2AM", "FC2EB", "FC2VL"
          ,"MCPM", "MCAM", "MCEB", "MCVL"
          ,"UDTN", "FCCS1"]]

-- | ACED USR strings for named parameters.
dx7ii_aced_usr_str :: [(String,DX7_USR)]
dx7ii_aced_usr_str =
  [("PEGR","8VA;4VA;1VA;1/2VA")
  ,("LTRG","SINGLE;MULTI")
  ,("VPSW","OFF;ON")
  ,("PMOD","POLYPHONIC;MONOPHONIC;UNISON-POLY;UNISON-MONO")
  ,("PBM","NORMAL;LOW;HIGH;K.ON")
  ,("PORM","RTN-FNGRD;FLLW-FLLTM") -- RETAIN-FINGERED;FOLLOW-FULLTIME
  ,("BCPB",dx7_usr_range True (-50,49))
  ,("ATPB",dx7_usr_range True (-50,49))
  ,("FCCS1","OFF;ON")]

-- * 5-3. PERFORMANCE PARAMETERS (PCED / PMEM - 51-BYTES)

-- | PCED (51-BYTES)
type DX7II_PCED = [U8]

-- | GROUP structure of PCED, without NAME.
--
-- > sum dx7ii_pced_grp_n == 31
dx7ii_pced_grp_n :: [Int]
dx7ii_pced_grp_n = [3,3,2,1,1,2,1,2,2,3,3,4,4]

-- | Separate into GROUP structure.
dx7ii_pced_grp :: DX7II_PCED -> [[U8]]
dx7ii_pced_grp = Split.splitPlaces dx7ii_pced_grp_n

-- | PCED short parameter names, in GROUP structure.
--
-- > map length dx7ii_pced_param_grp == dx7ii_pced_grp_n
dx7ii_pced_param_grp :: [[String]]
dx7ii_pced_param_grp =
  map words
  ["PLMD VNMA VNMB","MCTB MCKY MCSW","DDTN SPPT"
  ,"FDMP","SFSW","FSAS FSW","SPRNG","NSFTA NSFTB","BLNC TVLM","CSLD1 CSLD2 CSSW"
  ,"PNMD PNRNG PNASN","PNEGR1 PNEGR2 PNEGR3 PNEGR4","PNEGL1 PNEGL2 PNEGL3 PNEGL4"]

-- | Abbreviated PCED parameter names.
dx7ii_pced_param_abbrev :: [String]
dx7ii_pced_param_abbrev =
  concatMap words
  ["PLMD VNMA -B MCTB -KY -SW DDTN SPPT"
  ,"FDMP SFSW FSAS -W SPRNG NSFTA -B BLNC TVLM CSLD1 -2 -SW"
  ,"PNMD -RNG -ASN -R1 -R2 -R3 -R4 -L1 -L2 -L3 -L4"]

-- | (NAME,IX) table for PCED param.
--
-- > unwords $ map fst dx7ii_pced_param_tbl
-- > maximum (map (length . fst) dx7ii_pced_param_tbl) == 6
dx7ii_pced_param_tbl :: [(String,U8)]
dx7ii_pced_param_tbl = zip (concat dx7ii_pced_param_grp) [0..]

-- | Lookup PCED index by name.
--
-- > map dx7ii_pced_param_ix ["PLMD","FDMP","TVLM","PNMD"] == [0,8,16,20]
dx7ii_pced_param_ix :: String -> U8
dx7ii_pced_param_ix nm =
  maybe (error "dx7ii_pced_param_ix") fromIntegral
  (lookup nm dx7ii_pced_param_tbl)

-- | PEFORMANCE NAME is 20-CHAR (31 - 51)
dx7ii_pced_name :: DX7II_PCED -> String
dx7ii_pced_name = map (dx7_ascii_char '?') . drop 31

-- | Table giving USR strings for named PARAM.
--
-- > map (dx7ii_pced_param_ix . fst) dx7ii_pced_usr_str_tbl == [0,5,9,10,11,15,19,20,22]
dx7ii_pced_usr_str_tbl :: [(U8,DX7_USR)]
dx7ii_pced_usr_str_tbl =
  [( 0 {-PLMD-}  ,"SINGLE;DUAL;SPLIT")
  ,( 5 {-MCSW-}  ,"00;01;10;11")
  ,( 9 {-SFSW-}  ,"00;01;10;11")
  ,(10 {-FSAS-}  ,"SUS;POR;KHLD;SFT")
  ,(11 {-FSW-}   ,"00;01;10;11")
  ,(13 {-NSFTA-} ,dx7_usr_range True (-24,24))
  ,(14 {-NSFTB-} ,dx7_usr_range True (-24,24))
  ,(15 {-BLNC-}  ,dx7_usr_range True (-50,50))
  ,(19 {-CSSW-}  ,intercalate ";" (map (Show.show_bin (Just 4)) [0::Int .. 15]))
  ,(20 {-PNMD-}  ,"MX;11;10;01") -- MIX;0N-ON;ON-OFF;OFF-ON
  ,(22 {-PNASN-} ,"LFO;VEL;KEY")] -- VELOCITY

-- | Synonym for 'genericIndex'
dx7ii_pced_get :: DX7II_PCED -> U8 -> U8
dx7ii_pced_get = (!!)

-- | Get parameter value by name.
dx7ii_pced_get_by_nm :: DX7II_PCED -> String -> U8
dx7ii_pced_get_by_nm pf nm = dx7ii_pced_get pf (dx7ii_pced_param_ix nm)

-- | Get USR string for indexed parameter at PCED.
dx7ii_pced_get_usr :: DX7II_PCED -> U8 -> String
dx7ii_pced_get_usr pf ix =
  let k = dx7ii_pced_get pf ix
  in case lookup ix dx7ii_pced_usr_str_tbl of
       Just usr -> dx7_usr_str_ix usr k
       _ -> show k

-- | Get USR by NAME from PCED.
dx7ii_pced_get_usr_by_nm :: DX7II_PCED -> String -> String
dx7ii_pced_get_usr_by_nm pf nm = dx7ii_pced_get_usr pf (dx7ii_pced_param_ix nm)

-- | DDTN shifts A up and B down by /x/ 1/32's of a semitone.  x is in (0 - 7).
--
-- > map (fst . dx7ii_pced_ddtn_cents) [0,2 .. 6] == [-0,-6.25,-12.5,-18.75]
dx7ii_pced_ddtn_cents :: Fractional n => U8 -> (n,n)
dx7ii_pced_ddtn_cents x = let y = (fromIntegral x / 8.0) * 25.0 in (negate y,y)

-- * SYSEX

{- | UNIVERSAL BULK DUMP REQUEST (15-BYTES)

Status         11110000  FO
ID No.         01000011  43
Sub-status     0010nnnn  2N
Format No.     01111110  7E
Classification 0aaaaaaa      (4-CHAR)
Data format    Ommmmmmm      (6-CHAR)
EOX            11110111  F7

-}
dx7ii_ubd_request_sysex :: U8-> String -> String -> [U8]
dx7ii_ubd_request_sysex ch s1 s2 =
  let f = map fromEnum
  in if length s1 /= 4 || length s1 /= 6
     then error "dx7_ubp_request_sysex?"
     else concat [[0xF0,0x43,0x20 + ch,0x7E],f s1,f s2,[0xF7]]

-- * SYSEX - 8973PM - DX7II PACKED 32 PERFORMANCE - 1642 BYTES

-- | Group structure (byte-counts) for 8973PM sysex.
dx7ii_8973PM_grp :: [Int]
dx7ii_8973PM_grp = [4+2+4+6,1632,1,1]

-- | 8973PM header (SYSEX-HDR=4,BYTE-COUNT=2,CLASSIFICATION=4,FORMAT=6)
--
-- > Sound.Midi.Common.bits_7_join_le (0x6A,0x0C) == 1642
dx7ii_8973PM_hdr :: U8 -> [[U8]]
dx7ii_8973PM_hdr ch =
  [[0xF0,0x43,0x00 + ch,0x7E]
  ,[0x0C,0x6A] -- 1642
  ,[0x4C,0x4D,0x20,0x20] -- "LM  "
  ,[0x38,0x39,0x37,0x33,0x50,0x4D] -- "8973PM"
  ]

-- | Verify that byte-data is a 8973PM header at indicated channel.
dx7ii_8973PM_hdr_verify :: U8 -> [U8] -> Bool
dx7ii_8973PM_hdr_verify ch h = h == concat (dx7ii_8973PM_hdr ch)

-- | Parse 8973PM sysex to list of PCED.
dx7ii_8973PM_parse :: [U8] -> [DX7II_PCED]
dx7ii_8973PM_parse syx =
  case Split.splitPlaces dx7ii_8973PM_grp syx of
    [hdr,dat,_,[0xF7]] -> dx7ii_assert (dx7ii_8973PM_hdr_verify 0 hdr) (Split.chunksOf 51 dat)
    _ -> error "dx7ii_8973PM_parse?"

-- | Load 8973PM sysex file.
dx7ii_8973PM_load :: FilePath -> IO [DX7II_PCED]
dx7ii_8973PM_load = fmap dx7ii_8973PM_parse . dx7_read_u8
