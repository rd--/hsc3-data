-- | Yamaha DX7II
module Sound.SC3.Data.Yamaha.DX7.DX7II where

import Data.List {- base -}
import Text.Printf {- base -}

import qualified Data.List.Split as Split {- split -}

import qualified Music.Theory.Byte as Byte {- hmt -}
import qualified Music.Theory.Show as Show {- hmt -}

import Sound.SC3.Data.Yamaha.DX7 {- hsc3-data -}

dx7ii_assert :: Bool -> t -> t
dx7ii_assert e r = if e then r else error "dx7ii_assert?"

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
  ,(15 {-BLNC-}  ,dx7_usr_range True (-50,50))
  ,(19 {-CSSW-}  ,intercalate ";" (map (Show.show_bin (Just 4)) [0::Int .. 15]))
  ,(20 {-PNMD-}  ,"MX;11;10;01") -- MIX;0N-ON;ON-OFF;OFF-ON
  ,(22 {-PNASN-},"LFO;VEL;KEY")] -- VELOCITY

-- | Get USR string for indexed parameter at PCED.
dx7ii_pced_get_usr :: DX7II_PCED -> U8 -> String
dx7ii_pced_get_usr pf ix =
  let k = Byte.word8_at pf ix
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

dx7ii_pced_pp_tbl :: [DX7II_PCED] -> [[String]]
dx7ii_pced_pp_tbl pf_seq =
  let hdr = dx7ii_pced_param_abbrev
      dat = map (\pf -> map (dx7ii_pced_get_usr pf) [0 .. 30]) pf_seq
  in hdr : dat

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
  let f = map Byte.char_to_word8
  in if length s1 /= 4 || length s1 /= 6
     then error "dx7_ubp_request_sysex?"
     else concat [[0xF0,0x43,0x20 + ch,0x7E],f s1,f s2,[0xF7]]

-- * SYSEX - 8973PM

dx7ii_8973PM_grp :: [Int]
dx7ii_8973PM_grp = [4+2+4+6,1632,1,1]

-- | 8973PM header (SYSEX-HDR:4,BYTE-COUNT:2,CLASSIFICATION:4,FORMAT:6)
--
-- > Sound.Midi.Common.bits_7_join_le (0x6A,0x0C) == 1642
dx7ii_8973PM_hdr :: U8 -> [[U8]]
dx7ii_8973PM_hdr ch =
  [[0xF0,0x43,0x00 + ch,0x7E]
  ,[0x0C,0x6A] -- 1642
  ,[0x4C,0x4D,0x20,0x20] -- "LM  "
  ,[0x38,0x39,0x37,0x33,0x50,0x4D] -- "8973PM"
  ]

dx7ii_8973PM_hdr_verify :: U8 -> [U8] -> Bool
dx7ii_8973PM_hdr_verify ch h = h == concat (dx7ii_8973PM_hdr ch)

dx7ii_8973PM_parse :: [U8] -> [[U8]]
dx7ii_8973PM_parse syx =
  case Split.splitPlaces dx7ii_8973PM_grp syx of
    [hdr,dat,_,[0xF7]] -> dx7ii_assert (dx7ii_8973PM_hdr_verify 0 hdr) (Split.chunksOf 51 dat)
    _ -> error "dx7ii_8973PM_parse?"

dx7ii_8973PM_load :: FilePath -> IO [[U8]]
dx7ii_8973PM_load = fmap dx7ii_8973PM_parse . dx7_read_u8

dx7ii_8973PM_summary :: [DX7_Voice] -> Int -> [U8] -> String
dx7ii_8973PM_summary vc n pf =
  let vc_nm k = dx7_voice_name '?' (Byte.word8_at vc k)
      (ix_a,ix_b) = (pf !! 1,pf !! 2)
  in printf
     "P-%02d  %s  %-6s  A-%03d  %s  B-%03d  %s"
     n (dx7ii_pced_name pf) (dx7ii_pced_get_usr_by_nm pf "PLMD")
     (ix_a + 1) (vc_nm ix_a) (ix_b + 1) (vc_nm ix_b)

dx7ii_8973PM_summary_wr :: [FilePath] -> [FilePath] -> IO ()
dx7ii_8973PM_summary_wr vc_fn pf_fn = do
  vc <- mapM dx7_load_fmt9_sysex_err vc_fn
  pf <- mapM dx7ii_8973PM_load pf_fn
  let str = zipWith (dx7ii_8973PM_summary (concat vc)) [1..] (concat pf)
  putStrLn $ unlines $ str

{-

let dir = "/home/rohan/sw/hsc3-data/data/yamaha/dx7ii/rom/"
let vc_fn = map (dir ++) (words "DX7II-32A.syx DX7II-64A.syx DX7II-32B.syx DX7II-64B.syx")
let pf_fn = map (dir ++) (words "DX7II-PFA.syx DX7II-PFB.syx")

dx7ii_8973PM_summary_wr vc_fn pf_fn

pf <- Music.Theory.Monad.concatMapM dx7ii_8973PM_load pf_fn
length pf == 64

import Music.Theory.Array.Text {- hmt -}
tbl = table_split [17,14] (dx7ii_pced_pp_tbl pf)
putStrLn $ unlines $ concatMap (table_pp table_opt_simple) tbl
-}

