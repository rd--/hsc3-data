-- | Yamaha DX7II
module Sound.SC3.Data.Yamaha.DX7.DX7II where

import Data.Maybe {- base -}
import Text.Printf {- base -}

import qualified Data.List.Split as Split {- split -}

import qualified Music.Theory.Byte as Byte {- hmt -}

import Sound.SC3.Data.Yamaha.DX7 {- hsc3-data -}

dx7ii_assert :: Bool -> t -> t
dx7ii_assert e r = if e then r else error "dx7ii_assert?"

-- * 5-3. PERFORMANCE PARAMETERS (PCED / PMEM - 51-BYTES)

-- | PEFORMANCE NAME is 20-CHAR (31 - 51)
dx7ii_pf_name :: [U8] -> String
dx7ii_pf_name = map (dx7_ascii_char '?') . drop 31

dx7ii_pf_plmd_usr_str :: String
dx7ii_pf_plmd_usr_str = "SINGLE;DUAL;SPLIT"

dx7ii_pf_pnmd_usr_str :: String
dx7ii_pf_pnmd_usr_str = "MIX;0N-ON;ON-OFF;OFF-ON"

dx7ii_pf_fsas_usr_str :: String
dx7ii_pf_fsas_usr_str = "SUS;POR;KHLD;SFT"

dx7ii_pf_panasn_usr_str :: String
dx7ii_pf_panasn_usr_str = "LFO;VELOCITY;KEY"

dx7ii_usr_str_tbl :: String -> [(U8,String)]
dx7ii_usr_str_tbl = zip [0..] . Split.splitOn ";"

dx7ii_usr_str_ix :: String -> U8 -> String
dx7ii_usr_str_ix s k = fromMaybe (error "dx7ii_usr_str_ix?") (lookup k (dx7ii_usr_str_tbl s))

dx7ii_pf_plmd_usr :: [U8] -> String
dx7ii_pf_plmd_usr p = dx7ii_usr_str_ix dx7ii_pf_plmd_usr_str (p !! 0)

-- | DDTN shifts A up and B down by /x/ 1/32's of a semitone.  x is in (0 - 7).
--
-- > map (fst . dx7ii_pf_ddtn_cents) [0,2 .. 6] == [-0,-6.25,-12.5,-18.75]
dx7ii_pf_ddtn_cents :: Fractional n => U8 -> (n,n)
dx7ii_pf_ddtn_cents x = let y = (fromIntegral x / 8.0) * 25.0 in (negate y,y)

-- > sum dx7ii_pf_grp_n == 31
dx7ii_pf_grp_n :: [Int]
dx7ii_pf_grp_n = [3,3,2,1,1,2,1,2,2,3,3,4,4]

dx7ii_pf_grp :: [U8] -> [[U8]]
dx7ii_pf_grp = Split.splitPlaces dx7ii_pf_grp_n

-- > map length dx7ii_pf_param_grp == dx7ii_pf_grp_n
dx7ii_pf_param_grp :: [[String]]
dx7ii_pf_param_grp =
  map words
  ["PLMD VNMA VNMB","MCTB MCKY MCSW","DDTN SPPT"
  ,"FDMP","SFSW","FSAS FSW","SPRNG","NSFTA NSFTB","BLNC TVLM","CSLD1 CSLD2 CSSW"
  ,"PNMD PANRNG PANASN","PNEGR1 PNEGR2 PNEGR3 PNEGR4","PNEGL1 PNEGL2 PNEGL3 PNEGL4"]

-- > maximum (map (length . snd) dx7ii_pf_param_tbl) == 6
dx7ii_pf_param_tbl :: [(U8,String)]
dx7ii_pf_param_tbl = zip [0..] (concat dx7ii_pf_param_grp)

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
     "P-%02d  %s  %-6s  I-%02d  %s  I-%02d  %s"
     n (dx7ii_pf_name pf) (dx7ii_pf_plmd_usr pf) (ix_a + 1) (vc_nm ix_a) (ix_b + 1) (vc_nm ix_b)

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
map dx7ii_pf_name pf
map dx7ii_pf_plmd_usr pf
map ((!! 0) . dx7ii_pf_grp) pf
map (!! 6) pf

-}

