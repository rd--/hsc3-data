-- | Yamaha DX7II
module Sound.SC3.Data.Yamaha.DX7.DX7II where

import Data.Maybe {- base -}

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

-- > map length dx7ii_pf_name_grp == dx7ii_pf_grp_n
dx7ii_pf_name_grp :: [[String]]
dx7ii_pf_name_grp =
  map words
  ["PLMD VNMA VNMB","MCTB MCKY MCSW","DDTN SPPT"
  ,"FDMP","SFSW","FSAS FSW","SPRNG","NSFTA NSFTB","BLNC TVLM","CSLD1 CSLD2 CSSW"
  ,"PNMD PANRNG PANASN","PNEGR1 PNEGR2 PNEGR3 PNEGR4","PNEGL1 PNEGL2 PNEGL3 PNEGL4"]

-- > maximum (map (length . snd) dx7ii_pf_name_tbl) == 6
dx7ii_pf_name_tbl :: [(U8,String)]
dx7ii_pf_name_tbl = zip [0..] (concat dx7ii_pf_name_grp)

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

{-

let dir = "/home/rohan/sw/hsc3-data/data/yamaha/dx7ii/rom/"
let fn_a = dir ++ "DX7II-PFA.syx"
let fn_b = dir ++ "DX7II-PFB.syx"

a <- dx7_read_u8 fn_a
length a == 1650
let p = dx7ii_8973PM_parse a
map dx7ii_pf_name p
map dx7ii_pf_plmd_usr p
map dx7ii_pf_grp p
map (!! 6) p

-}

