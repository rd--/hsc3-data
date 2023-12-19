-- | Dx7 / Pp (Pretty-Printers)
module Sound.Sc3.Data.Yamaha.Dx7.Pp where

import Control.Monad {- base -}
import Data.List {- base -}
import Text.Printf {- base -}

import qualified Data.ByteString.Lazy as L {- bytestring -}
import qualified Data.List.Split as Split {- split -}

import qualified Music.Theory.Array.Text as T {- hmt-base -}
import qualified Music.Theory.List as T {- hmt-base -}
import qualified Music.Theory.Tuple as T {- hmt-base -}

import qualified Sound.File.Next as Sf {- hsc3-sf -}

import Sound.Sc3.Data.Yamaha.Dx7
import Sound.Sc3.Data.Yamaha.Dx7.Dx7ii

-- * Enum

{- | Generate list of possible parameter Usr values, in sequence.

> map dx7_parameter_enum_usr dx7_op_parameter_tbl
-}
dx7_parameter_enum_usr :: Dx7_Parameter -> [String]
dx7_parameter_enum_usr (_, _, stp, d, u) =
  if u == "ASCII"
    then map (return . toEnum) [32 .. 126]
    else case Split.splitOn ";" u of
      [_] -> map (\x -> show (x + d)) [0 .. stp - 1]
      e -> e

-- * Char Counting

{- | Maximum Char widths for Usr representation of Op param.

> length dx7_op_char_count == 21
> sum dx7_op_char_count == 48
-}
dx7_op_char_count :: [Int]
dx7_op_char_count = [2, 2, 2, 2, 2, 2, 2, 2, 4, 2, 2, 4, 4, 1, 1, 1, 2, 5, 2, 2, 2]

{- | Maximum Char widths for Usr representation of Sh param.

> length dx7_sh_char_count == 19
> sum dx7_sh_char_count == 39
-}
dx7_sh_char_count :: [Int]
dx7_sh_char_count = [2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 3, 2, 2, 2, 2, 3, 2, 1, 3]

-- * Concise

-- | Concise Op and Sh tables for voice data.
dx7_voice_concise_tbl :: Dx7_Voice -> (String, T.Text_Table, T.Text_Table)
dx7_voice_concise_tbl v =
  let (o6, o5, o4, o3, o2, o1, sh, nm) = T.t8_from_list (dx7_voice_grp v)
      w_fn k l = zipWith (flip (T.pad_left ' ')) l k
      o_hdr = "OP" : map fst dx7ii_op_parameter_names
      o_fn i = (show i :) . zipWith (dx7_parameter_value_pp False . dx7_parameter_get) [0 ..]
      o_tbl = w_fn (2 : dx7_op_char_count) o_hdr : zipWith o_fn [6 :: U8, 5, 4, 3, 2, 1] [o6, o5, o4, o3, o2, o1]
      sh_rw s = case uncons s of
        Just (h, t) -> if h == 'P' then t else s
        _ -> s
      sh_hdr = "" : map (sh_rw . fst) dx7ii_sh_parameter_names
      sh_fn = ("SH" :) . zipWith (dx7_parameter_value_pp False . dx7_parameter_get) [126 ..]
      sh_tbl = [w_fn (2 : dx7_sh_char_count) sh_hdr, sh_fn sh]
  in (map (dx7_ascii_char '?') nm, o_tbl, sh_tbl)

{- | Concise Pp using parameter names from Dx7ii manual.

>>> putStr $ unlines $ dx7_voice_concise_str dx7_init_voice
INIT VOICE
<BLANKLINE>
OP R1 R2 R3 R4 L1 L2 L3 L4   BP LD RD   LC   RC RS AMS TS TL    PM PC PF PD
 6 99 99 99 99 99 99 99  0   C3  0  0 -LIN -LIN  0   0  0  0 RATIO  1  0  0
 5 99 99 99 99 99 99 99  0   C3  0  0 -LIN -LIN  0   0  0  0 RATIO  1  0  0
 4 99 99 99 99 99 99 99  0   C3  0  0 -LIN -LIN  0   0  0  0 RATIO  1  0  0
 3 99 99 99 99 99 99 99  0   C3  0  0 -LIN -LIN  0   0  0  0 RATIO  1  0  0
 2 99 99 99 99 99 99 99  0   C3  0  0 -LIN -LIN  0   0  0  0 RATIO  1  0  0
 1 99 99 99 99 99 99 99  0   C3  0  0 -LIN -LIN  0   0  0 99 RATIO  1  0  0
<BLANKLINE>
   R1 R2 R3 R4 L1 L2 L3 L4 ALS FBL OPI LFS LFD LPMD LAMD LFKS LFW LPMS TRNP
SH 99 99 99 99 50 50 50 50   1   0  ON  35   0    0    0   ON  TR    3   C2
-}
dx7_voice_concise_str :: Dx7_Voice -> [String]
dx7_voice_concise_str v =
  let (nm, op, sh) = dx7_voice_concise_tbl v
      op_str = "" : T.table_pp (True, True, False, " ", False) op
      sh_str = "" : T.table_pp (True, True, False, " ", False) sh
  in nm : (op_str ++ sh_str)

-- | Concise Pp of sequence of voices.
dx7_voice_concise_seq :: [Dx7_Voice] -> String
dx7_voice_concise_seq = unlines . intercalate [""] . map dx7_voice_concise_str

-- * Plain

{- | Pretty print value give 'Dx7_Parameter'.

>>> dx7_parameter_value_pp False (dx7_parameter_tbl !! 134) 3
" 4"
-}
dx7_parameter_value_pp :: Bool -> Dx7_Parameter -> U8 -> String
dx7_parameter_value_pp leadingZero p x =
  let (_, _, stp, d, u) = p
      (x_clip, err) =
        if x >= stp
          then (stp - 1, printf "(Error Byte=0x%02X)" x)
          else (x, "")
      r =
        if u == "ASCII"
          then ['\'', dx7_ascii_char '?' x_clip, '\'']
          else case Split.splitOn ";" u of
            [_] -> printf (if leadingZero then "%02d" else "%2d") (x_clip + d)
            e -> e !! x_clip
  in r ++ err

-- | (Leading-Zero, With-Index)
type Dx7_Parameter_Pp_Opt = (Bool, Bool)

{- | Pretty Dx7_Parameter.

>>> dx7_parameter_pp (False, True) (dx7_parameter_tbl !! 134) 3
"134: ALGORITHM # =  4"
-}
dx7_parameter_pp :: Dx7_Parameter_Pp_Opt -> Dx7_Parameter -> U8 -> String
dx7_parameter_pp (leadingZero, with_ix) p x =
  let (ix, nm, _, _, _) = p
      pp = dx7_parameter_value_pp leadingZero p x
  in if with_ix
      then printf "%03d: %s = %s" ix nm pp
      else printf "%s = %s" nm pp

dx7_parameter_set_pp :: Dx7_Parameter -> [U8] -> String
dx7_parameter_set_pp p x =
  let (_, nm, _, _, _) = p
  in printf "%s = %s" nm (intercalate "," (map (dx7_parameter_value_pp False p) x))

-- | Print complete parameter sequence.
dx7_parameter_seq_pp :: Dx7_Parameter_Pp_Opt -> Dx7_Voice -> [String]
dx7_parameter_seq_pp opt = zipWith (dx7_parameter_pp opt) dx7_parameter_tbl

{- | Dx7_Voice pretty-printer.

>>> putStr $ unlines $ dx7_voice_pp dx7_init_voice
PITCH EG RATE 1 = 99
PITCH EG RATE 2 = 99
PITCH EG RATE 3 = 99
PITCH EG RATE 4 = 99
PITCH EG LEVEL 1 = 50
PITCH EG LEVEL 2 = 50
PITCH EG LEVEL 3 = 50
PITCH EG LEVEL 4 = 50
ALGORITHM # =  1
FEEDBACK =  0
OSCILLATOR SYNC = ON
LFO SPEED = 35
LFO DELAY =  0
LFO PITCH MOD DEPTH =  0
LFO AMP MOD DEPTH =  0
LFO SYNC = ON
LFO WAVEFORM = TR
PITCH MOD SENSITIVITY =  3
TRANSPOSE = C2
EG RATE 1 = 99,99,99,99,99,99
EG RATE 2 = 99,99,99,99,99,99
EG RATE 3 = 99,99,99,99,99,99
EG RATE 4 = 99,99,99,99,99,99
EG LEVEL 1 = 99,99,99,99,99,99
EG LEVEL 2 = 99,99,99,99,99,99
EG LEVEL 3 = 99,99,99,99,99,99
EG LEVEL 4 =  0, 0, 0, 0, 0, 0
KBD LEV SCL BRK PT = C3,C3,C3,C3,C3,C3
KBD LEV SCL LFT DEPTH =  0, 0, 0, 0, 0, 0
KBD LEV SCL RHT DEPTH =  0, 0, 0, 0, 0, 0
KBD LEV SCL LFT CURVE = -LIN,-LIN,-LIN,-LIN,-LIN,-LIN
KBD LEV SCL RHT CURVE = -LIN,-LIN,-LIN,-LIN,-LIN,-LIN
KBD RATE SCALING =  0, 0, 0, 0, 0, 0
AMP MOD SENSITIVITY =  0, 0, 0, 0, 0, 0
KEY VEL SENSITIVITY =  0, 0, 0, 0, 0, 0
OPERATOR OUTPUT LEVEL =  0, 0, 0, 0, 0,99
OSC MODE = RATIO,RATIO,RATIO,RATIO,RATIO,RATIO
OSC FREQ COARSE =  1, 1, 1, 1, 1, 1
OSC FREQ FINE =  0, 0, 0, 0, 0, 0
OSC DETUNE =  0, 0, 0, 0, 0, 0
-}
dx7_voice_pp :: Dx7_Voice -> [String]
dx7_voice_pp p =
  let p_grp = dx7_voice_grp p
  in concat
      [ zipWith (dx7_parameter_pp (False, False)) dx7_sh_parameter_tbl (p_grp !! 6)
      , zipWith dx7_parameter_set_pp dx7_op_parameter_tbl (transpose (take 6 p_grp))
      ]

-- * Voice-Data List

{- | Plain text voice data list.

> putStrLn$unlines$ dx7_voice_data_list_pp (replicate 155 0)
-}
dx7_voice_data_list_pp :: Bool -> Dx7_Voice -> [String]
dx7_voice_data_list_pp leadingZero d =
  let u8_at = (!!)
      op_ix_set n = [n, n + dx7_op_nparam .. n + dx7_op_nparam * 5]
      op_ix_pp n =
        map
          (dx7_parameter_value_pp leadingZero (u8_at dx7_op_parameter_tbl n) . u8_at d)
          (op_ix_set n)
      is_op_ix n = n < 126
      ix_val n =
        if is_op_ix n
          then intercalate "," (op_ix_pp n)
          else dx7_parameter_value_pp leadingZero (u8_at dx7_sh_parameter_tbl (n - 126)) (u8_at d n)
      pp z nm ix = concat [z, nm, "=", ix_val ix]
      f (grp, nm, ix) =
        if null grp
          then zipWith (pp "") nm ix
          else grp : zipWith (pp "  ") nm ix
      vc_nm = "NAME=" ++ dx7_voice_name '?' d
  in vc_nm : concatMap f dx7_voice_data_list

-- * Csv

{- | Encode 'Dx7_Voice' as Csv entry.
  The first column is the voice name as an Ascii string, followed by the parameters in sequence.
  Parameters are written as decimal integers.
-}
dx7_voice_to_csv :: Dx7_Voice -> String
dx7_voice_to_csv v =
  let nm = map (\c -> if c == ',' then '_' else c) . dx7_voice_name '?'
  in intercalate "," (nm v : map show (dx7_voice_param v))

-- | Decode 'Dx7_Voice' from Csv entry.
dx7_voice_from_csv :: String -> Dx7_Voice
dx7_voice_from_csv str =
  case Split.splitOn "," str of
    nm : p -> dx7_param_to_dx7_voice nm (map read p)
    _ -> error "dx7_voice_from_csv?"

{- | Write sequence of 'Dx7_Voice' to Csv file.

> let fn = "/home/rohan/sw/hsc3-data/data/yamaha/dx7/rom/DX7-ROM1A.syx"
> d <- dx7_load_fmt9_sysex_err fn
> dx7_store_csv True "/tmp/dx7.csv" d
> r <- dx7_load_csv "/tmp/dx7.csv"
> r == d
-}
dx7_store_csv :: Bool -> FilePath -> [Dx7_Voice] -> IO ()
dx7_store_csv chk_rng fn v = do
  dx7_voice_set_verify chk_rng v
  writeFile fn (unlines (map dx7_voice_to_csv v))

-- | Read sequence of 'Dx7_Voice' from Csv file.
dx7_load_csv :: FilePath -> IO [Dx7_Voice]
dx7_load_csv fn = do
  s <- readFile fn
  let v = map dx7_voice_from_csv (lines s)
      chk_rng = False
  dx7_voice_set_verify chk_rng v
  return v

-- * Next/Au

{- | Write Vced data to Next/Au audio file.
     Data is encoded as signed 8-bit linear Pcm.

> let fn = "/home/rohan/sw/hsc3-data/data/yamaha/dx7/rom/DX7-ROM1A.syx"
> d <- dx7_load_fmt9_sysex_err fn
> dx7_store_au "/tmp/dx7.snd" d
-}
dx7_store_au :: FilePath -> [Dx7_Voice] -> IO ()
dx7_store_au fn vc = do
  let dat = L.pack (map fromIntegral (concat vc))
      hdr = Sf.Sf_Header (length vc * 155) Sf.Linear8 155 1
  Sf.au_write_b fn hdr dat

{- | Read Vced data from Next/Au audio file.
     Data must be encoded as signed 8-bit linear Pcm.

> vc <- dx7_load_au "/tmp/dx7.snd"
> dx7_voice_set_verify True vc
-}
dx7_load_au :: FilePath -> IO [Dx7_Voice]
dx7_load_au fn = do
  (Sf.Sf_Header nf enc _sr nc, dat) <- Sf.au_read_b fn
  when (nf `mod` 155 /= 0 || enc /= Sf.Linear8 || nc /= 1) (error "dx7_load_au?")
  return (Split.chunksOf 155 (map fromIntegral (L.unpack dat)))

-- * Dx7-II

-- | Pced summary: P-Ix P-Name Plmd A-Ix A-Name B-Ix B-Name
dx7ii_pced_summary :: [Dx7_Voice] -> (Int, Dx7ii_Pced) -> String
dx7ii_pced_summary vc (n, pf) =
  let vc_nm k = dx7_voice_name '?' (vc !! k)
      (ix_a, ix_b) = (pf !! 1, pf !! 2)
  in printf
      "P-%02d  %s  %-6s  A-%03d  %s  B-%03d  %s"
      n
      (dx7ii_pced_name pf)
      (dx7ii_pced_get_usr_by_nm pf "PLMD")
      (ix_a + 1)
      (vc_nm ix_a)
      (ix_b + 1)
      (vc_nm ix_b)

-- | 'dx7ii_pced_summary' for sequence.
dx7ii_pced_summary_seq :: ([Dx7_Voice], [Dx7ii_Pced]) -> [String]
dx7ii_pced_summary_seq (vc, pf) = zipWith (curry (dx7ii_pced_summary vc)) [1 ..] pf

dx7ii_pced_summary_hdr :: [String]
dx7ii_pced_summary_hdr =
  [ "P-IX  P-NAME                PLMD    A-IX   A-NAME      B-IX   B-NAME   "
  , "----  --------------------  ------  -----  ----------  -----  ---------"
  ]

{- | Load Vced voice sysex files and Pced performance sysex files and print summary

> let dir = "/home/rohan/sw/hsc3-data/data/yamaha/dx7ii/rom/"
> let vc_fn = map (dir ++) (words "DX7II-32A.syx DX7II-64A.syx DX7II-32B.syx DX7II-64B.syx")
> let pf_fn = map (dir ++) (words "DX7II-PFA.syx DX7II-PFB.syx")
> dx7ii_pced_summary_syx vc_fn pf_fn
-}
dx7ii_pced_summary_syx :: [FilePath] -> [FilePath] -> IO ()
dx7ii_pced_summary_syx vc_fn pf_fn = do
  vc <- mapM dx7_load_fmt9_sysex_err vc_fn
  pf <- mapM dx7ii_8973pm_load pf_fn
  putStrLn $ unlines $ dx7ii_pced_summary_hdr ++ dx7ii_pced_summary_seq (concat vc, concat pf)

{- | Table of Pced data in Usr format.
  Prefixed with P-Ix, therefore 32 fields.
-}
dx7ii_pced_pp_tbl :: [Dx7ii_Pced] -> [[String]]
dx7ii_pced_pp_tbl pf_seq =
  let hdr = "#" : dx7ii_pced_param_abbrev
      pfx = zipWith (:) (map show [1 :: Int ..])
      dat = pfx (map (\pf -> map (dx7ii_pced_get_usr pf) [0 .. 30]) pf_seq)
  in hdr : dat

{- | Print Pced data as text table.  Split into parts if /pt/ is True.

> pf <- Music.Theory.Monad.concatMapM dx7ii_8973pm_load pf_fn
> dx7ii_pced_pp (Just [18,14]) pf
-}
dx7ii_pced_pp :: Maybe [Int] -> [Dx7ii_Pced] -> IO ()
dx7ii_pced_pp pt pf = do
  let tbl = dx7ii_pced_pp_tbl pf
      txt = case pt of
        Nothing -> T.table_pp T.table_opt_simple tbl
        Just pl -> concatMap (T.table_pp T.table_opt_simple) (T.table_split pl tbl)
  putStrLn (unlines txt)
