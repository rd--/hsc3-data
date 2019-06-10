-- | DX7 / PP (PRETTY-PRINTERS)
module Sound.SC3.Data.Yamaha.DX7.PP where

import Control.Monad {- base -}
import Data.List {- base -}
import Text.Printf {- base -}

import qualified Data.ByteString.Lazy as L {- bytestring -}
import qualified Data.List.Split as Split {- split -}

import qualified Music.Theory.Array.Text as T {- hmt -}
import qualified Music.Theory.Byte as Byte {- hmt -}
import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Math.Convert as T {- hmt -}

import qualified Sound.File.NeXT as SF {- hsc3-sf -}

import Sound.SC3.Data.Yamaha.DX7
import Sound.SC3.Data.Yamaha.DX7.DX7II

-- * ENUM

-- | Generate list of possible parameter USR values, in sequence.
--
-- > map dx7_parameter_enum_usr dx7_op_parameter_tbl
dx7_parameter_enum_usr :: DX7_Parameter -> [String]
dx7_parameter_enum_usr (_,_,stp,d,u) =
  if u == "ASCII"
  then map (return . toEnum) [32 .. 126]
  else case Split.splitOn ";" u of
         [_] -> map (\x -> show (T.word8_to_int8 x + d)) [0 .. stp - 1]
         e -> e

-- * CHAR COUNTING

-- | Maximum CHAR widths for USR representation of OP param.
--
-- > length dx7_op_char_count == 21
-- > sum dx7_op_char_count == 48
dx7_op_char_count :: [Int]
dx7_op_char_count = [2,2,2,2, 2,2,2,2, 4,2,2,4,4,1, 1,1,2, 5,2,2,2]

-- | Maximum CHAR widths for USR representation of SH param.
--
-- > length dx7_sh_char_count == 19
-- > sum dx7_sh_char_count == 39
dx7_sh_char_count :: [Int]
dx7_sh_char_count = [2,2,2,2, 2,2,2,2, 2,1,3, 2,2,2,2,3,2, 1,3]

-- * CONCISE

-- | Concise OP and SH TABLEs for voice data.
dx7_voice_concise_tbl :: DX7_Voice -> (String,T.TABLE,T.TABLE)
dx7_voice_concise_tbl v =
  let [o6,o5,o4,o3,o2,o1,sh,nm] = dx7_voice_grp v
      w_fn k l = zipWith (\s i -> T.pad_left ' ' i s) l k
      o_hdr = "OP" : map fst dx7ii_op_parameter_names
      o_fn i = (show i :) . map (\(k,x) -> dx7_parameter_value_pp (dx7_parameter_get k) x) . zip [0..]
      o_tbl = w_fn (2 : dx7_op_char_count) o_hdr : zipWith o_fn [6::U8,5,4,3,2,1] [o6,o5,o4,o3,o2,o1]
      sh_rw s = if head s == 'P' then tail s else s
      sh_hdr = "" : map (sh_rw . fst) dx7ii_sh_parameter_names
      sh_fn = ("SH" :) . map (\(k,x) -> dx7_parameter_value_pp (dx7_parameter_get k) x) . zip [126..]
      sh_tbl = [w_fn (2 : dx7_sh_char_count) sh_hdr,sh_fn sh]
  in (map (dx7_ascii_char '?') nm,o_tbl,sh_tbl)

-- | Concise PP using parameter names from DX7II manual.
--
-- > putStrLn $ unlines $ "" : dx7_voice_concise_str dx7_init_voice
dx7_voice_concise_str :: DX7_Voice -> [String]
dx7_voice_concise_str v =
  let (nm,op,sh) = dx7_voice_concise_tbl v
      op_str = "" : T.table_pp (True,True,False," ",False) op
      sh_str = "" : T.table_pp (True,True,False," ",False) sh
  in nm : (op_str ++ sh_str)

-- | Concise PP of sequence of voices.
dx7_voice_concise_seq :: [DX7_Voice] -> String
dx7_voice_concise_seq = unlines . intercalate [""] . map dx7_voice_concise_str

-- * PLAIN

-- | Pretty print value give 'DX7_Parameter'.
dx7_parameter_value_pp :: DX7_Parameter -> U8 -> String
dx7_parameter_value_pp p x =
  let (_,_,stp,d,u) = p
      (x_clip,err) = if x >= stp
                     then (stp - 1,printf "(ERROR BYTE=0x%02X)" x)
                     else (x,"")
      r = if u == "ASCII"
          then ['\'',dx7_ascii_char '?' x_clip,'\'']
          else case Split.splitOn ";" u of
                 [_] -> printf "%2d" (T.word8_to_int8 x_clip + d)
                 e -> Byte.word8_at e x_clip
  in r ++ err

dx7_parameter_pp :: Bool -> DX7_Parameter -> U8 -> String
dx7_parameter_pp with_ix p x =
  let (ix,nm,_,_,_) = p
  in if with_ix
     then printf "%03d: %s = %s" ix nm (dx7_parameter_value_pp p x)
     else printf "%s = %s" nm (dx7_parameter_value_pp p x)

dx7_parameter_set_pp :: DX7_Parameter -> [U8] -> String
dx7_parameter_set_pp p x =
  let (_,nm,_,_,_) = p
  in printf "%s = %s" nm (intercalate "," (map (dx7_parameter_value_pp p) x))

-- | Print complete parameter sequence.
dx7_parameter_seq_pp :: DX7_Voice -> [String]
dx7_parameter_seq_pp = zipWith (dx7_parameter_pp True) dx7_parameter_tbl

-- | DX7_Voice pretty-printer.
--
-- > putStrLn$unlines$ dx7_voice_pp dx7_init_voice
dx7_voice_pp :: DX7_Voice -> [String]
dx7_voice_pp p =
  let p_grp = dx7_voice_grp p
  in concat [zipWith (dx7_parameter_pp False) dx7_sh_parameter_tbl (Byte.word8_at p_grp 6)
            ,zipWith dx7_parameter_set_pp dx7_op_parameter_tbl (transpose (take 6 p_grp))]

-- * VOICE-DATA LIST

-- | Plain text voice data list.
--
-- > putStrLn$unlines$ dx7_voice_data_list_pp (replicate 155 0)
dx7_voice_data_list_pp :: DX7_Voice -> [String]
dx7_voice_data_list_pp d =
  let u8_at = Byte.word8_at
      op_ix_set n = [n, n + dx7_op_nparam .. n + dx7_op_nparam * 5]
      op_ix_pp n = map
                   (dx7_parameter_value_pp (u8_at dx7_op_parameter_tbl n))
                   (map (u8_at d) (op_ix_set n))
      is_op_ix n = n < 126
      ix_val n =
        if is_op_ix n
        then intercalate "," (op_ix_pp n)
        else dx7_parameter_value_pp (u8_at dx7_sh_parameter_tbl (n - 126)) (u8_at d n)
      pp z nm ix = concat [z,nm,"=",ix_val ix]
      f (grp,nm,ix) =
        if null grp
        then zipWith (pp "") nm ix
        else grp : zipWith (pp "  ") nm ix
      vc_nm = "NAME=" ++ dx7_voice_name '?' d
  in vc_nm : concatMap f dx7_voice_data_list

-- * CSV

-- | Encode 'DX7_Voice' as CSV entry.
--   The first column is the voice name as an ASCII string, followed by the parameters in sequence.
--   Parameters are written as decimal integers.
dx7_voice_to_csv :: DX7_Voice -> String
dx7_voice_to_csv v =
  let nm = map (\c -> if c == ',' then '_' else c) . dx7_voice_name '?'
  in intercalate "," (nm v : map show (dx7_voice_param v))

-- | Decode 'DX7_Voice' from CSV entry.
dx7_voice_from_csv :: String -> DX7_Voice
dx7_voice_from_csv str =
  case Split.splitOn "," str of
    nm:p -> dx7_param_to_dx7_voice nm (map read p)
    _ -> error "dx7_voice_from_csv?"

{- | Write sequence of 'DX7_Voice' to CSV file.

> let fn = "/home/rohan/sw/hsc3-data/data/yamaha/dx7/rom/DX7-ROM1A.syx"
> d <- dx7_load_fmt9_sysex_err fn
> dx7_store_csv True "/tmp/dx7.csv" d
> r <- dx7_load_csv "/tmp/dx7.csv"
> r == d

-}
dx7_store_csv :: Bool -> FilePath -> [DX7_Voice] -> IO ()
dx7_store_csv chk_rng fn v = do
  dx7_voice_set_verify chk_rng v
  writeFile fn (unlines (map dx7_voice_to_csv v))

-- | Read sequence of 'DX7_Voice' from CSV file.
dx7_load_csv :: FilePath -> IO [DX7_Voice]
dx7_load_csv fn = do
  s <- readFile fn
  let v = map dx7_voice_from_csv (lines s)
      chk_rng = False
  dx7_voice_set_verify chk_rng v
  return v

-- * NeXT/AU

{- | Write VCED data to NeXT/AU audio file.
     Data is encoded as signed 8-bit linear PCM.

> let fn = "/home/rohan/sw/hsc3-data/data/yamaha/dx7/rom/DX7-ROM1A.syx"
> d <- dx7_load_fmt9_sysex_err fn
> dx7_store_au "/tmp/dx7.snd" d
-}
dx7_store_au :: FilePath -> [DX7_Voice] -> IO ()
dx7_store_au fn vc = do
  let dat = L.pack (concat vc)
      hdr = SF.SF_Header (length vc * 155) SF.Linear8 155 1
  SF.au_write_b fn hdr dat

{- | Read VCED data from NeXT/AU audio file.
     Data must be encoded as signed 8-bit linear PCM.

> vc <- dx7_load_au "/tmp/dx7.snd"
> dx7_voice_set_verify True vc
-}
dx7_load_au :: FilePath -> IO [DX7_Voice]
dx7_load_au fn = do
  (SF.SF_Header nf enc _sr nc,dat) <- SF.au_read_b fn
  when (nf `mod` 155 /= 0 || enc /= SF.Linear8 || nc /= 1) (error "dx7_load_au?")
  return (Split.chunksOf 155 (L.unpack dat))

-- * DX7-II

-- | PCED summary: P-IX P-NAME PLMD A-IX A-NAME B-IX B-NAME
dx7ii_pced_summary :: [DX7_Voice] -> (Int,DX7II_PCED) -> String
dx7ii_pced_summary vc (n,pf) =
  let vc_nm k = dx7_voice_name '?' (Byte.word8_at vc k)
      (ix_a,ix_b) = (pf !! 1,pf !! 2)
  in printf
     "P-%02d  %s  %-6s  A-%03d  %s  B-%03d  %s"
     n (dx7ii_pced_name pf) (dx7ii_pced_get_usr_by_nm pf "PLMD")
     (ix_a + 1) (vc_nm ix_a) (ix_b + 1) (vc_nm ix_b)

-- | 'dx7ii_pced_summary' for sequence.
dx7ii_pced_summary_seq :: ([DX7_Voice], [DX7II_PCED]) -> [String]
dx7ii_pced_summary_seq (vc,pf) = map (dx7ii_pced_summary vc) (zip [1..] pf)

dx7ii_pced_summary_hdr :: [String]
dx7ii_pced_summary_hdr =
  ["P-IX  P-NAME                PLMD    A-IX   A-NAME      B-IX   B-NAME   "
  ,"----  --------------------  ------  -----  ----------  -----  ---------"]

{- | Load VCED voice sysex files and PCED performance sysex files and print summary

> let dir = "/home/rohan/sw/hsc3-data/data/yamaha/dx7ii/rom/"
> let vc_fn = map (dir ++) (words "DX7II-32A.syx DX7II-64A.syx DX7II-32B.syx DX7II-64B.syx")
> let pf_fn = map (dir ++) (words "DX7II-PFA.syx DX7II-PFB.syx")
> dx7ii_pced_summary_syx vc_fn pf_fn
-}
dx7ii_pced_summary_syx :: [FilePath] -> [FilePath] -> IO ()
dx7ii_pced_summary_syx vc_fn pf_fn = do
  vc <- mapM dx7_load_fmt9_sysex_err vc_fn
  pf <- mapM dx7ii_8973PM_load pf_fn
  putStrLn $ unlines $ dx7ii_pced_summary_hdr ++ dx7ii_pced_summary_seq (concat vc,concat pf)

-- | Table of PCED data in USR format.
--   Prefixed with P-IX, therefore 32 fields.
dx7ii_pced_pp_tbl :: [DX7II_PCED] -> [[String]]
dx7ii_pced_pp_tbl pf_seq =
  let hdr = "#" : dx7ii_pced_param_abbrev
      pfx = zipWith (:) (map show [1::Int ..])
      dat = pfx (map (\pf -> map (dx7ii_pced_get_usr pf) [0 .. 30]) pf_seq)
  in hdr : dat

{- | Print PCED data as text table.  Split into parts if /pt/ is True.

> pf <- Music.Theory.Monad.concatMapM dx7ii_8973PM_load pf_fn
> dx7ii_pced_pp (Just [18,14]) pf
-}
dx7ii_pced_pp :: Maybe [Int] -> [DX7II_PCED] -> IO ()
dx7ii_pced_pp pt pf = do
  let tbl = dx7ii_pced_pp_tbl pf
      txt = case pt of
              Nothing -> T.table_pp T.table_opt_simple tbl
              Just pl -> concatMap (T.table_pp T.table_opt_simple) (T.table_split pl tbl)
  putStrLn (unlines txt)
