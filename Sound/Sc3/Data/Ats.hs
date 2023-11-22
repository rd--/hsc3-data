-- | Reader for Ats analyis data files.
--
-- Juan Pampin. /Ats: A LISP Environment for Spectral Modeling/.
-- In Proc. ICMC, 1999.
module Sound.Sc3.Data.Ats where

import Data.Int {- base -}
import Data.List {- base -}

import qualified Data.ByteString.Lazy as B {- bytestring -}
import qualified Data.List.Split as Split {- split -}

import qualified Music.Theory.List as List {- hmt-base -}

import qualified Sound.File.Next as Au {- hsc3-sf -}

import qualified Sound.Osc.Coding.Byte as Osc {- hosc -}

import qualified Sound.Sc3 as Sc3 {- hsc3 -}

-- | Ats analysis frame data.
type Ats_Frame = [Double]

-- | Ats file type.
type Ats_File_Type = Int

-- | Ats parameter types, the residual is 25-band.
data Ats_Param = Ats_Time | Ats_Amplitude | Ats_Frequency | Ats_Phase | Ats_Residual

-- | Ats analysis meta-data.
data Ats_Header = Ats_Header
    {ats_sample_rate :: Double
    ,ats_frame_size :: Int
    ,ats_window_size :: Int
    ,ats_n_partials :: Int
    ,ats_n_frames :: Int
    ,ats_max_amplitude :: Double
    ,ats_max_frequency :: Double
    ,ats_analysis_duration :: Double
    ,ats_file_type :: Int
    ,ats_frame_length :: Ats_File_Type} deriving (Eq, Show)

-- | Ats analysis data.
data Ats = Ats {ats_header :: Ats_Header
               ,ats_frames :: [Ats_Frame]}
           deriving (Eq, Show)

splitOn2 :: Eq a => [a] -> [a] -> ([a], [a])
splitOn2 sep str =
  case Split.splitOn sep str of
    [p, q] -> (p, q)
    _ -> error "splitOn2"

-- | Very simple minded generic fields viewer for records.
record_fields :: Show r => r -> [(String,String)]
record_fields =
    map ((\(p,q) -> (dropWhile (== ' ') p,q)) . List.split_on_1_err " = ") .
    Split.splitOn "," .
    takeWhile (/= '}') .
    tail .
    dropWhile (/= '{') .
    show

-- | Pretty printer for 'record_fields'.
record_fields_pp :: Show r => r -> String
record_fields_pp = unlines . map (\(k,v) -> k ++ " = " ++ v) . record_fields

-- | Type specialised 'record_fields'.
ats_header_fields :: Ats_Header -> [(String,String)]
ats_header_fields = record_fields

-- | Type specialised 'record_fields_pp'.
ats_header_pp :: Ats_Header -> String
ats_header_pp = record_fields_pp

-- | Split 'B.ByteString' into /i/ parts each of /n/ bytes.
--
-- > bs_sep 3 4 (B.pack [1..12]) == map B.pack [[1..4],[5..8],[9..12]]
bs_sep :: Int64 -> Int64 -> B.ByteString -> [B.ByteString]
bs_sep i n d =
    if i == 1
    then if B.length d == n then [d] else error "bs_sep"
    else let (p,q) = B.splitAt n d in p : bs_sep (i - 1) n q

-- | The first eight bytes of the file determine endianess, and hence the decoder.
ats_get_decoder :: B.ByteString -> B.ByteString -> Double
ats_get_decoder v =
    let f_be = Osc.decode_f64
        f_le = f_be . B.reverse
        err = error "ats_get_decoder: not Ats file?"
    in if f_be v == 123.0 then f_be else if f_le v == 123.0 then f_le else err

-- | Ats files are sequences of 64-bit Ieee doubles.
ats_read_f64 :: FilePath -> IO [Double]
ats_read_f64 fn = do
  d <- B.readFile fn
  let n = B.length d `div` 8
      f = ats_get_decoder (B.take 8 d)
  return (map f (bs_sep n 8 d))

-- | Parse Ats header.
ats_parse_header :: [Double] -> Ats_Header
ats_parse_header d =
    let f_at j = d !! j
        i_at = floor . f_at
        ft = i_at 9
        (_,n,x) = ats_ftype_n ft
        np = i_at 4
        nf = i_at 5
        fl = np * n + x
    in Ats_Header {ats_sample_rate = f_at 1
                  ,ats_frame_size = i_at 2
                  ,ats_window_size = i_at 3
                  ,ats_n_partials = np
                  ,ats_n_frames = nf
                  ,ats_max_amplitude = f_at 6
                  ,ats_max_frequency = f_at 7
                  ,ats_analysis_duration = f_at 8
                  ,ats_file_type = ft
                  ,ats_frame_length = fl}

{- | Read an Ats data file.

>>> ats <- ats_read "/home/rohan/sw/hsc3-data/data/ats/pf-c5.4.ats"
>>> let Ats hdr frm = ats
>>> map length frm == replicate (ats_n_frames hdr) (ats_frame_length hdr)
True

> putStrLn $ ats_header_pp hdr

@
ats_sample_rate = 44100.0
ats_frame_size = 2205
ats_window_size = 8821
ats_n_partials = 36
ats_n_frames = 234
ats_max_amplitude = 8.300120462729725e-2
ats_max_frequency = 2112.43387414816
ats_analysis_duration = 11.591451644897461
ats_file_type = 4
ats_frame_length = 134
@

> import Sound.Sc3.Plot {- hcs3-plot -}
> plot_p1_ln [ats_time ats]
> plot_p1_ln (ats_freq ats)
> plot_p1_ln (ats_ampl ats)
> plot_p1_ln (ats_phase ats)
> plot_p3_ln (map (map (\(t,f,a) -> (f,t,a))) (ats_tm_fr_am ats))
-}
ats_read :: FilePath -> IO Ats
ats_read fn = do
  d <- ats_read_f64 fn
  let hdr = ats_parse_header d
  return (Ats hdr (Split.chunksOf (ats_frame_length hdr) (drop 10 d)))

-- | Calculate partial depth and frame constant from filetype.
--
-- t = time, a = ampl, f = freq, p = phase, r = residual/25-band,
-- 1 = (t,a,f), 2 = (t,a,f,p), 3 = (t,a,f,r), 4 = (t,a,f,p,r)
ats_ftype_n :: Ats_File_Type -> ([Ats_Param],Int,Int)
ats_ftype_n n =
    case n of
      1 -> ([Ats_Time,Ats_Amplitude,Ats_Frequency],2,1)
      2 -> ([Ats_Time,Ats_Amplitude,Ats_Frequency,Ats_Phase],3,1)
      3 -> ([Ats_Time,Ats_Amplitude,Ats_Frequency,Ats_Residual],2,26)
      4 -> ([Ats_Time,Ats_Amplitude,Ats_Frequency,Ats_Phase,Ats_Residual],3,26)
      _ -> error "ats_ftype_n"

-- | Calculate indices for 'Ats_Param' given 'Ats_Header'.
ats_param_ix :: Ats_Param -> Ats_Header -> Maybe [Int]
ats_param_ix p hdr =
    let ft = ats_file_type hdr
        np = ats_n_partials hdr
        (_,pw,_) = ats_ftype_n ft
    in case p of
         Ats_Time -> Just [0]
         Ats_Amplitude -> Just [1, 1 + pw .. np * pw]
         Ats_Frequency -> Just [2, 2 + pw .. np * pw + 1]
         Ats_Phase -> if ft == 2 || ft == 4 then Just [3, 3 + pw .. np * pw + 2] else Nothing
         Ats_Residual -> if ft == 3 || ft == 4
                         then let x = np * (pw + 1) in Just [x .. x + 25]
                         else Nothing

-- | Get channels associated with 'Ats_Param' at 'Ats'.
ats_param_ch :: Ats_Param -> Ats -> (Int,[[Double]])
ats_param_ch sel (Ats hdr frm) =
    case ats_param_ix sel hdr of
      Nothing -> error "ats_param_ch"
      Just ix -> let ch = transpose frm in (length ix,map (ch !!) ix)

ats_time :: Ats -> [Double]
ats_time (Ats _ frm) = let ch = transpose frm in ch !! 0

ats_freq :: Ats -> [[Double]]
ats_freq = snd . ats_param_ch Ats_Frequency

ats_ampl :: Ats -> [[Double]]
ats_ampl = snd . ats_param_ch Ats_Amplitude

ats_phase :: Ats -> [[Double]]
ats_phase = snd . ats_param_ch Ats_Phase

ats_tm_fr_am :: Ats -> [[(Double,Double,Double)]]
ats_tm_fr_am ats =
    let tm = ats_time ats
        fr = ats_freq ats
        am = ats_ampl ats
    in zipWith (zip3 tm) fr am

-- * Sc3

-- | Write data given by 'ats_read_f64' as 32-bit floating point NeXT/Au file.
ats_write_au :: FilePath -> FilePath -> IO ()
ats_write_au ats_fn au_fn = do
  d <- ats_read_f64 ats_fn
  let ats_hdr = ats_parse_header d
      sr = round (ats_sample_rate ats_hdr)
      au_hdr = Au.Sf_Header (length d) Au.Float sr 1
  Au.au_write au_fn au_hdr [d]

{- | Run 'ats_write_au' and then 'Sc3.b_allocRead.

> ats_load_sc3 0 "/home/rohan/sw/hsc3-data/data/ats/pf-c5.4.ats"
> Sc3.withSc3 (Sc3.b_query1_unpack 0)
-}
ats_load_sc3 :: Int -> FilePath -> IO ()
ats_load_sc3 b ats_fn = do
  let au_fn = ats_fn ++ ".au"
  ats_write_au ats_fn au_fn
  _ <- Sc3.withSc3 (Sc3.async (Sc3.b_allocRead b au_fn 0 0))
  return ()
