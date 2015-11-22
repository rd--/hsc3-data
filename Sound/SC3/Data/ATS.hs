-- | Reader for ATS analyis data files.
module Sound.SC3.Data.ATS where

import qualified Data.ByteString.Lazy as B {- bytestring -}
import Data.Int {- base -}
import Data.List {- base -}
import Data.List.Split {- split -}

import qualified Sound.OSC.Coding.Byte as O {- hosc -}

-- | ATS analysis frame data.
type ATS_Frame = [Double]

-- | ATS file type.
type ATS_File_Type = Int

-- | ATS analysis meta-data.
data ATS_Header = ATS_Header
    {ats_sample_rate :: Double
    ,ats_frame_size :: Int
    ,ats_window_size :: Int
    ,ats_n_partials :: Int
    ,ats_n_frames :: Int
    ,ats_max_amplitude :: Double
    ,ats_max_frequency :: Double
    ,ats_analysis_duration :: Double
    ,ats_file_type :: Int
    ,ats_frame_length :: ATS_File_Type} deriving (Eq, Show)

-- | ATS analysis data.
data ATS = ATS {ats_header :: ATS_Header
               ,ats_frames :: [ATS_Frame]}
           deriving (Eq, Show)

-- | Very simple minded generic PP for records.
record_fields_pp :: Show r => r -> [(String,String)]
record_fields_pp =
    map ((\[p,q] -> (dropWhile (== ' ') p,q)) . splitOn " = ") .
    splitOn "," .
    takeWhile (/= '}') .
    tail .
    dropWhile (/= '{') .
    show

ats_header_pp :: ATS_Header -> [(String,String)]
ats_header_pp = record_fields_pp

bs_sep :: Int64 -> Int64 -> B.ByteString -> [B.ByteString]
bs_sep n i d = if i == 1 then [d] else let (p,q) = B.splitAt n d in p : bs_sep n (i - 1) q

ats_read_f64 :: FilePath -> IO [Double]
ats_read_f64 fn = do
  d <- B.readFile fn
  let n = B.length d `div` 8
      f = ats_get_decoder (B.take 8 d)
  return (map f (bs_sep 8 n d))

-- | Parse ATS header.
ats_parse_header :: [Double] -> ATS_Header
ats_parse_header d =
    let f_at j = d !! j
        i_at = floor . f_at
        ft = i_at 9
        (_,n,x) = ats_ftype_n ft
        np = i_at 4
        nf = i_at 5
        fl = np * n + x
    in ATS_Header {ats_sample_rate = f_at 1
                  ,ats_frame_size = i_at 2
                  ,ats_window_size = i_at 3
                  ,ats_n_partials = np
                  ,ats_n_frames = nf
                  ,ats_max_amplitude = f_at 6
                  ,ats_max_frequency = f_at 7
                  ,ats_analysis_duration = f_at 8
                  ,ats_file_type = ft
                  ,ats_frame_length = fl}

-- | Read an ATS data file.
--
-- > ats <- ats_read "/home/rohan/data/audio/pf-c5.1.ats"
-- > let ATS hdr frm = ats
-- > ats_header_pp hdr
-- > map length frm == replicate (ats_n_frames hdr) (ats_frame_length hdr)
--
-- > import Sound.SC3.Plot {- hcs3-plot -}
-- > plotTable [ats_time ats]
-- > plotTable (ats_freq ats)
-- > plotTable (ats_ampl ats)
-- > plotTable (ats_phase ats)
ats_read :: FilePath -> IO ATS
ats_read fn = do
  d <- ats_read_f64 fn
  let hdr = ats_parse_header d
  return (ATS hdr (chunksOf (ats_frame_length hdr) (drop 10 d)))

-- | The first eight bytes of the file determine endianess, and hence the decoder.
ats_get_decoder :: B.ByteString -> B.ByteString -> Double
ats_get_decoder v =
    let f_be = O.decode_f64
        f_le = f_be . B.reverse
        err = error "ats_get_decoder: not ATS file?"
    in if f_be v == 123.0 then f_be else if f_le v == 123.0 then f_le else err

data ATS_Param = ATS_Time | ATS_Amplitude | ATS_Frequency | ATS_Phase | ATS_Residual

-- | Calculate partial depth and frame constant from filetype.
--
-- t = time, a = ampl, f = freq, p = phase, r = residual/25-band,
-- 1 = (t,a,f), 2 = (t,a,f,p), 3 = (t,a,f,r), 4 = (t,a,f,p,r)
ats_ftype_n :: ATS_File_Type -> ([ATS_Param],Int,Int)
ats_ftype_n n =
    case n of
      1 -> ([ATS_Time,ATS_Amplitude,ATS_Frequency],2,1)
      2 -> ([ATS_Time,ATS_Amplitude,ATS_Frequency,ATS_Phase],3,1)
      3 -> ([ATS_Time,ATS_Amplitude,ATS_Frequency,ATS_Residual],2,26)
      4 -> ([ATS_Time,ATS_Amplitude,ATS_Frequency,ATS_Phase,ATS_Residual],3,26)
      _ -> error "ats_ftype_n"

ats_param_ix :: ATS_Param -> ATS_Header -> Maybe [Int]
ats_param_ix p hdr =
    let ft = ats_file_type hdr
        np = ats_n_partials hdr
        (_,pw,_) = ats_ftype_n ft
    in case p of
         ATS_Time -> Just [0]
         ATS_Amplitude -> Just [1, 1 + pw .. np * pw]
         ATS_Frequency -> Just [2, 2 + pw .. np * pw + 1]
         ATS_Phase -> if ft == 2 || ft == 4 then Just [3, 3 + pw .. np * pw + 2] else Nothing
         ATS_Residual -> if ft == 3 || ft == 4
                         then let x = np * (pw + 1) in Just [x .. x + 25]
                         else Nothing

ats_param_ch :: ATS_Param -> ATS -> [[Double]]
ats_param_ch sel (ATS hdr frm) =
    case ats_param_ix sel hdr of
      Nothing -> error "ats_param_ch"
      Just ix -> let ch = transpose frm in map (ch !!) ix

ats_time :: ATS -> [Double]
ats_time (ATS _ frm) = let ch = transpose frm in ch !! 0

ats_freq :: ATS -> [[Double]]
ats_freq = ats_param_ch ATS_Frequency

ats_ampl :: ATS -> [[Double]]
ats_ampl = ats_param_ch ATS_Amplitude

ats_phase :: ATS -> [[Double]]
ats_phase = ats_param_ch ATS_Phase
