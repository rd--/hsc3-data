-- | <http://www.csounds.com/manual/html/pvanal.html>
module Sound.SC3.Data.PVOC where

import Data.Word {- base -}

import qualified Data.ByteString.Lazy as L {- bytestring -}
import qualified Data.Vector.Unboxed as V {- vector -}

import qualified Sound.OSC.Coding.Byte as O {- hosc -}
import qualified Sound.OSC.Coding.Convert as O {- hosc -}

import qualified Sound.File.RIFF as RIFF {- hsc3-sf -}
import qualified Sound.File.WAVE as WAVE {- hsc3-sf -}

-- | PVOC-EX files

pvx_guid_text :: String
pvx_guid_text = "8312B9C2-2E6E-11D4-A824-DE5B96C3AB21"

-- > map (show_hex_uc 2) pvx_guid_u8
pvx_guid_u8 :: [Word8]
pvx_guid_u8 = [194,185,18,131,110,46,212,17,168,36,222,91,150,195,171,33]

data PVOC_WORDFORMAT =
    PVOC_IEEE_FLOAT | PVOC_IEEE_DOUBLE
    deriving (Eq,Enum,Show)

data PVOC_FRAMETYPE =
    PVOC_AMP_FREQ | PVOC_AMP_PHASE | PVOC_COMPLEX
    deriving (Eq,Enum,Show)

data PVOC_WINDOWTYPE =
    PVOC_HAMMING | PVOC_UNUSED | PVOC_HANN | PVOC_KAISER | PVOC_RECT | PVOC_CUSTOM
    deriving (Eq,Enum,Show)

data PVOC_SAMPLETYPE =
    STYPE_16 | STYPE_24 | STYPE_32 | STYPE_IEEE_FLOAT
    deriving (Eq,Enum,Show)

{- | WAVE FORMAT PVOC 80

<https://docs.microsoft.com/en-us/windows/desktop/api/mmreg/ns-mmreg-twaveformatex>
<https://docs.microsoft.com/en-us/windows/desktop/api/mmreg/ns-mmreg-waveformatextensible>

-}
data WAVE_FMT_PVOC_80 =
    WAVE_FMT_PVOC_80
    {cbSize :: Word16
    ,wValidBitsPerSample :: Word16
    ,dwChannelMask :: Word32
    ,subFormat :: String
    ,dwVersion :: Word32
    ,dwDataSize :: Word32
    ,wWordFormat :: PVOC_WORDFORMAT
    ,wAnalFormat :: PVOC_FRAMETYPE
    ,wSourceFormat :: WAVE.WAVE_FORMAT
    ,wWindowType :: PVOC_WINDOWTYPE
    ,nAnalysisBins :: Word32
    ,dwWinlen :: Word32
    ,dwOverlap :: Word32
    ,dwFrameAlign :: Word32
    ,fAnalysisRate :: Float
    ,fWindowParam :: Float}
    deriving (Show)

plain_record_pp :: String -> String
plain_record_pp =
    let f c = if c `elem` ",}" then ['\n',c] else [c]
        g s = if s == "{" then "\n{ " else s
    in concatMap (g . f)

fmt_pvoc_80_pp :: WAVE_FMT_PVOC_80 -> String
fmt_pvoc_80_pp = plain_record_pp . show

fmt_pvoc_80_parse_dat :: L.ByteString -> WAVE_FMT_PVOC_80
fmt_pvoc_80_parse_dat dat =
    WAVE_FMT_PVOC_80
    (O.decode_word16_le (RIFF.section 16 2 dat))
    (O.decode_word16_le (RIFF.section 18 2 dat))
    (O.decode_word32_le (RIFF.section 20 4 dat))
    (RIFF.decode_guid (RIFF.section 24 16 dat))
    (O.decode_word32_le (RIFF.section 40 4 dat))
    (O.decode_word32_le (RIFF.section 44 4 dat))
    (O.word16_to_enum (O.decode_word16_le (RIFF.section 48 2 dat)))
    (O.word16_to_enum (O.decode_word16_le (RIFF.section 50 2 dat)))
    (O.word16_to_enum (O.decode_word16_le (RIFF.section 52 2 dat)))
    (O.word16_to_enum (O.decode_word16_le (RIFF.section 54 2 dat)))
    (O.decode_word32_le (RIFF.section 56 4 dat))
    (O.decode_word32_le (RIFF.section 60 4 dat))
    (O.decode_word32_le (RIFF.section 64 4 dat))
    (O.decode_word32_le (RIFF.section 68 4 dat))
    (O.decode_f32_le (RIFF.section 72 4 dat))
    (O.decode_f32_le (RIFF.section 76 4 dat))

fmt_pvoc_80_check_guid :: WAVE_FMT_PVOC_80 -> Bool
fmt_pvoc_80_check_guid f = subFormat f /= pvx_guid_text

type PVOC_HDR = (WAVE.WAVE_FMT_16,WAVE_FMT_PVOC_80)

fmt_pvoc_80_parse :: RIFF.CHUNK -> PVOC_HDR
fmt_pvoc_80_parse ch =
  let ((ty,sz),dat) = ch
  in case (ty,sz) of
       ("fmt ",80) -> (WAVE.wave_fmt_16_parse ch,fmt_pvoc_80_parse_dat dat)
       _ -> error "fmt_pvoc_80_parse"

type PVOC_RAW = (PVOC_HDR,RIFF.CHUNK)

pvoc_nc :: PVOC_HDR -> Int
pvoc_nc = O.word16_to_int . WAVE.numChannels . fst

pvoc_nb :: PVOC_HDR -> Int
pvoc_nb = O.word32_to_int . nAnalysisBins . snd

-- | Data is stored in frame order, channel interleaved.
pvoc_data_parse_f32 :: PVOC_RAW -> (Int,Int,V.Vector Float)
pvoc_data_parse_f32 ((wv,pv),((ty,sz),dat)) =
    let n = O.word32_to_int sz `div` 4
        nc = O.word16_to_int (WAVE.numChannels wv)
        nb = O.word32_to_int (nAnalysisBins pv)
        nf = n `div` (nc * nb * 2)
    in if ty /= "data" || wWordFormat pv /= PVOC_IEEE_FLOAT || dwDataSize pv /= 32
       then error "pvoc_data_parse_f32"
       else (n,nf,V.generate n (\i -> O.decode_f32_le (RIFF.section_int (i * 4) 4 dat)))

type PVOC_VEC_F32 = (PVOC_HDR,(Int,Int,V.Vector Float))

pvoc_load_vec_f32 :: FilePath -> IO PVOC_VEC_F32
pvoc_load_vec_f32 fn = do
  c <- WAVE.wave_load_ch fn
  case (RIFF.find_chunk "fmt " c,RIFF.find_chunk "data" c) of
    (Just fmt,Just dat) -> let hdr = fmt_pvoc_80_parse fmt
                               vec = pvoc_data_parse_f32 (hdr,dat)
                           in return (hdr,vec)
    _ -> error "pvoc_load"

pvoc_vec_f32_bin :: PVOC_VEC_F32 -> (Int,Int) -> V.Vector (Float,Float)
pvoc_vec_f32_bin (hdr,(_,nf,vec)) (ch,bin) =
    let nc = pvoc_nc hdr
        nb = pvoc_nb hdr
        f i = let j = i * nb * 2 * nc + ch * nb * 2 + bin * 2
              in (vec V.! j,vec V.! (j + 1))
    in V.generate nf f

{-

let fn = "/home/rohan/uc/invisible/clarity/pvx/z.01.pvx"

pv <- pvoc_load_vec_f32 fn
((hdr1,hdr2),(n,nf,v)) = pv
putStrLn (fmt_pvoc_80_pp hdr2)

fmt ((a,f),i) = (f,fromIntegral i,a)
gen b = let p = V.toList (pvoc_vec_f32_bin pv (0,b)) in map fmt (zip p [0..])

import Sound.SC3.Plot {- hsc3-plot -}
plot_p3_ln (map gen [12 .. 24])

-}

-- * PV files

-- | <https://www.ee.columbia.edu/~dpwe/resources/pvanal.html>.
-- All fields are stored as 32bit values.
data PV_Header =
    PV_Header {pv_magic :: Int -- 517730
              ,pv_headerSize :: Int -- 56 BYTES
              ,pv_fileSize :: Int
              ,pv_fileFormat :: Int -- 36 (FLOAT)
              ,pv_sr :: Double
              ,pv_numChannels :: Int -- 1
              ,pv_windowSize :: Int
              ,pv_hopSize :: Int
              ,pv_frameBSize :: Int
              ,pv_pvpvoc :: Int -- 7 (POLAR, PHI-DOT)
              ,pv_minFreq :: Double -- 0.0
              ,pv_maxFreq :: Double
              ,pv_freqFormat :: Int -- 1 (LINEAR)
              ,pv_Info :: [Word8] -- 4 BYTES
              }

-- | Endianess is determined by reading the magic number.
pv_magic_n :: Int
pv_magic_n = 517730

pv_nFrames :: PV_Header -> Int
pv_nFrames h = pv_fileSize h `div` pv_frameBSize h

pv_nBins :: PV_Header -> Int
pv_nBins h = pv_frameBSize h `div` 8

pv_sndDur :: PV_Header -> Double
pv_sndDur h = fromIntegral (pv_nFrames h * pv_hopSize h) / pv_sr h
