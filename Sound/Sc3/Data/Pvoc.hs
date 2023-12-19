-- | <http://www.csounds.com/manual/html/pvanal.html>
module Sound.Sc3.Data.Pvoc where

import Data.Word {- base -}

import qualified Data.ByteString.Lazy as ByteString {- bytestring -}
import qualified Data.Vector.Unboxed as Vector {- vector -}

import qualified Sound.Osc.Coding.Byte as Byte {- hosc -}
import qualified Sound.Osc.Coding.Convert as Convert {- hosc -}

import qualified Sound.File.Riff as Riff {- hsc3-sf -}
import qualified Sound.File.Wave as Wave {- hsc3-sf -}

-- | Pvoc-Ex files

-- | Pvx Guid Text
pvx_guid_text :: String
pvx_guid_text = "8312B9C2-2E6E-11D4-A824-DE5B96C3AB21"

{- | Pvx Guid U8

>>> import Sound.File.Riff
>>> map (show_hex_uc 2) pvx_guid_u8
["C2","B9","12","83","6E","2E","D4","11","A8","24","DE","5B","96","C3","AB","21"]
-}
pvx_guid_u8 :: [Word8]
pvx_guid_u8 = [194, 185, 18, 131, 110, 46, 212, 17, 168, 36, 222, 91, 150, 195, 171, 33]

data Pvoc_WordFormat
  = Pvoc_Ieee_Float
  | Pvoc_Ieee_Double
  deriving (Eq, Enum, Show)

data Pvoc_FrameType
  = Pvoc_Amp_Freq
  | Pvoc_Amp_Phase
  | Pvoc_Complex
  deriving (Eq, Enum, Show)

data Pvoc_WindowType
  = Pvoc_Hamming
  | Pvoc_Unused
  | Pvoc_Hann
  | Pvoc_Kaiser
  | Pvoc_Rect
  | Pvoc_Custom
  deriving (Eq, Enum, Show)

data Pvoc_SampleType
  = Stype_16
  | Stype_24
  | Stype_32
  | Stype_Ieee_Float
  deriving (Eq, Enum, Show)

{- | Wave Format Pvoc 80

<https://docs.microsoft.com/en-us/windows/desktop/api/mmreg/ns-mmreg-twaveformatex>
<https://docs.microsoft.com/en-us/windows/desktop/api/mmreg/ns-mmreg-waveformatextensible>
-}
data Wave_Fmt_Pvoc_80 = Wave_Fmt_Pvoc_80
  { cbSize :: Word16
  , wValidBitsPerSample :: Word16
  , dwChannelMask :: Word32
  , subFormat :: String
  , dwVersion :: Word32
  , dwDataSize :: Word32
  , wWordFormat :: Pvoc_WordFormat
  , wAnalFormat :: Pvoc_FrameType
  , wSourceFormat :: Wave.Wave_Format
  , wWindowType :: Pvoc_WindowType
  , nAnalysisBins :: Word32
  , dwWinlen :: Word32
  , dwOverlap :: Word32
  , dwFrameAlign :: Word32
  , fAnalysisRate :: Float
  , fWindowParam :: Float
  }
  deriving (Show)

plain_record_pp :: String -> String
plain_record_pp =
  let f c = if c `elem` ",}" then ['\n', c] else [c]
      g s = if s == "{" then "\n{ " else s
  in concatMap (g . f)

fmt_pvoc_80_pp :: Wave_Fmt_Pvoc_80 -> String
fmt_pvoc_80_pp = plain_record_pp . show

fmt_pvoc_80_parse_dat :: ByteString.ByteString -> Wave_Fmt_Pvoc_80
fmt_pvoc_80_parse_dat dat =
  Wave_Fmt_Pvoc_80
    (Byte.decode_word16_le (Riff.section_int64 16 2 dat))
    (Byte.decode_word16_le (Riff.section_int64 18 2 dat))
    (Byte.decode_word32_le (Riff.section_int64 20 4 dat))
    (Riff.decode_guid (Riff.section_int64 24 16 dat))
    (Byte.decode_word32_le (Riff.section_int64 40 4 dat))
    (Byte.decode_word32_le (Riff.section_int64 44 4 dat))
    (Convert.word16_to_enum (Byte.decode_word16_le (Riff.section_int64 48 2 dat)))
    (Convert.word16_to_enum (Byte.decode_word16_le (Riff.section_int64 50 2 dat)))
    (Convert.word16_to_enum (Byte.decode_word16_le (Riff.section_int64 52 2 dat)))
    (Convert.word16_to_enum (Byte.decode_word16_le (Riff.section_int64 54 2 dat)))
    (Byte.decode_word32_le (Riff.section_int64 56 4 dat))
    (Byte.decode_word32_le (Riff.section_int64 60 4 dat))
    (Byte.decode_word32_le (Riff.section_int64 64 4 dat))
    (Byte.decode_word32_le (Riff.section_int64 68 4 dat))
    (Byte.decode_f32_le (Riff.section_int64 72 4 dat))
    (Byte.decode_f32_le (Riff.section_int64 76 4 dat))

fmt_pvoc_80_check_guid :: Wave_Fmt_Pvoc_80 -> Bool
fmt_pvoc_80_check_guid f = subFormat f /= pvx_guid_text

type Pvoc_Hdr = (Wave.Wave_Fmt_16, Wave_Fmt_Pvoc_80)

fmt_pvoc_80_parse :: Riff.Chunk -> Pvoc_Hdr
fmt_pvoc_80_parse ch =
  let ((ty, sz), dat) = ch
  in case (ty, sz) of
      ("fmt ", 80) -> (Wave.wave_fmt_16_parse ch, fmt_pvoc_80_parse_dat dat)
      _ -> error "fmt_pvoc_80_parse"

type Pvoc_Raw = (Pvoc_Hdr, Riff.Chunk)

pvoc_nc :: Pvoc_Hdr -> Int
pvoc_nc = Convert.word16_to_int . Wave.numChannels . fst

pvoc_nb :: Pvoc_Hdr -> Int
pvoc_nb = Convert.word32_to_int . nAnalysisBins . snd

-- | Data is stored in frame order, channel interleaved.
pvoc_data_parse_f32 :: Pvoc_Raw -> (Int, Int, Vector.Vector Float)
pvoc_data_parse_f32 ((wv, pv), ((ty, sz), dat)) =
  let n = Convert.word32_to_int sz `div` 4
      nc = Convert.word16_to_int (Wave.numChannels wv)
      nb = Convert.word32_to_int (nAnalysisBins pv)
      nf = n `div` (nc * nb * 2)
  in if ty /= "data" || wWordFormat pv /= Pvoc_Ieee_Float || dwDataSize pv /= 32
      then error "pvoc_data_parse_f32"
      else (n, nf, Vector.generate n (\i -> Byte.decode_f32_le (Riff.section_int (i * 4) 4 dat)))

type Pvoc_Vec_F32 = (Pvoc_Hdr, (Int, Int, Vector.Vector Float))

pvoc_load_vec_f32 :: FilePath -> IO Pvoc_Vec_F32
pvoc_load_vec_f32 fn = do
  c <- Wave.wave_load_ch fn
  case (Riff.find_chunk "fmt " c, Riff.find_chunk "data" c) of
    (Just fmt, Just dat) ->
      let hdr = fmt_pvoc_80_parse fmt
          vec = pvoc_data_parse_f32 (hdr, dat)
      in return (hdr, vec)
    _ -> error "pvoc_load"

pvoc_vec_f32_bin :: Pvoc_Vec_F32 -> (Int, Int) -> Vector.Vector (Float, Float)
pvoc_vec_f32_bin (hdr, (_, nf, vec)) (ch, bin) =
  let nc = pvoc_nc hdr
      nb = pvoc_nb hdr
      f i =
        let j = i * nb * 2 * nc + ch * nb * 2 + bin * 2
        in (vec Vector.! j, vec Vector.! (j + 1))
  in Vector.generate nf f

{-

let fn = "/home/rohan/uc/invisible/clarity/pvx/z.01.pvx"

pv <- pvoc_load_vec_f32 fn
((hdr1,hdr2),(n,nf,v)) = pv
putStrLn (fmt_pvoc_80_pp hdr2)

fmt ((a,f),i) = (f,fromIntegral i,a)
gen b = let p = Vector.toList (pvoc_vec_f32_bin pv (0,b)) in map fmt (zip p [0..])

import Sound.Sc3.Plot {- hsc3-plot -}
plot_p3_ln (map gen [12 .. 24])

-}

-- * PV files

{- | <https://www.ee.columbia.edu/~dpwe/resources/pvanal.html>.
All fields are stored as 32bit values.
-}
data Pv_Header = Pv_Header
  { pv_magic :: Int -- 517730
  , pv_headerSize :: Int -- 56 BYTES
  , pv_fileSize :: Int
  , pv_fileFormat :: Int -- 36 (Float)
  , pv_sr :: Double
  , pv_numChannels :: Int -- 1
  , pv_windowSize :: Int
  , pv_hopSize :: Int
  , pv_frameBSize :: Int
  , pv_pvpvoc :: Int -- 7 (POLAR, PHI-DOT)
  , pv_minFreq :: Double -- 0.0
  , pv_maxFreq :: Double
  , pv_freqFormat :: Int -- 1 (LINEAR)
  , pv_Info :: [Word8] -- 4 BYTES
  }

-- | Endianess is determined by reading the magic number.
pv_magic_n :: Int
pv_magic_n = 517730

pv_nFrames :: Pv_Header -> Int
pv_nFrames h = pv_fileSize h `div` pv_frameBSize h

pv_nBins :: Pv_Header -> Int
pv_nBins h = pv_frameBSize h `div` 8

pv_sndDur :: Pv_Header -> Double
pv_sndDur h = fromIntegral (pv_nFrames h * pv_hopSize h) / pv_sr h
