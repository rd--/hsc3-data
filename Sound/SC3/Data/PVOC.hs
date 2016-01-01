module Sound.SC3.Data.PVOC where

import Control.Monad {- base -}
import qualified Data.Binary as B {- binary -}
import qualified Data.ByteString.Lazy as L {- bytestring -}
import Data.Int {- base -}
import Data.List {- base -}
import qualified Data.Vector.Unboxed as V {- vector -}
import Data.Word {- base -}
import System.IO {- base -}

import qualified Sound.OSC.Coding.Byte as O

decode_u16_le :: L.ByteString -> Word16
decode_u16_le = B.decode . L.reverse

decode_u32_le :: L.ByteString -> Word32
decode_u32_le = B.decode . L.reverse

read_u32_be :: Handle -> IO Word32
read_u32_be h = fmap B.decode (L.hGet h 4)

read_u32_le :: Handle -> IO Word32
read_u32_le h = fmap decode_u32_le (L.hGet h 4)

decode_f32_le :: L.ByteString -> Float
decode_f32_le = O.decode_f32 . L.reverse

u32_to_bytes :: Word32 -> [Word8]
u32_to_bytes = L.unpack . B.encode

u8_to_char :: Word8 -> Char
u8_to_char = toEnum . fromIntegral

char_to_u8 :: Char -> Word8
char_to_u8 = fromIntegral . fromEnum

-- > u32_to_ascii 0x2e736e64 == ".snd"
-- > u32_to_ascii 0x52494646 == "RIFF"
u32_to_ascii :: Word32 -> String
u32_to_ascii = map u8_to_char . u32_to_bytes

-- > ascii_to_u32 ".snd" == 0x2e736e64
-- > ascii_to_u32 "RIFF" == 0x52494646
-- > ascii_to_u32 "fmt " == 0x666d7420
ascii_to_u32 :: String -> Word32
ascii_to_u32 = B.decode . L.pack . map char_to_u8

pvx_guid_text :: [Char]
pvx_guid_text = "8312B9C2-2E6E-11d4-A824-DE5B96C3AB21"

pvx_guid_u8 :: [Word8]
pvx_guid_u8 = [194,185,18,131,110,46,212,17,168,36,222,91,150,195,171,33]

read_u32_ascii :: Handle -> IO String
read_u32_ascii = fmap u32_to_ascii . read_u32_be

riff_read_chunk_hdr :: Handle -> IO (String, Int)
riff_read_chunk_hdr h = do
  ty <- read_u32_ascii h
  sz <- fmap fromIntegral (read_u32_le h)
  print (ty,sz)
  return (ty,sz)

type CHUNK = (String,Int,L.ByteString)

riff_read_chunk :: Handle -> IO CHUNK
riff_read_chunk h = do
  (ty,sz) <- riff_read_chunk_hdr h
  dat <- L.hGet h sz
  return (ty,sz,dat)

riff_read_chunk_seq :: Handle -> IO [CHUNK]
riff_read_chunk_seq h = do
  e <- hIsEOF h
  case e of
    True -> return []
    False -> riff_read_chunk h >>= \c -> fmap (c :) (riff_read_chunk_seq h)

wave_read :: Handle -> IO [CHUNK]
wave_read h = do
  (ty,sz) <- riff_read_chunk_hdr h
  when (ty /= "RIFF") (error "riff_read: not RIFF")
  ty' <- read_u32_ascii h
  when (ty' /= "WAVE") (error "riff_read: not WAVE")
  print (ty,sz,ty')
  riff_read_chunk_seq h

section :: Int64 -> Int64 -> L.ByteString -> L.ByteString
section n m = L.take m . L.drop n

section_int :: Int -> Int -> L.ByteString -> L.ByteString
section_int n m = L.take (fromIntegral m) . L.drop (fromIntegral n)

data WAVE_FMT_PCM_16 =
    WAVE_FMT_PCM_16
    {audioFormat :: Word16
    ,numChannels :: Word16
    ,sampleRate :: Word32
    ,byteRate :: Word32
    ,blockAlign :: Word16
    ,bitsPerSample :: Word16}
    deriving (Show)

fmt_pcm_16_parse_dat :: L.ByteString -> WAVE_FMT_PCM_16
fmt_pcm_16_parse_dat dat =
    WAVE_FMT_PCM_16
    (decode_u16_le (section 0 2 dat))
    (decode_u16_le (section 2 2 dat))
    (decode_u32_le (section 4 4 dat))
    (decode_u32_le (section 8 4 dat))
    (decode_u16_le (section 12 2 dat))
    (decode_u16_le (section 14 2 dat))

fmt_pcm_16_parse :: CHUNK -> WAVE_FMT_PCM_16
fmt_pcm_16_parse (ty,sz,dat) =
    case (ty,sz) of
      ("fmt ",16) -> fmt_pcm_16_parse_dat dat
      _ -> error "fmt_pcm_16_parse"

decode_guid :: L.ByteString -> [Word8]
decode_guid = L.unpack

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

data WAVE_FORMAT =
    WAVE_FORMAT_0 | WAVE_FORMAT_PCM | WAVE_FORMAT_2 | WAVE_FORMAT_IEEE_FLOAT
    deriving (Eq,Enum,Show)

data WAVE_FMT_PVOC_80 =
    WAVE_FMT_PVOC_80
    {cbSize :: Word16
    ,wValidBitsPerSample :: Word16
    ,dwChannelMask :: Word32
    ,subFormat :: [Word8]
    ,dwVersion :: Word32
    ,dwDataSize :: Word32
    ,wWordFormat :: PVOC_WORDFORMAT
    ,wAnalFormat :: PVOC_FRAMETYPE
    ,wSourceFormat :: WAVE_FORMAT
    ,wWindowType :: PVOC_WINDOWTYPE
    ,nAnalysisBins :: Word32
    ,dwWinlen :: Word32
    ,dwOverlap :: Word32
    ,dwFrameAlign :: Word32
    ,fAnalysisRate :: Float
    ,fWindowParam :: Float}
    deriving (Show)

generic_to_enum :: (Integral i,Enum e) => i -> e
generic_to_enum = toEnum . fromIntegral

fmt_pvoc_80_parse_dat :: L.ByteString -> WAVE_FMT_PVOC_80
fmt_pvoc_80_parse_dat dat =
    WAVE_FMT_PVOC_80
    (decode_u16_le (section 16 2 dat))
    (decode_u16_le (section 18 2 dat))
    (decode_u32_le (section 20 4 dat))
    (L.unpack (section 24 16 dat))
    (decode_u32_le (section 40 4 dat))
    (decode_u32_le (section 44 4 dat))
    (generic_to_enum (decode_u16_le (section 48 2 dat)))
    (generic_to_enum (decode_u16_le (section 50 2 dat)))
    (generic_to_enum (decode_u16_le (section 52 2 dat)))
    (generic_to_enum (decode_u16_le (section 54 2 dat)))
    (decode_u32_le (section 56 4 dat))
    (decode_u32_le (section 60 4 dat))
    (decode_u32_le (section 64 4 dat))
    (decode_u32_le (section 68 4 dat))
    (decode_f32_le (section 72 4 dat))
    (decode_f32_le (section 76 4 dat))

fmt_pvoc_80_check_guid :: WAVE_FMT_PVOC_80 -> Bool
fmt_pvoc_80_check_guid f = subFormat f /= pvx_guid_u8

type PVOC_HDR = (WAVE_FMT_PCM_16,WAVE_FMT_PVOC_80)

fmt_pvoc_80_parse :: CHUNK -> PVOC_HDR
fmt_pvoc_80_parse (ty,sz,dat) =
    case (ty,sz) of
      ("fmt ",80) -> (fmt_pcm_16_parse_dat dat,fmt_pvoc_80_parse_dat dat)
      _ -> error "fmt_pcm_16_parse"

wave_load :: FilePath -> IO [CHUNK]
wave_load fn = withFile fn ReadMode wave_read

type PVOC_RAW = (PVOC_HDR,CHUNK)

pvoc_nc :: PVOC_HDR -> Int
pvoc_nc = fromIntegral . numChannels . fst

pvoc_nb :: PVOC_HDR -> Int
pvoc_nb = fromIntegral . nAnalysisBins . snd

-- | Data is stored in frame order, channel interleaved.
pvoc_data_parse_f32 :: PVOC_RAW -> (Int,Int,V.Vector Float)
pvoc_data_parse_f32 ((wv,pv),(ty,sz,dat)) =
    let n = sz `div` 4
        nc = fromIntegral (numChannels wv)
        nb = fromIntegral (nAnalysisBins pv)
        nf = n `div` (nc * nb * 2)
    in if ty /= "data" || wWordFormat pv /= PVOC_IEEE_FLOAT || dwDataSize pv /= 32
       then error "pvoc_data_parse_f32"
       else (n,nf,V.generate n (\i -> decode_f32_le (section_int (i * 4) 4 dat)))

find_chunk :: String -> [CHUNK] -> Maybe CHUNK
find_chunk nm = find (\(ty,_,_) -> ty == nm)

type PVOC_VEC_F32 = (PVOC_HDR,(Int,Int,V.Vector Float))

pvoc_load_vec_f32 :: FilePath -> IO PVOC_VEC_F32
pvoc_load_vec_f32 fn = do
  c <- wave_load fn
  case (find_chunk "fmt " c,find_chunk "data" c) of
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

let fn = "/home/rohan/data/audio/pf-c5.wav"
c <- wave_load fn
length c == 2
fmt_pcm_16_parse (c !! 0)

let fn = "/home/rohan/data/audio/pf-c5.pvx"
pv <- pvoc_load_vec_f32 fn
let (hdr,(n,nf,dat)) = pv
n == 2020 * 513 * 2
V.length dat == n

import Sound.SC3.Plot

let f b = let p = V.toList (pvoc_vec_f32_bin pv (0,b))
          in map (\((a,f),i) -> (f,fromIntegral i,a)) (zip p [0..])
plot_p3_ln (map f [12 .. 36])

-}
