{- | Reader for Lpc analysis data files.

<http://www.csounds.com/manual/html/lpanal.html>
-}
module Sound.Sc3.Data.Lpc where

import qualified Control.Monad {- base -}
import qualified Data.List {- base -}
import qualified Data.Maybe {- base -}
import qualified System.IO {- base -}
import qualified Text.Printf {- base -}

import qualified Data.ByteString.Lazy as ByteString {- bytestring -}
import qualified Data.List.Split {- split -}

import qualified Data.Numbers.FloatingHex {- FloatingHex -}

import qualified Sound.Osc.Coding.Byte as Osc {- hosc -}

-- * Types

-- | Lpc analysis meta-data.
data LpcHeader = LpcHeader
  { lpcHeaderSize :: Int
  -- ^ bytes, 28 (0x001C)
  , lpcMagic :: Int
  -- ^ uid, 999 (0x03E7)
  , lpcNPoles :: Int
  -- ^ number of filter poles
  , lpcFrameSize :: Int
  -- ^ element count (4 + nPoles)
  , lpcFrameRate :: Float
  -- ^ frames-per-second
  , lpcSampleRate :: Float
  -- ^ samples-per-second
  , lpcAnalysisDuration :: Float
  -- ^ seconds
  , lpcNFrames :: Int
  -- ^ frame-count (ie. duration * frame-rate)
  }
  deriving (Eq, Show)

{- | Lpc analysis frame data.
  A frame consists of RMS2 (residual), RMS1 (input), ERRN, and CPS fields,
  followed by /n/ filter co-efficients.
-}
type LpcFrame = [Float]

-- | Lpc analysis data.
data Lpc = Lpc
  { lpcHeader :: LpcHeader
  , lpcFrames :: [LpcFrame]
  }
  deriving (Eq, Show)

-- * Text

{- | Read 32-bit floating point hex, as in C language (ISO-9899:TC3)
<https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf>

>>> read_f32_text "0x1.p0"
1.0

>>> read_f32_text "0x1.p3"
8.0

>>> read_f32_text "0x1.4p0"
1.25

>>> read_f32_text "0x1.4p3"
10.0

>>> read_f32_text "0x0.1p0" * (2 ^ 4)
1.0

>>> read_f32_text "0x0.1p4"
1.0

>>> read_f32_text "0xa1.f1p0"
161.9414

>>> (0xa1, 0xf1, 0xf1 / 0x100)
(161,241,0.94140625)

>>> read_f32_text "-0x1.9851ecp+3"
-12.76

>>> read_f32_text "0x1.921fb54442d18p+1"
3.1415927

>>> read_f32_text "0x1.f44abd5aa7ca4p+25"
6.5574268e7
-}
read_f32_text :: String -> Float
read_f32_text =
  Data.Maybe.fromMaybe (error "read_f32_text")
    . Data.Numbers.FloatingHex.readHFloat

read_i32_text :: String -> Int
read_i32_text = read

-- | Read text format lpanal files, written using -X flag.
lpc_read_text :: FilePath -> IO Lpc
lpc_read_text fn = do
  s <- readFile fn
  let h1 : h2 : h3 : rest = lines s
  Control.Monad.when (h1 /= "LPANAL") (error (Text.Printf.printf "lpc_read_text: not LPANAL file: %s" h1))
  let [hs, lm, np, fs] = map read_i32_text (words h2)
      [fr, sr, fd] = map read_f32_text (words h3)
      nf = length rest `div` fs
      hdr = LpcHeader hs lm np fs fr sr fd nf
      d = Data.List.Split.chunksOf fs (map read_f32_text rest)
  return (Lpc hdr d)

-- * Binary

-- | Endianness.
data Endian = LittleEndian | BigEndian deriving (Eq)

-- | Binary Int32 and Float readers.
endian_to_readers :: Endian -> (System.IO.Handle -> IO Int, System.IO.Handle -> IO Float)
endian_to_readers e =
  case e of
    LittleEndian -> (Osc.read_i32_le, Osc.read_f32_le)
    BigEndian -> (Osc.read_i32, Osc.read_f32)

{- | Read a lpanal binary format Lpc data file.
  RMS2 and RMS1 are /not/ normalised.
-}
lpc_read_binary :: Endian -> FilePath -> IO Lpc
lpc_read_binary e fn = do
  let (read_i32, read_f32) = endian_to_readers e
  h <- System.IO.openFile fn System.IO.ReadMode
  l <- System.IO.hFileSize h
  [hs, lm, np, fs] <- Control.Monad.replicateM 4 (read_i32 h)
  [fr, sr, fd] <- Control.Monad.replicateM 3 (read_f32 h)
  Control.Monad.when (lm /= 0x03e7) (error (Text.Printf.printf "lpcRead: illegal magic number: hs=%d lm=%X" hs lm))
  Control.Monad.when (fs /= 4 + np) (error (Text.Printf.printf "lpcRead: illegal frame-size: np=%d fs=%d" hs lm np fs))
  let nf = ((fromIntegral l - hs) `div` 4) `div` fs
      hdr = LpcHeader hs lm np fs fr sr fd nf
      hc = hs - (7 * 4)
      get_f = Control.Monad.replicateM fs (read_f32 h)
  _ <- ByteString.hGet h hc
  d <- Control.Monad.replicateM nf get_f
  System.IO.hClose h
  return (Lpc hdr d)

-- * Sc3

-- | Normalise Float signal.
f32_normalise :: [Float] -> [Float]
f32_normalise x = let m = recip (maximum x) in map (* m) x

-- | Transpose and normalise Lpc frame data.
lpc_sc3_data :: [[Float]] -> [[Float]]
lpc_sc3_data d =
  let rms2 : rms1 : rest = Data.List.transpose d
  in f32_normalise rms2 : f32_normalise rms1 : rest

{- | Analysis data in format required by the Sc3 Lpc UGens.
  Normalises rms2 and rms1 before packing.
-}
lpcSc3 :: Lpc -> [Float]
lpcSc3 (Lpc h d) =
  let to_f32 = fromIntegral
      np = lpcNPoles h
      nf = lpcNFrames h
      fs = lpcFrameSize h
  in to_f32 np : to_f32 nf : to_f32 fs : concat (lpc_sc3_data d)

{-
fn = "/home/rohan/uc/invisible/clarity/lpc/z.01.lpc"
lpc <- lpc_read_text fn
hdr = lpcHeader lpc
rms2:rms1:errn:cps:_ = transpose (lpcFrames lpc)
rms2:rms1:errn:cps:_ = lpc_sc3_data (lpcFrames lpc)
import qualified Music.Theory.List as T
map T.minmax [rms2,rms1,errn,cps]
-}
