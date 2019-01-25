-- | Reader for LPC analysis data files.
--
-- <http://www.csounds.com/manual/html/lpanal.html>
module Sound.SC3.Data.LPC where

import Control.Monad {- base -}
import qualified Data.ByteString.Lazy as B {- bytestring -}
import Data.List {- base -}
import System.IO {- base -}

import qualified Sound.OSC.Coding.Byte as O {- hosc -}

-- | LPC analysis meta-data.
data LPCHeader = LPCHeader { lpcHeaderSize :: Int -- ^ bytes, 28 (0x001C)
                           , lpcMagic :: Int -- ^ uid, 999 (0x03E7)
                           , lpcNPoles :: Int -- ^ number of filter poles
                           , lpcFrameSize :: Int -- ^ element count (4 + nPoles)
                           , lpcFrameRate :: Float -- ^ frames-per-second
                           , lpcSampleRate :: Float -- ^ samples-per-second
                           , lpcAnalysisDuration :: Float -- ^ seconds
                           , lpcNFrames :: Int -- ^ frame-count (ie. duration * frame-rate)
                           } deriving (Eq, Show)

-- | LPC analysis frame data.
--   A frame consists of RMS2 (residual), RMS1 (input), ERRN, and CPS fields,
--   followed by /n/ filter co-efficients.
type LPCFrame = [Float]

-- | LPC analysis data.
data LPC = LPC { lpcHeader :: LPCHeader
               , lpcFrames :: [LPCFrame] }
           deriving (Eq, Show)

-- | Read a lpanal binary format LPC data file.
--   rms2 and rms2 are /not/ normalised.
lpcRead :: FilePath -> IO LPC
lpcRead fn = do
  h <- openFile fn ReadMode
  l <- hFileSize h
  [hs, lm, np, fs] <- replicateM 4 (O.read_i32 h)
  [fr, sr, fd] <- replicateM 3 (O.read_f32 h)
  when (fs /= 4 + np) (error "lpcRead: illegal frame-size")
  let nf = ((fromIntegral l - hs) `div` 4) `div` fs
      hdr = LPCHeader hs lm np fs fr sr fd nf
      hc = hs - (7 * 4)
      get_f = replicateM fs (O.read_f32 h)
  _ <- B.hGet h hc
  d <- replicateM nf get_f
  hClose h
  return (LPC hdr d)

-- | Normalise rms2 signal.
rms2_normalise :: [Float] -> [Float]
rms2_normalise x = let m = recip (maximum x) in map (* m) x

-- | Normalise rms1 signal.
rms1_normalise :: Int -> [Float] -> [Float]
rms1_normalise nPoles x = let m = recip (maximum x * fromIntegral nPoles) in map (* m) x

-- | Transpose and normalise LPC frame data.
lpc_sc3_data :: Int -> [[Float]] -> [[Float]]
lpc_sc3_data np d =
  let rms2:rms1:rest = transpose d
  in rms2_normalise rms2 : rms1_normalise np rms1 : rest

-- | Analysis data in format required by the SC3 LPC UGens.
--   Normalises rms2 and rms1 before packing.
lpcSC3 :: LPC -> [Float]
lpcSC3 (LPC h d) =
  let flt = fromIntegral
      np = lpcNPoles h
      nf = lpcNFrames h
      fs = lpcFrameSize h
  in flt np : flt nf : flt fs : concat (lpc_sc3_data np d)
