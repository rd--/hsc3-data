{- | AKAI AKP files (S5000 / S6000).

<http://mda.smartelectronix.com/akai/AKPspec.html>

-}
module Sound.SC3.Data.AKAI.AKP where

import Control.Monad {- base -}
import Data.Word {- base -}
import System.IO {- base -}

import qualified Data.ByteString.Lazy as L {- bytestring -}

import Sound.File.RIFF {- hsc3-sf -}

-- | The kgrp CHUNK data is a sequence of nine CHUNKs.
akp_kgrp_chunks :: CHUNK -> [CHUNK]
akp_kgrp_chunks ch =
  case ch of
    (("kgrp",344),dat) -> riff_parse_chunk_seq dat
    _ -> error "akp_prg_parse?"

-- | Read AKP file.
--   The structure is a sequence of six header CHUNKS followed by a sequence of kgrp CHUNKS.
akp_read_ch :: Handle -> IO ([CHUNK],[[CHUNK]])
akp_read_ch h = do
  (ty,_) <- riff_read_chunk_hdr h
  when (ty /= "RIFF") (error "riff_read: not RIFF")
  ty' <- read_word32_ascii h
  when (ty' /= "APRG") (error "riff_read: not APRG")
  ch <- riff_read_chunk_seq h
  return (take 6 ch,map akp_kgrp_chunks (drop 6 ch))

-- | 'withFile' of 'akp_read_ch'
akp_load_ch :: FilePath -> IO ([CHUNK],[[CHUNK]])
akp_load_ch fn = withFile fn ReadMode akp_read_ch

-- | (MIDI-PRG-NUMBER,KEYGROUP-COUNT)
akp_prg_parse :: CHUNK -> (Word8,Word8)
akp_prg_parse ch =
  case ch of
    (("prg ",6),dat) -> (L.index dat 1,L.index dat 2)
    _ -> error "akp_prg_parse?"

{-

fn = "/home/rohan/EMU/UNIVERSE OF SOUNDS FAVOURITES/ARIEL PAD/S50_ARIEL.akp"
(hdr,kgrp) <- akp_load_ch fn
map fst hdr
akp_prg_parse (hdr !! 0)
map (map fst) kgrp

-}
