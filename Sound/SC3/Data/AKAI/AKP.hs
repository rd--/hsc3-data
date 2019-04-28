{- | AKAI AKP files (S5000 / S6000).

<http://mda.smartelectronix.com/akai/AKPspec.html>

-}
module Sound.SC3.Data.AKAI.AKP where

import Control.Monad {- base -}
--import Data.Int {- base -}
import Data.Word {- base -}
import System.IO {- base -}

import qualified Data.ByteString.Lazy as L {- bytestring -}

import qualified Music.Theory.Byte as T {- hmt -}
import qualified Music.Theory.Math.Convert as T {- hmt -}

import Sound.File.RIFF {- hsc3-sf -}

-- | The kgrp CHUNK data is a sequence of nine CHUNKs.
akp_kgrp_chunks :: CHUNK -> [CHUNK]
akp_kgrp_chunks ch =
  case ch of
    (("kgrp",344),dat) -> riff_parse_chunk_seq dat
    _ -> error "akp_kgrp_chunks?"

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

-- * PRG

-- | (MIDI-PRG-NUMBER,KEYGROUP-COUNT)
akp_prg_parse :: CHUNK -> (Word8,Word8)
akp_prg_parse ch =
  case ch of
    (("prg ",6),dat) -> (L.index dat 1,L.index dat 2)
    _ -> error "akp_prg_parse?"

-- * TUNE (22-BYTES)

{-

 1	Semitone Tune (0) -36 -> 36
 2	Fine Tune (0) -50 -> 50
 3	C detune (0) -50 -> 50
 4	C# detune (0) -50 -> 50
 5	D detune (0) -50 -> 50
 6	Eb detune (0) -50 -> 50
 7	E detune (0) -50 -> 50
 8	F detune (0) -50 -> 50
 9	F# detune (0) -50 -> 50
10	G detune (0) -50 -> 50
11	G# detune (0) -50 -> 50
12	A detune (0) -50 -> 50
13	Bb detune (0) -50 -> 50
14	B detune (0) -50 -> 50
15	Pitchbend Up (2) 0 -> 24
16	Pitchbend Down (2) 0 -> 24
17	Bend Mode (0) 0 = NORMAL, 1 = HELD
18	Aftertouch (0) -12 -> 12

-}

-- * KLOC (16-BYTES)

{-

 4	low note (21) 21 -> 127
 5	high note (127) 21 -> 127
 6	Semitone Tune (0) -36 -> 36
 7	Fine Tune (0) -50 -> 50
 8	Overide FX (0) 0 = OFF, 1 = FX1, 2 = FX2, 3 = RV3, 4 = RV4
 9	FX Send Level (0) 0 -> 100
10	Pitch Mod 1 (100) -100 -> 100
11	Pitch Mod 2 (0) -100 -> 100
12	Amp Mod (0) -100 -> 100
13	Zone Xfade (0) 0 = OFF, 1 = ON
14	Mute Group

-}

akp_kloc_parse :: CHUNK -> (Word8,Word8,Word8,Word8)
akp_kloc_parse ch =
  case ch of
    (("kloc",16),dat) ->
      let ix = L.index dat
      in (ix 4,ix 5,ix 6,ix 7)
    _ -> error "akp_kloc_parse?"

-- * ENV-AMP (18-BYTES)

{-

1	Attack (0) 0 -> 100
3	Decay (50) 0 -> 100
4	Release (15) 0 -> 100
6	Sustain (100) 0 -> 100

-}

-- | (ATTACK,DECAY,RELEASE,SUSTAIN)
akp_env_parse :: CHUNK -> (Word8,Word8,Word8,Word8)
akp_env_parse ch =
  case ch of
    (("env ",18),dat) ->
      let ix = L.index dat
      in (ix 1,ix 3,ix 4,ix 6)
    _ -> error "akp_env_parse?"

-- * ZONE (46-BYTES)

akp_playback_tbl :: [(Word8,String)]
akp_playback_tbl =
  [(0,"NO LOOPING")
  ,(1,"ONE SHOT")
  ,(2,"LOOP IN REL")
  ,(3,"LOOP UNTIL REL")
  ,(4,"AS SAMPLE")]

{-

1	Number of chars in Sample Name
2 - 21  Sample Name (pad with 00h) (if first character = 00h then no sample assigned)
34	Low Velocity (0) 0 -> 127
35	High Velocity (127) 0 -> 127
36	Fine Tune (0) -50 -> 50
37	Semitone Tune (0) -36 -> 36
38	Filter (0) -100 -> 100
39	Pan/Balance (0) -50 -> 50 = L50 -> R50
40	Playback (4)
41	Output (0)
42	Zone Level (0) -100 -> 100
43	Keyboard Track (1) 0 = OFF, 1 = ON
44	Velocity->Start LSB (0) }
45	Velocity->Start MSB (0) } -9999 -> 9999

-}

-- | (NAME,FINE-TUNE,TUNE,PAN,PLAYBACK,LEVEL)
akp_zone_parse :: CHUNK -> (String,Word8,Word8,Word8,Word8,Word8)
akp_zone_parse ch =
  case ch of
    (("zone",48),dat) ->
      let ix = L.index dat
          nm_n = ix 1
          nm = map T.word8_to_char (L.unpack (section 2 (T.word8_to_int64 nm_n) dat))
      in (nm,ix 36,ix 37,ix 39,ix 40,ix 42)
    _ -> error "akp_zone_parse?"


{-

fn = "/home/rohan/EMU/UNIVERSE OF SOUNDS FAVOURITES/ARIEL PAD/S50_ARIEL.akp"
(hdr,kgrp) <- akp_load_ch fn
map fst hdr
akp_prg_parse (hdr !! 0)
k = kgrp !! 0
map fst k
akp_kloc_parse (k !! 0)
akp_env_parse (k !! 1)
akp_zone_parse (k !! 5)

-}
