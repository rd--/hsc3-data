{- | Akai Akp files (S5000 / S6000).

<http://mda.smartelectronix.com/akai/AKPspec.html>

-}
module Sound.Sc3.Data.Akai.Akp where

import Control.Monad {- base -}
--import Data.Int {- base -}
import Data.Word {- base -}
import System.IO {- base -}

import qualified Data.ByteString.Lazy as L {- bytestring -}

import qualified Music.Theory.Byte as T {- hmt-base -}
import qualified Music.Theory.Math.Convert as T {- hmt-base -}

import Sound.File.Riff {- hsc3-sf -}

-- | The kgrp Chunk data is a sequence of nine Chunks.
--   Kloc,Env-{Amp,Flt,Aux},Filt,Zone-{1,2,3,4},
akp_kgrp_chunks :: Chunk -> [Chunk]
akp_kgrp_chunks ch =
  case ch of
    (("kgrp",344),dat) -> riff_parse_chunk_seq dat
    _ -> error "akp_kgrp_chunks?"

-- | Read Akp file.
--   The structure is a sequence of six header chunks followed by a sequence of kgrp chunks.
akp_read_ch :: Handle -> IO ([Chunk],[[Chunk]])
akp_read_ch h = do
  (ty,_) <- riff_read_chunk_hdr h
  when (ty /= "RIFF") (error "riff_read: not RIFF")
  ty' <- read_word32_ascii h
  when (ty' /= "APRG") (error "riff_read: not APRG")
  ch <- riff_read_chunk_seq h
  return (take 6 ch,map akp_kgrp_chunks (drop 6 ch))

-- | 'withFile' of 'akp_read_ch'
akp_load_ch :: FilePath -> IO ([Chunk],[[Chunk]])
akp_load_ch fn = withFile fn ReadMode akp_read_ch

-- * Prg

-- | (Midi-Prg-Number,Keygroup-Count)
type Akp_Prg = (Word8,Word8)

akp_prg_parse :: L.ByteString -> Akp_Prg
akp_prg_parse dat = (L.index dat 1,L.index dat 2)

-- * Tune (24-BYTES)

{- | Tune data

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
type Akp_Tune = ((Word8, Word8), [Word8], (Word8, Word8), Word8, Word8)

-- | TUNE default values.
akp_tune_def :: Akp_Tune
akp_tune_def = ((0,0),[0,0,0,0,0,0,0,0,0,0,0,0],(2,2),0,0)

-- | TUNE parser.
akp_tune_parse :: L.ByteString -> Akp_Tune
akp_tune_parse dat =
  let ix = L.index dat
      sec n m = L.unpack (L.take (m - n + 1) (L.drop n dat))
  in ((ix 1,ix 2),sec 3 14,(ix 15,ix 16),ix 17,ix 18)

-- * Lfo

akp_wave_tbl :: [(Word8,String)]
akp_wave_tbl =
  [(0,"SINE")
  ,(1,"TRIANGLE")
  ,(2,"SQUARE"),(3,"SQUARE+"),(4,"SQUARE-")
  ,(5,"SAW BI"),(6,"SAW UP"),(7,"SAW DOWN")
  ,(8,"RANDOM")]

{- | AKP LFO (14-BYTES)

 1	Waveform (1)
 2	Rate (43) 0 -> 100
 3	Delay (0) 0 -> 100
 4	Depth (0) 0 -> 100
 5	LFO Sync (0) 0 = OFF, 1 = ON
 7	Modwheel (15) 0 -> 100
 8	Aftertouch (0) 0 -> 100
 9	Rate Mod (0) -100 -> 100
10	Delay Mod (0) -100 -> 100
11	Depth Mod (0) -100 -> 100

-}
type Akp_Lfo = (Word8,Word8,Word8,Word8,Word8,(Word8,Word8),(Word8,Word8,Word8))

akp_lfo_def :: Akp_Lfo
akp_lfo_def = (1,43,0,0,0,(15,0),(0,0,0))

akp_lfo_parse :: L.ByteString -> Akp_Lfo
akp_lfo_parse dat =
  let ix = L.index dat
  in (ix 1,ix 2,ix 3,ix 4,ix 5,(ix 7,ix 8),(ix 9,ix 10,ix 11))

-- * KLOC (16-BYTES)

{- | AKP KLOC

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

(LOW,HIGH,TUNE,FINE-TUNE)

-}
type Akp_Kloc = (Word8,Word8,Word8,Word8)

akp_kloc_parse :: L.ByteString -> Akp_Kloc
akp_kloc_parse dat =
  let ix = L.index dat
  in (ix 4,ix 5,ix 6,ix 7)

-- * ENV (18-BYTES)

{- | AKP ENV

1	Attack (0) 0 -> 100
3	Decay (50) 0 -> 100
4	Release (15) 0 -> 100
6	Sustain (100) 0 -> 100

-}
type Akp_Env = (Word8,Word8,Word8,Word8)

akp_env_def :: Akp_Env
akp_env_def = (0,50,15,100)

akp_env_parse :: L.ByteString -> Akp_Env
akp_env_parse dat =
  let ix = L.index dat
  in (ix 1,ix 3,ix 4,ix 6)

-- * ZONE (48-BYTES)

akp_playback_tbl :: [(Word8,String)]
akp_playback_tbl =
  [(0,"NO LOOPING")
  ,(1,"ONE SHOT")
  ,(2,"LOOP IN REL")
  ,(3,"LOOP UNTIL REL")
  ,(4,"AS SAMPLE")]

{- | AKP ZONE

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

(NAME,..,..,FINE-TUNE,TUNE,..,PAN,PLAYBACK,..,LEVEL)

-}
type Akp_Zone = (String,Word8,Word8,Word8,Word8,Word8)

akp_zone_parse :: L.ByteString -> Akp_Zone
akp_zone_parse dat =
  let ix = L.index dat
      nm_n = ix 1
      nm = map T.word8_to_char (L.unpack (section 2 (T.word8_to_int64 nm_n) dat))
  in (nm,ix 36,ix 37,ix 39,ix 40,ix 42)

-- * CH

data AKP_CH =
    Akp_Prg Akp_Prg
  | Akp_Tune Akp_Tune
  | Akp_Lfo Akp_Lfo
  | Akp_Kloc Akp_Kloc
  | Akp_Env Akp_Env
  | Akp_Zone Akp_Zone
  | Akp_No_Parse Chunk_Hdr
  deriving (Show)

akp_ch_parse :: Chunk -> AKP_CH
akp_ch_parse ch =
  case ch of
    (("prg ",06),dat) -> Akp_Prg (akp_prg_parse dat)
    (("tune",24),dat) -> Akp_Tune (akp_tune_parse dat)
    (("lfo ",14),dat) -> Akp_Lfo (akp_lfo_parse dat)
    (("kloc",16),dat) -> Akp_Kloc (akp_kloc_parse dat)
    (("env ",18),dat) -> Akp_Env (akp_env_parse dat)
    (("zone",48),dat) -> Akp_Zone (akp_zone_parse dat)
    (hdr,_) -> Akp_No_Parse hdr

{-

fn = "/home/rohan/SYN/EMU/UNIVERSE OF SOUNDS FAVOURITES/ARIEL PAD/S50_ARIEL.akp"
(hdr,kgrp) <- akp_load_ch fn
map fst hdr
map akp_ch_parse hdr
k = kgrp !! 0
map fst k
map (map akp_ch_parse) kgrp

-}
