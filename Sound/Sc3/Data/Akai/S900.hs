{- | Akai S900

- <http://mda.smartelectronix.com/akai/akaiinfo.htm>
- akaiutil-3.6.2 (17-MAR-2019)
-}
module Sound.Sc3.Data.Akai.S900 where

import Control.Monad {- base -}
import Data.Maybe {- base -}
import System.FilePath {- filepath -}

import qualified Data.List.Split as Split {- split -}

import qualified Music.Theory.List as T {- hmt-base -}
import qualified Music.Theory.Tuple as T {- hmt-base -}

import qualified Sound.Midi.Common as M {- midi-osc -}

import qualified Sound.File.Header as Sf {- hsc3-sf -}
import qualified Sound.File.Wave as Sf {- hsc3-sf -}

import Sound.Sc3.Data.Byte {- hsc3-data -}
import Sound.Sc3.Data.Math.Types {- hsc3-data -}

{- * 2. S900/S950 disk format (.IMG)

Length   Format      Description
   -----------------------------------------------------------
     10     ASCII       Filename
      6                 0
      1     ASCII       File type: 'S'=sample, 'P'=program, etc.
      3     unsigned    File length in bytes
      2     unsigned    Starting block on disk
      2                 S900 ID = {0,0}
-}

-- | (File-Name,File-Type,File-Length,Starting-Block)
type S900_DISK_ENT = (String, Char, U24, U16)

s900_disk_ent_type :: S900_DISK_ENT -> Char
s900_disk_ent_type (_, ty, _, _) = ty

-- | Nil entry, ie. all zeroes.
s900_disk_ent_nil :: S900_DISK_ENT
s900_disk_ent_nil = (replicate 10 '\NUL', '\NUL', 0, 0)

-- | Byte length of disk sectors.
s900_sector_n :: Num n => n
s900_sector_n = 1024

-- | Byte length of low density disk, 2 sides of 80 tracks with 5 sectors per track.
s900_low_density_n :: Num n => n
s900_low_density_n = 2 * 80 * 5 * s900_sector_n

-- | Byte length of high density disk, 2 sides of 80 tracks with 10 sectors per track.
s900_high_density_n :: Num n => n
s900_high_density_n = s900_low_density_n * 2

-- | Load U8 IMG data.
s900_load_img :: FilePath -> IO [U8]
s900_load_img = M.bytes_load

-- | Predicate to test if data is a nil entry.
s900_disk_ent_is_nil :: [U8] -> Bool
s900_disk_ent_is_nil d = d == replicate 24 0

-- | Run checks and then segment data.
s900_segment :: [Int] -> [U8] -> [[U8]]
s900_segment n d = if sum n /= length d then error "s900_segment?" else Split.splitPlaces n d

-- | Parse 24-byte S900_DISK_ENT.
s900_disk_ent_parse :: [U8] -> Maybe S900_DISK_ENT
s900_disk_ent_parse d =
  if s900_disk_ent_is_nil d
    then Nothing
    else
      let (nm, unused, ty, len, blk, s900_id) = T.t6_from_list (s900_segment [10, 6, 1, 3, 2, 2] d)
          (len1, len2, len3) = T.t3_from_list len
          (blk1, blk2) = T.t2_from_list blk
      in if unused /= [0, 0, 0, 0, 0, 0] || s900_id /= [0, 0]
          then error "s900_disk_ent_parse?"
          else
            Just
              ( map toEnum nm
              , toEnum (ty !! 0)
              , u24_pack_le (len1, len2, len3)
              , u16_pack_le (blk1, blk2)
              )

-- | Read the non-NIL instances of the 64 possible 24-byte S900_DISK_ENT fields.
s900_disk_ent :: [U8] -> [S900_DISK_ENT]
s900_disk_ent = mapMaybe s900_disk_ent_parse . Split.chunksOf 24 . take 1536

{- | BLOCK-MAP.

Bytes 1536 to 3136 (1536 to 4736 for high density) contain a map of block sequences.

The entry for the last block of a file contains the value 0x8000 (32768).

Unused blocks have a map entry of 0x0000 (zero).

The first 4 blocks are needed for the DISK_ENT and BLK_MAP data.
-}
type S900_IMG_BLK_MAP = [U16]

{- | Parse S900_IMG_BLK_MAP.
  The S900-DATA size (low or high density) determines the map size.
-}
s900_img_blk_map_parse :: [U8] -> S900_IMG_BLK_MAP
s900_img_blk_map_parse d =
  let n = length d
      z = drop 1536 d
      m = if n == s900_low_density_n then take 1600 z else take 3200 z
  in map (u16_pack_le . T.t2_from_list) (Split.chunksOf 2 m)

{- | The marker at the block map to indicate there are no further blocks.

> s900_blk_eof == 32768
-}
s900_blk_eof :: U16
s900_blk_eof = 0x8000

-- | Given map and starting block, return block sequence.
s900_blk_seq :: S900_IMG_BLK_MAP -> U16 -> [U16]
s900_blk_seq m =
  let recur r i =
        case u16_at m i of
          0 -> error "s900_blk_seq?"
          0x8000 -> reverse (i : r)
          j -> recur (i : r) j
  in recur []

-- | Get data for sector /i/.
s900_blk_dat :: [U8] -> U16 -> [U8]
s900_blk_dat d i = take s900_sector_n (u24_drop (u16_to_u24 i * s900_sector_n) d)

-- | Given (S900-IMG-DATA,S900-IMG-BLOCK-MAP) and S900-DISK-ENT, extract FILE-DATA.
s900_get_file :: ([U8], S900_IMG_BLK_MAP) -> S900_DISK_ENT -> [U8]
s900_get_file (d, m) (_, _, len, blk) =
  let blk_seq = s900_blk_seq m blk
  in u24_take len (concatMap (s900_blk_dat d) blk_seq)

-- | Read S900 disk image.
s900_read_img :: FilePath -> IO [(S900_DISK_ENT, [U8])]
s900_read_img fn = do
  d <- s900_load_img fn
  let m = s900_img_blk_map_parse d
      e = s900_disk_ent d
      f = map (s900_get_file (d, m)) e
  when (length e /= length f) (error "s900_read_img?")
  return (zip e f)

{- * 3. S900/S950 sample format (60-bytes)

   Length   Format      Description
   ------------------------------------------------------------
     10     ASCII       Filename
      6                 0
      4     unsigned    Number of sample words
      2     unsigned    Sample rate (Hz)
      2     unsigned    Tuning (16ths of a semitone, C4=960=60*16)
      2                 0
      1     ASCII       Loop mode (O=one-shot, L=loop, A=alt)
      1                 0
      4     unsigned    End marker
      4     unsigned    Start marker
      4     unsigned    Loop length
     20                 140,185,0,78,0,0,0,0,0,0,0,0,0,0,224,43,38,0,0,0
-}

{- | S900 tuning is given in 16th of a semitone, 960 = C4 (ie. midi note number 60).

> map s900_tuning_to_fmidi [960,968,976] == [60,60.5,61]
-}
s900_tuning_to_fmidi :: U16 -> Double
s900_tuning_to_fmidi x = u16_to_f64 x / 16.0

-- | (FILE-NAME,SAMPLE-LENGTH,SAMPLE-RATE,TUNING,LOOP-MODE,END-MARK,START-MARK,LOOP-LENGTH)
type S900_Sf_HDR = (String, U32, U16, U16, Char, U32, U32, U32)

-- | Parse 60-byte S900 sound file header.
s900_sf_hdr :: [U8] -> S900_Sf_HDR
s900_sf_hdr d =
  let pl = [10, 6, 4, 2, 2, 2, 1, 1, 4, 4, 4, 20]
      (fn, _u1, ns, sr, tn, _u2, lp, _u3, em, sm, ll, _u4) = T.t12_from_list (s900_segment pl d)
      (sr1, sr2) = T.t2_from_list sr
      (tn1, tn2) = T.t2_from_list tn
      (ns1, ns2, ns3, ns4) = T.t4_from_list ns
      (em1, em2, em3, em4) = T.t4_from_list em
      (sm1, sm2, sm3, sm4) = T.t4_from_list sm
      (ll1, ll2, ll3, ll4) = T.t4_from_list ll
  in ( map toEnum fn
     , u32_pack_le (ns1, ns2, ns3, ns4)
     , u16_pack_le (sr1, sr2)
     , u16_pack_le (tn1, tn2)
     , toEnum (lp !! 0)
     , u32_pack_le (em1, em2, em3, em4)
     , u32_pack_le (sm1, sm2, sm3, sm4)
     , u32_pack_le (ll1, ll2, ll3, ll4)
     )

{- | Unpack N + N/2 bytes of 12-bit signed sample data.

The upper 4-bits of the first byte are the lower 4-bits of the first word.
The lower 4-bits of the first byte are the lower 4-bits of word N/2.
The second byte contains the upper 8-bits of the first word.
This repeats for the first N bytes.
Then there are N/2 bytes containing the upper 8-bits of the last N/2 words.
-}
s900_sf_data_unpack :: U32 -> [U8] -> [I12]
s900_sf_data_unpack n d =
  let (p, q) = unzip (T.adj2 2 (u32_take n d))
      (p0, p1) = (map M.bits8_hi p, map M.bits8_lo p)
      r = u32_drop n d
      f = curry u12_pack_le
  in map u12_as_i12 (zipWith f p0 q ++ zipWith f p1 r)

s900_sf_write_wav :: FilePath -> [U8] -> IO ()
s900_sf_write_wav dir d = do
  let hdr = s900_sf_hdr (take 60 d)
      (nm, nf, sr, _, _, _, _, _) = hdr
      dat = s900_sf_data_unpack nf (drop 60 d)
      sf_hdr = Sf.Sf_Header nf Sf.Linear16 sr 1
  Sf.wave_store_i16 (dir </> nm <.> "wav") sf_hdr [map fromIntegral dat]

s900_sf_export :: FilePath -> FilePath -> IO ()
s900_sf_export dir fn = do
  r <- s900_read_img fn
  let f (e, d) =
        if s900_disk_ent_type e == 'S'
          then print (e, s900_sf_hdr (take 60 d)) >> s900_sf_write_wav dir d
          else print "?"
  mapM_ f r
