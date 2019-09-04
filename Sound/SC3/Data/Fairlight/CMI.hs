{- | FAIRLIGHT CMI IIX DISKS
     <http://www.nattvard.com/iix/database.php>

This assumes some post-processing:
convert WAV audio files to 8-BIT SND files,
edit SFZ files to correct pitch_keycenter (C4 -> A3)
and to set sample format (.wav -> .snd)
-}
module Sound.SC3.Data.Fairlight.CMI where

import Data.List {- base -}
import Data.Maybe {- base -}
import Data.Word {- base -}
import System.FilePath {- filepath -}
import Text.Printf {- base -}

import Music.Theory.Directory {- hmt -}

import Sound.File.HSndFile {- hsc3-sf-sndfile -}

import Sound.SC3.Data.SFZ {- hsc3-data -}

-- | (DIR,FILENAME,VOLUME,KEY-CENTER,LOOP-MODE-SYM,LOOP-START,LOOP-END,EG-ATTACK,EG-RELEASE)
--
-- For CMI in all cases VOLUME=-3 ; KEYCENTER=A3=57
type CMI_SFZ = (FilePath,FilePath,Double, Word8, Char, Int, Int, Double, Double)

-- | Parse SFZ <region>.
cmi_sfz_rgn_parse :: FilePath -> SFZ_Region -> CMI_SFZ
cmi_sfz_rgn_parse dir r =
  let vol = sfz_region_volume r
      mnn = sfz_region_pitch_keycenter r
      lp_mode = fromMaybe (error "cmi_sfz_rgn_parse?") (sfz_region_loop_mode_sym r)
  in (dir,sfz_region_sample r,vol,mnn,lp_mode,sfz_region_loop_start r,sfz_region_loop_end r
     ,sfz_region_ampeg_attack r,sfz_region_ampeg_release r)

-- | SFZ region, directory of SFZ file, SND header.
type CMI_DAT = (SFZ_Region,FilePath,SF_Header)

-- | Parse SFZ <region>.
cmi_sfz_parse :: CMI_DAT -> CMI_SFZ
cmi_sfz_parse (r,dir,_) = cmi_sfz_rgn_parse dir r

-- | Load SFZ and SND-HEADER data.
cmi_load_dat :: FilePath -> String -> IO CMI_DAT
cmi_load_dat cmi_dir fn = do
  (_,_,[rgn]) <- sfz_load (cmi_dir </> fn)
  let dir = takeDirectory (cmi_dir </> fn)
      sf_fn = dir </> sfz_region_sample rgn
  hdr <- sf_header sf_fn
  return (rgn,dir,hdr)

-- | Load and parse SFZ.
cmi_load_sfz :: FilePath -> String -> IO CMI_SFZ
cmi_load_sfz dir = fmap cmi_sfz_parse . cmi_load_dat dir

-- | Pretty-printer.
cmi_sfz_pp :: (String,CMI_SFZ) -> String
cmi_sfz_pp (nm,(_,_,_,_,l1,l2,l3,e1,e2)) = printf "%-24s %c %5d %5d %3.1f %3.1f" nm l1 l2 l3 e1 e2

-- | Load all .sfz files below directory.
--   Names are of the form DISK/VOICE.
cmi_load_dir :: FilePath -> IO [(FilePath, CMI_SFZ)]
cmi_load_dir cmi_dir = do
  fn <- dir_find_ext_rel ".sfz" cmi_dir
  let nm_seq = map dropExtension (sort fn)
  cmi_seq <- mapM (cmi_load_sfz cmi_dir) (sort fn)
  return (zip nm_seq cmi_seq)
