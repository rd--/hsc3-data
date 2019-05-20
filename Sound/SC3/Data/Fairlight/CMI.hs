{- | FAIRLIGHT CMI IIX DISKS
     <http://www.nattvard.com/iix/database.php>

This assumes some post-processing:
of WAV audio files to 8-BIT SND files,
and re-naming of SFZ files so that all .SND files have matched .SFZ files.
-}
module Sound.SC3.Data.Fairlight.CMI where

import Data.List {- base -}
import Data.Maybe {- base -}
import System.FilePath {- filepath -}
import Text.Printf {- base -}

import Music.Theory.Directory {- hmt -}

import Sound.File.NeXT {- hsc3-sf -}

import Sound.SC3.Data.SFZ {- hsc3-data -}

-- | (LOOP-MODE,LOOP-START,LOOP-END,EG-ATTACK,EG-RELEASE)
--
-- In all cases VOLUME=-3 ; KEYCENTER=C4
type CMI_SFZ = (Char, Int, Int, Double, Double)

-- | Parse SFZ <region>.
cmi_sfz_rgn_parse :: SFZ_Region -> CMI_SFZ
cmi_sfz_rgn_parse r =
  let vol = sfz_region_volume r
      mnn = sfz_region_pitch_keycenter r
      err = error "cmi_sfz_rgn_parse?"
      lp_mode = fromMaybe err (sfz_region_loop_mode_sym r)
  in if vol /= -3 || mnn /= 60
     then err
     else (lp_mode,sfz_region_loop_start r,sfz_region_loop_end r
          ,sfz_region_ampeg_attack r,sfz_region_ampeg_release r)

-- | SFZ and SND data.
type CMI_DAT = ([SFZ_Region],SF_Header)

-- | Parse SFZ <region>.
cmi_sfz_parse :: CMI_DAT -> CMI_SFZ
cmi_sfz_parse l =
  case l of
    ([r],_) -> cmi_sfz_rgn_parse r
    _ -> error "cmi_sfz_parse?"

-- | Load SFZ and SND-HEADER data.
cmi_load_dat :: FilePath -> String -> IO CMI_DAT
cmi_load_dat dir nm = do
  sfz_rgn <- sfz_load_regions (dir </> nm <.> "sfz")
  snd_hdr <- au_header (dir </> nm <.> "snd")
  return (sfz_rgn,snd_hdr)

-- | Load and parse SFZ.
cmi_load_sfz :: FilePath -> String -> IO CMI_SFZ
cmi_load_sfz dir = fmap cmi_sfz_parse . cmi_load_dat dir

-- | Pretty-printer.
cmi_sfz_pp :: (String,CMI_SFZ) -> String
cmi_sfz_pp (nm,(l1,l2,l3,e1,e2)) = printf "%-24s %c %5d %5d %3.1f %3.1f" nm l1 l2 l3 e1 e2

-- | Load all .sfz files below directory.
--   Names are of the form DISK/VOICE.
cmi_load_dir :: FilePath -> IO [(FilePath, CMI_SFZ)]
cmi_load_dir cmi_dir = do
  fn <- dir_find_ext_rel ".sfz" cmi_dir
  let nm_seq = sort (map dropExtension fn)
  cmi_seq <- mapM (cmi_load_sfz cmi_dir) nm_seq
  return (zip nm_seq cmi_seq)
