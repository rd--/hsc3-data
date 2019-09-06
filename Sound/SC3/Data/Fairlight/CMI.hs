{- | FAIRLIGHT CMI IIX DISKS
     <http://www.nattvard.com/iix/database.php>
-}
module Sound.SC3.Data.Fairlight.CMI where

import Data.List {- base -}
import Data.Maybe {- base -}
import Data.Word {- base -}
import System.FilePath {- filepath -}
import Text.Printf {- base -}

import Music.Theory.Directory {- hmt -}

import Sound.SC3.Data.SFZ {- hsc3-data -}

-- | (DIR,FILENAME,VOLUME,KEY-CENTER,LOOP-MODE-SYM,LOOP-START,LOOP-END,EG-ATTACK,EG-RELEASE)
--
-- For CMI in all cases VOLUME=-3 ; KEYCENTER=A3=57
type CMI_SFZ = (FilePath,FilePath,Double, Word8, Char, Word32, Word32, Double, Double)

-- | Parse SFZ <region>.
cmi_sfz_rgn_parse :: FilePath -> SFZ_Region -> CMI_SFZ
cmi_sfz_rgn_parse dir r =
  let vol = sfz_region_volume r
      mnn = sfz_region_pitch_keycenter r
      lp_mode = fromMaybe (error "cmi_sfz_rgn_parse?") (sfz_region_loop_mode_sym r)
  in (dir,sfz_region_sample r,vol,mnn,lp_mode,sfz_region_loop_start r,sfz_region_loop_end r
     ,sfz_region_ampeg_attack r,sfz_region_ampeg_release r)

-- | Load and parse CMI SFZ.
cmi_load_sfz :: FilePath -> IO CMI_SFZ
cmi_load_sfz fn = do
  (_,_,rgn) <- sfz_load_data fn
  case rgn of
    [r] -> return (cmi_sfz_rgn_parse (takeDirectory fn) r)
    _ -> error "cmi_load_sfz?"

-- | Pretty-printer.
cmi_sfz_pp :: (String,CMI_SFZ) -> String
cmi_sfz_pp (nm,(_,_,_,_,l1,l2,l3,e1,e2)) = printf "%-24s %c %5d %5d %3.1f %3.1f" nm l1 l2 l3 e1 e2

-- | Load all .sfz files below directory.
--   Names are of the form DISK/VOICE.
cmi_load_dir :: FilePath -> IO [(String,CMI_SFZ)]
cmi_load_dir dir = do
  fn <- dir_find_ext_rel ".sfz" dir
  let nm_seq = map dropExtension (sort fn)
  cmi_seq <- mapM (cmi_load_sfz . ((<>) dir)) (sort fn)
  return (zip nm_seq cmi_seq)
