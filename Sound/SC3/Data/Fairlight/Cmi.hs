{- | Fairlight CMI IIx Disks
     <http://www.nattvard.com/iix/database.php>
-}
module Sound.SC3.Data.Fairlight.Cmi where

import Data.List {- base -}
import Data.Maybe {- base -}
import Data.Word {- base -}
import System.FilePath {- filepath -}
import Text.Printf {- base -}

import qualified Music.Theory.Directory as T {- hmt-base -}

import qualified Music.Theory.Pitch as T {- hmt -}

import Sound.SC3.Data.Sfz {- hsc3-data -}

-- | (Sfz-Dir,Snd-File,Volume,Key-Center,Loop-Mode-Sym,Loop-Start,Loop-End,Eg-Attack,Eg-Release)
--
-- For Cmi in all cases Volume=-3 ; Keycenter=C4
type Cmi_Sfz = (FilePath,FilePath,Double,T.Midi,Char,(Word32,Word32),(Double,Double))

-- | Parse Sfz <region>.
cmi_sfz_rgn_parse :: FilePath -> Sfz_Region -> Cmi_Sfz
cmi_sfz_rgn_parse dir r =
  let vol = sfz_region_volume r
      (mnn,_,_) = sfz_region_key r
      (lm,lp) = sfz_region_loop_data r
  in (dir,sfz_region_sample r,vol,mnn,sfz_loop_mode_sym lm,fromMaybe (0,0) lp -- allow NON-Cmi files...
     ,(sfz_region_ampeg_attack r,sfz_region_ampeg_release r))

-- | Load and parse Cmi Sfz.
cmi_load_sfz :: FilePath -> IO Cmi_Sfz
cmi_load_sfz fn = do
  (_,_,rgn) <- sfz_load_data fn
  case rgn of
    [r] -> return (cmi_sfz_rgn_parse (takeDirectory fn) r)
    _ -> error "cmi_load_sfz?"

-- | Pretty-printer.
cmi_sfz_pp :: (String,Cmi_Sfz) -> String
cmi_sfz_pp (nm,(_,_,_,_,l1,(l2,l3),(e1,e2))) = printf "%-24s %c %5d %5d %3.1f %3.1f" nm l1 l2 l3 e1 e2

-- | Load all .sfz files below directory.
--   Names are of the form DISK/VOICE.
cmi_load_dir :: FilePath -> IO [(String,Cmi_Sfz)]
cmi_load_dir dir = do
  fn <- T.dir_find_ext_rel ".sfz" dir
  let nm_seq = map dropExtension (sort fn)
  cmi_seq <- mapM (cmi_load_sfz . (dir <>)) (sort fn)
  return (zip nm_seq cmi_seq)
