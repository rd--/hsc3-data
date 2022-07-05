-- | <https://www.adventurekid.se/AKRTfiles/AKWF/view/waveforms_index.html>
module Sound.Sc3.Data.Wt.Akwf where

import Control.Monad {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import System.FilePath {- filepath -}
import Text.Printf {- base -}

import qualified Sound.Osc.Core as O {- hosc -}

import qualified Sound.Sc3.Common.Buffer as Sc3 {- hsc3 -}
import qualified Sound.Sc3.Server.Command.Plain as Sc3 {- hsc3 -}
import qualified Sound.Sc3.Server.Transport.Monad as Sc3 {- hsc3 -}

import qualified Sound.File.Header as Sf {- hsc3-sf -}
import qualified Sound.File.Wave as Sf {- hsc3-sf -}

type Name = String

-- | (NAME,[ENTRIES])
type Akwf_Grp = (Name, [String])

-- | Extension, ie. ".wav".
type Extension = String

type Directory = FilePath

akwf_fn :: Extension -> Directory -> Name -> FilePath
akwf_fn ext dir k = concat ["AKWF_",dir,"/AKWF_",k,ext]

-- > mapM_ putStrLn $ concatMap (akwf_gen_fn ".wav") akwf_grp
akwf_gen_fn :: Extension -> Akwf_Grp -> [FilePath]
akwf_gen_fn ext (dir,sq) = map (akwf_fn ext dir) sq

-- > map (\k -> show_int_k k 5) [2,4] == ["05","0005"]
show_int_k :: Int -> Int -> String
show_int_k = printf "%0*ld"

-- > gen_nseq 4 (0,100) []
gen_nseq :: Int -> (Int, Int) -> [Int] -> [String]
gen_nseq k (p,q) z = map (show_int_k k) (filter (`notElem` z) [p .. q])

gen_pseq_flt :: Int -> String -> (Int, Int) -> [Int] -> [String]
gen_pseq_flt k p n z = map (p ++) (gen_nseq k n z)

gen_pseq :: Int -> String -> (Int, Int) -> [String]
gen_pseq k p n = gen_pseq_flt k p n []

akwf_gen_nm_grp :: String -> Int -> Akwf_Grp
akwf_gen_nm_grp nm n = (nm,gen_pseq 4 (nm ++ "_") (1,n))

akwf_gen_ntables :: Int -> Akwf_Grp
akwf_gen_ntables n =
  let lhs = ((n - 1) * 100) + 1
  in (show_int_k 4 n,gen_nseq 4 (lhs,lhs + 99) [])

-- > akwf_lookup "bw_sawrounded"
akwf_lookup :: Name -> Maybe Akwf_Grp
akwf_lookup nm = find ((== nm) . fst) akwf_grp

akwf_lookup_err :: Name -> Akwf_Grp
akwf_lookup_err = fromMaybe (error "akwf_lookup") . akwf_lookup

-- > akwf_filenames ".wav" "bw_sawrounded"
akwf_filenames :: Extension -> Name -> [FilePath]
akwf_filenames ext = akwf_gen_fn ext . akwf_lookup_err

akwf_filepaths_grp :: Extension -> Directory -> Akwf_Grp -> [FilePath]
akwf_filepaths_grp ext dir = map (dir </>) . akwf_gen_fn ext

-- > akwf_filepaths ".wav" "." "bw_sawrounded"
akwf_filepaths :: Extension -> Directory -> Name -> [FilePath]
akwf_filepaths ext dir = akwf_filepaths_grp ext dir . akwf_lookup_err

akfw_get_png_cmd :: FilePath -> Akwf_Grp -> [(String,String,Maybe String)]
akfw_get_png_cmd dir grp =
  let png_loc = "https://www.adventurekid.se/AKRTfiles/AKWF/view/"
      uri_seq = akwf_filepaths_grp ".png" png_loc grp
      fn_seq = akwf_filepaths_grp ".png" dir grp
  in zip3 uri_seq fn_seq (repeat Nothing)

akfw_png_ix_grp :: FilePath -> Akwf_Grp -> [String]
akfw_png_ix_grp dir (cat,sq) =
  let f x = "![](" ++ (dir </> akwf_fn ".png" cat x) ++ ")"
  in ("# " ++ cat ++ "\n") : "" : map f sq ++ [""]

akfw_png_ix_all :: FilePath -> [String]
akfw_png_ix_all dir = concatMap (akfw_png_ix_grp dir) akwf_grp

-- | Load file-set to Sc3, at consecutive buffers from /k/, resampling to 512 elements.
akwf_load_to_sc3_cmd :: [FilePath] -> Sc3.Buffer_Id -> IO [O.Message]
akwf_load_to_sc3_cmd fn_set k = do
  let alloc_f ix = Sc3.b_alloc_setn1 ix 0
  fn_dat <- mapM akwf_load_512 fn_set
  return (zipWith alloc_f [k ..] (map Sc3.to_wavetable fn_dat))

akwf_resolve_grp_set :: Directory -> [(Name,Maybe [Int])] -> [FilePath]
akwf_resolve_grp_set dir nm_seq =
  let sel l u = map fst (filter (\(_,n) -> n `elem` u) (zip l [0..]))
  in concatMap (\(nm,u) -> let sq = akwf_filepaths ".wav" dir nm
                           in case u of
                                Nothing -> sq
                                Just u' -> sel sq u') nm_seq

akwf_load_to_sc3 :: Directory -> [(Name,Maybe [Int])] -> Sc3.Buffer_Id -> IO Int
akwf_load_to_sc3 dir nm_seq k = do
  let fn_set = akwf_resolve_grp_set dir nm_seq
  m <- akwf_load_to_sc3_cmd fn_set k
  Sc3.withSc3 (mapM_ Sc3.async m)
  return (length fn_set)

-- | Plain load function, check file is mono and frame count is 600.
akwf_load :: FilePath -> IO [Double]
akwf_load nm = do
  (hdr,dat) <- Sf.wave_load nm
  when (Sf.frameCount hdr /= 600 || Sf.channelCount hdr /= 1) (error "akwf_load")
  return (dat !! 0)

-- | Resampling variant of 'akwf_load'.
akwf_load_512 :: FilePath -> IO [Double]
akwf_load_512 = fmap (Sc3.resamp1 512) . akwf_load

akwf_load_grp_set :: Directory -> [(Name,Maybe [Int])] -> IO [[Double]]
akwf_load_grp_set dir nm_seq = do
  let fn_set = akwf_resolve_grp_set dir nm_seq
  mapM akwf_load_512 fn_set

akwf_grp :: [Akwf_Grp]
akwf_grp = concat [akwf_ntables,akwf_instr,akwf_bw,akwf_misc]

akwf_ntables :: [Akwf_Grp]
akwf_ntables = map akwf_gen_ntables [1 .. 20]

akwf_instr_set :: [(String,Int)]
akwf_instr_set =
  [("aguitar",38)
  ,("altosax",26)
  ,("cello",19)
  ,("clarinett",25)
  ,("clavinet",33)
  ,("dbass",69)
  ,("ebass",70)
  ,("eguitar",22)
  ,("eorgan",154)
  ,("epiano",73)
  ,("flute",17)
  ,("oboe",13)
  ,("piano",30)
  ,("violin",14)]

akwf_instr :: [Akwf_Grp]
akwf_instr = map (uncurry akwf_gen_nm_grp) akwf_instr_set

akwf_bw :: [Akwf_Grp]
akwf_bw =
  [("bw_blended",gen_pseq_flt 4 "blended_" (1,80) [10,20,23,27,34,40,43])
  ,("bw_perfectwaves",["saw","sin","squ","tri"])
  ,("bw_saw",gen_pseq 4 "saw_" (1,50))
  ,("bw_sawbright",gen_pseq 4 "bsaw_" (1,10))
  ,("bw_sawgap",gen_pseq 4 "gapsaw_" (1,42))
  ,("bw_sawrounded",gen_pseq 2 "R_asym_saw_" (1,26) ++ gen_pseq 2 "R_sym_saw_" (1,26))
  ,("bw_sin",gen_pseq 4 "sin_" (1,12))
  ,("bw_squ",gen_pseq 4 "squ_" (1,100))
  ,("bw_squrounded",gen_pseq 2 "rAsymSqu_" (1,26) ++ gen_pseq 2 "rSymSqu_" (1,26))
  ,("bw_tri",gen_pseq 4 "tri_" (1,25))]

akwf_misc :: [Akwf_Grp]
akwf_misc =
  [("birds",gen_pseq 4 "birds_" (1,14))
  ,("bitreduced"
   ,concat
     [gen_pseq 4 "bitreduced_" (1,40)
     ,["saw2bit","saw3bit","saw4bit","saw5bit","saw6bit","saw7bit","saw8bit"
      ,"sin2bit","sin3bit","sin4bit","sin5bit","sin6bit","sin7bit","sin8bit"
      ,"squ2bit","squ3bit","squ4bit","squ5bit","squ6bit","squ7bit","squ8bit"
      ,"tri2bit","tri3bit","tri4bit","tri5bit","tri6bit","tri7bit","tri8bit"]])
  ,("c604",gen_pseq 4 "c604_" (1,32))
  ,("distorted",gen_pseq 4 "distorted_" (1,45))
  ,("fmsynth",gen_pseq 4 "fmsynth_" (1,122))
  ,("granular",gen_pseq 4 "granular_" (1,44))
  ,("hdrawn",gen_pseq 4 "hdrawn_" (1,50))
  ,("hvoice",gen_pseq 4 "hvoice_" (1,104))
  ,("linear",gen_pseq 4 "linear_" (1,85))
  ,("oscchip",gen_pseq 4 "oscchip_" (1,158))
  ,("overtone",gen_pseq 4 "overtone_" (1,44))
  ,("pluckalgo",gen_pseq 4 "pluckalgo_" (1,9))
  ,("raw",gen_pseq 4 "raw_" (1,36))
  ,("sinharm",gen_pseq 4 "sinharm_" (1,16))
  ,("snippets",gen_pseq 4 "snippet_" (1,47))
  ,("stereo",gen_pseq 4 "stereo_" (1,200))
  ,("stringbox",gen_pseq 4 "cheeze_" (1,6))
  ,("symetric",gen_pseq 4 "symetric_" (1,17))
  ,("theremin",gen_pseq 4 "tannerin_" (1,4) ++ gen_pseq 4 "theremin_" (1,22))
  ,("vgame",gen_pseq 4 "vgame_" (1,137))
  ,("vgamebasic",
     concat
     [gen_pseq 4 "vgsaw_" (1,16)
     ,gen_pseq 4 "vgsin_" (1,16)
     ,gen_pseq 4 "vgsqu_" (1,16)
     ,gen_pseq 4 "vgtri_" (1,16)])
  ]
