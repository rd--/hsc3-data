module Sound.SC3.Data.WT.AKWF where

import Control.Monad {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import System.FilePath {- filepath -}
import Text.Printf {- base -}

import qualified Sound.OSC.Core as O {- hosc -}

import qualified Sound.SC3.Common.Buffer as SC3 {- hsc3 -}
import qualified Sound.SC3.Server.Command.Plain as SC3 {- hsc3 -}
import qualified Sound.SC3.Server.Transport.Monad as SC3 {- hsc3 -}

import qualified Sound.File.Header as SF {- hsc3-sf -}
import qualified Sound.File.WAVE as SF {- hsc3-sf -}

import qualified WWW.Minus.Fetch as WWW {- www-minus -}

-- > mapM_ putStrLn $ concatMap (akwf_gen_fn ".wav") akwf_grp
akwf_gen_fn :: String -> AKWF_GRP -> [FilePath]
akwf_gen_fn ext (dir,sq) = map (\k -> concat ["AKWF_",dir,"/AKWF_",k,ext]) sq

-- > map (\k -> show_int_k k 5) [2,4]
show_int_k :: Int -> Int -> String
show_int_k k n = printf "%0*ld" k n

-- > gen_nseq 4 (0,100) []
gen_nseq :: Int -> (Int, Int) -> [Int] -> [String]
gen_nseq k (p,q) z = map (show_int_k k) (filter (`notElem` z) [p .. q])

gen_pseq_flt :: Int -> String -> (Int, Int) -> [Int] -> [String]
gen_pseq_flt k p n z = map (\s -> p ++ s) (gen_nseq k n z)

gen_pseq :: Int -> String -> (Int, Int) -> [String]
gen_pseq k p n = gen_pseq_flt k p n []

akwf_gen_nm_grp :: String -> Int -> AKWF_GRP
akwf_gen_nm_grp nm n = (nm,gen_pseq 4 (nm ++ "_") (1,n))

type AKWF_GRP = (String, [String])
type AKWF_DSC = (String, String, [String])

akwf_gen_ntables :: Int -> AKWF_GRP
akwf_gen_ntables n =
  let lhs = ((n - 1) * 100) + 1
  in (show_int_k 4 n,gen_nseq 4 (lhs,lhs + 99) [])

-- > akwf_lookup "bw_sawrounded"
akwf_lookup :: String -> Maybe AKWF_GRP
akwf_lookup nm = find ((== nm) . fst) akwf_grp

akwf_lookup_err :: String -> AKWF_GRP
akwf_lookup_err = fromMaybe (error "akwf_lookup") . akwf_lookup

-- > akwf_filenames ".wav" "bw_sawrounded"
akwf_filenames :: String -> String -> [FilePath]
akwf_filenames ext = akwf_gen_fn ext . akwf_lookup_err

akwf_filepaths_grp :: String -> FilePath -> AKWF_GRP -> [FilePath]
akwf_filepaths_grp ext dir = map (dir </>) . akwf_gen_fn ext

akwf_filepaths :: String -> FilePath -> String -> [FilePath]
akwf_filepaths ext dir = akwf_filepaths_grp ext dir . akwf_lookup_err

akfw_get_png_cmd :: FilePath -> AKWF_GRP -> [WWW.URL_FETCH]
akfw_get_png_cmd dir grp =
  let png_loc = "https://www.adventurekid.se/AKRTfiles/AKWF/view/"
      uri_seq = akwf_filepaths_grp ".png" png_loc grp
      fn_seq = akwf_filepaths_grp ".png" dir grp
  in zip3 uri_seq fn_seq (repeat Nothing)

-- > mapM_ (akfw_get_png "/home/rohan/data/audio/wt/akwf/png" . akwf_lookup_err . fst) akwf_grp
akfw_get_png :: FilePath -> AKWF_GRP -> IO ()
akfw_get_png dir = mapM_ WWW.url_fetch . akfw_get_png_cmd dir

akwf_load_to_sc3_cmd :: FilePath -> String -> SC3.Buffer_Id -> IO [O.Message]
akwf_load_to_sc3_cmd dir nm k = do
  let fn_set = akwf_filepaths ".wav" dir nm
      alloc_f ix = SC3.b_alloc_setn1 ix 0
  fn_dat <- mapM akwf_load_512 fn_set
  return (zipWith alloc_f [k ..] (map SC3.to_wavetable fn_dat))

{-
> ld = akwf_load_to_sc3 "/home/rohan/data/audio/wt/akwf"
> ld "bw_sawrounded" 0
> ld "bw_squrounded" 0
> ld "bw_tri" 0
> ld "granular" 384
> ld "hdrawn" 512
> ld "birds" 640
> ld "bw_blended" 768
> ld "0001" 896
> ld "0009" 0
> ld "fmsynth" 0
> ld "clarinett" 0
> ld "flute" 0
> ld "oboe" 0
> ld "bitreduced" 0
> ld "c604" 0
> ld "hvoice" 0
> ld "eorgan" 0
> ld "linear" 0
-}
akwf_load_to_sc3 :: FilePath -> String -> SC3.Buffer_Id -> IO ()
akwf_load_to_sc3 dir nm k = do
  m <- akwf_load_to_sc3_cmd dir nm k
  SC3.withSC3 (mapM_ SC3.async m)

akwf_load :: FilePath -> IO [Double]
akwf_load nm = do
  (hdr,dat) <- SF.wave_load nm
  when (SF.frameCount hdr /= 600 || SF.channelCount hdr /= 1) (error "akwf_load")
  return (dat !! 0)

akwf_load_512 :: FilePath -> IO [Double]
akwf_load_512 = fmap (SC3.resamp1 512) . akwf_load

akwf_grp :: [AKWF_GRP]
akwf_grp = concat [akwf_ntables,akwf_instr,akwf_bw,akwf_misc]

akwf_ntables :: [AKWF_GRP]
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

akwf_instr :: [AKWF_GRP]
akwf_instr = map (uncurry akwf_gen_nm_grp) akwf_instr_set

akwf_bw :: [AKWF_GRP]
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

akwf_misc :: [AKWF_GRP]
akwf_misc =
  [("birds",gen_pseq 4 "birds_" (1,14))
  ,("bitreduced"
   ,gen_pseq 4 "bitreduced_" (1,40)
     ++
     ["saw2bit","saw3bit","saw4bit","saw5bit","saw6bit","saw7bit","saw8bit"
     ,"sin2bit","sin3bit","sin4bit","sin5bit","sin6bit","sin7bit","sin8bit"
     ,"squ2bit","squ3bit","squ4bit","squ5bit","squ6bit","squ7bit","squ8bit"
     ,"tri2bit","tri3bit","tri4bit","tri5bit","tri6bit","tri7bit","tri8bit"])
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
  ,("vgame",gen_pseq 4 "vgame_" (1,137))
  ]

{-

/home/rohan/data/audio/wt/akwf/

  ,("theremin","",["tannerin_0001","tannerin_0002","tannerin_0003","tannerin_0004"
                  ,"theremin_0001","theremin_0002","theremin_0003","theremin_0004","theremin_0005","theremin_0006","theremin_0007","theremin_0008","theremin_0009","theremin_0010","theremin_0011","theremin_0012","theremin_0013","theremin_0014","theremin_0015","theremin_0016","theremin_0017","theremin_0018","theremin_0019","theremin_0020","theremin_0021","theremin_0022"])
  ,("vgamebasic","",["vgsaw_0001","vgsaw_0002","vgsaw_0003","vgsaw_0004","vgsaw_0005","vgsaw_0006","vgsaw_0007","vgsaw_0008","vgsaw_0009","vgsaw_0010","vgsaw_0011","vgsaw_0012","vgsaw_0013","vgsaw_0014","vgsaw_0015","vgsaw_0016"
                    ,"vgsin_0001","vgsin_0002","vgsin_0003","vgsin_0004","vgsin_0005","vgsin_0006","vgsin_0007","vgsin_0008","vgsin_0009","vgsin_0010","vgsin_0011","vgsin_0012","vgsin_0013","vgsin_0014","vgsin_0015","vgsin_0016"
                    ,"vgsqu_0001","vgsqu_0002","vgsqu_0003","vgsqu_0004","vgsqu_0005","vgsqu_0006","vgsqu_0007","vgsqu_0008","vgsqu_0009","vgsqu_0010","vgsqu_0011","vgsqu_0012","vgsqu_0013","vgsqu_0014","vgsqu_0015","vgsqu_0016"
                    ,"vgtri_0001","vgtri_0002","vgtri_0003","vgtri_0004","vgtri_0005","vgtri_0006","vgtri_0007","vgtri_0008","vgtri_0009","vgtri_0010","vgtri_0011","vgtri_0012","vgtri_0013","vgtri_0014","vgtri_0015","vgtri_0016"])
  ]
-}
