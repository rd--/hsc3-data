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

--import qualified Sound.Sc3.Server.Command.MemCpy as MemCpy {- sc3-rdu -}

import qualified Sound.File.Header as Sf {- hsc3-sf -}
import qualified Sound.File.Wave as Sf {- hsc3-sf -}

type Name = String

-- | (Name,[Entries])
type Akwf_Grp = (Name, [String])

-- | Extension, ie. ".wav".
type Extension = String

type Directory = FilePath

akwf_fn :: Extension -> Directory -> Name -> FilePath
akwf_fn ext dir k = concat ["AKWF_",dir,"/AKWF_",k,ext]

{- | Akwf Gen Fn

>>> let headErr = (!! 0)
>>> mapM_ putStrLn $ map (headErr . akwf_gen_fn ".wav") akwf_grp
AKWF_0001/AKWF_0001.wav
AKWF_0002/AKWF_0101.wav
AKWF_0003/AKWF_0201.wav
AKWF_0004/AKWF_0301.wav
AKWF_0005/AKWF_0401.wav
AKWF_0006/AKWF_0501.wav
AKWF_0007/AKWF_0601.wav
AKWF_0008/AKWF_0701.wav
AKWF_0009/AKWF_0801.wav
AKWF_0010/AKWF_0901.wav
AKWF_0011/AKWF_1001.wav
AKWF_0012/AKWF_1101.wav
AKWF_0013/AKWF_1201.wav
AKWF_0014/AKWF_1301.wav
AKWF_0015/AKWF_1401.wav
AKWF_0016/AKWF_1501.wav
AKWF_0017/AKWF_1601.wav
AKWF_0018/AKWF_1701.wav
AKWF_0019/AKWF_1801.wav
AKWF_0020/AKWF_1901.wav
AKWF_aguitar/AKWF_aguitar_0001.wav
AKWF_altosax/AKWF_altosax_0001.wav
AKWF_cello/AKWF_cello_0001.wav
AKWF_clarinett/AKWF_clarinett_0001.wav
AKWF_clavinet/AKWF_clavinet_0001.wav
AKWF_dbass/AKWF_dbass_0001.wav
AKWF_ebass/AKWF_ebass_0001.wav
AKWF_eguitar/AKWF_eguitar_0001.wav
AKWF_eorgan/AKWF_eorgan_0001.wav
AKWF_epiano/AKWF_epiano_0001.wav
AKWF_flute/AKWF_flute_0001.wav
AKWF_oboe/AKWF_oboe_0001.wav
AKWF_piano/AKWF_piano_0001.wav
AKWF_violin/AKWF_violin_0001.wav
AKWF_bw_blended/AKWF_blended_0001.wav
AKWF_bw_perfectwaves/AKWF_saw.wav
AKWF_bw_saw/AKWF_saw_0001.wav
AKWF_bw_sawbright/AKWF_bsaw_0001.wav
AKWF_bw_sawgap/AKWF_gapsaw_0001.wav
AKWF_bw_sawrounded/AKWF_R_asym_saw_01.wav
AKWF_bw_sin/AKWF_sin_0001.wav
AKWF_bw_squ/AKWF_squ_0001.wav
AKWF_bw_squrounded/AKWF_rAsymSqu_01.wav
AKWF_bw_tri/AKWF_tri_0001.wav
AKWF_birds/AKWF_birds_0001.wav
AKWF_bitreduced/AKWF_bitreduced_0001.wav
AKWF_c604/AKWF_c604_0001.wav
AKWF_distorted/AKWF_distorted_0001.wav
AKWF_fmsynth/AKWF_fmsynth_0001.wav
AKWF_granular/AKWF_granular_0001.wav
AKWF_hdrawn/AKWF_hdrawn_0001.wav
AKWF_hvoice/AKWF_hvoice_0001.wav
AKWF_linear/AKWF_linear_0001.wav
AKWF_oscchip/AKWF_oscchip_0001.wav
AKWF_overtone/AKWF_overtone_0001.wav
AKWF_pluckalgo/AKWF_pluckalgo_0001.wav
AKWF_raw/AKWF_raw_0001.wav
AKWF_sinharm/AKWF_sinharm_0001.wav
AKWF_snippets/AKWF_snippet_0001.wav
AKWF_stereo/AKWF_stereo_0001.wav
AKWF_stringbox/AKWF_cheeze_0001.wav
AKWF_symetric/AKWF_symetric_0001.wav
AKWF_theremin/AKWF_tannerin_0001.wav
AKWF_vgame/AKWF_vgame_0001.wav
AKWF_vgamebasic/AKWF_vgsaw_0001.wav
-}
akwf_gen_fn :: Extension -> Akwf_Grp -> [FilePath]
akwf_gen_fn ext (dir,sq) = map (akwf_fn ext dir) sq

{- | Show Int k

>>> map (\k -> show_int_k k 5) [2,4]
["05","0005"]
-}
show_int_k :: Int -> Int -> String
show_int_k = printf "%0*ld"

{- | Gen Nseq

>>> gen_nseq 4 (0,100) []
["0000","0001","0002","0003","0004","0005","0006","0007","0008","0009","0010","0011","0012","0013","0014","0015","0016","0017","0018","0019","0020","0021","0022","0023","0024","0025","0026","0027","0028","0029","0030","0031","0032","0033","0034","0035","0036","0037","0038","0039","0040","0041","0042","0043","0044","0045","0046","0047","0048","0049","0050","0051","0052","0053","0054","0055","0056","0057","0058","0059","0060","0061","0062","0063","0064","0065","0066","0067","0068","0069","0070","0071","0072","0073","0074","0075","0076","0077","0078","0079","0080","0081","0082","0083","0084","0085","0086","0087","0088","0089","0090","0091","0092","0093","0094","0095","0096","0097","0098","0099","0100"]
-}
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

{- |

> akwf_lookup "bw_sawrounded"
-}
akwf_lookup :: Name -> Maybe Akwf_Grp
akwf_lookup nm = find ((== nm) . fst) akwf_grp

akwf_lookup_err :: Name -> Akwf_Grp
akwf_lookup_err = fromMaybe (error "akwf_lookup") . akwf_lookup

{- |

> akwf_filenames ".wav" "bw_sawrounded"
-}
akwf_filenames :: Extension -> Name -> [FilePath]
akwf_filenames ext = akwf_gen_fn ext . akwf_lookup_err

akwf_filepaths_grp :: Extension -> Directory -> Akwf_Grp -> [FilePath]
akwf_filepaths_grp ext dir = map (dir </>) . akwf_gen_fn ext

{- |

> akwf_filepaths ".wav" "." "bw_sawrounded"
-}
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
