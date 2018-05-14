module Sound.SC3.Data.WT.AKWF where

import Text.Printf {- base -}

-- > mapM_ putStrLn $ concatMap akwf_gen_fn akwf_grp
akwf_gen_fn :: AKWF_GRP -> [FilePath]
akwf_gen_fn (dir,sq) = map (\k -> concat ["AKWF_",dir,"/AKWF_",k,".wav"]) sq

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

akwf_grp :: [AKWF_GRP]
akwf_grp = concat [akwf_ntables,akwf_instr,akwf_bw,akwf_misc]

akwf_ntables :: [AKWF_GRP]
akwf_ntables = map akwf_gen_ntables [1 .. 20]

akwf_instr :: [AKWF_GRP]
akwf_instr =
  [akwf_gen_nm_grp "aguitar" 38
  ,akwf_gen_nm_grp "altosax" 26
  ,akwf_gen_nm_grp "cello" 19
  ,akwf_gen_nm_grp "clarinett" 25
  ,akwf_gen_nm_grp "clavinet" 33
  ,akwf_gen_nm_grp "dbass" 69
  ,akwf_gen_nm_grp "ebass" 70
  ,akwf_gen_nm_grp "eguitar" 22
  ,akwf_gen_nm_grp "eorgan" 154
  ,akwf_gen_nm_grp "epiano" 73
  ,akwf_gen_nm_grp "flute" 17
  ,akwf_gen_nm_grp "oboe" 13
  ,akwf_gen_nm_grp "piano" 30
  ,akwf_gen_nm_grp "violin" 14]

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


,("bitreduced","bitreduced_",["bitreduced_0001","bitreduced_0002","bitreduced_0003","bitreduced_0004","bitreduced_0005","bitreduced_0006","bitreduced_0007","bitreduced_0008","bitreduced_0009","bitreduced_0010","bitreduced_0011","bitreduced_0012","bitreduced_0013","bitreduced_0014","bitreduced_0015","bitreduced_0016","bitreduced_0017","bitreduced_0018","bitreduced_0019","bitreduced_0020","bitreduced_0021","bitreduced_0022","bitreduced_0023","bitreduced_0024","bitreduced_0025","bitreduced_0026","bitreduced_0027","bitreduced_0028","bitreduced_0029","bitreduced_0030","bitreduced_0031","bitreduced_0032","bitreduced_0033","bitreduced_0034","bitreduced_0035","bitreduced_0036","bitreduced_0037","bitreduced_0038","bitreduced_0039","bitreduced_0040"
                               ,"saw2bit","saw3bit","saw4bit","saw5bit","saw6bit","saw7bit","saw8bit"
                               ,"sin2bit","sin3bit","sin4bit","sin5bit","sin6bit","sin7bit","sin8bit"
                               ,"squ2bit","squ3bit","squ4bit","squ5bit","squ6bit","squ7bit","squ8bit"
                               ,"tri2bit","tri3bit","tri4bit","tri5bit","tri6bit","tri7bit","tri8bit"])
  ,("theremin","",["tannerin_0001","tannerin_0002","tannerin_0003","tannerin_0004"
                  ,"theremin_0001","theremin_0002","theremin_0003","theremin_0004","theremin_0005","theremin_0006","theremin_0007","theremin_0008","theremin_0009","theremin_0010","theremin_0011","theremin_0012","theremin_0013","theremin_0014","theremin_0015","theremin_0016","theremin_0017","theremin_0018","theremin_0019","theremin_0020","theremin_0021","theremin_0022"])
  ,("vgamebasic","",["vgsaw_0001","vgsaw_0002","vgsaw_0003","vgsaw_0004","vgsaw_0005","vgsaw_0006","vgsaw_0007","vgsaw_0008","vgsaw_0009","vgsaw_0010","vgsaw_0011","vgsaw_0012","vgsaw_0013","vgsaw_0014","vgsaw_0015","vgsaw_0016"
                    ,"vgsin_0001","vgsin_0002","vgsin_0003","vgsin_0004","vgsin_0005","vgsin_0006","vgsin_0007","vgsin_0008","vgsin_0009","vgsin_0010","vgsin_0011","vgsin_0012","vgsin_0013","vgsin_0014","vgsin_0015","vgsin_0016"
                    ,"vgsqu_0001","vgsqu_0002","vgsqu_0003","vgsqu_0004","vgsqu_0005","vgsqu_0006","vgsqu_0007","vgsqu_0008","vgsqu_0009","vgsqu_0010","vgsqu_0011","vgsqu_0012","vgsqu_0013","vgsqu_0014","vgsqu_0015","vgsqu_0016"
                    ,"vgtri_0001","vgtri_0002","vgtri_0003","vgtri_0004","vgtri_0005","vgtri_0006","vgtri_0007","vgtri_0008","vgtri_0009","vgtri_0010","vgtri_0011","vgtri_0012","vgtri_0013","vgtri_0014","vgtri_0015","vgtri_0016"])
  ]
-}
