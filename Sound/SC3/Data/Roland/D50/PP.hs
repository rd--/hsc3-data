-- | D50 / PRETTY-PRINTERS
module Sound.SC3.Data.Roland.D50.PP where

import Data.Char {- base -}
import Data.Either {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Data.List.Split as Split {- split -}

import qualified Music.Theory.List as T {- hmt -}

import Sound.SC3.Data.Byte {- hsc3-data -}
import Sound.SC3.Data.Roland.D50 {- hsc3-data -}

-- * STRUCTURE

-- | Tone structure number diagram in plain text (zero indexed).
--
-- > map d50_structure_pp [0 .. 6]
d50_structure_pp :: U8 -> String
d50_structure_pp n =
    case n + 1 of
      1 -> "S1 + S2"
      2 -> "S1 + RMOD (S1 + S2)"
      3 -> "P1 + S2"
      4 -> "P1 + RMOD (P1 + S2)"
      5 -> "S1 + RMOD (S1 + P2)"
      6 -> "P1 + P2"
      7 -> "P1 + RMOD (P1 + P2)"
      _ -> error "structure_text: ix?"

-- | USR string variant of 'd50_chorus_type_enum'.
d50_chorus_type_usr :: String
d50_chorus_type_usr = intercalate ";" (map (map toUpper . filter (/= ' ')) d50_chorus_type_enum)

-- * CHORUS

-- | Names of chorus types (1-8). (6-8 CHAR)
d50_chorus_type_enum :: [String]
d50_chorus_type_enum =
    ["Chorus 1","Chorus 2"
    ,"Flanger1","Flanger2"
    ,"FBChorus" -- Feedback Chorus
    ,"Tremolo","C Trem" -- Chorus Tremolo
    ,"Dimensn"] -- Dimension

-- * GROUP

{- | (GROUP-NAME,PARAMETER-NAME-SEQ,PARAMETER-IX-SEQ)

The names are as given in the D-50 editor display (2-LINE, 40-CHAR).
The GROUP-NAME (9-CHAR) is printed at the right of the first line.
The PARAMETER-NAME-SEQ (4-CHAR) is semi-colon separated in left to right sequence.

-}
type PARAM_GROUP = (String,String,[U24])

-- | Group structure of partial parameters, as in D-50 menu system.
--
-- > maximum (map (\(nm,_,_) -> length nm) d50_partial_groups) == 9
-- > maximum (map (\(_,_,ix) -> length ix) d50_partial_groups) == 5
-- > concatMap (\(_,_,ix) -> ix) d50_partial_groups == [0 .. 9] ++ [12,10,11] ++ [13 .. 53]
d50_partial_groups :: [PARAM_GROUP]
d50_partial_groups =
    [("WG Pitch","Cors;Fine;KF",[0..2]) -- WG
    ,("WG Mod","LFO;ENV;Bend",[3..5]) -- WG Modulation
    ,("WG Form","Wave;PCM",[6..7]) -- WG Waveform
    ,("WG PW","PW;Velo;Aftr;LFO;LFOD",[8,9,12,10,11]) -- WG Pulse Width
    ,("TVF","Freq;Reso;KF;BP;Blvl",[13..17]) -- TVF
    ,("TVF ENV 1","Dpth;Velo;DKF;TKF",[18..21]) -- TVF ENV
    ,("TVF ENV 2","T1;T2;T3;T4;T5",[22..26]) -- TVF ENV Time
    ,("TVF ENV 3","L1;L2;L3;SusL;EndL",[27..31]) -- TVF ENV Level
    ,("TVF MOD","LFO;LFOD;Aftr",[32..34]) -- TVF Modulation
    ,("TVA","Levl;Velo;BP;Blvl",[35..38]) -- TVA
    ,("TVA ENV 1","T1;T2;T3;T4;T5",[39..43]) -- TVA ENV Time
    ,("TVA ENV 2","L1;L2;L3;SusL;EndL",[44..48]) -- TVA ENV Level
    ,("TVA ENV 3","Velo;TKF",[49..50])
    ,("TVA MOD","LFO;LFOD;Aftr",[51..53]) -- TVA Modulation
    ]

-- | Group structure of common parameters, as in D-50 menu system.
--   PMut (partial-mute) and PBal (partial-balance) are not in the menu system.
--
-- > concatMap (\(_,_,ix) -> ix) d50_common_groups == [0 .. 10] ++ [46,47] ++ [11 .. 45]
d50_common_groups :: [PARAM_GROUP]
d50_common_groups =
    [("Tone Name Edit",";;;;;;;;;",[0..9])
    ,("Structure","Str;PMut;PBal",[10,46,47]) -- NON-MENU
    ,("P-ENV Edit 1","Velo;TKF",[11..12])
    ,("P-ENV Edit 2","T1;T2;T3;T4",[13..16])
    ,("P-ENV Edit 3","LO;L1;L2;SusL;EndL",[17..21])
    ,("Pitch Mod Edit","LFOD;Levr;Aftr",[22..24])
    ,("LFO-1 Edit","Wave;Rate;Dely;Sync",[25..28])
    ,("LFO-2 Edit","Wave;Rate;Dely;Sync",[29..32])
    ,("LFO-3 Edit","Wave;Rate;Dely;Sync",[33..36])
    ,("EQ Edit","Lf;Lg;Hf;HQ;Hg",[37..41])
    ,("Chorus Edit","Type;Rate;Dpth;Bal",[42..45])
    ]

-- | Group structure of patch parameters.
--   KeyM (key-mode), SP (split-point) and Bal (tone-balance) are not in the menu system.
--
-- > concatMap (\(_,_,ix) -> ix) d50_patch_groups
d50_patch_groups :: [PARAM_GROUP]
d50_patch_groups =
    [("Patch Name Edit",";;;;;;;;;;;;;;;;;",[0..17])
    ,("MAIN","KeyMod;Spl;Bal",[18,19,33]) -- NON-MENU
    ,("Tone Tune","LKey;UKey;LTun;UTun",[23,22,25,24])
    ,("Control Edit","Bend;AfPB;PrtT;PrtM;Hold",[26,27,28,20,21])
    ,("Output Mode Edit","Mode;Rev;Rbal;Vol",[29..32])
    ,("Chase Edit","Mode;Levl;Time",[34..36])
    ]

-- | 'PARAM_GROUP' in ADDRESS sequence.
--
-- > maximum (map (\(nm,_,_) -> length nm) (concat d50_group_seq)) == 16
d50_group_seq :: [[PARAM_GROUP]]
d50_group_seq =
    let tn = [d50_partial_groups,d50_partial_groups,d50_common_groups]
    in concat [tn,tn,[d50_patch_groups]]

-- | Pretty printer for parameter group.
d50_group_pp :: [(D50_Parameter,U8)] -> PARAM_GROUP -> String
d50_group_pp x_seq (g_nm,p_nm_seq,ix) =
    let f p_nm (p,x) = let x_def = d50_parameter_value_usr_def "?" p x
                       in if null p_nm {- ie. CHAR -} then x_def else concat [p_nm,"=",x_def]
        gr_p = zipWith f (Split.splitOn ";" p_nm_seq) (map (u24_at x_seq) ix)
    in T.pad_right ' ' 16 g_nm ++ " -> " ++ unwords gr_p

{- | Pretty printer for D-50 patch following group structure (ie. HW screen layout).

> dir = "/home/rohan/uc/invisible/light/d50/"
> p:_ <- d50_load_hex (dir ++ "d50.hex.text")
> putStrLn$unlines$d50_patch_group_pp p
> writeFile (dir ++ "d50.group.text") (unlines (d50_patch_group_pp p))
-}
d50_patch_group_pp :: D50_Patch -> [String]
d50_patch_group_pp =
    let f gr pr = "" : d50_parameter_type_pp (fst pr) : map (d50_group_pp (snd pr)) gr
    in concat . zipWith f d50_group_seq . d50_parameter_segment . zip d50_parameters_seq

-- * ABBREV

-- | Table to abbreviate further (to 3 CHAR) the already abbreviated parameter names.
--   There are not entries for cases where the abbreviation is simply the first three letters.
d50_param_name_abbrev_tbl :: [(String,String)]
d50_param_name_abbrev_tbl =
  [("Cors","Crs"),("Fine","Fne"),("Bend","Bnd"),("LFOD","LFD") -- Wave=Wav Velo=Vel Aftr=Aft
  ,("Freq","Frq"),("Dpth","Dep") -- Reso=Res Blvl=Blv SusL=Sus EndL=End
  ,("Levl","Lvl")
  ,("PMut","Mut"),("PBal","Bal"),("Levr","Lvr")
  ,("Dely","Dly"),("Sync","Snc") -- Rate=Rat
  -- Type=Typ
  ,("LKey","LKy"),("UKey","UKy"),("LTun","LTn"),("UTun","UTn")
  ,("AfPB","APB"),("PrtT","PrT"),("PrtM","PrM"),("Hold","Hld")
  ,("Mode","Mde"),("Rbal","Bal"),("Time","Tme")]

-- | Lookup 'd50_param_name_abbrev_tbl'.
d50_param_name_abbrev :: String -> String
d50_param_name_abbrev nm =
  case lookup nm d50_param_name_abbrev_tbl of
    Nothing -> take 3 nm
    Just r -> r

-- * AREA

-- | [(NAME,IX,CHAR-WIDTH)]
type PARAM_AREA = [(String,U24,Int)]

-- | Given area names and group lengths and char-width table convert to areas.
--   Widths are 3 CHAR unless specified.
d50_param_groups_to_areas :: [Int] -> [(U24,Int)] -> [PARAM_GROUP] -> [PARAM_AREA]
d50_param_groups_to_areas pl wd gr =
  let spl = Split.splitPlaces pl
      (_,g_nm,g_ix) = unzip3 gr
      a_nm = map (concatMap (Split.splitOn ";")) (spl g_nm)
      a_ix = map concat (spl g_ix)
      f nm ix = (nm,ix,fromMaybe 3 (lookup ix wd))
  in zipWith (zipWith f) a_nm a_ix

-- | Partial data in areas.
d50_partial_areas :: [PARAM_AREA]
d50_partial_areas =
  let wd = [(7,6),(16,4),(37,4)]
  in d50_param_groups_to_areas [4,5,5] wd d50_partial_groups

-- | Common data in areas.
d50_common_areas :: [PARAM_AREA]
d50_common_areas =
  d50_param_groups_to_areas [1,5,5] [] d50_common_groups

-- | Patch data in areas.
d50_patch_areas :: [PARAM_AREA]
d50_patch_areas =
  d50_param_groups_to_areas [1,5] [(18,6)] d50_patch_groups

-- | 'PARAM_AREA' in ADDRESS sequence.
d50_area_seq :: [[PARAM_AREA]]
d50_area_seq =
    let tn = [d50_partial_areas,d50_partial_areas,d50_common_areas]
    in concat [tn,tn,[d50_patch_areas]]

-- | Pretty printer for HEADER for parameter area.
--
-- > putStrLn$unlines$map d50_area_hdr_pp (concat [d50_partial_areas,d50_common_areas,d50_patch_areas])
d50_area_hdr_pp :: PARAM_AREA -> String
d50_area_hdr_pp dat =
    let (nm,_,wd) = unzip3 dat
    in unwords (zipWith (T.pad_left ' ') wd (map (map toUpper . d50_param_name_abbrev) nm))

-- | Pretty printer for DATA parameter area.
d50_area_dat_pp :: [(D50_Parameter,U8)] -> PARAM_AREA -> String
d50_area_dat_pp x_seq dat =
    let (_,ix,wd) = unzip3 dat
        f n = let (p,x) = u24_at x_seq n in d50_parameter_value_usr_def "?" p x
    in unwords (zipWith (\k n -> T.pad_left ' ' k (f n)) wd ix)

-- | Make area structure text, entries are of the form (HEADER,[DATA]).
--   The number of DATA entries is 4 for PART (U1 U2 L1 L2), 2 for COMMON (U L) and 1 for PATCH.
--   The number of sets for is 3 for PART, 2 for COMMON and 1 for PATCH.
--   NAME data, ie. for UPPER LOWER and PATCH, is excluded.
d50_patch_area_gen :: D50_Patch -> [[(String, [String])]]
d50_patch_area_gen p =
  let pr_seg = map snd (d50_parameter_segment (zip d50_parameters_seq p))
      mk_dat ar pr = map (d50_area_dat_pp pr) ar
      reorder lst =
        case lst of
          [u1,u2,uc,l1,l2,lc,pm] -> [u1,u2,l1,l2,uc,lc,pm]
          _ -> error "d50_patch_area_pp?"
      dat_ln = concat (zipWith mk_dat (reorder d50_area_seq) (reorder pr_seg))
      dat_ix = [0,3,6,9, 1,4,7,10, 2,5,8,11, 13,16, 14,17, 19]
      dat_sq = Split.splitPlaces ([4,4,4,2,2,1] :: [Int]) (map (dat_ln !!) dat_ix)
      hdr_ln = map d50_area_hdr_pp (concat [d50_partial_areas,d50_common_areas,d50_patch_areas])
      hdr_ix = [0,1,2, 4,5, 7]
      hdr_sq = map (hdr_ln !!) hdr_ix
  in Split.splitPlaces ([3,2,1] :: [Int]) (zip hdr_sq dat_sq)

{- | Pretty printer for D-50 patch following area structure (ie. concise layout).

> dir = "/home/rohan/uc/invisible/light/d50/"
> p:_ <- d50_load_hex (dir ++ "d50.hex.text")
> putStrLn (unlines (d50_patch_area_pp p))
-}
d50_patch_area_pp :: D50_Patch -> [String]
d50_patch_area_pp p =
  let (hdr_sq,dat_sq) = unzip (concat (d50_patch_area_gen p))
  in concat (intersperse [""] (zipWith (:) hdr_sq dat_sq))

d50_area_names :: [[String]]
d50_area_names = [words "WG TVF TVA",words "NAME STR+P LFO+EQ+CH",words "NAME PDAT"]

-- * CSV

-- | Given (ADDR,VALUE) for (TYPE,PARAM) make CSV entry.
d50_parameter_csv :: (D50_ADDRESS,U8) -> (D50_Parameter_Type,D50_Parameter) -> String
d50_parameter_csv (a,x) (ty,p) =
    let (ix,nm,_,_,_usr_str) = p
        x' = d50_parameter_value_verify p x
    in intercalate "," [show a,d50_parameter_type_pp ty,show ix,nm
                       ,show x',d50_range_pp (d50_parameter_range p)
                       ,d50_parameter_value_usr_def "?" p x]

-- | Given sequence of parameter values, generate /CSV/ of patch, unused param are Left.
d50_diff_csv_e :: D50_Diff -> [Either D50_ADDRESS String]
d50_diff_csv_e =
    let f (a,v) =
          case d50_address_to_parameter a of
            Just p -> Right (d50_parameter_csv (a,v) p)
            _ -> if v == 0
                 then Left a
                 else error ("d50_diff_csv_e: VALUE NOT ZERO AT NON-PARAMETER ADDRESS" ++ show (a,v))
    in map f

{- | 'd50_patch_csv_e', if /u/ write unused entries as empty rows, else discard them.

> dir = "/home/rohan/uc/invisible/light/d50/"
> p:_ <- d50_load_hex (dir ++ "d50.hex.text")
> writeFile (dir ++ "d50.csv") (unlines (d50_patch_csv True p))
-}
d50_patch_csv :: Bool -> D50_Patch -> [String]
d50_patch_csv u =
  let hdr = "ADDRESS,PARAMETER-TYPE,INDEX,NAME,VALUE,RANGE,VALUE-USER"
  in (hdr :) .
     (if u then map (either (\a -> show a ++ ",,,,,,") id) else rights) .
     d50_diff_csv_e .
     zip [0..]

-- * CHAR COUNTING

-- | CHAR-count required for each parameter value, grouped in areas.
--
-- > map length d50_partial_char == [13,22,19]
-- > map sum d50_partial_char == [41,64,52]
d50_partial_char :: [[Int]]
d50_partial_char =
  [[3,3,3, 3,3,3, 3,6, 3,3,3,2,3] -- WG
  ,[3,2,3,4,3, 3,3,2,2, 3,3,3,3,3, 3,3,3,3,3, 3,3,3] -- TVF
  ,[3,3,4,3, 3,3,3,3,3, 3,3,3,3,3, 1,1,2,3,2] -- TVA
  ]

-- | CHAR-count required for each parameter value, grouped in areas.
--
-- > map length d50_common_char == [10,15,12,11]
-- > sum [10,15,12,11] == 10 + 38
-- > map sum d50_common_char == [10,35,36,30]
d50_common_char :: [[Int]]
d50_common_char =
  [replicate 10 1 -- NAME
  ,[1, 1,1, 2,2,2,2, 3,3,3,3,3, 3,3,3] -- STR+P
  ,[3,3,3,3, 3,3,3,3, 3,3,3,3] -- LFO
  ,[3,3, 3,3,3, 1,3,3,3, 2,3] -- EQ+CH
  ]
