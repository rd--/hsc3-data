-- | <https://www.propellerheads.se/dx7-px7-converter/>
module Sound.Sc3.Data.Yamaha.Dx7.Px7 where

import Data.List {- base -}
import Data.Maybe {- base -}

import Sound.Sc3.Data.Yamaha.Dx7 {- hsc3-data -}

{- | Non-operator Px7 parameter names paired with Dx7 parameter index.

> length px7_param_names_rem == 19
> sort (map snd px7_param_names_rem) == [126 .. 144]
-}
px7_param_names_rem :: [(String, U8)]
px7_param_names_rem =
  [ ("Algorithm", 134)
  , ("Feedback", 135)
  , ("Transpose", 144)
  , ("LFO_Waveform", 142)
  , ("LFO_Speed", 137)
  , ("LFO_PitchModSens", 143)
  , ("LFO_PMD", 139)
  , ("LFO_AMD", 140)
  , ("LFO_Sync", 141)
  , ("LFO_Delay", 138)
  , ("Oscillator_Sync", 136)
  , ("Pitch_EG_R1_reversed", 126)
  , ("Pitch_EG_R2_reversed", 127)
  , ("Pitch_EG_R3_reversed", 128)
  , ("Pitch_EG_R4_reversed", 129)
  , ("Pitch_EG_L1", 130)
  , ("Pitch_EG_L2", 131)
  , ("Pitch_EG_L3", 132)
  , ("Pitch_EG_L4", 133)
  ]

{- | Operator parameter names, without operator prefix, with Dx7 index.

> length px7_param_names_op_tbl == 21
> 19 + 21 * 6 == 145
> sort (map snd px7_param_names_op_tbl) == [0 .. 20]
-}
px7_param_names_op_tbl :: [(String, U8)]
px7_param_names_op_tbl =
  [ ("FreqMode", 17)
  , ("FreqCoarse", 18)
  , ("FreqFine", 19)
  , ("Detune", 20)
  , ("KRS", 13)
  , ("EG_R1_reversed", 0)
  , ("EG_R2_reversed", 1)
  , ("EG_R3_reversed", 2)
  , ("EG_R4_reversed", 3)
  , ("EG_L1", 4)
  , ("EG_L2", 5)
  , ("EG_L3", 6)
  , ("EG_L4", 7)
  , ("OutputLevel", 16)
  , ("KLS_LeftDepth", 9)
  , ("KLS_RightDepth", 10)
  , ("KLS_LeftCurve", 11)
  , ("KLS_RightCurve", 12)
  , ("KLS_BreakPoint", 8)
  , ("KeybVelSens", 15)
  , ("LFO_AmplitudeModSens", 14)
  ]

-- | Generate parameter data for operator /n/ with Dx7 parameter index.
px7_param_names_op :: U8 -> [(String, U8)]
px7_param_names_op n =
  let f (nm, ix) = ("OP" ++ show n ++ "_" ++ nm, ix + (21 * (6 - n)))
  in map f px7_param_names_op_tbl

{- | Complete set of Px7 parameters with Dx7 indices in sequence given in Px7 file.

> length px7_param_seq == 145
> sort (map snd px7_param_seq) == [0 .. 144]
-}
px7_param_seq :: [(String, U8)]
px7_param_seq = px7_param_names_rem ++ concatMap px7_param_names_op [1 .. 6]

{- | Rate values are stored reversed (ie. Px7 /n/ is Dx7 /99 - n/ and vice versa).

> map px7_reverse [27,23,89,67] == [72,76,10,32]
> map px7_reverse [72,76,10,32] == [27,23,89,67]
-}
px7_reverse :: Num a => a -> a
px7_reverse n = 99 - n

{- | Predicate over parmeter name for reversed value.

> map (\(nm,ix) -> (nm,px7_param_is_reversed nm,ix)) px7_param_seq
-}
px7_param_is_reversed :: String -> Bool
px7_param_is_reversed nm = reverse "_reversed" == take 9 (reverse nm)

-- | Table of (Px7,Dx7) parameter indices.
px7_ix_tbl :: [(U8, U8)]
px7_ix_tbl = zip [0 ..] (map snd px7_param_seq)

{- | Given Px7 index, lookup Dx7 index.

> px7_ix_to_dx7_ix 0 == 134
-}
px7_ix_to_dx7_ix :: U8 -> U8
px7_ix_to_dx7_ix =
  fromMaybe (error "px7_ix_to_dx7_ix")
    . flip lookup px7_ix_tbl

-- | 'fromIntegral' of 'findIndex'.
find_index_generic :: Num b => (a -> Bool) -> [a] -> Maybe b
find_index_generic f = fmap fromIntegral . findIndex f

{- | Inverse of 'px7_ix_to_dx7_ix'.

> px7_ix_from_dx7_ix 134 == 0
-}
px7_ix_from_dx7_ix :: U8 -> U8
px7_ix_from_dx7_ix k =
  fromMaybe
    (error "px7_ix_from_dx7_ix")
    (find_index_generic ((==) k . snd) px7_param_seq)

-- | Px7 data is a sequence of 145 U8.
type Px7_Param = [U8]

-- | Translate data sequence in Px7 order to Dx7 order, reverse data as required.
px7_param_data_to_dx7 :: Px7_Param -> Dx7_Param
px7_param_data_to_dx7 =
  let f (nm, ix) d = (ix, if px7_param_is_reversed nm then px7_reverse d else d)
  in map snd . sort . zipWith f px7_param_seq

-- | Inverse of 'px7_param_data_to_dx7'.
px7_param_data_from_dx7 :: Dx7_Param -> Px7_Param
px7_param_data_from_dx7 =
  let f (nm, ix) d =
        ( px7_ix_from_dx7_ix ix
        , if px7_param_is_reversed nm then px7_reverse d else d
        )
  in map snd . sort . zipWith f (sortOn snd px7_param_seq)
