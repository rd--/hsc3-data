-- | Peterson/Barney (1952) data set giving formant locations.
module Sound.Sc3.Data.Speech.Pb where

import qualified Music.Theory.Array.Csv as Csv {- hmt-base -}

-- | Voice table
pb_vc_tbl :: [(Int, String)]
pb_vc_tbl = zip [1 ..] (words "M F C")

-- | Phoneme table
pb_ph_tbl :: [(Int, String)]
pb_ph_tbl = zip [1 ..] (words "IY IH EH AE AH AA AO UH UW ER")

{- | Pb table entry
    (Voice-Id,Speaker-Id,Phoneme-Id,(F0,F1,F2,F3))
-}
type Pb_Ent = (Int, Int, Int, (Int, Int, Int, Int))

-- | Parse Pb entry.
pb_parse :: [String] -> Pb_Ent
pb_parse l =
  case l of
    [vc, k, ph, _, f0, f1, f2, f3] -> (read vc, read k, read ph, (read f0, read f1, read f2, read f3))
    _ -> error "pb_parse?"

{- | Load Csv and parse run pb_parse over rows.

>>> e <- pb_load "/home/rohan/sw/hsc3-data/data/speech/pb.csv"
>>> length e
1520
-}
pb_load :: FilePath -> IO [Pb_Ent]
pb_load fn = do
  tbl <- Csv.csv_table_read_def id fn
  return (map pb_parse tbl)
