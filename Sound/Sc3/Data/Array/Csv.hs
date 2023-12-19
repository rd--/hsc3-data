-- | Csv Array as Osc Datum
module Sound.Sc3.Data.Array.Csv where

import Sound.Osc.Datum {- hosc -}
import Sound.Osc.Text {- hosc -}

import qualified Music.Theory.Array.Csv as Csv {- hmt-base -}

-- | /ty/ gives the type tag for each /column/ of the table.
array_to_datum :: [DatumType] -> [[String]] -> [[Datum]]
array_to_datum ty tbl = map (\row -> zipWith parseDatum ty row) tbl

-- | 'array_to_datum' of 'Csv.csv_table_read_def'.
csv_read_datum :: [DatumType] -> FilePath -> IO [[Datum]]
csv_read_datum ty fn = do
  tbl <- Csv.csv_table_read_def id fn
  return (array_to_datum ty tbl)

{- | Write array of 'Datum' to CSV file.
/fp_prec/ is the precision to write floating point values at.
-}
csv_write_datum :: Int -> FilePath -> [[Datum]] -> IO ()
csv_write_datum fp_prec fn tbl =
  let tbl' = map (map (showDatum (Just fp_prec))) tbl
  in Csv.csv_table_write_def id fn tbl'
