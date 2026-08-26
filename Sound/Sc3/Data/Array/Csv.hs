-- | Csv Array as Osc Datum
module Sound.Sc3.Data.Array.Csv where

import qualified Sound.Osc.Datum {- hosc -}
import qualified Sound.Osc.Text {- hosc -}

import qualified Music.Theory.Array.Csv as Csv {- hmt-base -}

-- | /ty/ gives the type tag for each /column/ of the table.
array_to_datum :: [Sound.Osc.Datum.DatumType] -> [[String]] -> [[Sound.Osc.Datum.Datum]]
array_to_datum ty tbl = map (\row -> zipWith Sound.Osc.Text.parseDatum ty row) tbl

-- | 'array_to_datum' of 'Csv.csv_table_read_def'.
csv_read_datum :: [Sound.Osc.Datum.DatumType] -> FilePath -> IO [[Sound.Osc.Datum.Datum]]
csv_read_datum ty fn = do
  tbl <- Csv.csv_table_read_def id fn
  return (array_to_datum ty tbl)

{- | Write array of 'Datum' to CSV file.
/fp_prec/ is the precision to write floating point values at.
-}
csv_write_datum :: Int -> FilePath -> [[Sound.Osc.Datum.Datum]] -> IO ()
csv_write_datum fp_prec fn tbl =
  let tbl' = map (map (Sound.Osc.Text.showDatum (Just fp_prec))) tbl
  in Csv.csv_table_write_def id fn tbl'
