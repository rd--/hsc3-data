-- | Csv Array as Osc Datum
module Sound.SC3.Data.Array.CSV where

import Sound.OSC.Datum {- hosc -}
import Sound.OSC.Datum.Parse {- hosc -}
import Sound.OSC.Datum.Pp {- hosc -}

import qualified Music.Theory.Array.CSV as Csv {- hmt-base -}

-- | /ty/ gives the type tag for each /column/ of the table.
array_to_datum :: [DatumType] -> [[String]] -> [[Datum]]
array_to_datum ty tbl = map (\row -> zipWith parse_datum_err ty row) tbl

-- | 'array_to_datum' of 'Csv.csv_table_read_def'.
csv_read_datum :: [DatumType] -> FilePath -> IO [[Datum]]
csv_read_datum ty fn = do
  tbl <- Csv.csv_table_read_def id fn
  return (array_to_datum ty tbl)

-- | Write array of 'Datum' to CSV file.
-- /fp_prec/ is the precision to write floating point values at.
csv_write_datum :: Int -> FilePath -> [[Datum]] -> IO ()
csv_write_datum fp_prec fn tbl =
  let tbl' = map (map (datumPP (Just fp_prec))) tbl
  in Csv.csv_table_write_def id fn tbl'
