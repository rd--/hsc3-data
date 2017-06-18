module Sound.SC3.Data.Array.CSV where

import Sound.OSC.Type {- hosc -}

import qualified Music.Theory.Array.CSV as T {- hmt -}

-- | /ty/ gives the type tag for each /column/ of the table.
array_to_datum :: [Datum_Type] -> [[String]] -> [[Datum]]
array_to_datum ty tbl =
    let f row = zipWith parse_datum_err ty row
    in map f tbl

-- | 'array_to_datum' of 'T.csv_table_read_def'.
csv_read_datum :: [Datum_Type] -> FilePath -> IO [[Datum]]
csv_read_datum ty fn = do
  tbl <- T.csv_table_read_def id fn
  return (array_to_datum ty tbl)

-- | Write array of 'Datum' to CSV file.
-- /k/ is the precision to write floating point values at.
csv_write_datum :: Int -> FilePath -> [[Datum]] -> IO ()
csv_write_datum fp_prec fn tbl =
    let tbl' = map (map (datumPP (Just fp_prec))) tbl
    in T.csv_table_write_def id fn tbl'
