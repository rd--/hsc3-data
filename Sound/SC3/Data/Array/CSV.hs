module Sound.SC3.Data.Array.CSV where

import Sound.OSC.Type {- hosc -}

import qualified Music.Theory.Array.CSV as T {- hmt -}

array_to_datum :: [Datum_Type] -> [[String]] -> [[Datum]]
array_to_datum ty tbl =
    let f row = zipWith parse_datum_err ty row
    in map f tbl

csv_read_datum :: [Datum_Type] -> FilePath -> IO [[Datum]]
csv_read_datum ty fn = do
  tbl <- T.csv_table_read_def id fn
  return (array_to_datum ty tbl)

csv_write_datum :: Int -> FilePath -> [[Datum]] -> IO ()
csv_write_datum fp_prec fn tbl =
    let tbl' = map (map (datumPP (Just fp_prec))) tbl
    in T.csv_table_write_def id fn tbl'
