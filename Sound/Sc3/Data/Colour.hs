module Sound.Sc3.Data.Colour where

import qualified Music.Theory.Array.Csv as T {- hmt-base -}

type U8 = Int
type Rgb24 = (U8, U8, U8)

{- | Read (name,red,green,blue) CSV table.

>>> let fn = "/home/rohan/sw/hsc3-data/data/colour/svg.csv"
>>> tbl <- clr_read_csv_rgb24_table fn
>>> lookup "powderblue" tbl
Just (176,224,230)
-}
clr_read_csv_rgb24_table :: FilePath -> IO [(String, Rgb24)]
clr_read_csv_rgb24_table fn = do
  tbl <- T.csv_table_read_def id fn
  let f e =
        case e of
          [nm, r, g, b] -> (nm, (read r, read g, read b))
          _ -> error "clr_read_csv_rgb24_table"
  return (map f tbl)
