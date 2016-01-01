import Data.List {- base -}
import System.Environment {- base -}
import qualified Text.CSV.Lazy.String as C {- lazy-csv -}

import Sound.SC3.Plot {- hsc3-plot -}

csv_load :: (String -> n) -> FilePath -> IO [[n]]
csv_load f fn = do
  s <- readFile fn
  let t = C.fromCSVTable (C.csvTable (C.parseCSV s))
  return (map (map f) t)

csv_load_double :: FilePath -> IO [[Double]]
csv_load_double = csv_load read

csv_load_col :: FilePath -> IO [[Double]]
csv_load_col = fmap transpose . csv_load_double

with_csv_col :: ([[Double]] -> IO r) -> FilePath -> IO r
with_csv_col f fn = csv_load_col fn >>= \c -> f c

csv_plot_1 :: ([[Double]] -> IO ()) -> Int -> FilePath -> IO ()
csv_plot_1 f i = with_csv_col (\c -> f [c !! i])

csv_plot_2 :: (Int,Int) -> FilePath -> IO ()
csv_plot_2 (i,j) = with_csv_col (\c -> plot_p2_ln [zip (c !! i) (c !! j)])

csv_plot_3 :: ([[(Double,Double,Double)]] -> IO ()) -> (Int,Int,Int) -> FilePath -> IO ()
csv_plot_3 f (i,j,k) = with_csv_col (\c -> f [zip3 (c !! i) (c !! j) (c !! k)])

{-
derive_mode :: String -> String
derive_mode md =
    case md of
      "ln" -> "with lines"
      "im" -> "with impulses"
      _ -> error "derive_mode"
-}

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["ln",fn,x] -> csv_plot_1 plotTable (read x) fn
    ["im",fn,x] -> csv_plot_1 plotImpulses (read x) fn
    ["ln",fn,x,y] -> csv_plot_2 (read x,read y) fn
    ["ln",fn,x,y,z] -> csv_plot_3 plot_p3_ln (read x,read y,read z) fn
    ["im",fn,x,y,z] -> csv_plot_3 plot_p3_impulses (read x,read y,read z) fn
    _ -> putStrLn "csv-plot {ln|im} csv-file x [y z]"
