import System.Environment {- base -}

import Sound.SC3.Data.Roland.D50

usage :: IO ()
usage =
    let h = ["print g file-name"]
    in putStrLn (unlines h)

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["print","g",fn] -> d50_load_sysex fn >>= putStrLn . unlines . concatMap d50_patch_group_pp
    _ -> usage
