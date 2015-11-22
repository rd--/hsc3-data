import System.Environment {- base -}

import Sound.SC3.Data.ATS {- hsc3-data -}

header :: FilePath -> IO ()
header fn = ats_read fn >>= putStrLn . ats_header_pp . ats_header

help :: [String]
help = ["ats header file-name"]

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["header",fn] -> header fn
    _ -> putStrLn (unlines help)
