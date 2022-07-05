import System.Environment {- base -}

import Sound.Sc3.Data.ATS {- hsc3-data -}

header :: FilePath -> IO ()
header fn = ats_read fn >>= putStrLn . ats_header_pp . ats_header

help :: [String]
help =
  ["ats header file-name"
  ,"ats write-au ats-file au-file"]

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["header",fn] -> header fn
    ["write-au",ats_fn,au_fn] -> ats_write_au ats_fn au_fn
    _ -> putStrLn (unlines help)
