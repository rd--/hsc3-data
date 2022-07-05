import Data.Maybe {- base -}
import System.Environment {- base -}

import Sound.Sc3.Data.Chemistry.Pdb {- hsc3-data -}
import Sound.Sc3.Data.Chemistry.Pdb.Parse {- hsc3-data -}
import Sound.Sc3.Data.Chemistry.Pdb.Query {- hsc3-data -}
import Sound.Sc3.Data.Chemistry.Pdb.Types {- hsc3-data -}

cli_txt :: (Dat -> [String]) -> FilePath -> IO ()
cli_txt f pdb_fn = pdb_load_dat pdb_fn >>= putStr . unlines . f

cli_header :: FilePath -> IO ()
cli_header = let pp (x,y,z) = concat [z," - ",y," - ",x] in cli_txt (return . pp . dat_header)

cli_stat :: FilePath -> IO ()
cli_stat = let pp (i,j) = concat [i,": ",j] in cli_txt (map pp . dat_stat)

cli_title :: FilePath -> IO ()
cli_title = cli_txt (\x -> [concat [header_id4 (dat_header x)," - ",title_group (dat_title x)]])

cli_seqres :: Bool -> FilePath -> IO ()
cli_seqres iupac =
  let mk = if iupac then map (fromMaybe '.' . pdb_seqres_code_lookup) else unwords
      pp (c,r) = c : ':' : ' ' : mk r
      sq = map pp . seqres_group . dat_seqres
  in cli_txt (\x -> header_id4 (dat_header x) : sq x)

cli_add_eol :: (t -> IO ()) -> t -> IO ()
cli_add_eol f x = f x >> putStrLn ""

help :: [String]
help =
  ["pdb header file-name..."
  ,"pdb stat file-name..."
  ,"pdb title file-name..."]

main :: IO ()
main = do
  a <- getArgs
  case a of
    "header":fn -> mapM_ cli_header fn
    "seqres":fn -> mapM_ (cli_add_eol (cli_seqres False)) fn
    "seqres-iupac":fn -> mapM_ (cli_add_eol (cli_seqres True)) fn
    "stat":fn -> mapM_ cli_stat fn
    "title":fn -> mapM_ cli_title fn
    _ -> putStrLn (unlines help)
