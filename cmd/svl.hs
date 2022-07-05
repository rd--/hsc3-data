import System.Environment {- base -}

import qualified Sound.Sc3.Data.Xml.Svl as Svl {- hsc3-data -}

print_text :: Int -> FilePath -> IO ()
print_text q fn =
  Svl.svl_node_p_csec_seq_wr =<<
  Svl.svl_load_node_p (Svl.svl_frame_to_csec q) id fn

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["print","text",q,fn] -> print_text (read q) fn
    _ -> putStrLn "svl print text q:int/100 filename:string"
