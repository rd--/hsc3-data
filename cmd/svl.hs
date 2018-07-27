import System.Environment {- base -}

import qualified Sound.SC3.Data.XML.SVL as SVL {- hsc3-data -}

print_text :: Int -> FilePath -> IO ()
print_text q fn =
  SVL.svl_node_p_csec_seq_wr =<<
  SVL.svl_load_node_p (SVL.svl_frame_to_csec q) id fn

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["print","text",q,fn] -> print_text (read q) fn
    _ -> putStrLn "svl print text q:int/100 filename:string"
