import System.Environment {- base -}

import Sound.SC3.Data.Chemistry.STRUCT {- hsc3-data -}

usage :: [String]
usage =
  ["chemistry"
  ,"  xyz-to-obj k:int t1:real t2:real xyz-file-name obj-file-name"]

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["xyz-to-obj",k,t1,t2,xyz_fn,obj_fn] -> xyz_to_obj (read k) (read t1,read t2) xyz_fn obj_fn
    _ -> putStrLn (unlines usage)
