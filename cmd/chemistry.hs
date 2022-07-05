import System.Environment {- base -}

import Sound.Sc3.Data.Chemistry.Struct {- hsc3-data -}

usage :: [String]
usage =
  ["chemistry"
  ,"  xyz-to-obj k:int t1:real t2:real xyz-file-name obj-file-name"
  ,"  xyz-to-obj-dir k:int t1:real t2:real xyz-dir obj-dir"]

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["xyz-to-obj",k,t1,t2,xyz_fn,obj_fn] -> xyz_to_obj (read k) (read t1,read t2) xyz_fn obj_fn
    ["xyz-to-obj-dir",k,t1,t2,xyz_dir,obj_dir] -> xyz_to_obj_dir (read k) (read t1,read t2) xyz_dir obj_dir
    _ -> putStrLn (unlines usage)
