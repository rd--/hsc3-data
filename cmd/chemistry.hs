import System.Environment {- base -}

import qualified Sound.Sc3.Data.Chemistry.Struct as Struct {- hsc3-data -}

struct_summary :: FilePath -> IO ()
struct_summary fn = do
  struct <- Struct.struct_load_txt fn
  putStrLn (unlines (Struct.struct_stat struct))

usage :: [String]
usage =
  ["chemistry"
  ,"  struct to-obj k:int t1:real t2:real struct-file-name obj-file-name"
  ,"  struct summary struct-file-name"
  ,"  xyz to-obj k:int t1:real t2:real xyz-file-name obj-file-name"
  ,"  xyz-dir to-obj k:int t1:real t2:real xyz-dir obj-dir"]

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["struct", "to-obj", k, t1, t2, struct_fn, obj_fn] -> Struct.ext_to_obj (read k) (Just (read t1,read t2)) struct_fn obj_fn
    ["struct", "summary", fn] -> struct_summary fn
    ["xyz", "to-obj",k,t1,t2,xyz_fn,obj_fn] -> Struct.xyz_to_obj (read k) (read t1,read t2) xyz_fn obj_fn
    ["xyz-dir", "to-obj",k,t1,t2,xyz_dir,obj_dir] -> Struct.xyz_to_obj_dir (read k) (read t1,read t2) xyz_dir obj_dir
    _ -> putStrLn (unlines usage)
