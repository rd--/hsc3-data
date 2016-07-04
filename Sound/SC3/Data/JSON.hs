module Sound.SC3.Data.JSON where

import Data.Maybe {- base -}

import qualified Data.Aeson as A {- aeson -}
import qualified Data.ByteString.Lazy as B {- bytestring -}
import qualified Data.HashMap.Strict as M {- unordered-containers -}
import qualified Data.Text as T {- text -}

json_raw :: B.ByteString -> Maybe A.Object
json_raw = A.decode

obj_lookup :: String -> A.Object -> Maybe A.Value
obj_lookup k o = M.lookup (T.pack k) o

obj_lookup_err :: String -> A.Object -> A.Value
obj_lookup_err k o =
    let r = obj_lookup k o
        err = error ("obj_lookup: " ++ k ++ " -- " ++ show o)
    in fromMaybe err r

obj_lookup_str :: String -> A.Object -> String
obj_lookup_str k = val_string . obj_lookup_err k

obj_lookup_int :: String -> A.Object -> Int
obj_lookup_int k = val_int . obj_lookup_err k

obj_lookup_obj_list :: String -> A.Object -> [A.Object]
obj_lookup_obj_list k = val_obj_list . obj_lookup_err k

res_value :: A.Result t -> t
res_value r =
    case r of
      A.Error e -> error e
      A.Success r' -> r'

val_string :: A.Value -> String
val_string = res_value . A.fromJSON

val_int :: A.Value -> Int
val_int = res_value . A.fromJSON
--val_int = fromMaybe (error "val_int") . S.toBoundedInteger . res_value . A.fromJSON

val_list :: A.Value -> [A.Value]
val_list = res_value . A.fromJSON
--val_list = V.toList . res_value . A.fromJSON

val_obj_list :: A.Value -> [A.Object]
val_obj_list = res_value . A.fromJSON

-- * IO

read_json_obj :: FilePath -> IO A.Object
read_json_obj fn = do
  b <- B.readFile fn
  case json_raw b of
    Nothing -> error "read_json_obj"
    Just j -> return j
