module Sound.SC3.Data.JSON where

import Data.Maybe {- base -}

import qualified Data.Aeson as A {- aeson -}
import qualified Data.ByteString.Lazy as B {- bytestring -}
import qualified Data.HashMap.Strict as M {- unordered-containers -}
import qualified Data.Text as T {- text -}

json_decode_array :: B.ByteString -> Maybe A.Array
json_decode_array = A.decode

json_decode_object :: B.ByteString -> Maybe A.Object
json_decode_object = A.decode

from_json_err :: A.FromJSON a => A.Value -> a
from_json_err o =
    case A.fromJSON o of
      A.Error err -> error err
      A.Success res -> res

value_to_array :: A.Value -> A.Array
value_to_array o =
    case o of
      A.Array v -> v
      _ -> error "value_to_array"

value_to_object :: A.Value -> A.Object
value_to_object v =
    case v of
      A.Object o -> o
      _ -> error "value_to_object"

obj_lookup :: String -> A.Object -> Maybe A.Value
obj_lookup k o = M.lookup (T.pack k) o

obj_lookup_err :: String -> A.Object -> A.Value
obj_lookup_err k o =
    let r = obj_lookup k o
        err = error ("obj_lookup: " ++ k ++ " -- " ++ show o)
    in fromMaybe err r

obj_lookup_str :: String -> A.Object -> Maybe String
obj_lookup_str k = fmap val_string . obj_lookup k

obj_lookup_str_err :: String -> A.Object -> String
obj_lookup_str_err k = val_string . obj_lookup_err k

obj_lookup_int_err :: String -> A.Object -> Int
obj_lookup_int_err k = val_int . obj_lookup_err k

obj_lookup_obj_list_err :: String -> A.Object -> [A.Object]
obj_lookup_obj_list_err k = val_obj_list . obj_lookup_err k

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

read_json :: (B.ByteString -> Maybe t) -> FilePath -> IO t
read_json decode_f fn = do
  b <- B.readFile fn
  case decode_f b of
    Nothing -> error ("read_json: decode failed: " ++ fn)
    Just j -> return j

read_json_obj :: FilePath -> IO A.Object
read_json_obj = read_json json_decode_object

read_json_array :: FilePath -> IO A.Array
read_json_array = read_json json_decode_array
