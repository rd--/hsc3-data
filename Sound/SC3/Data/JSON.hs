-- | JSON
module Sound.SC3.Data.JSON where

import Data.Maybe {- base -}

import qualified Data.Aeson as A {- aeson -}
import qualified Data.ByteString.Lazy as B {- bytestring -}
import qualified Data.HashMap.Strict as Map {- unordered-containers -}
import qualified Data.Text as Text {- text -}

-- * DECODE

-- | Type-specialised decode.
json_decode_array :: B.ByteString -> Maybe A.Array
json_decode_array = A.decode

-- | Type-specialised decode.
json_decode_object :: B.ByteString -> Maybe A.Object
json_decode_object = A.decode

-- | Type-specialised decode.
json_decode_value :: B.ByteString -> Maybe A.Value
json_decode_value = A.decode

-- * ERR

-- | 'error' on 'A.Error'
from_json_err :: A.FromJSON a => A.Value -> a
from_json_err o =
    case A.fromJSON o of
      A.Error err -> error err
      A.Success res -> res

-- * VALUE

value_to_string :: A.Value -> Maybe String
value_to_string o =
    case o of
      A.String t -> Just (Text.unpack t)
      _ -> Nothing

value_to_null :: A.Value -> Maybe ()
value_to_null o =
    case o of
      A.Null -> Just ()
      _ -> Nothing

value_to_string_or_null :: A.Value -> Maybe (Either String ())
value_to_string_or_null o =
    case o of
      A.String t -> Just (Left (Text.unpack t))
      A.Null -> Just (Right ())
      _ -> Nothing

value_to_array :: A.Value -> Maybe A.Array
value_to_array o =
    case o of
      A.Array v -> Just v
      _ -> Nothing

value_to_array_err :: A.Value -> A.Array
value_to_array_err = fromMaybe (error "value_to_array") . value_to_array

value_to_object :: A.Value -> Maybe A.Object
value_to_object v =
    case v of
      A.Object o -> Just o
      _ -> Nothing

value_to_object_err :: A.Value -> A.Object
value_to_object_err = fromMaybe (error "value_to_object") . value_to_object

-- * OBJECT

object_lookup :: String -> A.Object -> Maybe A.Value
object_lookup k o = Map.lookup (Text.pack k) o

object_lookup_err :: String -> A.Object -> A.Value
object_lookup_err k o =
    let r = object_lookup k o
        err = error ("object_lookup: " ++ k ++ " -- " ++ show o)
    in fromMaybe err r

object_lookup_str :: String -> A.Object -> Maybe String
object_lookup_str k = fmap value_to_string_err . object_lookup k

object_lookup_str_err :: String -> A.Object -> String
object_lookup_str_err k = value_to_string_err . object_lookup_err k

object_lookup_str_or_null_err :: String -> A.Object -> Either String ()
object_lookup_str_or_null_err k =
    fromMaybe (error "object_lookup_str_or_null") .
    value_to_string_or_null .
    object_lookup_err k

object_lookup_int_err :: String -> A.Object -> Int
object_lookup_int_err k = value_to_int_err . object_lookup_err k

object_lookup_object_list_err :: String -> A.Object -> [A.Object]
object_lookup_object_list_err k = value_to_object_list_err . object_lookup_err k

-- * VALUE-ERR

result_to_value :: A.Result t -> t
result_to_value r =
    case r of
      A.Error e -> error e
      A.Success x -> x

value_to_string_err :: A.Value -> String
value_to_string_err = result_to_value . A.fromJSON

value_to_int_err :: A.Value -> Int
value_to_int_err = result_to_value . A.fromJSON

value_to_list_err :: A.Value -> [A.Value]
value_to_list_err = result_to_value . A.fromJSON

value_to_object_list_err :: A.Value -> [A.Object]
value_to_object_list_err = result_to_value . A.fromJSON

-- * IO

-- | Read and decode or error
read_json :: (B.ByteString -> Maybe t) -> FilePath -> IO t
read_json decode_f fn = do
  b <- B.readFile fn
  case decode_f b of
    Nothing -> error ("read_json: decode failed: " ++ fn)
    Just j -> return j

-- | Type-specialised reader
read_json_obj :: FilePath -> IO A.Object
read_json_obj = read_json json_decode_object

-- | Type-specialised reader
read_json_array :: FilePath -> IO A.Array
read_json_array = read_json json_decode_array
