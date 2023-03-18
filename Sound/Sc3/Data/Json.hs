module Sound.Sc3.Data.Json where

import qualified Data.ByteString.Lazy as ByteString {- bytestring -}
import qualified Data.Text as Text {- text -}

import qualified Data.Aeson.Micro as Json {- microaeson -}

type Value = Json.Value

boolean :: Bool -> Value
boolean = Json.Bool

string :: String -> Value
string = Json.String . Text.pack

int :: Int -> Value
int = Json.Number . fromIntegral

double :: Double -> Value
double = Json.Number

type Association = (String, Value)

association :: Association -> Json.Pair
association (k, v) = (Text.pack k, v)

object :: [Association] -> Value
object = Json.object . map association

array :: [Value] -> Value
array = Json.Array

writeFile :: FilePath -> Value -> IO ()
writeFile fn json = ByteString.writeFile fn (Json.encode json)
