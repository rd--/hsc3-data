-- | STR functions over 'ByteString'
module Sound.Sc3.Data.String.ByteString where

import Numeric {- base -}

import qualified Data.ByteString.Char8 as S {- bytestring -}
import qualified Data.ByteString.Lex.Fractional as S.F {- bytestring-lexing -}
import qualified Data.ByteString.Lex.Integral as S.I {- bytestring-lexing -}

-- * STR

type STR = S.ByteString

-- > str "STR"
str :: String -> STR
str = S.pack

-- > str_concat (map str ["STR","ING"])
str_concat :: [STR] -> STR
str_concat = S.concat

str_read_err :: String -> Maybe (t,STR) -> t
str_read_err msg = maybe (error msg) (\(i,j) -> if str_null j then i else error msg)

-- > str_read_int (str "12345678901234567890")
str_read_int :: STR -> Int
str_read_int = str_read_err "str_read_int" . S.I.readSigned S.I.readDecimal

-- > str_read_integer (str "12345678901234567890")
str_read_integer :: STR -> Integer
str_read_integer = str_read_err "str_read_integer" . S.I.readSigned S.I.readDecimal

-- > str_read_float (str "1.2345678901234567890")
str_read_float :: STR -> Float
str_read_float = str_read_err "str_read_float" . S.F.readSigned S.F.readDecimal

-- > str_read_double (str "1.2345678901234567890")
str_read_double :: STR -> Double
str_read_double = str_read_err "str_read_double" . S.F.readSigned S.F.readDecimal

-- > str_read_fractional (str "1.2345678901234567890")
str_read_fractional :: Fractional t => STR -> t
str_read_fractional = realToFrac . str_read_double

-- > str_words (str "STR ING")
str_words :: STR -> [STR]
str_words = S.words

-- > str_unwords (map str ["STR","ING"])
str_unwords :: [STR] -> STR
str_unwords = S.unwords

-- > str_lines (str "STR\nING")
str_lines :: STR -> [STR]
str_lines = S.lines

-- > str_unlines (map str ["STR","ING"])
str_unlines :: [STR] -> STR
str_unlines = S.unlines

-- > map (str_null . str) ["STR",""]
str_null :: STR -> Bool
str_null = S.null

-- > str_index (str "STR") 0
str_index :: STR -> Int -> Char
str_index = S.index

str_read_file :: FilePath -> IO STR
str_read_file = S.readFile

str_write_file :: FilePath -> STR -> IO ()
str_write_file = S.writeFile

-- > str_show_integral 1234567890123456789
str_show_integral :: (Integral t,Show t) => t -> STR
str_show_integral = str . show

-- > str_show_real_float 2 1.234567890123456789
str_show_real_float :: (RealFloat t,Show t) => Int -> t -> STR
str_show_real_float k n = str (showFFloat (Just k) n "")

