-- | STR functions over 'String'
module Sound.Sc3.Data.String.String where

import Numeric {- base -}

type STR = String

-- > str "STR"
str :: String -> STR
str = id

-- > str_concat (map str ["STR","ING"])
str_concat :: [STR] -> STR
str_concat = concat

-- > str_read_int (str "12345678901234567890")
str_read_int :: STR -> Int
str_read_int = read

-- > str_read_integer (str "12345678901234567890")
str_read_integer :: STR -> Integer
str_read_integer = read

-- > str_read_float (str "1.2345678901234567890")
str_read_float :: STR -> Float
str_read_float = read

-- > str_read_double (str "1.2345678901234567890")
str_read_double :: STR -> Double
str_read_double = read

-- > str_read_fractional (str "1.2345678901234567890")
str_read_fractional :: Fractional t => STR -> t
str_read_fractional = realToFrac . str_read_double

-- > str_words (str "STR ING")
str_words :: STR -> [STR]
str_words = words

-- > str_unwords (map str ["STR","ING"])
str_unwords :: [STR] -> STR
str_unwords = unwords

-- > str_lines (str "STR\nING")
str_lines :: STR -> [STR]
str_lines = lines

-- > str_unlines (map str ["STR","ING"])
str_unlines :: [STR] -> STR
str_unlines = unlines

-- > map (str_null . str) ["STR",""]
str_null :: STR -> Bool
str_null = null

-- > str_index (str "STR") 0
str_index :: STR -> Int -> Char
str_index = (!!)

str_read_file :: FilePath -> IO STR
str_read_file = readFile

str_write_file :: FilePath -> STR -> IO ()
str_write_file = writeFile

-- > str_show_integral 1234567890123456789
str_show_integral :: (Integral t, Show t) => t -> STR
str_show_integral = show

-- > str_show_real_float 4 1.234567890123456789
str_show_real_float :: (RealFloat t, Show t) => Int -> t -> STR
str_show_real_float k n = showFFloat (Just k) n ""
