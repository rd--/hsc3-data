-- | Homophones <ftp://svr-ftp.eng.cam.ac.uk/pub/comp.speech/data/homophones-1.01.txt>
module Sound.Sc3.Data.Speech.Homophones where

import Data.Char {- base -}
import Data.List {- base -}

import qualified Data.List.Split as Split {- split -}

-- | Case-insenstive string comparison.
str_cmp_ci :: String -> String -> Ordering
str_cmp_ci p q = compare (map toLower p) (map toLower q)

-- | Homophone data.
type Hmph = [[String]]

-- | The original list has each word as an initial word, this uniqifies the list.
hmph_uniq :: Hmph -> Hmph
hmph_uniq = nub . sortOn (map toLower . head) . map (sortBy str_cmp_ci)

-- | Parser, skips /k/ leading lines (header).
hmph_parse :: Int -> String -> Hmph
hmph_parse k = map (Split.splitOn ",") . drop k . lines

-- | Pretty-printer.
hmph_pp :: Hmph -> String
hmph_pp = let f = intercalate "," in unlines . map f

-- | 'hmph_parse' of 'readFile'.
hmph_load :: Int -> FilePath -> IO [[String]]
hmph_load k = fmap (hmph_parse k) . readFile

{-

h <- hmph_load 78 "/home/rohan/sw/hsc3-data/data/speech/homophones-1.01.txt"
length h == 1539
u = hmph_uniq h
length u == 710
writeFile "/home/rohan/sw/hsc3-data/data/speech/homophones.text" (hmph_pp u)

-}
