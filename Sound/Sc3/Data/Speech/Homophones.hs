-- | Homophones <ftp://svr-ftp.eng.cam.ac.uk/pub/comp.speech/data/homophones-1.01.txt>
module Sound.Sc3.Data.Speech.Homophones where

import qualified Data.Char {- base -}
import qualified Data.List {- base -}

import qualified Data.List.Split as Split {- split -}

import qualified Music.Theory.List as List {- hmt-base -}

-- | Case-insensitive string comparison.
str_cmp_ci :: String -> String -> Ordering
str_cmp_ci p q = compare (map Data.Char.toLower p) (map Data.Char.toLower q)

-- | Homophone data.
type Hmph = [[String]]

-- | The original list has each word as an initial word, this uniqueifies the list.
hmph_uniq :: Hmph -> Hmph
hmph_uniq =
  Data.List.nub
    . Data.List.sortOn (map Data.Char.toLower . List.head_err)
    . map (Data.List.sortBy str_cmp_ci)

-- | Parser, skips /k/ leading lines (header).
hmph_parse :: Int -> String -> Hmph
hmph_parse k = map (Split.splitOn ",") . drop k . lines

-- | Pretty-printer.
hmph_pp :: Hmph -> String
hmph_pp =
  let f = Data.List.intercalate ","
  in unlines . map f

{- | 'hmph_parse' of 'readFile'.

>>> h <- hmph_load 78 "/home/rohan/sw/hsc3-data/data/speech/homophones-1.01.txt"
>>> length h
1539

>>> let u = hmph_uniq h
>>> length u
710
-}
hmph_load :: Int -> FilePath -> IO [[String]]
hmph_load k = fmap (hmph_parse k) . readFile

{-

writeFile "/home/rohan/sw/hsc3-data/data/speech/homophones.text" (hmph_pp u)

-}
