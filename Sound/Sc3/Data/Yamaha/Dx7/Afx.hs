{- | <​https://github.com/everythingwillbetakenaway/DX7-Supercollider>

Afx files are text files with one 145-element Px7.Px7_Param listing per line.
Data is written as two-digit decimal numbers (00-99).
-}
module Sound.Sc3.Data.Yamaha.Dx7.Afx where

import Data.Char {- base -}

import qualified Data.ByteString.Char8 as B {- bytestring -}

import Sound.Sc3.Data.Yamaha.Dx7 {- hsc3-data -}
import qualified Sound.Sc3.Data.Yamaha.Dx7.Px7 as Px7 {- hsc3-data -}

-- * Util

{- | Parse two digit decimal number, ie. in 0-99.

>>> afx_parse_u8 ('3','2')
32
-}
afx_parse_u8 :: (Char, Char) -> U8
afx_parse_u8 (c1, c2) = digitToInt c1 * 10 + digitToInt c2

{- | Inverse of 'afx_parse_u8'.

>>> map (afx_parse_u8 . afx_encode_u8) [0 .. 99] == [0 .. 99]
True
-}
afx_encode_u8 :: U8 -> (Char, Char)
afx_encode_u8 n = let (p, q) = n `divMod` 10 in (intToDigit p, intToDigit q)

-- * Afx

-- | Allow variant Afx with appended voice name.
afx_entry_verify :: B.ByteString -> Bool
afx_entry_verify = (\n -> n == 290 || n == 310) . B.length

{- | Each entry is 145 two digit decimal numbers with no spaces.
The data is in Px7 patch file sequence.
-}
afx_parse_line :: B.ByteString -> Px7.Px7_Param
afx_parse_line s =
  let f k = afx_parse_u8 (B.index s (k * 2), B.index s (k * 2 + 1))
  in if afx_entry_verify s then map f [0 .. 144] else error "afx_parse_line"

-- | 'afx_parse_line' of 'B.lines'.
afx_parse :: B.ByteString -> [Px7.Px7_Param]
afx_parse = map afx_parse_line . B.lines

{- | Load Afx file (in Px7 order).

>>> d <- afx_load "/home/rohan/opt/src/everythingwillbetakenaway/DX7-Supercollider/DX7.afx"
>>> length d
16384

>>> map length d == replicate 16384 145
True

>>> let d0 = d !! 0
>>> let d0_dx7 = Px7.px7_param_data_to_dx7 d0
>>> d0 == Px7.px7_param_data_from_dx7 d0_dx7
True

>>> afx_encode d0
"17072404510300000139000532043950505050000100070716606538969993009900000000000000000100070700316000999999978200650000000000000100070700167732999280008909000000340000000100070700577729999760007492750102390000000100070700097744809385007499160200480000000200070700140053779298001453200000390000"
-}
afx_load :: FilePath -> IO [Px7.Px7_Param]
afx_load = fmap afx_parse . B.readFile

-- | Afx pretty printer (inverse of 'afx_parse_line').
afx_encode :: Px7.Px7_Param -> String
afx_encode = concatMap ((\(p, q) -> [p, q]) . afx_encode_u8)

-- | Write Afx file (in Px7 order).
afx_store :: FilePath -> [Px7.Px7_Param] -> IO ()
afx_store fn = writeFile fn . unlines . map afx_encode

-- * Dx7

{- | Load Afx file in Dx7 order, ie. run 'Px7.px7_param_data_to_dx7' at each entry.

Note that the Afx data does not include a voice name, hence 'Dx7_Param' and not 'Dx7_Voice'.

>>> d <- afx_load_dx7 "/home/rohan/opt/src/everythingwillbetakenaway/DX7-Supercollider/DX7.afx"
>>> length d
16384

>>> let d0 = d !! 0
>>> length d0
145

> import Sound.Sc3.Data.Yamaha.Dx7.Pp
> putStrLn $ unlines $ dx7_parameter_seq_pp (True,False) d0

>>> import qualified Music.Theory.List as List
>>> let n = dx7_parameter_index "ALGORITHM #"
>>> h = List.histogram (map (!! n) d)
>>> map snd h
[550,1550,1024,638,1906,513,525,462,788,257,357,322,199,351,851,1286,880,1485,317,96,39,574,135,106,76,105,56,197,251,61,128,299]

> import Data.Bifunctor
> import Sound.Sc3.Plot
> plot_p2_stp [h]
-}
afx_load_dx7 :: FilePath -> IO [Dx7_Param]
afx_load_dx7 = fmap (map Px7.px7_param_data_to_dx7) . afx_load

-- | 'afx_encode' of 'Px7.px7_param_data_from_dx7'
afx_dx7_encode :: Dx7_Param -> String
afx_dx7_encode = afx_encode . Px7.px7_param_data_from_dx7

-- | 'writeFile' of 'afx_dx7_encode'.
afx_store_dx7 :: FilePath -> [Dx7_Param] -> IO ()
afx_store_dx7 fn = writeFile fn . unlines . map afx_dx7_encode
