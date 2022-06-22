{- | <​https://github.com/everythingwillbetakenaway/DX7-Supercollider>

AFX files are text files with one 145-element PX7.PX7_Param listing per line.
Data is written as two-digit decimal numbers (00-99).

-}
module Sound.Sc3.Data.Yamaha.DX7.AFX where

import Data.Char {- base -}

import qualified Data.ByteString.Char8 as B {- bytestring -}

import Sound.Sc3.Data.Yamaha.DX7 {- hsc3-data -}
import qualified Sound.Sc3.Data.Yamaha.DX7.PX7 as PX7 {- hsc3-data -}

-- * UTIL

-- | Parse two digit decimal number, ie. in 0-99.
--
-- > afx_parse_u8 ('3','2') == 32
afx_parse_u8 :: (Char,Char) -> U8
afx_parse_u8 (c1,c2) = digitToInt c1 * 10 + digitToInt c2

-- | Inverse of 'afx_parse_u8'.
--
-- > map (afx_parse_u8 . afx_encode_u8) [0 .. 99] == [0 .. 99]
afx_encode_u8 :: U8 -> (Char, Char)
afx_encode_u8 n = let (p,q) = n `divMod` 10 in (intToDigit p,intToDigit q)

-- * AFX

-- | Allow variant AFX with appended voice name.
afx_entry_verify :: B.ByteString -> Bool
afx_entry_verify = (\n -> n == 290 || n == 310) . B.length

-- | Each entry is 145 two digit decimal numbers with no spaces.
--   The data is in PX7 patch file sequence.
afx_parse_line :: B.ByteString -> PX7.PX7_Param
afx_parse_line s =
  let f k = afx_parse_u8 (B.index s (k * 2),B.index s (k * 2 + 1))
  in if afx_entry_verify s then map f [0 .. 144] else error "afx_parse_line"

-- | 'afx_parse_line' of 'B.lines'.
afx_parse :: B.ByteString -> [PX7.PX7_Param]
afx_parse = map afx_parse_line . B.lines

{- | Load AFX file (in PX7 order).

> d <- afx_load "/home/rohan/opt/src/DX7-Supercollider/DX7.afx"
> length d == 16384
> map length d == replicate 16384 145
> let d0:_ = d
> let d0_dx7 = PX7.px7_param_data_to_dx7 d0
> d0 == PX7.px7_param_data_from_dx7 d0_dx7
> afx_encode d0

-}
afx_load :: FilePath -> IO [PX7.PX7_Param]
afx_load = fmap afx_parse . B.readFile

-- | AFX pretty printer (inverse of 'afx_parse_line').
afx_encode :: PX7.PX7_Param -> String
afx_encode = concatMap ((\(p,q) -> [p,q]) . afx_encode_u8)

-- | Write AFX file (in PX7 order).
afx_store :: FilePath -> [PX7.PX7_Param] -> IO ()
afx_store fn = writeFile fn . unlines . map afx_encode

-- * DX7

{- | Load AFX file in DX7 order, ie. run 'PX7.px7_param_data_to_dx7' at each entry.

Note that the AFX data does not include a voice name, hence 'DX7_Param' and not 'DX7_Voice'.

> d <- afx_load_dx7 "/home/rohan/opt/src/DX7-Supercollider/DX7.uniq.afx"
> length d /= 2 ^ 14
> length d == 9606
> let d0:_ = d
> length d0 == 145
> putStrLn$unlines$ dx7_parameter_seq_pp d0

> import qualified Music.Theory.List as T {- hmt -}
> let n = dx7_parameter_index "ALGORITHM #"
> h = T.histogram (map (`Byte.word8_at` n) d)
> map snd h

> import Data.Bifunctor {- base -}
> import qualified Music.Theory.Math.Convert as T {- hmt -}
> import Sound.Sc3.Plot {- hsc3-plot -}
> plot_p2_stp [map (bimap T.word8_to_int id) h]

-}
afx_load_dx7 :: FilePath -> IO [DX7_Param]
afx_load_dx7 = fmap (map PX7.px7_param_data_to_dx7) . afx_load

-- | 'afx_encode' of 'PX7.px7_param_data_from_dx7'
afx_dx7_encode :: DX7_Param -> String
afx_dx7_encode = afx_encode . PX7.px7_param_data_from_dx7

-- | 'writeFile' of 'afx_dx7_encode'.
afx_store_dx7 :: FilePath -> [DX7_Param] -> IO ()
afx_store_dx7 fn = writeFile fn . unlines . map afx_dx7_encode
