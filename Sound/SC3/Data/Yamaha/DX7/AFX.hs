-- | <​https://github.com/everythingwillbetakenaway/DX7-Supercollider>
module Sound.SC3.Data.Yamaha.DX7.AFX where

import qualified Data.ByteString.Char8 as B {- bytestring -}

import qualified Music.Theory.Byte as Byte {- hmt -}

import Sound.SC3.Data.Yamaha.DX7 {- hsc3-data -}
import qualified Sound.SC3.Data.Yamaha.DX7.PX7 as PX7 {- hsc3-data -}

-- * UTIL

-- | Parse two digit number, ie. in 0-99.
--
-- > u8_parse_c2 ('3','2') == 32
u8_parse_c2 :: (Char,Char) -> U8
u8_parse_c2 (c1,c2) = Byte.digit_to_word8 c1 * 10 + Byte.digit_to_word8 c2

-- | Inverse of 'u8_parse_c2'.
--
-- > map (u8_parse_c2 . u8_pp_c2) [0 .. 99] == [0 .. 99]
u8_pp_c2 :: U8 -> (Char, Char)
u8_pp_c2 n = let (p,q) = n `divMod` 10 in (Byte.word8_to_digit p,Byte.word8_to_digit q)

-- * AFX

-- | Allow variant AFX with appended voice name.
afx_entry_verify :: B.ByteString -> Bool
afx_entry_verify = (\n -> n == 290 || n == 310) . B.length

-- | AFX data is a sequence of 145 U8 in PX7 order.
type AFX = [U8]

-- | Each entry is 145 two digit decimal numbers with no spaces.
--   The data is in PX7 patch file sequence.
afx_parse_line :: B.ByteString -> AFX
afx_parse_line s =
  let f k = u8_parse_c2 (B.index s (k * 2),B.index s (k * 2 + 1))
  in if afx_entry_verify s then map f [0 .. 144] else error "afx_parse_line"

-- | 'afx_parse_line' of 'B.lines'.
afx_parse :: B.ByteString -> [AFX]
afx_parse = map afx_parse_line . B.lines

{- | Load AFX file (in PX7 order).

> d <- afx_load "/home/rohan/opt/src/DX7-Supercollider/DX7.afx"
> length d == 16384
> map length d == replicate 16384 145
> let d0:_ = d
> let d0_dx7 = PX7.px7_param_data_to_dx7 d0
> d0 == PX7.px7_param_data_from_dx7 d0_dx7
> afx_pp d0

-}
afx_load :: FilePath -> IO [AFX]
afx_load = fmap afx_parse . B.readFile

-- | AFX pretty printer (inverse of 'afx_parse_line').
afx_pp :: AFX -> String
afx_pp = concatMap ((\(p,q) -> [p,q]) . u8_pp_c2)

-- | Write AFX file (in PX7 order).
afx_store :: FilePath -> [AFX] -> IO ()
afx_store fn = writeFile fn . unlines . map afx_pp

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

> import Sound.SC3.Plot {- hsc3-plot -}
> plotImpulses [map snd h]

> import Data.Bifunctor {- base -}
> import qualified Music.Theory.Math.Convert as T {- hmt -}
> import Sound.SC3.Plot {- hsc3-plot -}
> plot_p2_stp [map (bimap T.word8_to_int id) h]

-}
afx_load_dx7 :: FilePath -> IO [DX7_Param]
afx_load_dx7 = fmap (map PX7.px7_param_data_to_dx7) . afx_load

afx_dx7_pp :: DX7_Param -> String
afx_dx7_pp = afx_pp . PX7.px7_param_data_from_dx7

afx_store_dx7 :: FilePath -> [DX7_Param] -> IO ()
afx_store_dx7 fn = writeFile fn . unlines . map afx_dx7_pp
