-- | <​https://github.com/everythingwillbetakenaway/DX7-Supercollider>
module Sound.SC3.Data.Yamaha.DX7.AFX where

import Data.Char {- base -}

import qualified Data.ByteString.Char8 as B {- bytestring -}

import Sound.SC3.Data.Yamaha.DX7
import qualified Sound.SC3.Data.Yamaha.DX7.PX7 as PX7

afx_entry_verify :: B.ByteString -> Bool
afx_entry_verify = (== 290) . B.length

-- | Each entry is 145 two digit decimal numbers with no spaces.
--   The data is in PX7 patch file sequence.
afx_parse_line :: B.ByteString -> [U8]
afx_parse_line s =
  let f k = digitToInt (B.index s (k * 2)) * 10 + digitToInt (B.index s (k * 2 + 1))
  in if afx_entry_verify s then map f [0 .. 144] else error "afx_parse_line"

afx_parse :: B.ByteString -> [[U8]]
afx_parse = map afx_parse_line . B.lines

{- | Load AFX file in PX7 order.

> d <- afx_load "/home/rohan/opt/src/DX7-Supercollider/DX7.afx"
> length d == 16384
> map length d == replicate 16384 145
-}
afx_load :: FilePath -> IO [[U8]]
afx_load = fmap afx_parse . B.readFile

{- | Load AFX file in DX7 order, ie. run 'PX7.px7_param_data_to_dx7' at each entry.

> d <- afx_load_dx7 "/home/rohan/opt/src/DX7-Supercollider/DX7.afx"
> putStrLn$unlines$ dx7_parameter_pp (d !! 0)

-}
afx_load_dx7 :: FilePath -> IO [[U8]]
afx_load_dx7 = fmap (map PX7.px7_param_data_to_dx7) . afx_load
