-- | <​https://github.com/everythingwillbetakenaway/DX7-Supercollider>
module Sound.SC3.Data.Yamaha.DX7.AFX where

import qualified Data.List.Split as Split {- split -}

import Sound.SC3.Data.Yamaha.DX7
import qualified Sound.SC3.Data.Yamaha.DX7.PX7 as PX7

-- | Each entry is 145 two digit decimal numbers with no spaces.
--   The data is in PX7 patch file format, the parser runs 'PX7.px7_param_data_to_dx7'.
afx_parse_line :: String -> [U8]
afx_parse_line = PX7.px7_param_data_to_dx7 . map read . Split.chunksOf 2

afx_parse :: String -> [[U8]]
afx_parse = map afx_parse_line . lines

{- Load AFX file.

> d <- afx_load "/home/rohan/opt/src/DX7-Supercollider/DX7.afx"
> length d == 16384
> map length d == replicate 16384 145
> putStrLn$unlines$ dx7_parameter_pp (d !! 0)

-}
afx_load :: FilePath -> IO [[U8]]
afx_load = fmap afx_parse . readFile
