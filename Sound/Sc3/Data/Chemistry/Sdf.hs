{- | Sdf/Mol file format.

The Sdf file format is a superset of the Mol file format.

<https://en.wikipedia.org/wiki/Chemical_table_file#SDF>
-}
module Sound.Sc3.Data.Chemistry.Sdf where

import qualified Data.List {- base -}
import qualified Data.Maybe {- base -}

import qualified Data.List.Split {- split -}

import qualified Sound.Sc3.Data.Chemistry.Mol as Mol {- hsc3-data -}

-- * Associated Data Items

-- | (Key,[Value])
type Sdf_Adi = (String, [String])

{- | Read the associated data items entries from an Sdf file.

> txt <- readFile "/home/rohan/rd/j/2020-02-22/sdf/DB01452.sdf"
> txt <- readFile "/home/rohan/rd/j/2026-07-24/sdf/73415757.sdf"
> putStrLn $ sdf_adi_pp $ sdf_adi txt
-}
sdf_adi :: String -> [Sdf_Adi]
sdf_adi =
  let un_key =
        takeWhile (/= '>')
          . Data.Maybe.fromMaybe (error "sdf_adi: non-key?")
          . Data.List.stripPrefix "> <"
      not_term = (/=) "$$$$"
      not_end = (/=) "M  END"
      rem_null = filter (not . null)
      f ln = case ln of
        k : v -> (un_key k, v)
        _ -> error "sdf_adi: no-key?"
  in map f
      . rem_null
      . Data.List.Split.splitWhen null
      . takeWhile not_term
      . tail
      . dropWhile not_end
      . lines

sdf_adi_pp :: [Sdf_Adi] -> String
sdf_adi_pp =
  let f (k, v) = concat [k, ": ", Data.List.intercalate "\\n" v]
  in unlines . map f

-- * Load

-- | 'sdf_adi' of 'readFile'.
sdf_load_adi :: FilePath -> IO [Sdf_Adi]
sdf_load_adi = fmap sdf_adi . readFile

{- | 'sdf_load_adi' of 'mol_dir_filenames'.

> adi <- sdf_load_dir_adi ".sdf" "/home/rohan/rd/j/2020-02-22/sdf/"
> mapM_ (putStrLn . sdf_adi_pp) adi
-}
sdf_load_dir_adi :: String -> FilePath -> IO [[Sdf_Adi]]
sdf_load_dir_adi ext dir = do
  fn <- Mol.mol_dir_filenames ext dir
  mapM sdf_load_adi fn
