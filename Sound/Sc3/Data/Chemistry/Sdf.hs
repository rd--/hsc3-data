{- | Sdf/Mol file format.

The Sdf file format is a superset of the Mol file format.

<https://en.wikipedia.org/wiki/Chemical_table_file#SDF>
-}
module Sound.Sc3.Data.Chemistry.Sdf where

import qualified Data.List {- base -}
import qualified Data.Maybe {- base -}

import System.FilePath {- filepath -}

import qualified Data.List.Split {- split -}

import qualified Sound.Sc3.Data.Chemistry.Mol as Mol {- hsc3-data -}

-- * Associated Data Items

-- | (Key,[Value])
type Sdf_Adi = (String, [String])

{- | Read the associated data items entries from an Sdf file.

>>> sdf_entry_adi []

> txt <- readFile "/home/rohan/rd/j/2020-02-22/sdf/DB01452.sdf"
> putStrLn $ sdf_adi_pp $ sdf_entry_adi (lines txt)

> txt <- readFile "/home/rohan/rd/j/2026-07-24/sdf/73415757.sdf"
> putStrLn $ sdf_adi_pp $ sdf_entry_adi (lines txt)
-}
sdf_entry_adi :: [String] -> [Sdf_Adi]
sdf_entry_adi =
  let un_key =
        takeWhile (/= '>')
          . Data.Maybe.fromMaybe (error "sdf_adi: non-key?")
          . Data.List.stripPrefix "> <"
      not_end = (/=) "M  END"
      rem_null = filter (not . null)
      f ln = case ln of
        k : v -> (un_key k, v)
        _ -> error "sdf_adi: no-key?"
  in map f
      . rem_null
      . Data.List.Split.splitWhen null
      . tail
      . dropWhile not_end

sdf_adi_pp :: [Sdf_Adi] -> String
sdf_adi_pp =
  let f (k, v) = concat [k, ": ", Data.List.intercalate "\\n" v]
  in unlines . map f

-- * Load

-- | 'sdf_entry_adi' of 'lines' of 'readFile'.
sdf_load_adi :: FilePath -> IO [Sdf_Adi]
sdf_load_adi = fmap (sdf_entry_adi . lines) . readFile

{- | 'sdf_load_adi' of 'mol_dir_filenames'.

> adi <- sdf_load_dir_adi ".sdf" "/home/rohan/rd/j/2020-02-22/sdf/"
> mapM_ (putStrLn . sdf_adi_pp) adi
-}
sdf_load_dir_adi :: String -> FilePath -> IO [[Sdf_Adi]]
sdf_load_dir_adi ext dir = do
  fn <- Mol.mol_dir_filenames ext dir
  mapM sdf_load_adi fn

{- | Load .sdf file that may have multiple entries.

>>> e <- sdf_load "/home/rohan/data/pdb/components.sdf"
>>> let (m,_) = e !! 12
>>> (Mol.mol_name m,Mol.mol_degree m)
("00C",(17,16))
-}
sdf_load :: FilePath -> IO [(Mol.Mol, [Sdf_Adi])]
sdf_load fn = do
  txt <- readFile fn
  let ln = lines txt
      prt = Data.List.Split.splitWhen (== "$$$$") ln
      -- prt' = if null (last prt) then take (length prt - 1) prt else prt
      f x = (Mol.mol_parse x,sdf_entry_adi x)
  return (map f prt)

{- | Split .sdf file into seperate .sdf files each written to the specified directory.

> sdf_split "/home/rohan/data/pdb/components.sdf" "/home/rohan/data/pdb/components/"
-}
sdf_split :: FilePath -> FilePath -> IO ()
sdf_split fn dir = do
  txt <- readFile fn
  let ln = lines txt
      prt = Data.List.Split.splitWhen (== "$$$$") ln
      f x = if null x
            then return ()
            else writeFile (dir </> head x <.> "sdf") (unlines x)
  mapM_ f prt
