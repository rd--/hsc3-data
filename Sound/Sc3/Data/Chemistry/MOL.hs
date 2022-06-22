{- | MOL/SDF file format.

Dalby, A. et al. (1992). "Description of several chemical structure
file formats used by computer programs developed at Molecular Design
Limited". Journal of Chemical Information and Modeling. 32 (3): 244.

<https://pubs.acs.org/doi/abs/10.1021/ci00007a012>

MOL fields are fixed length.

-}
module Sound.Sc3.Data.Chemistry.MOL where

import Data.Char {- base -}
import Data.List {- base -}
import Data.List.Split {- split -}
import Data.Maybe {- base -}
import Safe {- safe -}
import System.Directory {- directory -}
import System.FilePath {- filepath -}

import Data.CG.Minus.Plain {- hcg-minus -}

-- * Types

-- | (xyz-coordinate, atomic-symbol)
type MOL_ATOM = (V3 Double, String)

-- | ((i,j),bond-type)
--   MOL files include BOND data.
--   MOL bond atom-id data is ONE-INDEXED.
type MOL_BOND = (V2 Int, Int)

-- | (name,description,atom-count,bond-count,atoms,bonds,version)
type MOL = (String, String, Int, Int, [MOL_ATOM], [MOL_BOND], Int)

mol_empty :: MOL
mol_empty = ("","",0,0,[],[],2000)

-- * GENERA

mol_version :: String -> Int
mol_version x =
  case x of
    " V2000" -> 2000
    " V3000" -> 3000
    _ -> error "mol_version"

mol_bond_type_tbl :: [(Int,String)]
mol_bond_type_tbl = [(1,"Single"),(2,"Double"),(3,"Triple"),(4,"Aromatic")]

mol_bond_stereo_tbl :: [(Int,String)]
mol_bond_stereo_tbl = [(0,"Not stereo"),(1,"Up"),(6,"Down")]

-- * V20

{- | MOL counts line field lengths.

> length mol_counts_flen == 12
> sum mol_counts_flen == 39
-}
mol_v20_counts_flen :: [Int]
mol_v20_counts_flen = replicate 11 3 ++ [6]

{- | This function returns the COUNT fields (1,2,12), known fields are:

1. number of atoms
2. number of bonds
5. chiral flag (1 = chiral;0 = not chiral)
11. number of lines of additional properties (999 in V3000)
12. version number (V2000 or V3000)

> txt = " 16 16  0  0  0  0  0  0  0  0999 V2000"
> txt = "  5  4  0  0  0  0  0  0  0  0  0"
> mol_v20_read_counts txt
-}
mol_v20_read_counts :: String -> (Int,Int,Int)
mol_v20_read_counts s =
  case splitPlaces mol_v20_counts_flen s of
    [a,b,_,_,_,_,_,_,_,_,_,v] -> (read a,read b,mol_version v)
    a:b:_ -> (read a,read b,mol_version " V2000") -- ALLOW OMITTED FIELDS
    r -> error (show ("mol_v20_read_counts",s,r))

-- > length mol_atom_flen == 17
-- > sum mol_atom_flen == 69
mol_v20_atom_flen :: [Int]
mol_v20_atom_flen = [10,10,10,1,3,2,3] ++ replicate 10 3

-- > mol_v20_read_atom "    0.0000    0.0000    0.0000 S   0  0  0  0  0"
mol_v20_read_atom :: String -> MOL_ATOM
mol_v20_read_atom s =
  case splitPlaces mol_v20_atom_flen s of
    [x,y,z," ",a,_,_,_,_,_,_,_,_,_,_,_,_] -> ((read x,read y,read z),takeWhile (not . isSpace) a)
    x:y:z:" ":a:_ -> ((read x,read y,read z),takeWhile (not . isSpace) a) -- ALLOW OMITTED FIELDS
    r -> error (show ("mol_v20_read_atom",s,r))

-- > length mol_bond_flen == 7
-- > sum mol_bond_flen == 21
mol_v20_bond_flen :: [Int]
mol_v20_bond_flen = replicate 7 3

-- > mol_v20_read_bond "  1  2  2  0  0  0"
mol_v20_read_bond :: String -> MOL_BOND
mol_v20_read_bond s =
  case splitPlaces mol_v20_bond_flen s of
    [a0,a1,ty,_,_,_,_] -> ((read a0,read a1),read ty)
    a0:a1:ty:_ -> ((read a0,read a1),read ty) -- ALLOW OMITTED FIELDS
    r -> error (show ("mol_v20_read_bond",s,r))

mol_v20_parse :: [String] -> MOL
mol_v20_parse l =
  case l of
    [] -> mol_empty -- ALLOW NIL MOL FILES
    nm:dsc:_:cnt:dat ->
      let (a_n,b_n,v) = mol_v20_read_counts cnt
          a = map mol_v20_read_atom (take a_n dat)
          b = map mol_v20_read_bond (take b_n (drop a_n dat))
      in (nm,dsc,a_n,b_n,a,b,v)
    _ -> error (show ("mol_v20_parse",l))

-- * V30

{- | Returns fields (1,2), fields are:

1. number of atoms,
2. number of bonds,
3. number of S-groups,
4. number of 3D constraints,
5. chirality (1 = chiral),
6. molecule reg. no. (OPT)
-}
mol_v30_counts :: String -> (Int,Int)
mol_v30_counts s =
  case words s of
    ["M","V30","COUNTS",a,b,_,_,_] -> (read a,read b)
    _ -> error "mol_v30_counts"

{- | Returns fields ((3,4,5),2), fields are:

1. atom-id
2. name
3. x
4. y
5. z
7. key=value

> mol_v30_atom "M  V30 34 N -8.538 -51.035 -7.336 0 CHG=1"
-}
mol_v30_atom :: String -> MOL_ATOM
mol_v30_atom s =
  case words s of
    "M":"V30":_k:nm:x:y:z:"0":_ -> ((read x,read y,read z),nm)
    _ -> error ("mol_v30_atom: " ++ s)

mol_v30_bond :: String -> MOL_BOND
mol_v30_bond s =
  case words s of
    ["M","V30",_k,ty,a0,a1] -> ((read a0,read a1),read ty)
    _ -> error "mol_v30_atom"

mol_v30_parse :: (String,String) -> [String] -> MOL
mol_v30_parse (nm,dsc) l =
  let ix = atNote "mol_v30_parse"
      verify k s = (ix l k == s) || error (show ("mol_v30_parse",k,l !! k,s))
      (a,b) = mol_v30_counts (ix l 1)
  in if verify 0 "M  V30 BEGIN CTAB" &&
        verify 2 "M  V30 BEGIN ATOM" &&
        verify (a + b + 6) "M  V30 END CTAB"
     then (nm
          ,dsc
          ,a
          ,b
          ,map mol_v30_atom (take a (drop 3 l))
          ,map mol_v30_bond (take b (drop (5 + a) l))
          ,3000)
     else error "mol_v30_parse?"

mol_v30_ent :: [String] -> [String]
mol_v30_ent = filter ("M  V30 " `isPrefixOf`)

-- * ASSOCIATED DATA ITEMS

-- | (KEY,[VALUE])
type MOL_ADI = (String,[String])

{- | Read the associated data items entries from MOL/SDF file.

> txt <- readFile "/home/rohan/rd/j/2020-02-22/sdf/DB01452.sdf"
> putStrLn $ mol_adi_pp $ mol_adi txt
-}
mol_adi :: String -> [MOL_ADI]
mol_adi =
  let un_key = takeWhile (/= '>') . fromMaybe (error "mol_adi: NON-KEY?") . stripPrefix "> <"
      not_term = (/=) "$$$$"
      not_end = (/=) "M  END"
      rem_null = filter (not . null)
      f ln = case ln of
               k:v -> (un_key k,v)
               _ -> error "mol_adi: NO-KEY?"
  in map f . rem_null . splitWhen null . takeWhile not_term . tail . dropWhile not_end . lines

mol_adi_pp :: [MOL_ADI] -> String
mol_adi_pp =
  let f (k,v) = concat [k,": ",intercalate "\\n" v]
  in unlines . map f

-- * LOAD

-- | 'mol_v20_parse' or 'mol_v30_parse' of 'readFile'.
mol_load :: FilePath -> IO MOL
mol_load fn = do
  s <- readFile fn
  let l = lines s
      r = mol_v20_parse l
  case r of
    (nm,dsc,0,0,[],[],3000) -> return (mol_v30_parse (nm,dsc) (mol_v30_ent l))
    _ -> return r

-- | 'mol_adi' of 'readFile'.
mol_load_adi :: FilePath -> IO [MOL_ADI]
mol_load_adi = fmap mol_adi . readFile

-- | List of all .ext files at /dir/.  SDF is a superset of MOL, extensions are ".mol" and ".sdf".
--
-- > mol_dir_entries ".mol" "/home/rohan/rd/j/2020-03-30/mol/"
mol_dir_entries :: String -> FilePath -> IO [FilePath]
mol_dir_entries ext = fmap (filter ((==) ext . takeExtension)) . listDirectory

mol_dir_filenames :: String -> FilePath -> IO [FilePath]
mol_dir_filenames ext dir = do
  fn <- mol_dir_entries ext dir
  return (map (dir </>) fn)

-- | Load all .ext files at directory, extensions are ".mol" or ".sdf"
mol_load_dir :: String -> FilePath -> IO [(String, MOL)]
mol_load_dir ext dir = do
  fn <- mol_dir_entries ext dir
  let nm = map takeBaseName fn
  dat <- mapM (mol_load . (</>) dir) fn
  return (zip nm dat)

-- | 'mol_load_adi' of 'mol_dir_filenames'.
--
-- > adi <- mol_load_dir_adi ".sdf" "/home/rohan/rd/j/2020-02-22/sdf/"
-- > mapM_ (putStrLn . mol_adi_pp) adi
mol_load_dir_adi :: String -> FilePath -> IO [[MOL_ADI]]
mol_load_dir_adi ext dir = do
  fn <- mol_dir_filenames ext dir
  mapM mol_load_adi fn

{-

let fn = "/home/rohan/rd/j/2019-10-08/sdf/5288826.sdf"
m <- mol_load fn
m

let fn = "/home/rohan/rd/j/2020-03-30/mol/1poc.mol"
m <- mol_load fn
m
-}
