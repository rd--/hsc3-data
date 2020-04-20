{- | XYZ file format.

<http://www.ccl.net/chemistry/resources/messages/1996/10/21.005-dir/index.html>
Coordinates are ordinarily Angstroms.
-}
module Sound.SC3.Data.Chemistry.XYZ where

import Data.List {- base -}
import System.FilePath {- filepath -}

import qualified Music.Theory.Directory as T {- hmt -}
import qualified Music.Theory.Show as T {- hmt -}

import Data.CG.Minus.Plain {- hcg-minus -}

-- | Allow elided 0 before decimal place.
--
-- > map read_r ["-.5",".5"] == [-0.5,0.5]
read_r :: String -> Double
read_r s =
  case s of
    '-':'.':s' -> read ('-':'0':'.':s')
    '.':_ -> read ('0':s)
    _ -> read s

-- | (atomic-symbol,xyz-coordinate)
type XYZ_ATOM = (String,V3 Double)

-- | (k = n-atoms,d = description,e = [atom]
type XYZ = (Int,String,[XYZ_ATOM])

-- | Number of atoms.
xyz_degree :: XYZ -> Int
xyz_degree (k,_,_) = k

-- | k == |atoms|
xyz_is_valid :: XYZ -> Bool
xyz_is_valid (k,_dsc,ent) = k == length ent

-- | Set of atoms present.
xyz_atom_set :: XYZ -> [String]
xyz_atom_set (_,_,ent) = nub (sort (map fst ent))

{- | The first line is the number of atoms.  This may be preceded by
whitespace and anything following is ignored. -}
xyz_parse_cnt :: String -> Int
xyz_parse_cnt s =
  case words s of
    k:_ -> read k
    _ -> error ("xyz_parse_cnt: " ++ s)

{- | Each entry describing an atom must contain at least four fields of
information separated by whitespace: the atom's type and its X, Y, and
Z positions. -}
xyz_parse_entry :: String -> String -> XYZ_ATOM
xyz_parse_entry fn s =
  case words s of
    a:x:y:z:_ -> (a,(read_r x,read_r y,read_r z))
    _ -> error ("xyz_parse_entry: " ++ fn)

-- | Parse ".xyz" file.
xyz_parse :: FilePath -> String -> XYZ
xyz_parse fn s =
  case lines s of
    k:dsc:ent -> (xyz_parse_cnt k,dsc,map (xyz_parse_entry fn) ent)
    _ -> error ("xyz_parse: " ++ fn)

-- | Generate XYZ file text.
--
-- > unlines (xyz_pp 4 (1,"x",[("C",(1,2,3))])) == "1\nx\nC  1.0000 2.0000 3.0000\n"
xyz_pp :: Int -> XYZ -> [String]
xyz_pp k (n_a,dsc,a) =
  let e_pp x = if length x == 2 then x else x ++ " "
      a_pp ((e,(x,y,z))) = intercalate " " (e_pp e : map (T.double_pp k) [x,y,z])
  in [show n_a,dsc] ++ map a_pp a

-- | (minima,maxima) of atoms.
xyz_bounds :: XYZ -> V2 (V3 Double)
xyz_bounds (_,_,a) =
  let c = map snd a
      r = unzip3 c
  in (v3_uop minimum r,v3_uop maximum r)

-- | Load ".xyz" file.
--
-- > xyz <- xyz_load "/home/rohan/sw/hsc3-data/data/chemistry/cls/xyz/Al12W.xyz"
-- > xyz_bounds xyz == ((0.0,0.0,0.0),(7.5803,7.5803,7.5803))
xyz_load :: FilePath -> IO XYZ
xyz_load fn = fmap (xyz_parse fn) (readFile fn)

-- | Write ".xyz" file, /k/ is precision to write co-ordinates to.
--
-- > xyz <- xyz_load "/home/rohan/sw/hsc3-data/data/chemistry/cls/xyz/Al12W.xyz"
-- > xyz_store 6 "/tmp/Al12W.xyz" xyz
xyz_store :: Int -> FilePath -> XYZ -> IO ()
xyz_store k fn = writeFile fn . unlines . xyz_pp k

-- | List of all ".xyz" files at /dir/.
xyz_dir_entries :: FilePath -> IO [FilePath]
xyz_dir_entries = T.dir_subset [".xyz"]

-- | Load all ".xyz" files at /dir/.
xyz_load_dir :: FilePath -> IO [(String, XYZ)]
xyz_load_dir dir = do
  fn <- xyz_dir_entries dir
  let nm = map takeBaseName fn
  dat <- mapM (xyz_load . (</>) dir) fn
  return (zip nm dat)

{-

XYZ datafiles specify molecular geometries using a Cartesian
coordinate system.  This simple, stripped-down, ASCII-readable format
is intended to serve as a "transition" format for the XMol series of
applications.  For example, suppose a molecular datafile was in a
format not supported by XMol.  In order to read the data into XMol, it
would be possible to modify the datafile, perhaps by creating a shell
script, so that it fit the relatively lenient requirements of the XYZ
format specification.  Once data is in XYZ format, it may be examined
by XMol, or converted to yet another format.

The XYZ format supports multi-step datasets.  Each step is represented
by a two-line "header," followed by one line for each atom.  The first
line of a step's header is the number of atoms in that step.  This
integer may be preceded by whitespace; anything on the line after the
integer is ignored.  The second line of the header leaves room for a
descriptive string.  This line may be blank, or it may contain some
information pertinent to that particular step, but it must exist, and
it must be just one line long.

Each line of text describing a single atom must contain at least four
fields of information, separated by whitespace: the atom's type (a
short string of alphanumeric characters), and its x-, y-, and
z-positions.  Optionally, extra fields may be used to specify a charge
for the atom, and/or a vector associated with the atom. If an input
line contains five or eight fields, the fifth field is interpreted as
the atom's charge; otherwise, a charge of zero is assumed.  If an
input line contains seven or eight fields, the last three fields are
interpreted as the components of a vector.  These components should be
specified in angstroms.

Note that the XYZ format doesn't contain connectivity information.
This intentional omission allows for greater flexibility: to create an
XYZ file, you don't need to know where a molecule's bonds are; you
just need to know where its atoms are.  Connectivity information is
generated automatically for XYZ files as they are read into
XMol-related applications.  Briefly, if the distance between two atoms
is less than the sum of their covalent radii, they are considered
bonded.

-}
