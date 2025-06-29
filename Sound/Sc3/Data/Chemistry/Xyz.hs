{- | Xyz file format.

<http://www.ccl.net/chemistry/resources/messages/1996/10/21.005-dir/index.html>
Coordinates are ordinarily Angstroms.
-}
module Sound.Sc3.Data.Chemistry.Xyz where

import Data.List {- base -}
import System.FilePath {- filepath -}

import qualified Music.Theory.Directory as Directory {- hmt-base -}
import qualified Music.Theory.Read as Read {- hmt-base -}
import qualified Music.Theory.Show as Show {- hmt-base -}

import qualified Music.Theory.Geometry.Vector as Vector {- hmt-base -}

-- | (atomic-symbol,xyz-coordinate)
type Xyz_ATOM = (String, Vector.V3 Double)

-- | (k = n-atoms,d = description,e = [atom]
type Xyz = (Int, String, [Xyz_ATOM])

-- | Number of atoms.
xyz_degree :: Xyz -> Int
xyz_degree (k, _, _) = k

-- | k == |atoms|
xyz_is_valid :: Xyz -> Bool
xyz_is_valid (k, _dsc, ent) = k == length ent

-- | Set of atoms present.
xyz_atom_set :: Xyz -> [String]
xyz_atom_set (_, _, ent) = nub (sort (map fst ent))

{- | The first line is the number of atoms.  This may be preceded by
whitespace and anything following is ignored.
-}
xyz_parse_cnt :: String -> Int
xyz_parse_cnt s =
  case words s of
    k : _ -> read k
    _ -> error ("xyz_parse_cnt: " ++ s)

{- | Each entry describing an atom must contain at least four fields of
information separated by whitespace: the atom's type and its X, Y, and
Z positions.
-}
xyz_parse_entry :: String -> String -> Xyz_ATOM
xyz_parse_entry fn s =
  let rd = Read.read_fractional_allow_trailing_point_err
  in case words s of
      a : x : y : z : _ -> (a, (rd x, rd y, rd z))
      _ -> error ("xyz_parse_entry: " ++ fn)

-- | Parse ".xyz" file.
xyz_parse :: FilePath -> String -> Xyz
xyz_parse fn s =
  case lines s of
    k : dsc : ent -> (xyz_parse_cnt k, dsc, map (xyz_parse_entry fn) ent)
    _ -> error ("xyz_parse: " ++ fn)

{- | Generate Xyz file text.

>>> unlines (xyz_pp 4 (1,"x",[("C",(1,2,3))]))
"1\nx\nC  1.0000 2.0000 3.0000\n"
-}
xyz_pp :: Int -> Xyz -> [String]
xyz_pp k (n_a, dsc, a) =
  let e_pp x = if length x == 2 then x else x ++ " "
      a_pp (e, (x, y, z)) = unwords (e_pp e : map (Show.double_pp k) [x, y, z])
  in [show n_a, dsc] ++ map a_pp a

-- | (minima,maxima) of atoms.
xyz_bounds :: Xyz -> Vector.V2 (Vector.V3 Double)
xyz_bounds (_, _, a) =
  let c = map snd a
  in Vector.v3_bounds c

{- | Load ".xyz" file.

>>> xyz <- xyz_load "/home/rohan/sw/hsc3-data/data/chemistry/cls/xyz/Al12W.xyz"
>>> xyz_bounds xyz
((0.0,0.0,0.0),(7.5803,7.5803,7.5803))
-}
xyz_load :: FilePath -> IO Xyz
xyz_load fn = fmap (xyz_parse fn) (readFile fn)

{- | Write ".xyz" file, /k/ is precision to write co-ordinates to.

> xyz <- xyz_load "/home/rohan/sw/hsc3-data/data/chemistry/cls/xyz/Al12W.xyz"
> xyz_store 6 "/tmp/Al12W.xyz" xyz
-}
xyz_store :: Int -> FilePath -> Xyz -> IO ()
xyz_store k fn = writeFile fn . unlines . xyz_pp k

-- | List of all ".xyz" files at /dir/.
xyz_dir_entries :: FilePath -> IO [FilePath]
xyz_dir_entries = Directory.dir_subset [".xyz"]

-- | Load all ".xyz" files at /dir/.
xyz_load_dir :: FilePath -> IO [(String, Xyz)]
xyz_load_dir dir = do
  fn <- xyz_dir_entries dir
  let nm = map takeBaseName fn
  dat <- mapM (xyz_load . (</>) dir) fn
  return (zip nm dat)

{-

Xyz datafiles specify molecular geometries using a Cartesian
coordinate system.  This simple, stripped-down, ASCII-readable format
is intended to serve as a "transition" format for the XMol series of
applications.  For example, suppose a molecular datafile was in a
format not supported by XMol.  In order to read the data into XMol, it
would be possible to modify the datafile, perhaps by creating a shell
script, so that it fit the relatively lenient requirements of the Xyz
format specification.  Once data is in Xyz format, it may be examined
by XMol, or converted to yet another format.

The Xyz format supports multi-step datasets.  Each step is represented
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

Note that the Xyz format doesn't contain connectivity information.
This intentional omission allows for greater flexibility: to create an
Xyz file, you don't need to know where a molecule's bonds are; you
just need to know where its atoms are.  Connectivity information is
generated automatically for Xyz files as they are read into
XMol-related applications.  Briefly, if the distance between two atoms
is less than the sum of their covalent radii, they are considered
bonded.

-}
