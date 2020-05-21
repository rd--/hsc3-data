{- | STRUCT, minimal structure data type.

Files in XYZ, MOL/SDF and POSCAR file formats can be loaded into the STRUCT data-type.

There is also a simle plain text format for storing STRUCT data.
-}
module Sound.SC3.Data.Chemistry.STRUCT where

import Data.Maybe {- base -}
import Data.List {- base -}
import System.FilePath {- filepath -}

import Data.CG.Minus.Plain {- hcg-minus -}

import qualified Music.Theory.Directory as T {- hmt -}
import qualified Music.Theory.Graph.OBJ as T {- hmt -}
import qualified Music.Theory.Graph.PLY as T {- hmt -}
import qualified Music.Theory.Graph.Type as T {- hmt -}
import qualified Music.Theory.Show as T {- hmt -}

import qualified Sound.SC3.Data.Chemistry.Elements as E {- hsc3-data -}
import qualified Sound.SC3.Data.Chemistry.MOL as MOL {- hsc3-data -}
import qualified Sound.SC3.Data.Chemistry.PDB.Types as PDB {- hsc3-data -}
import qualified Sound.SC3.Data.Chemistry.POSCAR as POSCAR {- hsc3-data -}
import qualified Sound.SC3.Data.Chemistry.XYZ as XYZ {- hsc3-data -}

-- * TYPES

-- | (atomic-symbol,xyz-coordinate)
type ATOM = (String,V3 Double)

atom_sym :: ATOM -> String
atom_sym (e,_) = e

-- | (i,j), indicies into ATOM sequence (ZERO indexed)
type BOND = (Int,Int)

-- | (name,degree=(n-atoms,n-bonds),description,atoms,bonds)
type STRUCT = (String,(Int,Int),String,[ATOM],[BOND])

struct_name :: STRUCT -> String
struct_name (nm,_,_,_,_) = nm

struct_degree :: STRUCT -> (Int,Int)
struct_degree (_,k,_,_,_) = k

struct_n_atoms :: STRUCT -> Int
struct_n_atoms = fst . struct_degree

struct_n_bonds :: STRUCT -> Int
struct_n_bonds = snd . struct_degree

struct_description :: STRUCT -> String
struct_description (_,_,dsc,_,_) = dsc

struct_atoms :: STRUCT -> [ATOM]
struct_atoms (_,_,_,atm,_) = atm

struct_description_edit :: (String -> String) -> STRUCT -> STRUCT
struct_description_edit f (nm,k,dsc,a,b) = (nm,k,f dsc,a,b)

struct_bonds :: STRUCT -> [BOND]
struct_bonds (_nm,_k,_dsc,_a,b) = b

struct_bonds_atoms :: STRUCT -> [(ATOM,ATOM)]
struct_bonds_atoms (_nm,_k,_dsc,a,b) =
  let f (i,j) = (a !! i,a !! j)
  in map f b

-- | Do atom count and bond count match atom and bond data?
struct_validate :: STRUCT -> Bool
struct_validate (_,(n_a,n_b),_,a,b) = n_a == length a && n_b == length b

-- | Is STRUCT valid and empty?
struct_is_empty :: STRUCT -> Bool
struct_is_empty s =
  case s of
    (_,(0,0),_,[],[]) -> True
    _ -> False

-- * CONNECTION

-- | Covalent radius (angstroms) of element /sym/, defaults to 250.
--
-- > map sym_radius ["Al","C","Cu","Fe","FE","S"]
sym_radius :: Fractional n => String -> n
sym_radius sym =
  let r = E.covalent_radius (E.atomic_number_err False sym)
  in E.picometres_to_angstroms (fromMaybe 250 r)

-- | (-0.8,+0.4)
type TOLERANCE = (Double,Double)

-- | Replaces existing BONDS, if any, with calculated bonds.
struct_calculate_bonds :: TOLERANCE -> STRUCT -> STRUCT
struct_calculate_bonds tol (nm,(n_a,_),dsc,a,_b) =
  let b = map fst (E.calculate_bonds sym_radius tol a)
      n_b = length b
  in (nm,(n_a,n_b),dsc,a,b)

-- * QUERY/EDIT

-- | (minima,maxima) of atoms.
struct_bounds :: STRUCT -> V2 (V3 Double)
struct_bounds (_,_,_,atm,_) =
  let c = map snd atm
      r = unzip3 c
  in (v3_map minimum r,v3_map maximum r)

-- | Apply /f/ at all atom positions.
struct_v3_map :: (V3 Double -> V3 Double) -> STRUCT -> STRUCT
struct_v3_map f (nm,k,dsc,a,b) =
  let (sym,pt) = unzip a
  in (nm,k,dsc,zip sym (map f pt),b)

-- | Translate atom positions so structure is centered at /c/.
struct_center :: V3 Double -> STRUCT -> STRUCT
struct_center c (nm,k,dsc,a,b) =
  let (sym,pt) = unzip a
  in (nm,k,dsc,zip sym (v3_center_at c pt),b)

-- * CONVERT - MOD/SDF, POSCAR, XYZ, PDB

-- | Convert MOL data to STRUCT data.  MOL bond data is one-indexed.
mol_to_struct :: (String,MOL.MOL) -> STRUCT
mol_to_struct (fn,(nm,dsc,a_n,b_n,a,b,_)) =
  let atom_f (i,j) = (j,i)
      bond_f ((i,j),_) = (i - 1,j - 1)
  in (fn,(a_n,b_n),concat [nm," -- ",dsc],map atom_f a,map bond_f b)

-- | Convert POSCAR data to STRUCT data.
poscar_to_struct :: POSCAR.POSCAR_TY -> (String,POSCAR.POSCAR) -> STRUCT
poscar_to_struct ty (nm,p) =
  let k = POSCAR.poscar_degree p
      dsc = POSCAR.poscar_description p
      atoms = POSCAR.poscar_atoms ty p
      swap (i,j) = (j,i)
  in (nm,(k,0),dsc,map swap atoms,[])

-- | Convert XYZ data to STRUCT data.  XYZ files have no connection (bond) data.
xyz_to_struct :: (String,XYZ.XYZ) -> STRUCT
xyz_to_struct (nm,(k,dsc,atoms)) = (nm,(k,0),dsc,atoms,[])

pdb_atom_to_xyz :: (String,[PDB.ATOM]) -> XYZ.XYZ
pdb_atom_to_xyz (dsc,a) =
  let n = length a
  in (n,dsc,zip (map PDB.atom_element_or_name a) (map PDB.atom_coord a))

pdb_atom_to_xyz_wr :: FilePath -> (String, [PDB.ATOM]) -> IO ()
pdb_atom_to_xyz_wr fn = XYZ.xyz_store 5 fn . pdb_atom_to_xyz

pdb_atom_to_struct :: TOLERANCE -> String -> (String, [PDB.ATOM]) -> STRUCT
pdb_atom_to_struct tol nm = struct_calculate_bonds tol . xyz_to_struct . ((,) nm) . pdb_atom_to_xyz

-- * STAT

-- | Print summary of STRUCT data.
struct_stat :: STRUCT -> [String]
struct_stat s =
  let (nm,(n_a,n_b),dsc,a,_) = s
      e = sort (map atom_sym a)
      u = nub e
      f (i,j) = concat [i,": ",j]
      q = [("NAME",nm)
          ,("DESCRIPTION",dsc)
          ,("N-ATOMS",show n_a)
          ,("N-BONDS",show n_b)
          ,("N-ELEMENTS",show (length u))
          ,("ELEMENTS",unwords u)
          ,("FORMULA",E.hill_formula e)
          ,("VALID",show (struct_validate s))]
  in map f q

-- | 'putStrLn' of 'unlines' of 'stuct_stat'
struct_stat_wr :: STRUCT -> IO ()
struct_stat_wr = putStrLn . unlines . struct_stat

-- * FORMAT - STRUCT

{- | Write simple plain text format for STRUCT data.

1. struct-name
2. atom-count bond-count
3. description
4. atom-data: element x y z
   bond-data: atom-ix atom-ix
-}
struct_pp :: Int -> STRUCT -> [String]
struct_pp k (nm,(n_a,n_b),dsc,a,b) =
  let hdr = [nm
            ,unwords (map show [n_a,n_b])
            ,dsc]
      a_pp ((e,(x,y,z))) = unwords (e : map (T.double_pp k) [x,y,z])
      b_pp (p,q) = unwords (map show [p,q])
  in concat [hdr,map a_pp a,map b_pp b]

-- | Read simple plain text format for STRUCT data.
struct_parse :: [String] -> STRUCT
struct_parse txt =
  let err = error ("struct_parse: " ++ unlines txt)
      k_parse s = case words s of
                    [n_a,n_b] -> (read n_a,read n_b)
                    _ -> err
      a_parse s = case words s of
                    [e,x,y,z] -> (e,(read x,read y,read z))
                    _ -> err
      b_parse s = case words s of
                    [p,q] -> (read p,read q)
                    _ -> err
  in case txt of
       nm:k:dsc:dat -> let (n_a,n_b) = k_parse k
                           a = take n_a dat
                           b = drop n_a dat
                       in (nm,(n_a,n_b),dsc,map a_parse a,map b_parse b)
       _ -> err

-- * IO - STRUCT

-- | 'struct_parse' of 'readFile'.
struct_load_txt :: FilePath -> IO STRUCT
struct_load_txt fn = return . struct_parse . lines =<< readFile fn

-- | 'writeFile' of 'struct_pp'.
struct_store_txt :: Int -> FilePath -> STRUCT -> IO ()
struct_store_txt k fn = writeFile fn . unlines . struct_pp k

-- * I/O - MOL/SDF,POSCAR,XYZ

-- | Load STRUCT based on file-extension (mol/sdf,poscar,struct,xyz)
struct_load_ext :: FilePath -> IO STRUCT
struct_load_ext fn =
  let f x = (takeBaseName fn,x)
  in case takeExtension fn of
       ".mol" -> fmap (mol_to_struct . f) (MOL.mol_load fn)
       ".poscar" -> fmap (poscar_to_struct POSCAR.POSCAR_C . f) (POSCAR.poscar_load fn)
       ".sdf" -> fmap (mol_to_struct . f) (MOL.mol_load fn)
       ".struct" -> struct_load_txt fn
       ".xyz" -> fmap (xyz_to_struct . f) (XYZ.xyz_load fn)
       ext -> error (show ("load_struct",fn,ext))

{- | List of all STRUCT, MOL/SDF, POSCAR and XYZ files at /dir/.
     Names are relative.

> struct_dir_entries "/home/rohan/rd/j/2018-09-26/xyz/"
> struct_dir_entries "/home/rohan/rd/j/2020-03-30/mol/"
-}
struct_dir_entries :: FilePath -> IO [FilePath]
struct_dir_entries =
  let ext = words ".mol .poscar .sdf .struct .xyz"
  in T.dir_subset ext

{- | 'struct_load_ext' of 'struct_dir_entries'

> s <- struct_load_dir "/home/rohan/rd/j/2018-09-26/xyz/"
> length s == 268
> maximum (map struct_n_atoms s) == 272
-}
struct_load_dir :: FilePath -> IO [STRUCT]
struct_load_dir dir = struct_dir_entries dir >>= mapM struct_load_ext . map ((</>) dir)

-- * IO - TYPE

-- | 'poscar_to_struct' of 'POSCAR.poscar_load_dir'
load_poscar_structs_ty :: POSCAR.POSCAR_TY -> FilePath -> IO [STRUCT]
load_poscar_structs_ty ty = fmap (map (poscar_to_struct ty)) . POSCAR.poscar_load_dir

-- * GRAPH

-- | 'STRUCT' to 'T.LBL' with 'ATOM' labels at vertices.
struct_to_lbl :: STRUCT -> T.LBL ATOM ()
struct_to_lbl (_,_,_,a,b) = (zip [0..] a,zip b (repeat ()))

-- | 'T.v3_graph_to_ply' of 'struct_to_lbl', element names are discarded.
struct_to_ply :: Int -> STRUCT -> [String]
struct_to_ply k =
  let f (v,e) = (map (fmap snd) v,e)
  in T.v3_graph_to_ply (Just k) . f . struct_to_lbl

-- | 'T.v3_graph_to_obj' of 'struct_to_lbl', element names are discarded.
struct_to_obj :: Int -> STRUCT -> [String]
struct_to_obj k =
  let f (v,e) = (map (fmap snd) v,e)
  in T.v3_graph_to_obj k . f . struct_to_lbl

-- * CONVERT

ext_to_obj :: Int -> Maybe TOLERANCE -> FilePath -> FilePath -> IO ()
ext_to_obj k tol xyz_fn obj_fn = do
  s <- fmap (maybe id struct_calculate_bonds tol) (struct_load_ext xyz_fn)
  writeFile obj_fn (unlines (struct_to_obj k s))

ext_to_obj_dir :: String -> Int -> Maybe TOLERANCE -> FilePath -> FilePath -> IO ()
ext_to_obj_dir ext k tol ext_dir obj_dir = do
  fn <- T.dir_subset [ext] ext_dir
  let rw x = obj_dir </> replaceExtension (takeFileName x) ".obj"
      cv x = ext_to_obj k tol x (rw x)
  mapM_ cv fn

-- | Load XYZ file, calculate bonds, write OBJ file.
xyz_to_obj :: Int -> TOLERANCE -> FilePath -> FilePath -> IO ()
xyz_to_obj k t = ext_to_obj k (Just t)

-- | 'xyz_to_obj' at all XYZ files at /xyz_dir/.  Output files are written to /obj_dir/.
xyz_to_obj_dir :: Int -> TOLERANCE -> FilePath -> FilePath -> IO ()
xyz_to_obj_dir k t = ext_to_obj_dir ".xyz" k (Just t)

-- | Load POSCAR file, calculate bonds, write OBJ file.
poscar_to_obj :: Int -> TOLERANCE -> FilePath -> FilePath -> IO ()
poscar_to_obj k t = ext_to_obj k (Just t)

-- | 'poscar_to_obj' at all POSCAR files at /poscar_dir/.  Output files are written to /obj_dir/.
poscar_to_obj_dir :: Int -> TOLERANCE -> FilePath -> FilePath -> IO ()
poscar_to_obj_dir k t = ext_to_obj_dir ".poscar" k (Just t)

{-
-- * I/O - DIR

-- | 'mol_to_struct' of 'MOL.mol_load_dir', extension is either ".mol" or ".sdf"
load_mol_structs :: String -> FilePath -> IO [STRUCT]
load_mol_structs ext = fmap (map mol_to_struct) . MOL.mol_load_dir ext

load_poscar_structs :: FilePath -> IO [STRUCT]
load_poscar_structs = load_poscar_structs_ty POSCAR.POSCAR_C

-- | 'xyz_to_struct' of 'XYZ.xyz_load_dir'
load_xyz_structs :: FilePath -> IO [STRUCT]
load_xyz_structs = fmap (map xyz_to_struct) . XYZ.xyz_load_dir
-}
