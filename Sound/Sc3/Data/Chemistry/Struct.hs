{- | Struct, minimal structure data type.

Files in Xyz, Mol/Sdf and Poscar file formats can be loaded into the Struct data-type.

There is also a simle plain text format for storing Struct data.
-}
module Sound.Sc3.Data.Chemistry.Struct where

import Data.Maybe {- base -}
import Data.List {- base -}
import System.FilePath {- filepath -}

import Music.Theory.Geometry.Vector {- hmt-base -}

import qualified Music.Theory.Directory as T {- hmt-base -}
import qualified Music.Theory.Geometry.Obj as Obj {- hmt -}
import qualified Music.Theory.Graph.Type as T {- hmt-base -}
import qualified Music.Theory.Json as Json {- hmt-base -}
import qualified Music.Theory.Show as T {- hmt-base -}

import qualified Sound.Sc3.Data.Chemistry.Elements as E {- hsc3-data -}
import qualified Sound.Sc3.Data.Chemistry.Mol as Mol {- hsc3-data -}
import qualified Sound.Sc3.Data.Chemistry.Pdb.Types as Pdb {- hsc3-data -}
import qualified Sound.Sc3.Data.Chemistry.Poscar as Poscar {- hsc3-data -}
import qualified Sound.Sc3.Data.Chemistry.Xyz as Xyz {- hsc3-data -}

-- * Types

-- | (atomic-symbol,xyz-coordinate)
type Atom = (String,V3 Double)

atom_sym :: Atom -> String
atom_sym (e,_) = e

-- | (i,j), indicies into Atom sequence (zero indexed)
type Bond = (Int,Int)

-- | (name,degree=(n-atoms,n-bonds),description,atoms,bonds)
type Struct = (String,(Int,Int),String,[Atom],[Bond])

struct_json :: Struct -> Json.Value
struct_json (nm, _, dsc, atm, bnd) =
  let e (i, j) = Json.array (map Json.int [i, j])
      v (x, y, z) = Json.array (map Json.double [x, y, z])
  in Json.object
     [("name", Json.string nm)
     ,("description", Json.string dsc)
     ,("vertexCoordinates", Json.array (map (v . snd) atm))
     ,("vertexLabels", Json.array (map (Json.string . fst) atm))
     ,("edges", Json.array (map e bnd))]

struct_name :: Struct -> String
struct_name (nm,_,_,_,_) = nm

struct_degree :: Struct -> (Int,Int)
struct_degree (_,k,_,_,_) = k

struct_n_atoms :: Struct -> Int
struct_n_atoms = fst . struct_degree

struct_n_bonds :: Struct -> Int
struct_n_bonds = snd . struct_degree

struct_description :: Struct -> String
struct_description (_,_,dsc,_,_) = dsc

struct_atoms :: Struct -> [Atom]
struct_atoms (_,_,_,atm,_) = atm

struct_description_edit :: (String -> String) -> Struct -> Struct
struct_description_edit f (nm,k,dsc,a,b) = (nm,k,f dsc,a,b)

struct_bonds :: Struct -> [Bond]
struct_bonds (_nm,_k,_dsc,_a,b) = b

struct_bonds_atoms :: Struct -> [(Atom,Atom)]
struct_bonds_atoms (_nm,_k,_dsc,a,b) =
  let f (i,j) = (a !! i,a !! j)
  in map f b

-- | Do atom count and bond count match atom and bond data?
struct_validate :: Struct -> Bool
struct_validate (_,(n_a,n_b),_,a,b) = n_a == length a && n_b == length b

-- | Is Struct valid and empty?
struct_is_empty :: Struct -> Bool
struct_is_empty s =
  case s of
    (_,(0,0),_,[],[]) -> True
    _ -> False

-- * Connection

{- | Covalent radius (angstroms) of element /sym/, defaults to 250.

>>> map sym_radius ["Al","C","Cu","Fe","FE","S"]
[1.21,0.76,1.32,1.32,1.32,1.05]
-}
sym_radius :: Fractional n => String -> n
sym_radius sym =
  let r = E.covalent_radius (E.atomic_number_err False sym)
  in E.picometres_to_angstroms (fromMaybe 250 r)

-- | (-0.8,+0.4)
type Tolerance = (Double,Double)

-- | Replaces existing bonds, if any, with calculated bonds.
struct_calculate_bonds :: Tolerance -> Struct -> Struct
struct_calculate_bonds tol (nm,(n_a,_),dsc,a,_b) =
  let b = map fst (E.calculate_bonds sym_radius tol a)
      n_b = length b
  in (nm,(n_a,n_b),dsc,a,b)

-- * Query/Edit

-- | (minima,maxima) of atoms.
struct_bounds :: Struct -> V2 (V3 Double)
struct_bounds (_,_,_,atm,_) =
  let c = map snd atm
      r = unzip3 c
  in (v3_map minimum r,v3_map maximum r)

-- | Apply /f/ at all atom positions.
struct_v3_map :: (V3 Double -> V3 Double) -> Struct -> Struct
struct_v3_map f (nm,k,dsc,a,b) =
  let (sym,pt) = unzip a
  in (nm,k,dsc,zip sym (map f pt),b)

-- | Translate atom positions so structure is centered at /c/.
struct_center :: V3 Double -> Struct -> Struct
struct_center c (nm,k,dsc,a,b) =
  let (sym,pt) = unzip a
  in (nm,k,dsc,zip sym (v3_center_at c pt),b)

-- * Convert - Mod/Sdf, Poscar, Xyz, Pdb

-- | Convert Mol data to Struct data.  Mol bond data is one-indexed.
mol_to_struct :: (String,Mol.Mol) -> Struct
mol_to_struct (fn,(nm,dsc,a_n,b_n,a,b,_)) =
  let atom_f (i,j) = (j,i)
      bond_f ((i,j),_) = (i - 1,j - 1)
  in (fn,(a_n,b_n),concat [nm," -- ",dsc],map atom_f a,map bond_f b)

-- | Convert Poscar data to Struct data.
poscar_to_struct :: Poscar.Poscar_Ty -> (String,Poscar.Poscar) -> Struct
poscar_to_struct ty (nm,p) =
  let k = Poscar.poscar_degree p
      dsc = Poscar.poscar_description p
      atoms = Poscar.poscar_atoms ty p
      swap (i,j) = (j,i)
  in (nm,(k,0),dsc,map swap atoms,[])

-- | Convert Xyz data to Struct data.  Xyz files have no connection (bond) data.
xyz_to_struct :: (String,Xyz.Xyz) -> Struct
xyz_to_struct (nm,(k,dsc,atoms)) = (nm,(k,0),dsc,atoms,[])

pdb_atom_to_xyz :: (String,[Pdb.Atom]) -> Xyz.Xyz
pdb_atom_to_xyz (dsc,a) =
  let n = length a
  in (n,dsc,zip (map Pdb.atom_element_or_name a) (map Pdb.atom_coord a))

pdb_atom_to_xyz_wr :: FilePath -> (String, [Pdb.Atom]) -> IO ()
pdb_atom_to_xyz_wr fn = Xyz.xyz_store 5 fn . pdb_atom_to_xyz

pdb_atom_to_struct :: Tolerance -> String -> (String, [Pdb.Atom]) -> Struct
pdb_atom_to_struct tol nm = struct_calculate_bonds tol . xyz_to_struct . (,) nm . pdb_atom_to_xyz

-- * Stat

-- | Print summary of Struct data.
struct_stat :: Struct -> [String]
struct_stat s =
  let (nm,(n_a,n_b),dsc,a,_) = s
      e = sort (map atom_sym a)
      u = nub e
      f (i,j) = concat [i,": ",j]
      q = [("Name",nm)
          ,("Description",dsc)
          ,("N-Atoms",show n_a)
          ,("N-Bonds",show n_b)
          ,("N-Elements",show (length u))
          ,("Elements",unwords u)
          ,("Formula",E.hill_formula e)
          ,("Valid",show (struct_validate s))]
  in map f q

-- | 'putStrLn' of 'unlines' of 'stuct_stat'
struct_stat_wr :: Struct -> IO ()
struct_stat_wr = putStrLn . unlines . struct_stat

-- * Format - Struct

{- | Write simple plain text format for Struct data.

1. struct-name
2. atom-count bond-count
3. description
4. atom-data: element x y z
   bond-data: atom-ix atom-ix
-}
struct_pp :: Int -> Struct -> [String]
struct_pp k (nm,(n_a,n_b),dsc,a,b) =
  let hdr = [nm
            ,unwords (map show [n_a,n_b])
            ,dsc]
      a_pp (e,(x,y,z)) = unwords (e : map (T.double_pp k) [x,y,z])
      b_pp (p,q) = unwords (map show [p,q])
  in concat [hdr,map a_pp a,map b_pp b]

-- | Read simple plain text format for Struct data.
struct_parse :: [String] -> Struct
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

-- * Io - Struct

-- | 'struct_parse' of 'readFile'.
struct_load_txt :: FilePath -> IO Struct
struct_load_txt fn = fmap (struct_parse . lines) (readFile fn)

-- | 'writeFile' of 'struct_pp'.
struct_store_txt :: Int -> FilePath -> Struct -> IO ()
struct_store_txt k fn = writeFile fn . unlines . struct_pp k

-- * Io - Mol/Sdf, Poscar, Xyz

-- | Load Struct based on file-extension (mol/sdf,poscar,struct,xyz)
struct_load_ext :: FilePath -> IO Struct
struct_load_ext fn =
  let f x = (takeBaseName fn,x)
  in case takeExtension fn of
       ".mol" -> fmap (mol_to_struct . f) (Mol.mol_load fn)
       ".poscar" -> fmap (poscar_to_struct Poscar.Poscar_C . f) (Poscar.poscar_load fn)
       ".sdf" -> fmap (mol_to_struct . f) (Mol.mol_load fn)
       ".struct" -> struct_load_txt fn
       ".xyz" -> fmap (xyz_to_struct . f) (Xyz.xyz_load fn)
       ext -> error (show ("load_struct",fn,ext))

{- | List of all Struct, Mol/Sdf, Poscar and Xyz files at /dir/.
     Names are relative.

> struct_dir_entries "/home/rohan/rd/j/2018-09-26/xyz/"
> struct_dir_entries "/home/rohan/rd/j/2020-03-30/mol/"
-}
struct_dir_entries :: FilePath -> IO [FilePath]
struct_dir_entries =
  let ext = words ".mol .poscar .sdf .struct .xyz"
  in T.dir_subset ext

{- | 'struct_load_ext' of 'struct_dir_entries'

>>> s <- struct_load_dir "/home/rohan/rd/j/2018-09-26/xyz/"
>>> length s
268

>>> maximum (map struct_n_atoms s)
272
-}
struct_load_dir :: FilePath -> IO [Struct]
struct_load_dir dir = struct_dir_entries dir >>= mapM (struct_load_ext . (dir </>))

-- * Io - Type

-- | 'poscar_to_struct' of 'Poscar.poscar_load_dir'
load_poscar_structs_ty :: Poscar.Poscar_Ty -> FilePath -> IO [Struct]
load_poscar_structs_ty ty = fmap (map (poscar_to_struct ty)) . Poscar.poscar_load_dir

-- * Graph

-- | 'Struct' to 'T.Lbl' with 'Atom' labels at vertices.
struct_to_lbl :: Struct -> T.Lbl_ Atom
struct_to_lbl (_,_,_,a,b) = (zip [0..] a,zip b (repeat ()))

-- | 'T.v3_graph_to_obj' of 'struct_to_lbl', element names are discarded.
struct_to_obj :: Struct -> Obj.Obj
struct_to_obj =
  let f (v,e) = (map (fmap snd) v,e)
  in Obj.lbl_to_obj . f . struct_to_lbl

-- * Convert

ext_to_obj :: Int -> Maybe Tolerance -> FilePath -> FilePath -> IO ()
ext_to_obj k tol xyz_fn obj_fn = do
  s <- fmap (maybe id struct_calculate_bonds tol) (struct_load_ext xyz_fn)
  Obj.obj_store k obj_fn (struct_to_obj s)

ext_to_obj_dir :: String -> Int -> Maybe Tolerance -> FilePath -> FilePath -> IO ()
ext_to_obj_dir ext k tol ext_dir obj_dir = do
  fn <- T.dir_subset [ext] ext_dir
  let rw x = obj_dir </> replaceExtension (takeFileName x) ".obj"
      cv x = ext_to_obj k tol x (rw x)
  mapM_ cv fn

-- | Load Xyz file, calculate bonds, write Obj file.
xyz_to_obj :: Int -> Tolerance -> FilePath -> FilePath -> IO ()
xyz_to_obj k t = ext_to_obj k (Just t)

-- | 'xyz_to_obj' at all Xyz files at /xyz_dir/.  Output files are written to /obj_dir/.
xyz_to_obj_dir :: Int -> Tolerance -> FilePath -> FilePath -> IO ()
xyz_to_obj_dir k t = ext_to_obj_dir ".xyz" k (Just t)

-- | Load Poscar file, calculate bonds, write Obj file.
poscar_to_obj :: Int -> Tolerance -> FilePath -> FilePath -> IO ()
poscar_to_obj k t = ext_to_obj k (Just t)

-- | 'poscar_to_obj' at all Poscar files at /poscar_dir/.  Output files are written to /obj_dir/.
poscar_to_obj_dir :: Int -> Tolerance -> FilePath -> FilePath -> IO ()
poscar_to_obj_dir k t = ext_to_obj_dir ".poscar" k (Just t)

{-
-- * Io - Dir

-- | 'mol_to_struct' of 'Mol.mol_load_dir', extension is either ".mol" or ".sdf"
load_mol_structs :: String -> FilePath -> IO [Struct]
load_mol_structs ext = fmap (map mol_to_struct) . Mol.mol_load_dir ext

load_poscar_structs :: FilePath -> IO [Struct]
load_poscar_structs = load_poscar_structs_ty Poscar.Poscar_C

-- | 'xyz_to_struct' of 'Xyz.xyz_load_dir'
load_xyz_structs :: FilePath -> IO [Struct]
load_xyz_structs = fmap (map xyz_to_struct) . Xyz.xyz_load_dir
-}
