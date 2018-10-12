-- | STRUCT, minimal structure data type.
module Sound.SC3.Data.Chemistry.STRUCT where

import Data.Maybe {- base -}

import Data.CG.Minus.Plain {- hcg-minus -}

import qualified Sound.SC3.Data.Chemistry.Elements as E {- hsc3-data -}
import qualified Sound.SC3.Data.Chemistry.MOL as MOL {- hsc3-data -}
import qualified Sound.SC3.Data.Chemistry.POSCAR as POSCAR {- hsc3-data -}
import qualified Sound.SC3.Data.Chemistry.XYZ as XYZ {- hsc3-data -}

-- | (atomic-symbol,xyz-coordinate)
type ATOM = (String,V3 R)

-- | (i,j), indicies into ATOM sequence.
type BOND = (Int,Int)

-- | (name,degree=n-atoms,description,atoms,bonds)
type STRUCT = (String,Int,String,[ATOM],[BOND])

struct_name :: STRUCT -> String
struct_name (nm,_,_,_,_) = nm

struct_degree :: STRUCT -> Int
struct_degree (_,k,_,_,_) = k

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

-- > map sym_radius ["Al","C","Cu","Fe","S"]
sym_radius :: Fractional n => String -> n
sym_radius sym =
  let r = E.covalent_radius (E.atomic_number_err sym)
  in E.picometres_to_angstroms (fromMaybe 250 r)

struct_calculate_bonds :: (R,R) -> STRUCT -> STRUCT
struct_calculate_bonds tol (nm,k,dsc,a,_b) =
  (nm,k,dsc,a,map fst (E.calculate_bonds sym_radius tol a))

-- | (minima,maxima) of atoms.
struct_bounds :: STRUCT -> V2 (V3 R)
struct_bounds (_,_,_,atm,_) =
  let c = map snd atm
      r = unzip3 c
  in (v3_uop minimum r,v3_uop maximum r)

-- * Convert

mol_to_struct :: (String,MOL.MOL) -> STRUCT
mol_to_struct (nm,(_nm,dsc,a_n,_b_n,a,b)) =
  let swap (i,j) = (j,i)
  in (nm,a_n,dsc,map swap a,map fst b)

poscar_to_struct :: POSCAR.POSCAR_TY -> (String,POSCAR.POSCAR) -> STRUCT
poscar_to_struct ty (nm,p) =
  let k = POSCAR.poscar_degree p
      dsc = POSCAR.poscar_description p
      atoms = POSCAR.poscar_atoms ty p
      swap (i,j) = (j,i)
  in (nm,k,dsc,map swap atoms,[])

xyz_to_struct :: (String,XYZ.XYZ) -> STRUCT
xyz_to_struct (nm,(k,dsc,atoms)) = (nm,k,dsc,atoms,[])

-- * I/O

load_mol_structs :: FilePath -> IO [STRUCT]
load_mol_structs dir = do
  mol_set <- MOL.mol_load_dir dir
  return (map (mol_to_struct) mol_set)

load_poscar_structs :: POSCAR.POSCAR_TY -> FilePath -> IO [STRUCT]
load_poscar_structs ty dir = do
  poscar_set <- POSCAR.poscar_load_dir dir
  return (map (poscar_to_struct ty) poscar_set)

load_xyz_structs :: FilePath -> IO [STRUCT]
load_xyz_structs dir = do
  xyz_set <- XYZ.xyz_load_dir dir
  return (map xyz_to_struct xyz_set)
