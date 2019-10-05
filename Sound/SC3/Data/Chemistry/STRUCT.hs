-- | STRUCT, minimal structure data type.
module Sound.SC3.Data.Chemistry.STRUCT where

import Data.Maybe {- base -}
import System.FilePath {- filepath -}

import Data.CG.Minus.Plain {- hcg-minus -}

import qualified Sound.SC3.Data.Chemistry.Elements as E {- hsc3-data -}
import qualified Sound.SC3.Data.Chemistry.MOL as MOL {- hsc3-data -}
import qualified Sound.SC3.Data.Chemistry.POSCAR as POSCAR {- hsc3-data -}
import qualified Sound.SC3.Data.Chemistry.XYZ as XYZ {- hsc3-data -}

-- | (atomic-symbol,xyz-coordinate)
type ATOM = (String,V3 Double)

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

type TOLERANCE = (Double,Double)

struct_calculate_bonds :: TOLERANCE -> STRUCT -> STRUCT
struct_calculate_bonds tol (nm,k,dsc,a,_b) =
  (nm,k,dsc,a,map fst (E.calculate_bonds sym_radius tol a))

-- | (minima,maxima) of atoms.
struct_bounds :: STRUCT -> V2 (V3 Double)
struct_bounds (_,_,_,atm,_) =
  let c = map snd atm
      r = unzip3 c
  in (v3_uop minimum r,v3_uop maximum r)

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

-- | Load STRUCT based on file-extension (mol,poscar,xyz)
load_struct :: FilePath -> IO STRUCT
load_struct fn =
  let f x = (takeBaseName fn,x)
  in case takeExtension fn of
       ".mol" -> fmap (mol_to_struct . f) (MOL.mol_load fn)
       ".poscar" -> fmap (poscar_to_struct POSCAR.POSCAR_C . f) (POSCAR.poscar_load fn)
       ".xyz" -> fmap (xyz_to_struct . f) (XYZ.xyz_load fn)
       ext -> error (show ("load_struct",fn,ext))

-- * I/O - DIR

-- | 'mol_to_struct' of 'MOL.mol_load_dir'
load_mol_structs :: FilePath -> IO [STRUCT]
load_mol_structs = fmap (map mol_to_struct) . MOL.mol_load_dir

-- | 'poscar_to_struct' of 'POSCAR.poscar_load_dir'
load_poscar_structs_ty :: POSCAR.POSCAR_TY -> FilePath -> IO [STRUCT]
load_poscar_structs_ty ty = fmap (map (poscar_to_struct ty)) . POSCAR.poscar_load_dir

load_poscar_structs :: FilePath -> IO [STRUCT]
load_poscar_structs = load_poscar_structs_ty POSCAR.POSCAR_C

-- | 'xyz_to_struct' of 'XYZ.xyz_load_dir'
load_xyz_structs :: FilePath -> IO [STRUCT]
load_xyz_structs = fmap (map xyz_to_struct) . XYZ.xyz_load_dir
