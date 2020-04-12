{- | STRUCT, minimal structure data type.

Files in XYZ, MOL/SDF and POSCAR file formats can be loaded into the STRUCT data-type.

There is also a simle plain text format for storing STRUCT data.
-}
module Sound.SC3.Data.Chemistry.STRUCT where

import Data.Maybe {- base -}
import System.Directory {- directory -}
import System.FilePath {- filepath -}

import Data.CG.Minus.Plain {- hcg-minus -}

import qualified Sound.SC3.Data.Chemistry.Elements as E {- hsc3-data -}
import qualified Sound.SC3.Data.Chemistry.MOL as MOL {- hsc3-data -}
import qualified Sound.SC3.Data.Chemistry.POSCAR as POSCAR {- hsc3-data -}
import qualified Sound.SC3.Data.Chemistry.XYZ as XYZ {- hsc3-data -}

-- | (atomic-symbol,xyz-coordinate)
type ATOM = (String,V3 Double)

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

-- > map sym_radius ["Al","C","Cu","Fe","S"]
sym_radius :: Fractional n => String -> n
sym_radius sym =
  let r = E.covalent_radius (E.atomic_number_err True sym)
  in E.picometres_to_angstroms (fromMaybe 250 r)

struct_validate :: STRUCT -> Bool
struct_validate (_,(n_a,n_b),_,a,b) = n_a == length a && n_b == length b

type TOLERANCE = (Double,Double)

-- | Replaces existing BONDS, if any, with calculated bonds.
struct_calculate_bonds :: TOLERANCE -> STRUCT -> STRUCT
struct_calculate_bonds tol (nm,(n_a,_),dsc,a,_b) =
  let b = map fst (E.calculate_bonds sym_radius tol a)
      n_b = length b
  in (nm,(n_a,n_b),dsc,a,b)

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

-- * CONVERT - MOD/SDF, POSCAR, XYZ

mol_to_struct :: (String,MOL.MOL) -> STRUCT
mol_to_struct (fn,(nm,dsc,a_n,b_n,a,b,_)) =
  let atom_f (i,j) = (j,i)
      bond_f ((i,j),_) = (i - 1,j - 1) -- MOL bond data is one-indexed
  in (fn,(a_n,b_n),concat [nm," -- ",dsc],map atom_f a,map bond_f b)

poscar_to_struct :: POSCAR.POSCAR_TY -> (String,POSCAR.POSCAR) -> STRUCT
poscar_to_struct ty (nm,p) =
  let k = POSCAR.poscar_degree p
      dsc = POSCAR.poscar_description p
      atoms = POSCAR.poscar_atoms ty p
      swap (i,j) = (j,i)
  in (nm,(k,0),dsc,map swap atoms,[])

xyz_to_struct :: (String,XYZ.XYZ) -> STRUCT
xyz_to_struct (nm,(k,dsc,atoms)) = (nm,(k,0),dsc,atoms,[])

-- * FORMAT - STRUCT

-- | Write simple plain text format for STRUCT data.
struct_pp :: STRUCT -> [String]
struct_pp (nm,(n_a,n_b),dsc,a,b) =
  let hdr = [nm
            ,unwords (map show [n_a,n_b])
            ,dsc]
      a_pp ((e,(x,y,z))) = unwords (e : map show [x,y,z])
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
struct_load :: FilePath -> IO STRUCT
struct_load fn = return . struct_parse . lines =<< readFile fn

-- | 'writeFile' of 'struct_pp'.
struct_store :: FilePath -> STRUCT -> IO ()
struct_store fn = writeFile fn . unlines . struct_pp

-- * I/O - MOL/SDF,POSCAR,XYZ

-- | Load STRUCT based on file-extension (mol/sdf,poscar,struct,xyz)
struct_load_ext :: FilePath -> IO STRUCT
struct_load_ext fn =
  let f x = (takeBaseName fn,x)
  in case takeExtension fn of
       ".mol" -> fmap (mol_to_struct . f) (MOL.mol_load fn)
       ".poscar" -> fmap (poscar_to_struct POSCAR.POSCAR_C . f) (POSCAR.poscar_load fn)
       ".sdf" -> fmap (mol_to_struct . f) (MOL.mol_load fn)
       ".struct" -> struct_load fn
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
  in fmap (filter (flip elem ext . takeExtension)) . listDirectory

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
