-- | PDB TYPES.  <https://www.wwpdb.org/documentation/file-format-content/format33/v3.3.html>
module Sound.Sc3.Data.Chemistry.PDB.Types where

import Data.Function {- base -}
import Data.List {- base -}

import Data.CG.Minus.Plain {- hcg-minus -}

import qualified Music.Theory.List as T {- hmt -}

-- * MATRIX

type MTRX x = (M33 x,V3 x)

mtrx_identity :: Num n => MTRX n
mtrx_identity = (((1,0,0),(0,1,0),(0,0,1)),(0,0,0))

mtrx_apply :: Num n => MTRX n -> V3 n -> V3 n
mtrx_apply (m,v) x = v3_add (m33_apply m x) v

-- * RESIDUE-ID

-- | (NAME,CHAIN,SEQNO,INSCODE)
type RESIDUE_ID = (String,Char,Int,Char)

residue_id_name :: RESIDUE_ID -> String
residue_id_name (nm,_,_,_) = nm

residue_id_chain :: RESIDUE_ID -> Char
residue_id_chain (_,ch,_,_) = ch

residue_id_seqno_inscode :: RESIDUE_ID -> (Int,Char)
residue_id_seqno_inscode (_,_,k,i) = (k,i)

-- | Give (R1,R3) is R2 in range (inclusive).
residue_id_in_range :: (RESIDUE_ID,RESIDUE_ID) -> RESIDUE_ID -> Bool
residue_id_in_range ((_,c1,k1,i1),(_,c3,k3,i3)) (_,c2,k2,i2) =
  if c1 /= c3 || (k1,i1) >= (k3,i3)
  then error "residue_id_in_range?"
  else (c2 == c1) && ((k1,i1) <= (k2,i2) && (k2,i2) <= (k3,i3))

-- * RECORD-TYPES

-- | (HETATM,SERIAL,NAME,ALT-LOC,RESIDUE-ID,COORDINATE,ELEMENT)
type ATOM = (Bool,Int,String,Char,RESIDUE_ID,(Double,Double,Double),String)

altloc_id_set :: [Char] -> [Char]
altloc_id_set x =
  case nub (sort x) of
    " " -> "NIL"
    ' ':y -> y
    _ -> error "alt_id_set?"

atom_het :: ATOM -> Bool
atom_het (h,_,_,_,_,_,_) = h

atom_serial :: ATOM -> Int
atom_serial (_,k,_,_,_,_,_) = k

atom_name :: ATOM -> String
atom_name (_,_,nm,_,_,_,_) = nm

atom_altloc :: ATOM -> Char
atom_altloc (_,_,_,alt,_,_,_) = alt

atom_sel_altloc_A :: ATOM -> Bool
atom_sel_altloc_A = flip elem " A" . atom_altloc

atom_residue_id :: ATOM -> RESIDUE_ID
atom_residue_id (_,_,_,_,r,_,_) = r

atom_chain_id :: ATOM -> Char
atom_chain_id = residue_id_chain . atom_residue_id

atom_coord :: ATOM -> (Double,Double,Double)
atom_coord (_,_,_,_,_,c,_) = c

atom_element :: ATOM -> String
atom_element (_,_,_,_,_,_,e) = e

atom_element_or_name :: ATOM -> String
atom_element_or_name (_,_,nm,_,_,_,el) = if null el then nm else el

type CONECT = [(Int,Int)]

{- | ((A,B,C),(ALPHA,BETA,GAMMA),SPACE-GROUP,Z)

NON-CRYSTALLOGRAPHY = ((1,1,1),(90,90,90),P1,1)
-}
type CRYST1 = ((Double, Double, Double), (Double, Double, Double), String, Int)

-- | (CLASSIFICATION,DEP-DATE,ID-CODE)
type HEADER = (String,String,String)

header_id4 :: HEADER -> String
header_id4 (_,_,x) = x

-- | ((SERIAL,ID),INIT-RESIDUE,END-RESIDUE,CLASS,LENGTH)
type HELIX = ((Int,String),RESIDUE_ID,RESIDUE_ID,Int,Int)

helix_serial :: HELIX -> Int
helix_serial ((k,_),_,_,_,_) = k

helix_chain_id :: HELIX -> Char
helix_chain_id (_,(_,c1,_,_),(_,c2,_,_),_,_) = if c1 /= c2 then error "helix_chain_id?" else c1

type HET = (RESIDUE_ID,Int,String)

het_id :: HET -> String
het_id ((x,_,_,_),_,_) = x

het_chain :: HET -> Char
het_chain ((_,x,_,_),_,_) = x

het_n_atom :: HET -> Int
het_n_atom (_,x,_) = x

-- | (ATOM-NAME-1,ALT-LOC-1,RESIDUE-ID-1,ATOM-NAME-2,ALT-LOC-2,RESIDUE-ID-2,SYM-1,SYM-2,DISTANCE)
type LINK = (String,Char,RESIDUE_ID,String,Char,RESIDUE_ID,Int,Int,Double)

-- | (REMARK,HET,HELIX,SHEET,SITE,XFORM,COORD,TER,CONECT,SEQ)
type MASTER = (Int,Int,Int,Int,Int,Int,Int,Int,Int,Int)

-- | (CONTINUATION,TEXT)
type MDLTYP = (String,String)

-- | (ID,RESIDUE-ID,STD-RES,COMMENT)
type MODRES = (String,RESIDUE_ID,String,String)

-- | (resName,stdRes) fields of MODRES record.
modres_names :: MODRES -> (String,String)
modres_names (_,(r1,_,_,_),r2,_) = (r1,r2)

-- | (TYPE,TEXT)
type REMARK = (Int,String)

-- | (SERIAL,CHAIN-ID,NUM-RES,[RES])
type SEQRES = (Int,Char,Int,[String])

seqres_residue_names :: SEQRES -> [String]
seqres_residue_names (_,_,_,x) = x

-- | (STRAND,ID,NUM-STRANDS,INIT-RESIDUE,END-RESIDUE)
type SHEET = (Int,String,Int,RESIDUE_ID,RESIDUE_ID)

sheet_chain_id :: SHEET -> Char
sheet_chain_id (_,_,_,(_,c1,_,_),(_,c2,_,_)) = if c1 /= c2 then error "sheet_chain_id?" else c1

-- | (SERIAL,RESIDUE-ID-1,RESIDUE-ID-2,SYM-1,SYM-2,DISTANCE)
type SSBOND = (Int, RESIDUE_ID, RESIDUE_ID, Int, Int, Double)

-- | (SERIAL,RESIDUE-ID)
type TER = (Int,RESIDUE_ID)

-- | (CONTINUATION,TITLE)
type TITLE = (String,String)

-- * COMPOSITE

-- | (HEADER,TITLE,NUMMDL,CRYST1,(ATOM,HETATM),CONECT,SEQRES,HELIX,SHEET,LINK,SSBOND)
type PDB = (HEADER,String,Maybe Int,CRYST1
           ,([ATOM],[ATOM]),[(Int,Int)],[(Char,[String])]
           ,[HELIX],[SHEET],[LINK],[SSBOND])

-- * GROUP

-- | Group atoms by chain and ensure sequence
atom_group :: [ATOM] -> [(Char,[ATOM])]
atom_group = map (fmap (sortOn atom_serial)) . T.collate_on atom_chain_id id

-- | Merge CONECT records, sort and remove (i,j)-(j,i) duplicates.
conect_group :: [CONECT] -> [(Int,Int)]
conect_group =
  let o (i,j) = (min i j,max i j)
  in nub . sort . map o . concat

-- | Group helices by chain and ensure sequence
helix_group :: [HELIX] -> [(Char,[HELIX])]
helix_group = map (fmap (sortOn helix_serial)) . T.collate_on helix_chain_id id

mdltyp_group :: [MDLTYP] -> String
mdltyp_group = unwords . map snd . sort

-- | Group residues by CHAIN.
seqres_group :: [SEQRES] -> [(Char,[String])]
seqres_group =
  let f (k,c,_,_) = (c,k)
      g (_,c,_,_) = c
      h (_,c,_,r) = (c,r)
      i j = let (c,r) = unzip j in (head c,concat r)
  in map (i . map h) . groupBy ((==) `on` g) . sortOn f

-- | Group helices by chain
sheet_group :: [SHEET] -> [(Char,[SHEET])]
sheet_group = T.collate_on sheet_chain_id id

title_group :: [TITLE] -> String
title_group = unwords . map snd . sort

-- * TER

-- | Remove 'ATOM's that are located past any 'TER' entry for the chain.
atom_apply_ter :: [TER] -> (Char,[ATOM]) -> (Char,[ATOM])
atom_apply_ter ter (ch,a) =
  case find (\(_,(_,x,_,_)) -> x == ch) ter of
    Just (k,_) -> (ch,filter ((< k) . atom_serial) a)
    _ -> (ch,a)
