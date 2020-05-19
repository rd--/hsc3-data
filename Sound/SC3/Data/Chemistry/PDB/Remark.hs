-- | PDB-REMARK <https://www.wwpdb.org/documentation/file-format-content/format33/remarks.html>
module Sound.SC3.Data.Chemistry.PDB.Remark where

import Data.List {- base -}
import Data.List.Split {- split -}
import Data.Maybe {- base -}

import Data.CG.Minus.Plain {- hcg-minus -}

import qualified Music.Theory.List as T {- hmt -}

import Sound.SC3.Data.Chemistry.PDB.Parse {- hsc3-data -}
import Sound.SC3.Data.Chemistry.PDB.Types {- hsc3-data -}

-- * REMARK 350 - BIOMT

-- | (N,SEQ,MX-N,VEC-N)
type REMARK_350_BIOMT = (Int,Int,V3 Double,Double)

{- | Parse REMARK 350 - BIOMT

x = (350,"  BIOMT2   2  0.866025 -0.500000  0.000000     -237.03981")
parse_remark_350_biomt x == Just (2,2,(0.866025,-0.5,0.0),-237.03981)
-}
parse_remark_350_biomt :: REMARK -> Maybe REMARK_350_BIOMT
parse_remark_350_biomt (n,s) =
  if n == 350 && take 7 s == "  BIOMT"
  then let x = words (drop 7 s)
           (i,f) = (read . (x !!),read . (x !!))
       in Just (i 0,i 1,(f 2,f 3,f 4),f 5)
  else Nothing

remark_350_biomt_group :: [REMARK_350_BIOMT] -> [[MTRX Double]]
remark_350_biomt_group =
  let err x = error (show ("remark_350_biomt_group?",x))
      f1 (i,j,p,q) = (i,j,(p,q))
      f2 x = case unzip3 x of
               ([1,2,3],[n1,n2,n3],[(m1,v1),(m2,v2),(m3,v3)]) ->
                 if n1 == n2 && n1 == n3
                 then (n1,((m1,m2,m3),(v1,v2,v3)))
                 else err (n1,n2,n3)
               e -> err e
      f3 x = if null (head x) then tail x else err x
      f4 x = let (p,q) = unzip x in if p `isPrefixOf` [1..] then q else err p
  in map f4 . f3 . T.split_when_keeping_left ((== 1) . fst) . map f2 . map (map f1) . chunksOf 3

dat_remark_350_biomt :: DAT -> [[MTRX Double]]
dat_remark_350_biomt d =
  let r = dat_remark d
      m = mapMaybe parse_remark_350_biomt r
  in remark_350_biomt_group m
