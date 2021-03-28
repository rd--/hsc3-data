{- | OBJ file format functions

PDF=<http://www.cs.utah.edu/~boulos/cs3505/obj_spec.pdf>
TXT=<http://www.martinreddy.net/gfx/3d/OBJ.spec>
-}
module Sound.SC3.Data.Geometry.OBJ where

import Data.Bifunctor {- base -}
import Data.Either {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}

import qualified System.IO.Strict as Strict {- strict -}

import Data.CG.Minus.Plain {- hcg-minus -}

import qualified Music.Theory.Graph.Type as T {- hmt -}
import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Show as T {- hmt -}
import qualified Music.Theory.Tuple as T {- hmt -}

-- * OBJ

type OBJ_ t = ([t],[(Char,[Int])])

obj_vertex_map :: (t -> u) -> OBJ_ t -> OBJ_ u
obj_vertex_map f (v,c) = (map f v,c)

-- | R = REAL
type R = Double

{- | ([VERTEX],[(CMD,[VERTEX-INDEX])])

OBJ files store data one-indexed, the OBJ type is zero-indexed.
-}
type OBJ = OBJ_ (V3 R)

obj_parse_entry :: String -> Either (V3 R) (Char,[Int])
obj_parse_entry s =
  let read_ix = subtract 1 . read
  in case words s of
       ["v",x,y,z] -> Left (read x,read y,read z)
       "l":ix -> Right ('l',map read_ix ix)
       "f":ix -> Right ('f',map read_ix ix)
       "p":ix -> Right ('p',map read_ix ix)
       _ -> error "obj_parse_entry"

obj_parse :: [String] -> OBJ
obj_parse = partitionEithers . map obj_parse_entry

-- | Empty lines are allowed and ignored, comments are #-prefixed.
obj_is_nil_line :: String -> Bool
obj_is_nil_line s = null s || head s == '#'

obj_load :: FilePath -> IO OBJ
obj_load = fmap (obj_parse . filter (not . obj_is_nil_line) . lines) . Strict.readFile

obj_format_entry :: Int -> Either (V3 R) (Char,[Int]) -> String
obj_format_entry k =
  let f_pp = unwords . (:) "v" . map (T.realfloat_pp k) . T.t3_to_list
      i_pp (c,x) = unwords ([c] : map (show . (+) 1) x)
  in either f_pp i_pp

obj_store :: Int -> FilePath -> OBJ -> IO ()
obj_store k fn =
  let f (i,j) = map Left i ++ map Right j
  in writeFile fn . unlines . map (obj_format_entry k) . f

-- * LN

-- | L entries
type LN_DAT = ([V3 R],[[Int]])

obj_to_ln :: OBJ -> LN_DAT
obj_to_ln =
  let f (ty,ix) = if ty == 'l' then Just ix else Nothing
  in second (mapMaybe f)

ln_to_obj :: LN_DAT -> OBJ
ln_to_obj (v,l) = (v,map ((,) 'l') l)

obj_load_ln :: FilePath -> IO LN_DAT
obj_load_ln = fmap obj_to_ln . obj_load

ln_dat_from_vertex_seq :: [[V3 R]] -> LN_DAT
ln_dat_from_vertex_seq t =
  let reverse_lookup k = fmap fst . find ((== k) . snd)
      reverse_lookup_err k = fromMaybe (error "reverse_lookup") . reverse_lookup k
      p = nub (sort (concat t))
      v = zip [0..] p
  in (p,map (map (`reverse_lookup_err` v)) t)

obj_store_ln_dat :: Int -> FilePath -> LN_DAT -> IO ()
obj_store_ln_dat k fn = obj_store k fn . ln_to_obj

obj_store_ln :: Int -> FilePath -> [[V3 R]] -> IO ()
obj_store_ln k fn = obj_store_ln_dat k fn . ln_dat_from_vertex_seq

-- * GRAPH

obj_to_lbl_ :: OBJ -> T.LBL_ (V3 R)
obj_to_lbl_ =
  let f (ty,ix) = case ty of
                    'l' -> T.adj2 1 ix
                    'f' -> T.adj2_cyclic 1 ix
                    _ -> []
  in bimap (zip [0..]) (map (\i -> (i,())) . concatMap f)

-- | 'obj_to_lbl_' of 'obj_load'
obj_load_lbl_ :: FilePath -> IO (T.LBL_ (V3 R))
obj_load_lbl_ = fmap obj_to_lbl_ . obj_load

-- | Requires (but does not check) that graph vertices be indexed [0 .. #v - 1]
lbl_to_obj :: T.LBL_ (V3 R) -> OBJ
lbl_to_obj (v,e) = let f ((i,j),()) = ('l',[i,j]) in (map snd v,map f e)

-- | 'obj_store' of 'lbl_to_obj'.
obj_store_lbl_ :: Int -> FilePath -> T.LBL_ (V3 R) -> IO ()
obj_store_lbl_ k fn = obj_store k fn . lbl_to_obj

-- * FACES

-- | (vertices,[[v-indices]])
type FACE_DAT = ([V3 R],[[Int]])

-- | Rewrite a set of faces (CCW triples of (x,y,z) coordinates) as FACE_DAT.
--   Vertices are zero-indexed.
face_dat_from_vertex_seq :: [[V3 R]] -> FACE_DAT
face_dat_from_vertex_seq t =
  let v = nub (sort (concat t))
      v_ix = zip [0..] v
      f = map (map (`T.reverse_lookup_err` v_ix)) t
  in (v,f)

-- | Inverse of 'face_dat_from_vertex_seq'.
face_dat_to_vertex_seq :: FACE_DAT -> [[V3 R]]
face_dat_to_vertex_seq (v,f) = map (map (`T.lookup_err` zip [0..] v)) f

face_dat_to_obj :: FACE_DAT -> OBJ
face_dat_to_obj (v,f) = (v,map ((,) 'f') f)

obj_store_face_dat :: Int -> FilePath -> FACE_DAT -> IO ()
obj_store_face_dat k fn = obj_store k fn . face_dat_to_obj

-- | 'obj_store_face_dat' of 'face_dat_from_vertex_seq'
obj_store_face_set :: Int -> FilePath -> [[V3 R]] -> IO ()
obj_store_face_set k fn = obj_store_face_dat k fn . face_dat_from_vertex_seq

obj_to_face_dat :: OBJ -> FACE_DAT
obj_to_face_dat (v,c) = (v,map snd (filter ((== 'f') . fst) c))

-- | 'obj_to_face_dat' of 'obj_load'
obj_load_face_dat :: FilePath -> IO FACE_DAT
obj_load_face_dat = fmap obj_to_face_dat . obj_load
