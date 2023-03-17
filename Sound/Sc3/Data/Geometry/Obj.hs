{- | Obj file format functions

Pdf=<http://www.cs.utah.edu/~boulos/cs3505/obj_spec.pdf>
Txt=<http://www.martinreddy.net/gfx/3d/OBJ.spec>
-}
module Sound.Sc3.Data.Geometry.Obj where

import Data.Bifunctor {- base -}
import Data.Either {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}

import qualified System.IO.Strict as Strict {- strict -}

import Music.Theory.Geometry.Vector {- hmt-base -}

import qualified Music.Theory.Graph.Type as T {- hmt-base -}
import qualified Music.Theory.List as T {- hmt-base -}
import qualified Music.Theory.Show as T {- hmt-base -}
import qualified Music.Theory.Tuple as T {- hmt-base -}

-- * Obj

-- | See Obj
type Obj_ t = ([t],[(Char,[Int])])

-- | Apply f at vertices of Obj.
obj_vertex_map :: (t -> u) -> Obj_ t -> Obj_ u
obj_vertex_map f (v,c) = (map f v,c)

-- | R = Real
type R = Double

{- | ([Vertex],[(Cmd,[Vertex-Index])])

Obj files store data one-indexed, the Obj type is zero-indexed.
-}
type Obj = Obj_ (V3 R)

-- | Parse Obj entry, recognised types are v=vertex, p=point, l=line, f=face
obj_parse_entry :: String -> Either (V3 R) (Char,[Int])
obj_parse_entry s =
  let read_ix = subtract 1 . read
  in case words s of
       ["v",x,y,z] -> Left (read x,read y,read z)
       "p":ix -> Right ('p',map read_ix ix)
       "l":ix -> Right ('l',map read_ix ix)
       "f":ix -> Right ('f',map read_ix ix)
       _ -> error "obj_parse_entry"

-- | 'partitionEithers' of 'obj_parse_entry'
obj_parse :: [String] -> Obj
obj_parse = partitionEithers . map obj_parse_entry

-- | Empty lines are allowed and ignored, comments are #-prefixed.
obj_is_nil_line :: String -> Bool
obj_is_nil_line s = null s || head s == '#'

-- | 'obj_parse' of 'readFile'
obj_load :: FilePath -> IO Obj
obj_load = fmap (obj_parse . filter (not . obj_is_nil_line) . lines) . Strict.readFile

-- | Given k=precision for printing, format Obj entry.
obj_format_entry :: Int -> Either (V3 R) (Char,[Int]) -> String
obj_format_entry k =
  let f_pp = unwords . (:) "v" . map (T.realfloat_pp k) . T.t3_to_list
      i_pp (c,x) = unwords ([c] : map (show . (+) 1) x)
  in either f_pp i_pp

-- | 'writeFile' of 'obj_format_entry' given k=precision
obj_store :: Int -> FilePath -> Obj -> IO ()
obj_store k fn =
  let f (i,j) = map Left i ++ map Right j
  in writeFile fn . unlines . map (obj_format_entry k) . f

-- * Ln

-- | l=line entries
type Ln_Dat = ([V3 R],[[Int]])

-- | Select only l=line entries from 'Obj'.
obj_to_ln :: Obj -> Ln_Dat
obj_to_ln =
  let f (ty,ix) = if ty == 'l' then Just ix else Nothing
  in second (mapMaybe f)

ln_to_obj :: Ln_Dat -> Obj
ln_to_obj (v,l) = (v,map ((,) 'l') l)

-- | 'obj_to_ln' of 'obj_load'
obj_load_ln :: FilePath -> IO Ln_Dat
obj_load_ln = fmap obj_to_ln . obj_load

ln_dat_from_vertex_seq :: [[V3 R]] -> Ln_Dat
ln_dat_from_vertex_seq t =
  let reverse_lookup key = fmap fst . find ((== key) . snd)
      reverse_lookup_err key = fromMaybe (error "reverse_lookup") . reverse_lookup key
      p = nub (sort (concat t))
      v = zip [0..] p
  in (p,map (map (`reverse_lookup_err` v)) t)

-- | k=precision, fn=file-name
obj_store_ln_dat :: Int -> FilePath -> Ln_Dat -> IO ()
obj_store_ln_dat k fn = obj_store k fn . ln_to_obj

-- | k=precision, fn=file-name
obj_store_ln :: Int -> FilePath -> [[V3 R]] -> IO ()
obj_store_ln k fn = obj_store_ln_dat k fn . ln_dat_from_vertex_seq

-- * Graph

obj_to_lbl_ :: Obj -> T.Lbl_ (V3 R)
obj_to_lbl_ =
  let f (ty,ix) = case ty of
                    'l' -> T.adj2 1 ix
                    'f' -> T.adj2_cyclic 1 ix
                    _ -> []
  in bimap (zip [0..]) (map (\i -> (i,())) . concatMap f)

-- | 'obj_to_lbl_' of 'obj_load'
obj_load_lbl_ :: FilePath -> IO (T.Lbl_ (V3 R))
obj_load_lbl_ = fmap obj_to_lbl_ . obj_load

-- | Requires (but does not check) that graph vertices be indexed [0 .. #v - 1]
lbl_to_obj :: T.Lbl_ (V3 R) -> Obj
lbl_to_obj (v,e) = let f ((i,j),()) = ('l',[i,j]) in (map snd v,map f e)

-- | 'obj_store' of 'lbl_to_obj', k=precision
obj_store_lbl_ :: Int -> FilePath -> T.Lbl_ (V3 R) -> IO ()
obj_store_lbl_ k fn = obj_store k fn . lbl_to_obj

-- * Faces

-- | (vertices,[[v-indices]])
type Face_Dat = ([V3 R],[[Int]])

-- | Rewrite a set of faces (CCW triples of (x,y,z) coordinates) as Face_Dat.
--   Vertices are zero-indexed.
face_dat_from_vertex_seq :: [[V3 R]] -> Face_Dat
face_dat_from_vertex_seq t =
  let v = nub (sort (concat t))
      v_ix = zip [0..] v
      f = map (map (`T.reverse_lookup_err` v_ix)) t
  in (v,f)

-- | Inverse of 'face_dat_from_vertex_seq'.
face_dat_to_vertex_seq :: Face_Dat -> [[V3 R]]
face_dat_to_vertex_seq (v,f) = map (map (`T.lookup_err` zip [0..] v)) f

face_dat_to_obj :: Face_Dat -> Obj
face_dat_to_obj (v,f) = (v,map ((,) 'f') f)

-- | k=precision, fn=file-name
obj_store_face_dat :: Int -> FilePath -> Face_Dat -> IO ()
obj_store_face_dat k fn = obj_store k fn . face_dat_to_obj

-- | 'obj_store_face_dat' of 'face_dat_from_vertex_seq'
obj_store_face_set :: Int -> FilePath -> [[V3 R]] -> IO ()
obj_store_face_set k fn = obj_store_face_dat k fn . face_dat_from_vertex_seq

obj_to_face_dat :: Obj -> Face_Dat
obj_to_face_dat (v,c) = (v,map snd (filter ((== 'f') . fst) c))

-- | 'obj_to_face_dat' of 'obj_load'
obj_load_face_dat :: FilePath -> IO Face_Dat
obj_load_face_dat = fmap obj_to_face_dat . obj_load
