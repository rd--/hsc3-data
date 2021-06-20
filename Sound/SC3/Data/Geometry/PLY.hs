{- | PLY functions.

This module is used instead of 'Sound.SC3.Data.Geometry.OBJ' when faces are coloured.

There is no reader.

Greg Turk "The PLY Polygon File Format" (1994)

SEE "PLY_FILES.txt" in <https://www.cc.gatech.edu/projects/large_models/files/ply.tar.gz>

-}
module Sound.SC3.Data.Geometry.PLY where

import Data.List {- base -}

import qualified Music.Theory.Graph.Type as T {- hmt-base -}
import qualified Music.Theory.List as T {- hmt-base -}
import qualified Music.Theory.Show as T {- hmt-base -}

{- | ASCII PLY-1.0 header for object of (n-vertices,n-faces,n-edges).
     Faces and edges are (r,g,b) coloured.

> putStrLn $ unlines $ ply_header (8,6,0)
-}
ply_header :: (Int,Int,Int) -> [String]
ply_header (n_v,n_f,n_e) =
  concat
  [["ply"
   ,"format ascii 1.0"
   ,"element vertex " ++ show n_v
   ,"property float x"
   ,"property float y"
   ,"property float z"]
  ,if n_f > 0
   then ["element face " ++ show n_f
        ,"property list uchar int vertex_index"
        ,"property uchar red"
        ,"property uchar green"
        ,"property uchar blue"]
   else []
  ,if n_e > 0
   then ["element edge " ++ show n_e
        ,"property int vertex1"
        ,"property int vertex2"
        ,"property uchar red"
        ,"property uchar green"
        ,"property uchar blue"]
   else []
  ,["end_header"]]

{- | Requires (but does not check) that graph vertices be indexed [0 .. #v - 1]
     Edges are coloured as U8 (red,green,blue) triples.
     It is an error (not checked) for there to be no edges.
     PLY files are zero-indexed.
-}
v3_graph_to_ply_clr :: Int -> T.LBL (Double,Double,Double) (Int,Int,Int) -> [String]
v3_graph_to_ply_clr k (v,e) =
  let v_pp (_,(x,y,z)) = unwords (map (T.double_pp k) [x,y,z])
      e_pp ((i,j),(r,g,b)) = unwords (map show [i,j,r,g,b])
  in concat [ply_header (length v,0,length e)
            ,map v_pp v
            ,map e_pp e]

-- * FACES

-- | Rewrite a set of faces as (vertices,[[v-indices]]).
--   Indices are zero-indexed.
ply_face_set_dat :: Ord n => [([(n,n,n)],(i,i,i))] -> ([(Int,(n,n,n))],[([Int],(i,i,i))])
ply_face_set_dat t =
  let p = nub (sort (concatMap fst t))
      c = map snd t
      v = zip [0..] p
      f = map (map (`T.reverse_lookup_err` v) . fst) t
  in (v,zip f c)

-- | Format a set of coloured faces as an PLY file.
--    (CCW triples of (x,y,z) coordinates, (r,g,b) colour)
--   PLY files are one-indexed.
ply_face_set_fmt :: (Show n,Ord n,Show i) => [([(n,n,n)],(i,i,i))] -> [String]
ply_face_set_fmt t =
  let v_f (_,(x,y,z)) = unwords [show x,show y,show z]
      f_f (ix,(r,g,b)) = unwords (map show (length ix : ix) ++ map show [r,g,b])
      (v,f) = ply_face_set_dat t
  in concat [ply_header (length v,length f,0), map v_f v, map f_f f]

-- | 'writeFile' of 'ply_face_set_fmt'
ply_face_set_store :: (Show n,Ord n,Show i) => FilePath -> [([(n,n,n)],(i,i,i))] -> IO ()
ply_face_set_store fn = writeFile fn . unlines . ply_face_set_fmt
