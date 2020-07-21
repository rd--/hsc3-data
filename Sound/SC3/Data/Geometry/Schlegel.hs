-- | Schlegel Diagrams <http://u.math.biu.ac.il/~mlazar/schlegels.html>
module Sound.SC3.Data.Geometry.Schlegel where

import Data.CG.Minus.Plain {- hcg-minus -}
import Data.CG.Minus.Geometry {- hcg-minus -}

import qualified Data.CG.Minus.Geometry.OFF as OFF {- hcg-minus -}

import qualified Music.Theory.Graph.Type as T {- hmt -}
import qualified Music.Theory.List as T {- hmt -}

import qualified Sound.SC3.Data.Geometry.OBJ as OBJ {- hsc3-data -}

-- | R = real
type R = Double

-- | /k/ points on unit circle
--
-- > v_on_unit_circle 4
v_on_unit_circle :: Int -> [V2 R]
v_on_unit_circle k =
  let i = two_pi / (fromIntegral k)
  in map (\ph -> polar_to_rectangular (1,ph)) (take k [0,i ..])

-- | [(VERTEX,COORDINATE)]
type V_LOC = [(Int,V2 R)]

-- | k = n-vertices
--
-- > v_init_loc 8 [0,1,2,3]
v_init_loc :: Int -> [Int] -> V_LOC
v_init_loc k fc =
  let fc_v = zip fc (v_on_unit_circle (length fc))
      sel i = case lookup i fc_v of {Just j -> (i,j);_ -> (i,(0,0))}
  in map sel [0 .. k - 1]

-- | 'v2_centroid' of indexed 'V_LOC'
v_loc_centre :: V_LOC -> [Int] -> V2 R
v_loc_centre v e = v2_centroid (map (\i -> T.lookup_err i v) e)

-- | a = adj-mtx, fc = face, v = vertices-loc
schlegel_step :: T.ADJ_MTX Int -> [Int] -> V_LOC -> V_LOC
schlegel_step adj fc v =
  let f (i,j) = if i `elem` fc then (i,j) else (i,v_loc_centre v (T.adj_mtx_con (0,1) adj i))
  in map f v

-- | Generate sequence of Schlegels of /o/
schlegel_gen :: OFF.OFF3 R -> Int -> ([V_LOC],[V2 Int])
schlegel_gen o i =
  let ((k,_),(_,fc)) = o
      gr = T.lbl_to_g (OFF.off_graph o)
      adj = T.edg_to_adj_mtx_undir (0,1) (T.g_to_edg gr)
      (_,fc_i) = fc !! i
      v0 = v_init_loc k fc_i
  in (iterate (schlegel_step adj fc_i) v0,snd gr)

-- | Store schlegel to OBJ file.
schlegel_obj :: FilePath -> V_LOC -> [V2 Int] -> IO ()
schlegel_obj fn v e = do
  let e' = map (\(p,q) -> ('l',[p,q])) e
      add_z (x,y) = (x,y,0)
  OBJ.obj_store 4 fn (map (add_z . snd) v,e')
