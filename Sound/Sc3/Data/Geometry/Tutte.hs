-- | Tutte, W. T. (1963), "How to draw a graph",
--   Proceedings of the London Mathematical Society, 13: 743–767,
module Sound.Sc3.Data.Geometry.Tutte where

import Data.Bifunctor {- base -}

import Data.Cg.Minus.Plain {- hcg-minus -}
import Data.Cg.Minus.Geometry {- hcg-minus -}

import qualified Data.Cg.Minus.Plain.Svg as Svg {- hcg-minus -}
import qualified Data.Cg.Minus.Geometry.Off as Off {- hcg-minus -}

import qualified Music.Theory.Graph.Type as T {- hmt-base -}
import qualified Music.Theory.List as T {- hmt-base -}

import qualified Sound.Sc3.Data.Geometry.Obj as Obj {- hsc3-data -}

-- | R = real
type R = Double

-- | /k/ points on unit circle
--
-- > v_on_unit_circle 4
v_on_unit_circle :: Int -> [V2 R]
v_on_unit_circle k =
  let i = two_pi / fromIntegral k
  in map (\ph -> polar_to_rectangular (1,ph)) (take k [0,i ..])

-- | [(VERTEX,COORDINATE)]
type V_Loc = [(Int,V2 R)]

-- | k = n-vertices
--
-- > v_init_loc 8 [0,1,2,3]
v_init_loc :: Int -> [Int] -> V_Loc
v_init_loc k fc =
  let fc_v = zip fc (v_on_unit_circle (length fc))
      sel i = case lookup i fc_v of {Just j -> (i,j);_ -> (i,(0,0))}
  in map sel [0 .. k - 1]

-- | 'v2_centroid' of indexed 'V_Loc'
v_loc_centre :: V_Loc -> [Int] -> V2 R
v_loc_centre v e = v2_centroid (map (`T.lookup_err` v) e)

-- | a = adj-mtx, fc = face, v = vertices-loc
tutte_step :: T.Adj_Mtx Int -> [Int] -> V_Loc -> V_Loc
tutte_step adj fc v =
  let f (i,j) = if i `elem` fc then (i,j) else (i,v_loc_centre v (T.adj_mtx_con (0,1) adj i))
  in map f v

-- | Generate sequence of Tuttes given graph, face list and outer face index.
tutte_gen :: T.G -> [[Int]] -> Int -> [V_Loc]
tutte_gen (v,e) fc i =
  let k = length v
      adj = T.edg_to_adj_mtx_undir (0,1) (T.g_to_edg (v,e))
      fc_i = fc !! i
      v0 = v_init_loc k fc_i
  in iterate (tutte_step adj fc_i) v0

-- | 'tutte_gen' of Off3
tutte_gen_off3 :: Off.Off3 R -> Int -> ([V_Loc],[V2 Int])
tutte_gen_off3 o i =
  let (v,e) = T.lbl_to_g (Off.off_graph o)
      ((_,_),(_,fc)) = o
  in (tutte_gen (v,e) (map snd fc) i,e)

-- | Store tutte to Obj file.
tutte_obj :: FilePath -> V_Loc -> [V2 Int] -> IO ()
tutte_obj fn v e = do
  let e' = map (\(p,q) -> ('l',[p,q])) e
      add_z (x,y) = (x,y,0)
  Obj.obj_store 4 fn (map (add_z . snd) v,e')

-- | Store tutte to Svg file.
tutte_svg :: (V2 R,R,Int) -> FilePath -> V_Loc -> [V2 Int] -> IO ()
tutte_svg opt fn v e = do
  let ix k = T.lookup_err k v
      ln = map (bimap ix ix) e
  Svg.svg_store_line_unif ((0,0,0),1) fn opt ln
