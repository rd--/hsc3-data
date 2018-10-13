module Sound.SC3.Data.Graph.Dot where

import qualified Data.Foldable as F {- base -}
import Data.Maybe {- base -}
import System.Process {- process -}

import qualified Data.Text.Lazy as T {- text -}
import qualified Data.Text.Lazy.IO as T {- text -}

import Data.GraphViz {- graphviz -}
import Data.GraphViz.Attributes.Complete {- graphviz -}
import qualified Data.GraphViz.Types.Generalised as G {- graphviz -}

-- | Run dot to insert layout information into graph.
--
-- > g = "graph g {graph[layout=neato]; node[shape=point]; 0 -- 1; 0 -- 2; 0 -- 3;}"
-- > r <- fmap dg_parse (dot_run_layout g)
-- > dg_to_gr_pos r
-- > putStrLn (dg_print r)
dot_run_layout :: String -> IO String
dot_run_layout = readProcess "dot" ["-T","dot"]

-- | 'parseDotGraph' of 'T.pack'.
dg_parse :: (Ord t,ParseDot t) => String -> G.DotGraph t
dg_parse = parseDotGraph . T.pack

-- | 'T.unpack' of 'printDotGraph'.
dg_print :: (Ord t,PrintDot t) => G.DotGraph t -> String
dg_print = T.unpack . printDotGraph

-- | 'parseDotGraph' of 'T.readFile'.
dg_load :: (Ord t,ParseDot t) => FilePath -> IO (G.DotGraph t)
dg_load = fmap parseDotGraph . T.readFile

-- | Type specialised.
dg_load_int :: FilePath -> IO (G.DotGraph Int)
dg_load_int = dg_load

-- | 'DotStatement' to 'DotNode'.
ds_to_dn :: G.DotStatement n -> Maybe (DotNode n)
ds_to_dn st =
  case st of
    G.DN n -> Just n
    _ -> Nothing

-- | 'DotStatement' to 'DotEdge'.
ds_to_de :: G.DotStatement n -> Maybe (DotEdge n)
ds_to_de st =
  case st of
    G.DE e -> Just e
    _ -> Nothing

-- | 'Attribute' to position.
attr_to_pos :: Attribute -> Maybe (Double, Double)
attr_to_pos a =
  case a of
    Pos (PointPos (Point x y _ _)) -> Just (x,y)
    _ -> Nothing

-- | 'Attribute' to label.
attr_to_label :: Attribute -> Maybe String
attr_to_label a =
  case a of
    Label (StrLabel t) -> Just (T.unpack t)
    _ -> Nothing

-- | Vertex.
type V t = t

-- | Edge.
type E t = (t, t)

-- | 'V' with position.
type V_pos t = (t, (Double, Double))

-- | Graph of 'V_pos'.
type GR_pos t = ([V_pos t], [E t])

-- | 'V' with label and position.
type V_lbl_pos t = (t, (String, (Double, Double)))

-- | 'DotNode' to 'V_lbl_pos'.
dn_to_v_lbl_pos :: DotNode t -> V_lbl_pos t
dn_to_v_lbl_pos n =
  let a = G.nodeAttributes n
  in case (mapMaybe attr_to_label a,mapMaybe attr_to_pos a) of
       ([],[p]) -> (G.nodeID n,("",p))
       ([l],[p]) -> (G.nodeID n,(l,p))
       _ -> error "dn_parse"

-- | 'DotEdge' to 'E'.
de_to_e :: DotEdge t -> E t
de_to_e e = (G.fromNode e,G.toNode e)

-- | 'DotGraph' to 'V_lbl_pos' graph.
dg_to_gr_lbl_pos  :: G.DotGraph t -> ([V_lbl_pos t], [E t])
dg_to_gr_lbl_pos g =
  let st = F.toList (G.graphStatements g)
      n = mapMaybe ds_to_dn st
      e = mapMaybe ds_to_de st
  in (map dn_to_v_lbl_pos n,map de_to_e e)

-- | 'DotGraph' to 'V_pos' graph.
dg_to_gr_pos :: G.DotGraph t -> GR_pos t
dg_to_gr_pos g =
  let (n,e) = dg_to_gr_lbl_pos g
      f (k,(_,p)) = (k,p)
  in (map f n,e)

-- | (min,max) bounds of graph.
gr_pos_bounds :: GR_pos t -> ((Double,Double),(Double,Double))
gr_pos_bounds (v,_) =
  let r = unzip (map snd v)
      bimap f (i,j) = (f i,f j)
  in (bimap minimum r,bimap maximum r)

gr_pos_scale :: Double -> GR_pos t -> GR_pos t
gr_pos_scale mul (v,e) =
  let bimap f (i,j) = (f i,f j)
      v' = map (\(k,p) -> (k,bimap (* mul) p)) v
  in (v',e)

