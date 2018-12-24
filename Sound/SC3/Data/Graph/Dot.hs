-- | DOT/Graph functions.
module Sound.SC3.Data.Graph.Dot where

import qualified Data.Foldable as F {- base -}
import Data.Maybe {- base -}
import System.Process {- process -}

import qualified Data.Text.Lazy as T {- text -}
import qualified Data.Text.Lazy.IO as T {- text -}

import qualified Data.GraphViz as GV {- graphviz -}
import qualified Data.GraphViz.Attributes.Complete as GV {- graphviz -}
import qualified Data.GraphViz.Types.Generalised as G {- graphviz -}

{- | Run @dot@ to insert layout information into graph.

> g = "graph g {graph[layout=neato]; node[shape=point]; 0 -- 1; 0 -- 2; 0 -- 3;}"
> r <- fmap dg_parse (dot_run_layout g)
> dg_to_gr_pos r
> putStrLn (dg_print r)

-}
dot_run_layout :: String -> IO String
dot_run_layout = readProcess "dot" ["-T","dot"]

-- | 'GV.parseDotGraph' of 'T.pack'.
--
-- > let st = [G.DE (G.DotEdge 1 2 []),G.DE (G.DotEdge 2 3 [])]
-- > F.toList (G.graphStatements (dg_parse "graph {1 -- 2 -- 3}")) == st
dg_parse :: (Ord t,GV.ParseDot t) => String -> G.DotGraph t
dg_parse = GV.parseDotGraph . T.pack

-- | Type specialised to 'Int'.
dg_parse_int :: String -> G.DotGraph Int
dg_parse_int = GV.parseDotGraph . T.pack

-- | 'T.unpack' of 'GV.printDotGraph'.
dg_print :: (Ord t,GV.PrintDot t) => G.DotGraph t -> String
dg_print = T.unpack . GV.printDotGraph

-- | 'GV.parseDotGraph' of 'T.readFile'.
dg_load :: (Ord t,GV.ParseDot t) => FilePath -> IO (G.DotGraph t)
dg_load = fmap GV.parseDotGraph . T.readFile

-- | Type specialised to 'Int'.
dg_load_int :: FilePath -> IO (G.DotGraph Int)
dg_load_int = dg_load

-- | 'GV.DotStatement' to 'GV.DotNode'.
ds_to_dn :: G.DotStatement n -> Maybe (GV.DotNode n)
ds_to_dn st =
  case st of
    G.DN n -> Just n
    _ -> Nothing

-- | 'GV.DotStatement' to 'GV.DotEdge'.
ds_to_de :: G.DotStatement n -> Maybe (GV.DotEdge n)
ds_to_de st =
  case st of
    G.DE e -> Just e
    _ -> Nothing

-- | Position as (x,y) pair.
type POS = (Double, Double)

-- | 'Attribute' to position.
attr_to_pos :: GV.Attribute -> Maybe POS
attr_to_pos a =
  case a of
    GV.Pos (GV.PointPos (GV.Point x y _ _)) -> Just (x,y)
    _ -> Nothing

-- | 'Attribute' to label.
attr_to_label :: GV.Attribute -> Maybe String
attr_to_label a =
  case a of
    GV.Label (GV.StrLabel t) -> Just (T.unpack t)
    _ -> Nothing

-- | Vertex.
type V t = t

-- | Edge.
type E t = (t, t)

-- | Graph of 'V' and 'E'.
type GR t = ([V t],[E t])

-- | 'V' with position.
type V_pos t = (t, POS)

-- | Graph of 'V_pos'.
type GR_pos t = ([V_pos t], [E t])

-- | 'V' with label and position.
type V_lbl_pos t = (t, (String, POS))

-- | 'GV.DotNode' to 'V_lbl_pos'.
dn_to_v_lbl_pos :: GV.DotNode t -> V_lbl_pos t
dn_to_v_lbl_pos n =
  let a = G.nodeAttributes n
  in case (mapMaybe attr_to_label a,mapMaybe attr_to_pos a) of
       ([],[p]) -> (G.nodeID n,("",p))
       ([l],[p]) -> (G.nodeID n,(l,p))
       _ -> error "dn_parse"

-- | 'GV.DotEdge' to 'E'.
de_to_e :: GV.DotEdge t -> E t
de_to_e e = (G.fromNode e,G.toNode e)

-- | 'GV.DotGraph' to graph given node to vertex function.
-- Dot graphs need not contain any node statements.
-- However graphs that have been annotated with position data have a complete set.
dg_to_gr_f :: (G.DotNode t -> v) -> G.DotGraph t -> ([v], [E t])
dg_to_gr_f f g =
  let st = F.toList (G.graphStatements g)
      n = mapMaybe ds_to_dn st
      e = mapMaybe ds_to_de st
  in (map f n,map de_to_e e)

-- | 'GV.DotGraph' to 'E' set.
--
-- > dg_to_eset (dg_parse "graph {1 -- 2 -- 3}") == [(1,2),(2,3)]
dg_to_eset :: (Ord t,Eq t) => G.DotGraph t -> [E t]
dg_to_eset = snd . dg_to_gr_f G.nodeID

-- | 'GV.DotGraph' to 'V_lbl_pos' graph.
dg_to_gr_lbl_pos  :: G.DotGraph t -> ([V_lbl_pos t], [E t])
dg_to_gr_lbl_pos = dg_to_gr_f dn_to_v_lbl_pos

-- | 'GV.DotGraph' to 'V_pos' graph.
dg_to_gr_pos :: G.DotGraph t -> GR_pos t
dg_to_gr_pos g =
  let (n,e) = dg_to_gr_lbl_pos g
      f (k,(_,p)) = (k,p)
  in (map f n,e)

-- | (min,max) bounds of graph.
gr_pos_bounds :: GR_pos t -> (POS,POS)
gr_pos_bounds (v,_) =
  let r = unzip (map snd v)
      bimap f (i,j) = (f i,f j)
  in (bimap minimum r,bimap maximum r)

-- | Scale graph position data by /mul/.
gr_pos_scale :: Double -> GR_pos t -> GR_pos t
gr_pos_scale mul (v,e) =
  let bimap f (i,j) = (f i,f j)
      v' = map (\(k,p) -> (k,bimap (* mul) p)) v
  in (v',e)

