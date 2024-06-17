-- | Dot/Graph functions.
module Sound.Sc3.Data.Graph.Dot where

import Data.Bifunctor {- base -}
import qualified Data.Foldable as Foldable {- base -}
import Data.Maybe {- base -}
import System.Process {- process -}

import qualified Data.Text.Lazy as Text.Lazy {- text -}
import qualified Data.Text.Lazy.IO as Text.Lazy.IO {- text -}

import qualified Data.GraphViz as Gv hiding (DotGraph {- graphviz -} (..))
import qualified Data.GraphViz.Attributes.Complete as Gv {- graphviz -}
import qualified Data.GraphViz.Types.Generalised as Gv {- graphviz -}

{- | Run @dot@ to insert layout information into graph.

>>> g = "graph g {graph[layout=neato]; node[shape=point]; 0 -- 1; 0 -- 2; 0 -- 3;}"
>>> r <- fmap dg_parse (dot_run_layout g)
>>> dg_to_gr_pos r
([(0,(49.682,73.512)),(1,(125.56,85.289)),(2,(22.305,1.8)),(3,(1.8,133.5))],[(0,1),(0,2),(0,3)])

> putStrLn (dg_print r)
-}
dot_run_layout :: String -> IO String
dot_run_layout = readProcess "dot" ["-T", "dot"]

{- | 'Gv.parseDotGraph' of 'Text.Lazy.pack'.

>>> st = [Gv.DE (Gv.DotEdge 1 2 []),Gv.DE (Gv.DotEdge 2 3 [])]
>>> Foldable.toList (Gv.graphStatements (dg_parse "graph {1 -- 2 -- 3}")) == st
True
-}
dg_parse :: (Ord t, Gv.ParseDot t) => String -> Gv.DotGraph t
dg_parse = Gv.parseDotGraph . Text.Lazy.pack

-- | Type specialised to 'Int'.
dg_parse_int :: String -> Gv.DotGraph Int
dg_parse_int = Gv.parseDotGraph . Text.Lazy.pack

-- | 'Text.Lazy.unpack' of 'Gv.printDotGraph'.
dg_print :: (Ord t, Gv.PrintDot t) => Gv.DotGraph t -> String
dg_print = Text.Lazy.unpack . Gv.printDotGraph

-- | 'Gv.parseDotGraph' of 'Text.Lazy.readFile'.
dg_load :: (Ord t, Gv.ParseDot t) => FilePath -> IO (Gv.DotGraph t)
dg_load = fmap Gv.parseDotGraph . Text.Lazy.IO.readFile

-- | Type specialised to 'Int'.
dg_load_int :: FilePath -> IO (Gv.DotGraph Int)
dg_load_int = dg_load

-- | 'Gv.DotStatement' to 'Gv.DotNode'.
ds_to_dn :: Gv.DotStatement n -> Maybe (Gv.DotNode n)
ds_to_dn st =
  case st of
    Gv.DN n -> Just n
    _ -> Nothing

-- | 'Gv.DotStatement' to 'Gv.DotEdge'.
ds_to_de :: Gv.DotStatement n -> Maybe (Gv.DotEdge n)
ds_to_de st =
  case st of
    Gv.DE e -> Just e
    _ -> Nothing

-- | Position as (x,y) pair.
type Pos = (Double, Double)

-- | 'Attribute' to position.
attr_to_pos :: Gv.Attribute -> Maybe Pos
attr_to_pos a =
  case a of
    Gv.Pos (Gv.PointPos (Gv.Point x y _ _)) -> Just (x, y)
    _ -> Nothing

-- | 'Attribute' to label.
attr_to_label :: Gv.Attribute -> Maybe String
attr_to_label a =
  case a of
    Gv.Label (Gv.StrLabel t) -> Just (Text.Lazy.unpack t)
    _ -> Nothing

-- | Vertex.
type V t = t

-- | Edge.
type E t = (t, t)

-- | Graph of 'V' and 'E'.
type Gr t = ([V t], [E t])

-- | 'V' with position.
type V_pos t = (t, Pos)

-- | Graph of 'V_pos'.
type Gr_pos t = ([V_pos t], [E t])

-- | 'V' with label and position.
type V_lbl_pos t = (t, (String, Pos))

-- | 'Gv.DotNode' to 'V_lbl_pos'.
dn_to_v_lbl_pos :: Gv.DotNode t -> V_lbl_pos t
dn_to_v_lbl_pos n =
  let a = Gv.nodeAttributes n
  in case (mapMaybe attr_to_label a, mapMaybe attr_to_pos a) of
      ([], [p]) -> (Gv.nodeID n, ("", p))
      ([l], [p]) -> (Gv.nodeID n, (l, p))
      _ -> error "dn_parse"

-- | 'Gv.DotEdge' to 'E'.
de_to_e :: Gv.DotEdge t -> E t
de_to_e e = (Gv.fromNode e, Gv.toNode e)

{- | 'Gv.DotGraph' to graph given node to vertex function.
Dot graphs need not contain any node statements.
However graphs that have been annotated with position data have a complete set.
-}
dg_to_gr_f :: (Gv.DotNode t -> v) -> Gv.DotGraph t -> ([v], [E t])
dg_to_gr_f f g =
  let st = Foldable.toList (Gv.graphStatements g)
      n = mapMaybe ds_to_dn st
      e = mapMaybe ds_to_de st
  in (map f n, map de_to_e e)

{- | 'Gv.DotGraph' to 'E' set.

>>> dg_to_eset (dg_parse "graph {1 -- 2 -- 3}")
[(1,2),(2,3)]
-}
dg_to_eset :: (Ord t, Eq t) => Gv.DotGraph t -> [E t]
dg_to_eset = snd . dg_to_gr_f Gv.nodeID

-- | 'Gv.DotGraph' to 'V_lbl_pos' graph.
dg_to_gr_lbl_pos :: Gv.DotGraph t -> ([V_lbl_pos t], [E t])
dg_to_gr_lbl_pos = dg_to_gr_f dn_to_v_lbl_pos

-- | 'Gv.DotGraph' to 'V_pos' graph.
dg_to_gr_pos :: Gv.DotGraph t -> Gr_pos t
dg_to_gr_pos g =
  let (n, e) = dg_to_gr_lbl_pos g
      f (k, (_, p)) = (k, p)
  in (map f n, e)

-- | (min,max) bounds of graph.
gr_pos_bounds :: Gr_pos t -> (Pos, Pos)
gr_pos_bounds (v, _) =
  let r = unzip (map snd v)
      bimap1 f = bimap f f
  in (bimap1 minimum r, bimap1 maximum r)

-- | Scale graph position data by /mul/.
gr_pos_scale :: Double -> Gr_pos t -> Gr_pos t
gr_pos_scale mul (v, e) =
  let bimap1 f = bimap f f
      v' = map (second (bimap1 (* mul))) v
  in (v', e)
