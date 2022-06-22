-- | DX7 / Algorithm
module Sound.Sc3.Data.Yamaha.DX7.Algorithm where

import Data.Maybe {- base -}
import Data.List {- base -}
import Text.Printf {- base -}

import Sound.Sc3.Data.Yamaha.DX7 {- hsc3-data -}

{- | DX7 Algorithm.
     List of (dst,src) operator edges and a list of output (carrier) operators.
     Operators are zero-indexed.
-}
type DX7_Algorithm = ([(U8,U8)],[U8])

{- | Feedback is indicated by a (dst,src) pair where dst >= src.
     Feedback edges have an amplitude multiplier, ordinary edges are at unit gain.

> map dx7_algorithm_feedback_edge dx7_algorithms

Each algorithm has exactly one feedback edge.

> all (== 1) (map (length . filter (\(dst,src) -> dst >= src) . fst) dx7_algorithms)
-}
dx7_algorithm_feedback_edge :: DX7_Algorithm -> (U8,U8)
dx7_algorithm_feedback_edge =
  fromMaybe (error "dx7_algorithm_feedback_edge") .
  find (uncurry (>=)) .
  fst

-- | The 32 DX7 algorithms in sequence.
dx7_algorithms :: [DX7_Algorithm]
dx7_algorithms =
  [([(0,1),(2,3),(3,4),(4,5),(5,5)],[0,2]) -- 1
  ,([(0,1),(1,1),(2,3),(3,4),(4,5)],[0,2])
  ,([(0,1),(1,2),(3,4),(4,5),(5,5)],[0,3])
  ,([(0,1),(1,2),(3,4),(4,5),(5,3)],[0,3])
  ,([(0,1),(2,3),(4,5),(5,5)],[0,2,4]) -- 5
  ,([(0,1),(2,3),(4,5),(5,4)],[0,2,4])
  ,([(0,1),(2,3),(2,4),(4,5),(5,5)],[0,2])
  ,([(0,1),(2,3),(2,4),(4,5),(3,3)],[0,2])
  ,([(0,1),(2,3),(2,4),(4,5),(1,1)],[0,2])
  ,([(0,1),(1,2),(3,4),(3,5),(2,2)],[0,3]) -- 10
  ,([(0,1),(1,2),(3,4),(3,5),(5,5)],[0,3])
  ,([(0,1),(2,3),(2,4),(2,5),(1,1)],[0,2]) -- 12
  ,([(0,1),(2,3),(2,4),(2,5),(5,5)],[0,2])
  ,([(0,1),(2,3),(3,4),(3,5),(5,5)],[0,2]) -- 14
  ,([(0,1),(2,3),(3,4),(3,5),(1,1)],[0,2]) -- 15
  ,([(0,1),(0,2),(0,4),(2,3),(4,5),(5,5)],[0])
  ,([(0,1),(0,2),(0,4),(2,3),(4,5),(1,1)],[0])
  ,([(0,1),(0,2),(0,3),(3,4),(4,5),(2,2)],[0])
  ,([(0,1),(1,2),(3,5),(4,5),(5,5)],[0,3,4])
  ,([(0,2),(1,2),(3,4),(3,5),(2,2)],[0,1,3]) -- 20
  ,([(0,2),(1,2),(3,5),(4,5),(2,2)],[0,1,3,4])
  ,([(0,1),(2,5),(3,5),(4,5),(5,5)],[0,2,3,4])
  ,([(1,2),(3,5),(4,5),(5,5)],[0,1,3,4])
  ,([(2,5),(3,5),(4,5),(5,5)],[0,1,2,3,4])
  ,([(3,5),(4,5),(5,5)],[0,1,2,3,4]) -- 25
  ,([(1,2),(3,4),(3,5),(5,5)],[0,1,3])
  ,([(1,2),(3,4),(3,5),(2,2)],[0,1,3])
  ,([(0,1),(2,3),(3,4),(4,4)],[0,2,5])
  ,([(2,3),(4,5),(5,5)],[0,1,2,4])
  ,([(2,3),(3,4),(4,4)],[0,1,2,5]) -- 30
  ,([(4,5),(5,5)],[0,1,2,3,4])
  ,([(5,5)],[0,1,2,3,4,5])]

-- | The group structure of the DX7 algorithms, as drawn on the case.
--   Four lines, the first divided into two parts, algorithms grouped by spacing.
--
-- > sum (concat (concat dx7_algorithm_group_structure)) == 32
dx7_algorithm_group_structure :: Num t => [[[t]]]
dx7_algorithm_group_structure =
  [[[2,2,2],[3]]
  ,[[2,2,2,3]]
  ,[[1,1,1,1,1,1,1]]
  ,[[2,1,2,2]]]

{- | Simple dot graph of algorithm.
     Feedback edges are drawn in a distinct colour and do not constrain graph layout.

> let ad = unlines . dx7_algorithm_dot . (!!) dx7_algorithms
> let wr k = writeFile (printf "/tmp/dx7.%02d.dot" k) (ad k)
> mapM_ wr [0 .. 31]

-}
dx7_algorithm_dot :: DX7_Algorithm -> [String]
dx7_algorithm_dot (e,o) =
  let n_f k = printf "%d [shape=square,label=%d];" k (k + 1)
      e_f (dst,src) = printf
                      "%d -> %d [color=%s,constraint=%s];"
                      src
                      dst
                      (if src > dst then "black" else "slategray") -- orangered
                      (if src > dst then "true" else "false")
      o_f k = printf "%d -> o;" k
  in concat
     [["digraph g {"
      ,"graph [layout=dot,splines=ortho];" -- polyline ortho line
      ,"node [style=solid,color=black];"
      ,"edge [arrowhead=dot,arrowsize=0.35];"]
     ,map n_f [0::Int .. 5]
     ,["o [shape=\"circle\",label=\"·\"];"]
     ,map e_f e
     ,map o_f o
     ,["}"]]

-- | Table of (OP,[ALG]) indicating carriers.  One-indexed.
dx7_carrier_tbl :: (Num n,Enum n) => [(n,[n])]
dx7_carrier_tbl =
  let c = map (map (+ 1) . snd) dx7_algorithms
      f n (k,o) = if n `elem` o then Just k else Nothing
      x = map (\n -> mapMaybe (f n) (zip [1..] c)) [1..6]
  in zip [1..] x

-- | Is OP at ALG a carrier.  Zero-indexed.
dx7_is_carrier :: U8 -> U8 -> Bool
dx7_is_carrier op alg =
  case lookup (op + 1) dx7_carrier_tbl of
    Just alg_l -> (alg + 1) `elem` alg_l
    Nothing -> error "dx7_carrier"
