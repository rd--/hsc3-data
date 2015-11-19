import Control.Monad {- base -}
import Data.Function {- base -}
import Data.List {- base -}
import qualified Data.Map as M {- containers -}
import System.Environment {- base -}
import Text.Printf {- base -}

import qualified Sound.SC3.Data.Bitmap.PBM as I {- hsc3-data -}
import qualified Sound.SC3.Data.Bitmap.Type as B {- hsc3-data -}

read_pbm_bitmap :: FilePath -> IO B.BitMap
read_pbm_bitmap = fmap I.pbm_to_bitmap . I.read_pbm

{-
write_pbm_bitmap :: FilePath -> B.BitMap -> IO ()
write_pbm_bitmap fn = I.pbm4_write fn . I.bitmap_to_pbm
-}

write_pbm_bitindices :: FilePath -> B.Bitindices -> IO ()
write_pbm_bitindices fn = I.pbm4_write fn . I.bitindices_to_pbm

type TRACE = [B.Ix]

gen_neighbour_seq :: (Eq n,Num n) => (n,n) -> [(n,n)]
gen_neighbour_seq e =
    let vec = B.neighbour_vectors_at_1_cw ++ B.neighbour_vectors_at_2_cw
    in B.starting_from_err e vec

-- ix is already removed from the map, if there is a neighbour it is removed in the result map.
trace_step :: B.BitMap -> B.Ix -> Maybe (B.BitMap,B.Ix)
trace_step (d,m) ix =
    case B.bitmap_neighbour_1 (gen_neighbour_seq (0,1)) (d,m) ix of
      Nothing -> Nothing
      Just (ix',True) -> Just ((d,M.delete ix' m),ix')
      Just (_,False) -> error "trace_step: False element in map"

trace_rec :: TRACE -> B.BitMap -> B.Ix -> (B.BitMap,TRACE)
trace_rec r bm ix =
    case trace_step bm ix of
      Nothing -> (bm,reverse (ix : r))
      Just (bm',ix') -> trace_rec (ix : r) bm' ix'

trace_gen :: B.BitMap -> Maybe (B.BitMap,TRACE)
trace_gen (d,m) =
    case M.minViewWithKey m of
      Just ((ix,True),m') -> Just (trace_rec [] (d,m') ix)
      Just ((_,False),_) -> error "trace_gen: False element in map"
      Nothing -> Nothing

bm_trace :: B.BitMap -> [TRACE]
bm_trace bm =
    case trace_gen bm of
      Just (bm',tr) -> tr : bm_trace bm'
      Nothing -> []

-- | If 'TRACE's adjoin, in either direction, connect them.
trace_join :: TRACE -> TRACE -> Maybe TRACE
trace_join p q =
    case (p,q) of
      ([],_) -> error "trace_join: []"
      (_,[]) -> error "trace_join: []"
      _ -> if B.ix_are_neighbours 1 (last p) (head q)
           then Just (p ++ q)
           else if B.ix_are_neighbours 1 (last q) (head p)
                then Just (q ++ p)
                else Nothing

trace_join_set :: TRACE -> [TRACE] -> Maybe (TRACE,[TRACE])
trace_join_set c =
    let f lhs rhs =
            case rhs of
              [] -> Nothing
              tr:rhs' -> case trace_join c tr of
                           Nothing -> f (tr : lhs) rhs'
                           Just c' -> Just (c',reverse lhs ++ rhs')
    in f []

trace_join_all :: [TRACE] -> [TRACE]
trace_join_all tr =
    case tr of
      [] -> []
      c:tr' -> case trace_join_set c tr' of
                 Nothing -> c : trace_join_all tr'
                 Just (c',tr'') -> trace_join_all (c' : tr'')

help :: String
help =
    unlines
    ["pbm-trace join:bool limit:int layers:bool pbm-file"
    ,""
    ,"  join = run join post-processor (slow)"
    ,"  limit = discard traces that have fewer elements"
               ,"  layers = write each trace as a separate PBM file"]

pbm_trace :: (Bool,Int,Bool) -> FilePath -> IO ()
pbm_trace (jn,lm,ly) pbm_fn = do
  bm <- read_pbm_bitmap pbm_fn
  let (dm,_) = bm
      tr = bm_trace bm
      tr' = if jn then trace_join_all tr else tr
      tr'' = reverse (sortBy (compare `on` length) (filter ((> lm) . length) tr'))
      wr (n,t) = write_pbm_bitindices (printf "%s.trace.%03d.pbm" pbm_fn n) (dm,t)
  when ly (mapM_ wr (zip [0::Int ..] tr''))
  write_pbm_bitindices (pbm_fn ++ ".trace.pbm") (dm,concat tr'')

main :: IO ()
main = do
  a <- getArgs
  case a of
    [jn,lm,ly,fn] -> pbm_trace (jn == "t",read lm,ly == "t") fn
    _ -> putStrLn help

{-
let fn = "/home/rohan/sw/hsc3-data/data/pbm/fh.pbm"
pbm_trace (False,20,True) fn
-}
