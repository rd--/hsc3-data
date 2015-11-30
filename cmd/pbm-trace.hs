import Control.Monad {- base -}
import Data.Function {- base -}
import Data.List {- base -}
import qualified Data.Map as M {- containers -}
import System.Environment {- base -}
import System.FilePath {- base -}
import Text.Printf {- base -}

import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Read as T {- hmt -}

import qualified Sound.SC3.Data.Bitmap.PBM as I {- hsc3-data -}
import qualified Sound.SC3.Data.Bitmap.Type as B {- hsc3-data -}
import qualified Sound.SC3.Data.Trace as TR {- hsc3-data -}

type TRACE = [B.Ix]

gen_neighbour_seq :: (Eq n,Num n) => (n,n) -> [(n,n)]
gen_neighbour_seq e =
    let vec = B.neighbour_vectors_at_1_cw ++ B.neighbour_vectors_at_2_cw
    in T.rotate_starting_from_err e vec

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

trace2_set_to_trace :: [[a]] -> [(Int,a)]
trace2_set_to_trace = let f (n,l) = zip (repeat n) l in concatMap f . zip [0..]

trace2_set_write_csv :: Show n => FilePath -> [[(n,n)]] -> IO ()
trace2_set_write_csv fn =
    let f (p,q) = [show p,show q]
    in TR.trace_write_csv (show,f) fn . trace2_set_to_trace

trace_set_read_csv :: Read n => FilePath -> IO [[[n]]]
trace_set_read_csv fn = do
  tr <- TR.trace_read_csv (T.read_int,map read) fn
  return (map (map snd) (T.group_on fst tr))

pbm_trace :: (Bool,Int,Bool) -> FilePath -> FilePath -> IO ()
pbm_trace (jn,lm,ly) pbm_fn out_dir = do
  bm <- I.read_pbm_bitmap pbm_fn
  let nm = dropExtension (takeFileName pbm_fn)
      (dm,_) = bm
      tr = bm_trace bm
      tr' = if jn then trace_join_all tr else tr
      tr'' = reverse (sortBy (compare `on` length) (filter ((> lm) . length) tr'))
      out_fn ext = out_dir </> nm <.> ext
      wr (n,t) = I.write_pbm_bitindices (out_fn (printf "trace.%03d.pbm" n)) (dm,t)
  print out_dir
  when ly (mapM_ wr (zip [0::Int ..] tr''))
  I.write_pbm_bitindices (out_fn "trace.pbm") (dm,concat tr'')
  trace2_set_write_csv (out_fn "trace.csv") tr''

help :: String
help =
    unlines
    ["pbm-trace join:bool limit:int layers:bool pbm-file directory"
    ,""
    ,"  join = run join post-processor (slow)"
    ,"  limit = discard traces that have fewer elements"
    ,"  layers = write each trace as a separate PBM file"]

main :: IO ()
main = do
  a <- getArgs
  case a of
    [jn,lm,ly,fn,dir] -> pbm_trace (jn == "t",read lm,ly == "t") fn dir
    _ -> putStrLn help

{-
let fn = "/home/rohan/sw/hsc3-data/data/pbm/fh.pbm"
pbm_trace (False,20,True) fn "/tmp"
trace_set_read_csv (fn ++ ".csv") :: IO [[[Int]]]
-}
