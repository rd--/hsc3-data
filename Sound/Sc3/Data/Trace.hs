-- | Traces are sequences of (key,value) pairs where key is in Ord and the sequence is ascending.
module Sound.Sc3.Data.Trace where

import Control.Monad {- base -}
import Data.Bifunctor {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Data.List.Split as Split {- split -}
import qualified Safe {- safe -}
import qualified System.FilePath.Glob as Glob {- glob -}

import Data.Cg.Minus.Core {- hcg-minus -}
import Data.Cg.Minus.Types {- hcg-minus -}

import qualified Music.Theory.Array.Csv as T {- hmt-base -}
import qualified Music.Theory.List as T {- hmt-base -}
import qualified Music.Theory.Tuple as T {- hmt-base -}

import qualified Sound.File.HSndFile as F {- hsc3-sf-hsndfile -}

import qualified Sound.Sc3.Lang.Core as L {- hsc3-lang -}

import qualified Sound.Sc3.Plot as P {- hsc3-plot -}

{- | Traces are sequences @Ord t => [(t,a)]@ where t is ascending.

Ordinarily t is a time-point, and traces are temporal.

However /t/ may be, for instance, distance traversed so that line
segments (sequences of cartesian points) can be transformed into
Traces by associating each point with the distance along the line.

If there is an interpolation function (linear or otherwise) for the
type /a/ we can lookup a value for any index /t/ in the window of the
trace.

Traces can be both more accurate and more compact than sampled data
streams.

Break-point envelopes are Traces where /a/ is a scalar
@(interpolation-type,value)@.

Traces are /normal/ if t0 is >= 0 and tn is <= 1.

Traces are /strictly normal/ if t0 == 0 and tn == 1.

-}
type Trace t a = [(t,a)]

-- | Start time of trace, or zero for null trace.
trace_start_time :: Num t => Trace t a -> t
trace_start_time = maybe 0 fst . Safe.headMay

-- | End time of trace, or zero for null trace.
trace_end_time :: Num t => Trace t a -> t
trace_end_time = maybe 0 fst . Safe.lastMay

-- | A trace window is a pait (t0,t1) indicating the begin and end
-- time points.
type Window t = (t,t)

-- | Start and end times of trace, or (0,0) for null trace.
trace_window :: Num t => Trace t a -> Window t
trace_window t = (trace_start_time t,trace_end_time t)

-- | Interpolation function type.
type Lerp_f t a b = (t -> a -> a -> b)

-- | Synonym for real valued time point.
type Time = Double

-- * Io

trace_assert_nc :: Eq a => Maybe a -> a -> IO ()
trace_assert_nc nc n = when (maybe False (/= n) nc) (error "trace_load_sf: incorrect nc")

{- | Load real valued trace stored as a sound file.

The temporal data is in the first channel, subsequent channels are
associated data points.  If /nc/ is set it requires the file have
precisely the indicated number of _data_ channels, ie. /nc/ does not
include the _temporal_ channel.
-}
trace_load_sf :: Maybe Int -> FilePath -> IO (Trace Time [Double])
trace_load_sf nc fn = do
  (h,t:d) <- F.read fn
  trace_assert_nc nc (F.channelCount h - 1)
  return (zip t (transpose d))

-- | Require trace be of dogree two and translate to tuple form.
trace_to_t2 :: Trace t [n] -> Trace t (n,n)
trace_to_t2 = map (second T.t2_from_list)

-- | Variant for loading two-channel trace file.
trace_load_sf2 :: FilePath -> IO (Trace Time (Double,Double))
trace_load_sf2 = fmap trace_to_t2 . trace_load_sf (Just 2)

-- | Variant for set of traces given by 'glob' pattern'.
trace_load_sf_dir :: Maybe Int -> String -> IO [Trace Time [Double]]
trace_load_sf_dir n p = do
  nm <- Glob.glob p
  mapM (trace_load_sf n) nm

trace_load_sf2_dir :: String -> IO [Trace Time (Double,Double)]
trace_load_sf2_dir p = do
  nm <- Glob.glob p
  mapM trace_load_sf2 nm

trace_load_csv :: Maybe Int -> FilePath -> IO (Trace Time [Double])
trace_load_csv nc fn = do
  (_,tbl) <- T.csv_table_read (True,',',False,T.Csv_No_Align) read fn
  when (null tbl) (error "trace_load_csv: empty tbl")
  let (t, d) = T.headTail (transpose tbl)
  trace_assert_nc nc (length (head tbl) - 1)
  return (zip t (transpose d))

{- | Load degree two Trace from Csv file.

> t <- trace_load_csv2 "/home/rohan/sw/hsc3-data/data/csv/trace/b.csv"
-}
trace_load_csv2 :: FilePath -> IO (Trace Time (Double,Double))
trace_load_csv2 = fmap trace_to_t2 . trace_load_csv (Just 2)

trace_load_csv2_dir :: String -> IO [Trace Time (Double,Double)]
trace_load_csv2_dir p = do
  nm <- Glob.glob p
  mapM trace_load_csv2 nm

-- * Functor

-- | Map over trace times.
trace_map_t :: (t -> t') -> Trace t a -> Trace t' a
trace_map_t f = map (first f)

-- | Map over trace values.
trace_map :: (a -> b) -> Trace t a -> Trace t b
trace_map f = map (second f)

-- * Lookup

{- | Trace nodes that bracket time /t/, and trace starting from left neighbour.

> map (trace_locate (zip [0..9] ['a'..])) [-1,3.5,10] -- error
-}
trace_locate :: (Ord t,Fractional t) => Trace t a -> t -> Either String (((t,a),(t,a)),Trace t a)
trace_locate tr tm =
    case tr of
      p0:p1:r -> let (t0,_) = p0
                     (t1,_) = p1
                 in if tm < t0
                    then Left "trace_locate: time point before trace window"
                    else if tm <= t1
                         then Right ((p0,p1),tr)
                         else trace_locate (p1:r) tm
      _ -> Left "trace_locate: time point after trace window"

{- | 'fst' of 'trace_locate'

> trace_neighbours (zip [0..9] ['a'..]) 3.5 == Just ((3.0,'d'),(4.0,'e'))
-}
trace_neighbours :: (Ord t,Fractional t) => Trace t a -> t -> Maybe ((t,a),(t,a))
trace_neighbours = either (const Nothing) (Just . fst) L..: trace_locate

-- | 'fromJust' of 'trace_neighbours'.
trace_neighbours_err :: (Fractional t,Ord t) => Trace t a -> t -> ((t,a),(t,a))
trace_neighbours_err = fromJust L..: trace_neighbours

-- | Interpolate between to trace points using given interpolation function.
trace_lerp :: Fractional t => Lerp_f t a b -> t -> (t,a) -> (t,a) -> (t,b)
trace_lerp lerp_f n (t0,d0) (t1,d1) =
    let i = (n - t0) / (t1 - t0)
    in (n,lerp_f i d0 d1)

{- | Linear interpolating lookup, ie. 'trace_lerp' of 'trace_neighbours'.

> t <- trace_load_csv2_dir "/home/rohan/sw/hsc3-data/data/csv/trace/*.csv"
> map (\z -> trace_lookup lerpn2 z 0.5) t
-}
trace_lookup :: (Ord t,Fractional t) => Lerp_f t a b -> Trace t a -> t -> Maybe (t,b)
trace_lookup lerp_f t n =
    let f (p0,p1) = trace_lerp lerp_f n p0 p1
    in fmap f (trace_neighbours t n)

-- | 'trace_lookup' with default value.
trace_lookup_def :: (Ord t,Fractional t) => b -> Lerp_f t a b -> Trace t a -> t -> (t,b)
trace_lookup_def def lerp_f t n = fromMaybe (n,def) (trace_lookup lerp_f t n)

-- | 'fromJust' of 'trace_lookup'.
trace_lookup_err :: (Ord t,Fractional t) => Lerp_f t a b -> Trace t a -> t -> (t,b)
trace_lookup_err = fromJust L..:: trace_lookup

trace_lookup_seq_asc :: (Ord t,Fractional t) => Lerp_f t a b -> Trace t a -> [t] -> Trace t b
trace_lookup_seq_asc lerp_f =
    let loop tr n = case n of
                      n0:n' -> case trace_locate tr n0 of
                                 Right ((p0,p1),tr') -> trace_lerp lerp_f n0 p0 p1 : loop tr' n'
                                 Left err -> error err
                      _ -> []
    in loop

-- * Operate

{- | Normalise so that 'trace_window' is (0,1).

> let r = [(0,'a'),(0.2,'b'),(1,'c')]
> trace_normalise_t [(0,'a'),(1,'b'),(5,'c')] == r
-}
trace_normalise_t :: Fractional t => Trace t a -> Trace t a
trace_normalise_t trace =
    let (t0,t1) = trace_window trace
        d = t1 - t0
        f t = ((t - t0) / d)
    in trace_map_t f trace

-- | Transform trace to an /n/-point linear form (time-points are
-- equi-distant) over indicated 'Window' (which must be ascending, ie
-- /t0/ < /t1/).
trace_linearise :: (Ord t,Fractional t) => Int -> Lerp_f t a b -> Trace t a -> Window t -> Trace t b
trace_linearise n lerp_f t (t0,t1) = trace_lookup_seq_asc lerp_f t (iota t0 t1 n)

{- | Variant where the range is derived implicity from input trace ('trace_window').

> t <- trace_load_csv2_dir "/home/rohan/sw/hsc3-data/data/csv/trace/*.csv"
> P.plot_p2_ln (map (trace_linearise_w 1024 lerpn . trace_map fst) t)
> P.plot_p2_ln (map (trace_map fst) t)
> trace2_plot_tbl t
-}
trace_linearise_w :: (Ord t,Fractional t) => Int -> Lerp_f t a b -> Trace t a -> Trace t b
trace_linearise_w n lerp_f t = trace_linearise n lerp_f t (trace_window t)

{- | Values only of 'trace_linearise_w'.

> P.plot_p1_ln (map (trace_table 1024 lerpn . trace_map fst) t)
-}
trace_table :: (Ord t,Fractional t) => Int -> Lerp_f t a b -> Trace t a -> [b]
trace_table = map snd L..:: trace_linearise_w

{- | Variant of 'trace_linearize' assuming /t/ is normalised.

> trace_rescale lerpd [(0,[1]),(2,[2])] 3 == [(0,[1]),(0.5,[1.25]),(1,[1.5])]
-}
trace_rescale :: (Ord t,Fractional t) => Lerp_f t a b -> Trace t a -> Int -> Trace t b
trace_rescale lerp_f t = map (trace_lookup_err lerp_f t) . iota 0 1

{- | Interpolate maintaining temporal shape, divide each step in half.

> let r = [(0,[0]),(0.5,[0.5]),(1,[1]),(2.5,[2.5]),(4,[4])]
> trace_expand lerpd [(0,[0]),(1,[1]),(4,[4])] == r

> trace2_plot_3d (map (trace_expand lerpn2) t)
-}
trace_expand :: (Fractional t) => Lerp_f t a a -> Trace t a -> Trace t a
trace_expand lerp_f t =
    let f p0 p1 = trace_lerp lerp_f (h p0 p1) p0 p1
        h (t0,_) (t1,_) = ((t1 - t0) / 2.0) + t0
        t' = zipWith f t (tail t)
    in interleave2 (t,t')

{- | Recursive expansion

> length (trace_expand_n lerpd [(0,[0]),(1,[1]),(4,[4])] 3) == 17
-}
trace_expand_n :: (Fractional t,Integral n) => Lerp_f t a a -> Trace t a -> n -> Trace t a
trace_expand_n f t n =
    if n == 1
    then trace_expand f t
    else trace_expand_n f (trace_expand f t) (n - 1)

-- * Interpolation

{- | Linear interpolation.

> zipWith (lerpn 0.25) [4,5] [6,9] == [4.5,6.0]
-}
lerpn :: Num a => a -> a -> a -> a
lerpn i a b = a + ((b - a) * i)

{- | Variant at uniform 2-tuple.

> lerpn2 0.25 (4,5) (6,9) == (4.5,6.0)
-}
lerpn2 :: Num n => n -> (n,n) -> (n,n) -> (n,n)
lerpn2 i = T.t2_zipWith (lerpn i)

{- | Pointwise linear interpolation at lists.

> lerp_pw lerpn 0.25 [4,5] [6,9] == [4.5,6]
-}
lerp_pw :: Lerp_f t a b -> t -> [a] -> [a] -> [b]
lerp_pw lerp_f i = zipWith (lerp_f i)

{- | 'lerp_pw' of 'lerpn'.

> lerpd 0.25 [4,5] [6,9] == [4.5,6]
-}
lerpd :: Num c => c -> [c] -> [c] -> [c]
lerpd = lerp_pw lerpn

-- * Geometry

-- | Transform 'Ls' to 'Trace', /t/ is distance along line.
ls_with_distance :: Floating t => Ls t -> Trace t (Pt t)
ls_with_distance (Ls p) =
    let d = T.dx_d 0 (zipWith pt_distance p (tail p))
    in zip d p

-- * List

-- | Generic iota function (name courtesy scheme language) with
-- explicit increment.  The last value is the given end-point
-- regardless of accumulated errors.
--
-- > iota_incr 0 1 0.25 5 == [0,0.25,0.5,0.75,1]
iota_incr :: (Eq n,Num n,Eq m,Num m) => n -> n -> n -> m -> [n]
iota_incr a b i n =
    case n of
      0 -> []
      1 -> [b]
      _ -> a : iota_incr (a + i) b i (n - 1)

-- | Fractional iota function with implicit increment.
--
-- > iota 0 1 5 == [0,0.25,0.5,0.75,1]
iota :: (Integral m,Eq n,Fractional n) => n -> n -> m -> [n]
iota a b n = iota_incr a b ((b - a) / fromIntegral (n - 1)) n

-- | Alternate elements of two lists.
--
-- > interleave2 ("one","two") == "otnweo"
-- > interleave2 ("long","short") == "lsohnogrt"
interleave2 :: ([t],[t]) -> [t]
interleave2 = concat . transpose . T.t2_to_list

-- | Inverse of 'interleave2'.
--
-- > interleave2 ("abcd","ABCD") == "aAbBcCdD"
-- > deinterleave2 "aAbBcCdD" == ("abcd","ABCD")
deinterleave2 :: [a] -> ([a],[a])
deinterleave2 = T.t2_from_list . transpose . Split.chunksOf 2

-- * Plotting

{- | Three-dimensional plot of two-dimensional traces (/time/ on @x@ axis), ie. 'plot_p3_ln'.

> t <- trace_load_csv2 "/home/rohan/sw/hsc3-data/data/csv/trace/b.csv"
> trace2_plot_3d [t]
-}
trace2_plot_3d :: P.PNum t => [Trace t (t,t)] -> IO ()
trace2_plot_3d = P.plot_p3_ln . map (map (\(t,(p,q)) -> (t,p,q)))

-- | Two-dimensional plot of two-dimensional traces (/time/ not drawn), ie. 'plot_p2_ln'.
--
-- > trace2_plot_2d [t]
trace2_plot_2d :: P.PNum t => [Trace t (t,t)] -> IO ()
trace2_plot_2d = P.plot_p2_ln . map (map snd)

-- > trace2_plot_tbl [t]
trace2_plot_tbl :: P.PNum t => [Trace t (t,t)] -> IO ()
trace2_plot_tbl =
    let f t = [trace_map fst t,trace_map snd t]
    in P.plot_p2_ln . concatMap f

-- * Csv

trace_write_csv :: (n -> String,a -> [String]) -> FilePath -> [(n,a)] -> IO ()
trace_write_csv (n_pp,a_pp) fn =
    let f (n,a) = intercalate "," (n_pp n : a_pp a)
    in writeFile fn . unlines . map f

trace_read_csv :: Read n => (String -> n,[String] -> a) -> FilePath -> IO [(n,a)]
trace_read_csv (n_read,a_read) fn = do
  let opt = (False,',',False,undefined)
  (Nothing,tbl) <- T.csv_table_read opt id fn
  let f row =
          case row of
            [] -> error "trace_read_csv"
            n:a -> (n_read n,a_read a)
  return (map f tbl)
