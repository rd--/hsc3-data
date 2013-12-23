module Sound.SC3.Data.Trace where

import Control.Monad {- base -}
import Data.Bifunctor {- bifunctor -}
import Data.List {- base -}
import Data.List.Split {- split -}
import Data.Maybe {- base -}
import Safe {- safe -}

import qualified Music.Theory.Tuple as T {- hmt -}
import qualified Sound.File.HSndFile as F {- hsc3-sf-hsndfile -}
import Sound.SC3.Lang.Core {- hsc3-lang -}
import Sound.SC3.Plot {- hsc3-plot -}

{- | Traces are sequences @Ord t => [(t,a)]@ where t is ascending.

Ordinarily t is a time-point, and traces are temporal.

However /t/ may be, for instance, distance traversed so that line
segments (sequences of cartesian points) can be transformed into
Traces by associating each point with the distance along the line.

If there is an interpolation function (linear or otherwise) for the type /a/ we can lookup a value for any index /t/ in the window of the trace.

Traces can be both more accurate and more compact than sampled data streams.

Break-point envelopes are Traces where /a/ is a scalar
@(interpolation-type,value)@.

Traces are /normal/ if t0 is >= 0 and tn is <= 1.

Traces are /strictly normal/ if t0 == 0 and tn == 1.

-}
type Trace t a = [(t,a)]

-- | Start time of trace, or zero for null trace.
trace_start_time :: Num t => Trace t a -> t
trace_start_time = maybe 0 fst . headMay

-- | End time of trace, or zero for null trace.
trace_end_time :: Num t => Trace t a -> t
trace_end_time = maybe 0 fst . lastMay

-- | A trace window is a pait (t0,t1) indicating the begin and end
-- time points.
type Window t = (t,t)

-- | Start and end times of trace, or (0,0) for null trace.
trace_window :: Num t => Trace t a -> Window t
trace_window t = (trace_start_time t,trace_end_time t)

-- | Interpolation function type.
type Lerp_F t a b = (t -> a -> a -> b)

-- | Synonym for real numbers.
type R = Double

-- | Synonym for real valued time point.
type Time = R

-- * IO

-- | Load real valued trace stored as a sound file.
--
-- The temporal data is in the first channel, subsequent channels are
-- associated data points.  If set /nc/ is set it requires the file
-- have precisely the indicated number of _data_ channels, ie. /nc/
-- does not include the _temporal_ channel.
trace_load_sf :: Maybe Int -> FilePath -> IO (Trace Time [R])
trace_load_sf nc fn = do
  (h,t:d) <- F.read fn
  let nc' = F.channelCount h
  when (maybe False (/= (nc' - 1)) nc) (error "trace_load_sf: incorrect nc")
  return (zip t (transpose d))

-- | Variant for loading two-channel trace file.
trace_load_sf2 :: FilePath -> IO (Trace Time (R,R))
trace_load_sf2 =
    let f = map (bimap id T.t2)
    in fmap f . trace_load_sf (Just 2)

-- * Functor

-- | Map over trace times.
trace_map_t :: (t -> t') -> Trace t a -> Trace t' a
trace_map_t f = map (\(t,a) -> (f t,a))

-- | Map over trace values.
trace_map :: (a -> b) -> Trace t a -> Trace t b
trace_map f = map (\(t,a) -> (t,f a))

-- * Lookup

-- | Trace nodes that bracket time /t/.
--
-- > trace_neighbours (zip [0..9] ['a'..]) 3.5 == Just ((3.0,'d'),(4.0,'e'))
trace_neighbours :: (Ord t,Fractional t) => Trace t a -> t -> Maybe ((t,a),(t,a))
trace_neighbours tr tm =
    case tr of
      p0:p1:r -> let (t0,_) = p1
                 in if tm <= t0
                    then Just (p0,p1)
                    else trace_neighbours (p1:r) tm
      _ -> Nothing

-- | 'fromJust' of 'trace_neighbours'.
trace_neighbours_err :: (Fractional t,Ord t) => Trace t a -> t -> ((t,a),(t,a))
trace_neighbours_err = fromJust .: trace_neighbours

-- | Interpolate between to trace points using given interpolation function.
trace_lerp :: Fractional t => Lerp_F t a b -> t -> (t,a) -> (t,a) -> (t,b)
trace_lerp lerp_f n (t0,d0) (t1,d1) =
    let i = (n - t0) / (t1 - t0)
    in (n,lerp_f i d0 d1)

-- | Linear interpolating lookup, ie. 'trace_lerp' of 'trace_neighbours'.
--
-- > map (\z -> trace_lookup lerpn2 z 0.5) t
trace_lookup :: (Ord t,Fractional t) => Lerp_F t a b -> Trace t a -> t -> Maybe (t,b)
trace_lookup lerp_f t n =
    let f (p0,p1) = trace_lerp lerp_f n p0 p1
    in fmap f (trace_neighbours t n)

-- | 'trace_lookup' with default value.
trace_lookup_def :: (Ord t,Fractional t) => b -> Lerp_F t a b -> Trace t a -> t -> (t,b)
trace_lookup_def def lerp_f t n = maybe (n,def) id (trace_lookup lerp_f t n)

-- | 'fromJust' of 'trace_lookup'.
trace_lookup_err :: (Ord t,Fractional t) => Lerp_F t a b -> Trace t a -> t -> (t,b)
trace_lookup_err = fromJust .:: trace_lookup

-- * Operate

-- | Normalise so that 'trace_window' is (0,1).
--
-- > let r = [(0,'a'),(0.2,'b'),(1,'c')]
-- > in trace_normalise_t [(0,'a'),(1,'b'),(5,'c')] == r
trace_normalise_t :: Fractional t => Trace t a -> Trace t a
trace_normalise_t trace =
    let (t0,t1) = trace_window trace
        d = t1 - t0
        f t = ((t - t0) / d)
    in trace_map_t f trace

-- | Transform trace to an /n/-point linear form (time-points are
-- equi-distant) over indicated 'Window'.
trace_linearise :: (Ord t,Fractional t) => Int -> Lerp_F t a b -> Trace t a -> Window t -> Trace t b
trace_linearise n lerp_f t (t0,t1) = map (trace_lookup_err lerp_f t) (iota t0 t1 n)

-- | Variant where the range is derived implicity from input trace
-- ('trace_window').
--
-- > plotCoord (map (trace_linearise_w 1024 lerpn . trace_map fst) t)
-- > plotCoord (map (trace_map fst) t)
-- > trace2_plot_tbl t
trace_linearise_w :: (Ord t,Fractional t) => Int -> Lerp_F t a b -> Trace t a -> Trace t b
trace_linearise_w n lerp_f t = trace_linearise n lerp_f t (trace_window t)

-- | Values only of 'trace_linearise_w'.
--
-- > plotTable (map (trace_table 1024 lerpn . trace_map fst) t)
trace_table :: (Ord t,Fractional t) => Int -> Lerp_F t a b -> Trace t a -> [b]
trace_table = map snd .:: trace_linearise_w

-- | Variant of 'trace_linearize' assuming /t/ is normalised.
--
-- > trace_rescale lerpd [(0,[1]),(2,[2])] 3 == [(0,[1]),(0.5,[1.5]),(1,[2])]
trace_rescale :: (Eq t,Ord t,Fractional t) => Lerp_F t a b -> Trace t a -> Int -> Trace t b
trace_rescale lerp_f t = map (trace_lookup_err lerp_f t) . iota 0 1

-- | Interpolate maintaining temporal shape, divide each step in half.
--
-- > let r = [(0,[0]),(0.5,[0.5]),(1,[1]),(2.5,[2.5]),(4,[4])]
-- > in trace_expand lerpd [(0,[0]),(1,[1]),(4,[4])] == r
--
-- > plot_tr t
-- > plot_tr (map (trace_expand lerpd) t)
trace_expand :: (Fractional t) => Lerp_F t a a -> Trace t a -> Trace t a
trace_expand lerp_f t =
    let f p0 p1 = trace_lerp lerp_f (h p0 p1) p0 p1
        h (t0,_) (t1,_) = ((t1 - t0) / 2.0) + t0
        t' = zipWith f t (tail t)
    in interleave2 (t,t')

-- | Recursive expansion
--
-- > length (trace_expand_n [(0,[0]),(1,[1]),(4,[4])] 3) == 17
trace_expand_n :: (Fractional t,Integral n) => Lerp_F t a a -> Trace t a -> n -> Trace t a
trace_expand_n f t n =
    if n == 1
    then trace_expand f t
    else trace_expand_n f (trace_expand f t) (n - 1)

-- * Interpolation

-- | Linear interpolation.
--
-- > zipWith (lerpn 0.25) [4,5] [6,9] == [4.5,6.0]
lerpn :: Num a => a -> a -> a -> a
lerpn i a b = a + ((b - a) * i)

-- | Variant at uniform 2-tuple.
--
-- > lerpn2 0.25 (4,5) (6,9) == (4.5,6.0)
lerpn2 :: Num n => n -> (n,n) -> (n,n) -> (n,n)
lerpn2 i = T.t2_zipWith (lerpn i)

-- | Pointwise linear interpolation at lists.
--
-- > lerp_pw lerpn 0.25 [4,5] [6,9] == [4.5,6]
lerp_pw :: Lerp_F t a b -> t -> [a] -> [a] -> [b]
lerp_pw lerp_f i = zipWith (lerp_f i)

-- | 'lerp_pw' of 'lerpn'.
--
-- > lerpd 0.25 [4,5] [6,9] == [4.5,6]
lerpd :: Num c => c -> [c] -> [c] -> [c]
lerpd = lerp_pw lerpn

-- * List

-- | Generic iota function (name courtesy scheme language) with
-- explicit increment.  The last value is the given end-point
-- regardless of accumulated errors.
--
-- > iota' 0 1 0.25 5 == [0,0.25,0.5,0.75,1]
iota' :: (Eq n,Num n,Eq m,Num m) => n -> n -> n -> m -> [n]
iota' a b i n =
    case n of
      0 -> []
      1 -> [b]
      _ -> a : iota' (a + i) b i (n - 1)

-- | Fractional iota function with implicit increment.
--
-- > iota 0 1 5 == [0,0.25,0.5,0.75,1]
iota :: (Integral m,Eq n,Fractional n) => n -> n -> m -> [n]
iota a b n = iota' a b ((b - a) / fromIntegral (n - 1)) n

-- | Alternate elements of two lists.
--
-- > interleave2 ("one","two") == "otnweo"
-- > interleave2 ("long","short") == "lsohnogrt"
interleave2 :: ([t],[t]) -> [t]
interleave2 = concat . transpose . T.t2_list

-- | Inverse of 'interleave2'.
--
-- > interleave2 ("abcd","ABCD") == "aAbBcCdD"
-- > deinterleave2 "aAbBcCdD" == ("abcd","ABCD")
deinterleave2 :: [a] -> ([a],[a])
deinterleave2 = T.t2 . transpose . chunksOf 2

-- * Plotting

-- | Three-dimensional plot of two-dimensional trace (/time/ on @x@ axis).
trace2_plot_3d :: [Trace R (R,R)] -> IO ()
trace2_plot_3d = plotPath . map (map (\(t,(p,q)) -> (t,p,q)))

-- | Two-dimensional plot of two-dimensional trace (/time/ not drawn).
trace2_plot_2d :: [Trace R (R,R)] -> IO ()
trace2_plot_2d = plotCoord . map (map snd)

trace2_plot_tbl :: [Trace R (R,R)] -> IO ()
trace2_plot_tbl =
    let f t = [trace_map fst t,trace_map snd t]
    in plotCoord . concatMap f
