-- | Dimensions are (rows,columns), or (height,width).
-- Indices are (row,column), or (y,x) where y=0 is the upper row.
module Sound.SC3.Data.Image.PGM where

import qualified Data.Array.Unboxed as A {- array -}
import qualified Data.Vector.Storable as V {- vector -}
import qualified Data.Map as M {- containers -}
import Data.Maybe {- base -}
import Data.Word {- base -}
import qualified Graphics.Pgm as I {- pgm -}

import Sound.SC3.Data.Bitmap.Type

-- | Bit depth, either 8 or 16.
type Depth = Int

type Greyarray n = A.UArray Ix n

-- | PGM is an unboxed array of 'Int'.
type PGM = (Depth,Greyarray Int)

-- | PGMF is an unboxed array of 'Float'.
type PGMF = Greyarray Float

dimensions_to_bounds :: Dimensions -> ((Int,Int),(Int,Int))
dimensions_to_bounds (nr,nc) = ((0,0),(nr - 1,nc - 1))

a_dimensions :: (Num t,A.Ix t,A.IArray a e) => a (t,t) e -> (t,t)
a_dimensions a = let (_,(r,c)) = A.bounds a in (r + 1,c + 1)

pgm_dimensions :: PGM -> Dimensions
pgm_dimensions = a_dimensions . snd

pgmf_dimensions :: PGMF -> Dimensions
pgmf_dimensions = a_dimensions

pgm_indices :: Dimensions -> [Ix]
pgm_indices (nr,nc) = [(r,c) | r <- [0 .. nr - 1], c <- [0 .. nc - 1]]

pgm_load_0 :: FilePath -> IO PGM
pgm_load_0 fn = do
  r <- I.pgmsFromFile fn
  case r of
    Right (a:_) -> return (max_to_depth (greyarray_max a),a)
    _ -> error (show ("pgm_load_0",r))

-- | Will use 8-bits if values are in (0,255) and 16-bits if in (0,65535).
pgm5_save_0 ::  FilePath -> PGM -> IO ()
pgm5_save_0 fn a = I.arrayToFile fn (pgm_to_word16 a)

-- | Simple cast.
pgm_to_word16 :: PGM -> A.UArray Ix Word16
pgm_to_word16 = A.amap fromIntegral . snd

-- | Scan array for maximum value.
greyarray_max :: Greyarray Int -> Int
greyarray_max = maximum . A.elems

-- | Given maximum value (ie. 'greyarray_max') calculate depth (ie. 8 or 16).
max_to_depth :: Int -> Depth
max_to_depth n =
    if n < 0
    then error "max_to_depth: n < 0?"
    else if n <= 255
         then 8
         else if n <= 65535
              then 16
              else error "max_to_depth: n > 65535"

depth_to_max :: Depth -> Int
depth_to_max n =
    case n of
      8 -> 255
      16 -> 65535
      _ -> error "depth_to_max: not 8 or 16"

-- | Type specialised 'fromIntegral'.
int_to_float :: Int -> Float
int_to_float = fromIntegral

pgm_to_pgmf :: PGM -> PGMF
pgm_to_pgmf (d,a) =
    let n = int_to_float ((2 ^ d) - 1)
    in A.amap ((/ n) . int_to_float) a

-- | Type specialised 'round'.
float_to_int :: Float -> Int
float_to_int = round

-- | Given bit /depth/ (typically either 8 or 16) convert 'PGMF' to 'PGM'.
pgmf_to_pgm :: Depth -> PGMF -> PGM
pgmf_to_pgm d a =
    let n = int_to_float ((2 ^ d) - 1)
    in (d,A.amap (float_to_int . (* n)) a)

-- | Convert 'PGMF' to row order 'V.Vector'.
pgmf_to_vec :: Dimensions -> PGMF -> V.Vector Float
pgmf_to_vec dm img =
    let (nr,nc) = dm
        n = nr * nc
        f i = img A.! (linear_to_ix dm i)
    in V.generate n f

pgmf_from_list :: Dimensions -> [(Ix,Float)] -> PGMF
pgmf_from_list dm = A.array (dimensions_to_bounds dm)

vec_to_list :: V.Storable t => (Dimensions -> Ix -> Int) -> Dimensions -> V.Vector t -> [(Ix,t)]
vec_to_list linear_f dm vec =
    let f ix = (ix,vec V.! (linear_f dm ix))
    in map f (pgm_indices dm)

-- | Convert row order 'V.Vector' to 'PGMF'.
pgmf_from_vec :: Dimensions -> V.Vector Float -> PGMF
pgmf_from_vec dm = pgmf_from_list dm . vec_to_list ix_to_linear dm

pgmf_from_vec_co :: Dimensions -> V.Vector Float -> PGMF
pgmf_from_vec_co dm = pgmf_from_list dm . vec_to_list ix_to_linear_co dm

pgm_from_list :: Dimensions -> [(Ix,Int)] -> PGM
pgm_from_list (nr,nc) l =
    let a = A.array ((0,0),(nr - 1,nc - 1)) l
        d = max_to_depth (greyarray_max a)
    in (d,a)

pgm_from_vec :: Dimensions -> V.Vector Int -> PGM
pgm_from_vec dm = pgm_from_list dm . vec_to_list ix_to_linear dm

-- | Invert colour data.
pgm_invert :: PGM -> PGM
pgm_invert (d,a) = let mx = depth_to_max d in (d,A.amap (\n -> mx - n) a)

-- * Greymap

-- | A 'Greymap' is an 'M.Map' from 'Ix' to value.
type Greymap n = (Dimensions,M.Map Ix n)

-- | Lookup value, default is zero.
greymap_ix :: Num n => Greymap n -> Ix -> n
greymap_ix (_,m) i = fromMaybe 0 (M.lookup i m)

greymap_set :: Greymap n -> Ix -> n -> Greymap n
greymap_set (d,m) i n = let m' = M.insert i n m in (d,m')

a_to_greymap :: (Num t,Num e,Ord e,A.Ix t,A.IArray a e) => a (t,t) e -> ((t,t),M.Map (t,t) e)
a_to_greymap a =
    let d = a_dimensions a
        m = M.fromList (filter ((> 0) . snd) (A.assocs a))
    in (d,m)

pgm_to_greymap :: PGM -> Greymap Int
pgm_to_greymap = a_to_greymap . snd

greymap_to_pgm :: Greymap Int -> PGM
greymap_to_pgm (dm,m) = pgm_from_list dm (M.toList m)

pgmf_to_greymap :: PGMF -> Greymap Float
pgmf_to_greymap = a_to_greymap
