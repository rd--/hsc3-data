-- | Dimensions are (rows,columns), or (height,width).
-- Indices are (row,column), or (y,x) where y=0 is the upper row.
module Sound.Sc3.Data.Image.Pgm where

import Data.Maybe {- base -}
import Data.Word {- base -}

import qualified Data.Array.Unboxed as Array {- array -}
import qualified Data.Vector.Storable as Vector {- vector -}
import qualified Data.Map as Map {- containers -}

import qualified Graphics.Pgm as Pgm {- pgm -}

import qualified Music.Theory.Math.Convert as Convert {- hmt-base -}

import qualified Sound.Sc3.Data.Bitmap.Type as Bitmap {- hsc3-data -}

-- | Bit depth, either 8 or 16.
type Depth = Int

-- | Unboxed array with 'Bitmap.Ix' indices.
type Greyarray n = Array.UArray Bitmap.Ix n

-- | Pgm is an 'Depth' and an unboxed array of 'Int'.
type Pgm = (Depth,Greyarray Int)

-- | Array accessor.
pgm_array :: Pgm -> Greyarray Int
pgm_array = snd

-- | 'Bitmap.Dimensions' of 'Pgm'
pgm_dimensions :: Pgm -> Bitmap.Dimensions
pgm_dimensions = array_dimensions . pgm_array

-- | 'Bitmap.bm_indices' of 'pgm_dimensions'
pgm_indices :: Pgm -> [Bitmap.Ix]
pgm_indices = Bitmap.bm_indices . pgm_dimensions

-- | PgmF is an unboxed array of 'Float'.
type PgmF = Greyarray Float

-- | Dimensions of array.
array_dimensions :: (Num t,Array.Ix t,Array.IArray a e) => a (t,t) e -> (t,t)
array_dimensions a = let (_,(r,c)) = Array.bounds a in (r + 1,c + 1)

-- | 'Bitmap.Dimensions' of 'PgmF'
pgmf_dimensions :: PgmF -> Bitmap.Dimensions
pgmf_dimensions = array_dimensions

-- | Load 'Pgm' from file.
pgm_load_0 :: FilePath -> IO Pgm
pgm_load_0 fn = do
  r <- Pgm.pgmsFromFile fn
  case r of
    Right (a:_) -> return (max_to_depth (greyarray_max a),a)
    _ -> error (show ("pgm_load_0",r))

-- | Will use 8-bits if values are in (0,255) and 16-bits if in (0,65535).
pgm5_save_0 ::  FilePath -> Pgm -> IO ()
pgm5_save_0 fn a = Pgm.arrayToFile fn (pgm_to_word16 a)

-- | Simple cast.
pgm_to_word16 :: Pgm -> Array.UArray Bitmap.Ix Word16
pgm_to_word16 = Array.amap fromIntegral . pgm_array

-- | Scan array for maximum value.
greyarray_max :: Greyarray Int -> Int
greyarray_max = maximum . Array.elems

-- | Given maximum value (ie. 'greyarray_max') calculate depth (ie. 8 or 16).
--   This is a heuristic.  If the maximum value is <= 255 assumes 8-bit.
max_to_depth :: Int -> Depth
max_to_depth n =
    if n < 0
    then error "max_to_depth: n < 0?"
    else if n <= 255
         then 8
         else if n <= 65535
              then 16
              else error "max_to_depth: n > 65535"

{- | Maximum value given bit depth, ie 2 ^ n - 1.

>>> map depth_to_max [8,16] == [2 ^ 8 - 1,2 ^ 16 - 1]
True
-}
depth_to_max :: Depth -> Int
depth_to_max n =
    case n of
      8 -> 255
      16 -> 65535
      _ -> error "depth_to_max: not 8 or 16"

-- | Convert 'Pgm' to 'PgmF'.
pgm_to_pgmf :: Pgm -> PgmF
pgm_to_pgmf (d,a) =
    let n = Convert.int_to_float (depth_to_max d)
    in Array.amap ((/ n) . Convert.int_to_float) a

-- | Type specialised 'round'.
float_to_int :: Float -> Int
float_to_int = round

-- | Given bit /depth/ (typically either 8 or 16) convert 'PgmF' to 'Pgm'.
pgmf_to_pgm :: Depth -> PgmF -> Pgm
pgmf_to_pgm d a =
    let n = Convert.int_to_float (depth_to_max d)
    in (d,Array.amap (float_to_int . (* n)) a)

-- | Convert 'PgmF' to row order 'Vector.Vector'.
pgmf_to_vec :: Bitmap.Dimensions -> PgmF -> Vector.Vector Float
pgmf_to_vec dm img =
    let (nr,nc) = dm
        n = nr * nc
        f i = img Array.! Bitmap.linear_to_ix dm i
    in Vector.generate n f

-- | Derive 'PgmF' from dimensions and a list of (index,value) pairs.
pgmf_from_list :: Bitmap.Dimensions -> [(Bitmap.Ix,Float)] -> PgmF
pgmf_from_list dm = Array.array (Bitmap.dimensions_to_bounds dm)

vec_to_list :: Vector.Storable t => (Bitmap.Dimensions -> Bitmap.Ix -> Int) -> Bitmap.Dimensions -> Vector.Vector t -> [(Bitmap.Ix,t)]
vec_to_list linear_f dm vec =
    let f ix = (ix,vec Vector.! linear_f dm ix)
    in map f (Bitmap.bm_indices dm)

-- | Convert row order 'Vector.Vector' to 'PgmF'.
pgmf_from_vec :: Bitmap.Dimensions -> Vector.Vector Float -> PgmF
pgmf_from_vec dm = pgmf_from_list dm . vec_to_list Bitmap.ix_to_linear dm

pgmf_from_vec_co :: Bitmap.Dimensions -> Vector.Vector Float -> PgmF
pgmf_from_vec_co dm = pgmf_from_list dm . vec_to_list Bitmap.ix_to_linear_co dm

pgm_from_list :: Bitmap.Dimensions -> [(Bitmap.Ix,Int)] -> Pgm
pgm_from_list (nr,nc) l =
    let a = Array.array ((0,0),(nr - 1,nc - 1)) l
        d = max_to_depth (greyarray_max a)
    in (d,a)

pgm_from_vec :: Bitmap.Dimensions -> Vector.Vector Int -> Pgm
pgm_from_vec dm = pgm_from_list dm . vec_to_list Bitmap.ix_to_linear dm

-- | Invert colour data.
pgm_invert :: Pgm -> Pgm
pgm_invert (d,a) = let mx = depth_to_max d in (d,Array.amap (mx -) a)

-- * Greymap

-- | A 'Greymap' is a 'Map.Map' with 'Bitmap.Ix' keys.
type Greymap n = (Bitmap.Dimensions,Map.Map Bitmap.Ix n)

-- | Lookup value, default is zero.
greymap_ix :: Num n => Greymap n -> Bitmap.Ix -> n
greymap_ix (_,m) i = fromMaybe 0 (Map.lookup i m)

-- | Set value.
greymap_set :: Greymap n -> Bitmap.Ix -> n -> Greymap n
greymap_set (d,m) i n = let m' = Map.insert i n m in (d,m')

array_to_greymap :: (Num t,Num e,Ord e,Array.Ix t,Array.IArray a e) => a (t,t) e -> ((t,t),Map.Map (t,t) e)
array_to_greymap a =
    let d = array_dimensions a
        m = Map.fromList (filter ((> 0) . snd) (Array.assocs a))
    in (d,m)

-- | 'array_to_greymap' of 'pgm_array'.
pgm_to_greymap :: Pgm -> Greymap Int
pgm_to_greymap = array_to_greymap . pgm_array

greymap_to_pgm :: Greymap Int -> Pgm
greymap_to_pgm (dm,m) = pgm_from_list dm (Map.toList m)

pgmf_to_greymap :: PgmF -> Greymap Float
pgmf_to_greymap = array_to_greymap
