module Sound.SC3.Data.Image.PGM where

import qualified Data.Array.Unboxed as A {- array -}
import qualified Data.Vector.Storable as V {- vector -}
import Data.Word {- base -}
import qualified Graphics.Pgm as I {- pgm -}

-- | Indices are (row,column), or (y,x) where y=0 is the upper row.
type Ix = (Int,Int)

-- | Dimensions are (rows,columns), or (height,width).
type Dimensions = (Int,Int)

-- | PGM is an unboxed array of 'Int'.
type PGM = A.UArray Ix Int

-- | PGMF is an unboxed array of 'Float'.
type PGMF = A.UArray Ix Float

pgm_dimensions :: PGM -> Dimensions
pgm_dimensions i =
    let (_,(r,c)) = A.bounds i
    in (r + 1,c + 1)

pgm_load_0 :: FilePath -> IO PGM
pgm_load_0 fn = do
  r <- I.pgmsFromFile fn
  case r of
    Right (a:_) -> return a
    _ -> error (show ("pgm_load_0",r))

-- | Will use 8-bits if values are in (0,255) and 16-bits if in (0,65535).
pgm5_save_0 ::  FilePath -> PGM -> IO ()
pgm5_save_0 fn a = I.arrayToFile fn (pgm_to_word16 a)

-- | Simple cast.
pgm_to_word16 :: PGM -> A.UArray Ix Word16
pgm_to_word16 = A.amap fromIntegral

-- | Scan 'PGM' for maximum value.
pgm_max :: PGM -> Int
pgm_max = maximum . A.elems

-- | Given maximum value (ie. 'pgm_max') calculate depth (ie. 8 or 16).
pgm_depth :: Int -> Int
pgm_depth n =
    if n < 0
    then error "pgm_depth: n < 0?"
    else if n <= 255
         then 8
         else if n <= 65535
              then 16
              else error "pgm_depth: n > 65535"

-- | Type specialised 'fromIntegral'.
int_to_float :: Int -> Float
int_to_float = fromIntegral

pgm_to_pgmf :: PGM -> PGMF
pgm_to_pgmf a =
    let d = pgm_depth (pgm_max a)
        n = int_to_float ((2 ^ d) - 1)
    in A.amap ((/ n) . int_to_float) a

-- | Type specialised 'round'.
float_to_int :: Float -> Int
float_to_int = round

-- | Given bit /depth/ (typically either 8 or 16) convert 'PGMF' to 'PGM'.
pgmf_to_pgm :: Int -> PGMF -> PGM
pgmf_to_pgm d a =
    let n = int_to_float ((2 ^ d) - 1)
    in A.amap (float_to_int . (* n)) a

-- | Given 'Dimensions' and linear (row-order) index, calculate 'Ix'.
linear_to_ix :: Dimensions -> Int -> Ix
linear_to_ix (_,nc) i = i `divMod` nc

-- | Convert 'PGMF' to row order 'V.Vector'.
pgmf_to_vec :: Dimensions -> PGMF -> V.Vector Float
pgmf_to_vec dm img =
    let (nr,nc) = dm
        n = nr * nc
        f i = img A.! (linear_to_ix dm i)
    in V.generate n f
