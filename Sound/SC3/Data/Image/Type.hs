-- | Basic image types.  Indexes are in (x,y) or (column,row) form.
-- Origin is upper right.  Layout is row order unless indicated
-- otherwise.
module Sound.SC3.Data.Image.Type where

-- | Width (number of columns).
type Width = Int

-- | Height (number of rows).
type Height = Int

-- | 'Width' (number of columns) and 'Height' (number of rows).
-- The ordering follows the indexing scheme (ie. (column,row) or (x,y)).
type Dimensions = (Width,Height)

-- | (x,y) index.
type Ix = (Int,Int)

-- | Row-order indices for given 'Dimensions'.
--
-- > img_indices (3,2) == [(0,0),(1,0),(2,0),(0,1),(1,1),(2,1)]
img_indices :: Dimensions -> [Ix]
img_indices (w,h) = [(x,y) | y <- [0 .. h - 1], x <- [0 .. w - 1]]

-- | Translate 'Ix' to linear (row-order) index.
--
-- > map (ix_to_linear (4,3)) (img_indices (4,3)) == [0 .. 11]
ix_to_linear :: Dimensions -> Ix -> Int
ix_to_linear (w,_) (x,y) = y * w + x

-- | Inverse of 'ix_to_linear'.
--
-- > map (linear_to_ix (3,2)) [0 .. 5] == img_indices (3,2)
linear_to_ix :: Dimensions -> Int -> Ix
linear_to_ix (w,_) i = let (y,x) = i `divMod` w in (x,y)
