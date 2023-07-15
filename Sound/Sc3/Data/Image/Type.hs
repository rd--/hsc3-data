{- | Basic image types.
Indexes are in (x,y) or (column,row) form.
Origin is upper left.
Layout is row order unless indicated otherwise.
-}
module Sound.Sc3.Data.Image.Type where

-- | Width (number of columns).
type Width = Int

-- | Height (number of rows).
type Height = Int

-- | 'Width' (number of columns) and 'Height' (number of rows).
-- The ordering follows the indexing scheme (ie. (column,row) or (x,y)).
type Dimensions = (Width,Height)

-- | (x/column,y/row) index.
type Ix = (Int,Int)

{- | Row-order indices for given 'Dimensions'.

>>> img_indices (3,2)
[(0,0),(1,0),(2,0),(0,1),(1,1),(2,1)]
-}
img_indices :: Dimensions -> [Ix]
img_indices (w,h) = [(x,y) | y <- [0 .. h - 1], x <- [0 .. w - 1]]

{- | Translate 'Ix' to linear (row-order) index.

>>> map (ix_to_linear (4,3)) (img_indices (4,3)) == [0 .. 11]
True
-}
ix_to_linear :: Dimensions -> Ix -> Int
ix_to_linear (w,_) (x,y) = y * w + x

{- | Index to linear (column order)

>>> map (ix_to_linear_co (4,3)) (img_indices (4,3))
[0,3,6,9,1,4,7,10,2,5,8,11]
-}
ix_to_linear_co :: Dimensions -> Ix -> Int
ix_to_linear_co (_,h) (x,y) = x * h + y

{- | Inverse of 'ix_to_linear'.

>>> map (linear_to_ix (3,2)) [0 .. 5] == img_indices (3,2)
True
-}
linear_to_ix :: Dimensions -> Int -> Ix
linear_to_ix (w,_) i = let (y,x) = i `divMod` w in (x,y)

{- | Linear to index (column order)

>>> map (linear_to_ix_co (4,3)) [0,3,6,9,1,4,7,10,2,5,8,11]
[(0,0),(0,1),(0,2),(0,3),(1,0),(1,1),(1,2),(1,3),(2,0),(2,1),(2,2),(2,3)]
-}
linear_to_ix_co :: Dimensions -> Int -> Ix
linear_to_ix_co (_,h) i = let (y,x) = i `divMod` h in (x,y)
