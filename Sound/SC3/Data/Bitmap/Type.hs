module Sound.SC3.Data.Bitmap.Type where

import Data.Bits {- base -}
import Data.Maybe {- base -}

-- * Bitmaps, Bitarrays and Bitindices

-- | Width (number of columns).
type Width = Int

-- | Height (number of rows).
type Height = Int

-- | 'Height' and 'Width', the ordering follows the indexing scheme (ie. rows then columns).
type Dimensions = (Height,Width)

-- | Bit, as 0 = 'False' and 1 = 'True'.
type Bit = Bool

-- | List of 'Bit's, the first 'Bit' is the leftmost.
type Bitseq = [Bit]

-- | List of rows, each a 'Bitseq', the first is the uppermost.
type Bitarray = (Dimensions,[Bitseq])

-- | A 'Bitmap' is a list of rows (lines), each line is a bit sequence
-- of /width/ elements encoded using the type paramteter. The most
-- significant bit of each line represents the leftmost pixel.
type Bitmap b = (Dimensions,[b])

-- | Row index.
type Row = Int

-- | Column index.
type Column = Int

-- | ('Row','Column') index.
type Ix = (Row,Column)

-- | List of 'Ix'.
type Indices = [Ix]

-- | The (row,column) indices for 'True' bits of a 'Bitarray'.
type Bitindices = (Dimensions,Indices)

-- | Given 'Bits' value of size /sz/ test the /i/th _most_ significant
-- bit.
bitenc_test :: Bits a => Int -> a -> Int -> Bool
bitenc_test sz x i = testBit x (sz - 1 - i)

-- | Unpack the /n/ _most_ significant elements of a 'FiniteBits' value.
--
-- > bitseq_show (bitseq 4 (0x90::Word8)) == "@..@"
bitseq :: FiniteBits b => Int -> b -> Bitseq
bitseq n x = let sz = finiteBitSize x in map (bitenc_test sz x) [0 .. n - 1]

bitmap_to_bitarray :: FiniteBits b => Bitmap b -> Bitarray
bitmap_to_bitarray ((h,w),m) = ((h,w),map (bitseq w) m)

-- | Index into bitmap at (row,column).
bitmap_ix :: Bits b => Bitmap b -> (Int,Int) -> Bit
bitmap_ix (_,m) (i,j) = bitenc_test 8 (m !! i) j

bitindices_height :: Bitindices -> Height
bitindices_height = fst . fst

bitindices_row :: Bitindices -> Row -> [Column]
bitindices_row b r = map snd (filter ((== r) . fst) (snd b))

bitindices_rows :: Bitindices -> [[Column]]
bitindices_rows b = map (bitindices_row b) [0 .. bitindices_height b - 1]

bitindices_width :: Bitindices -> Width
bitindices_width = snd . fst

bitindices_column :: Bitindices -> Column -> [Row]
bitindices_column b c = map fst (filter ((== c) . snd) (snd b))

bitindices_columns :: Bitindices -> [[Row]]
bitindices_columns b = map (bitindices_column b) [0 .. bitindices_width b - 1]

bitindices_swap :: Bitindices -> Bitindices
bitindices_swap (dm,ix) = let f (i,j) = (j,i) in (f dm,map f ix)

-- | Magnify by (height,width) multipliers.
--
-- > bitindices_magnify (8,2) ((2,2),[(0,0),(1,1)])
bitindices_magnify :: (Int,Int) -> Bitindices -> Bitindices
bitindices_magnify (mx,my) ((h,w),ix) =
    let f (r,c) = let r' = r * my
                      c' = c * mx
                  in [(i,j) | i <- [r' .. r' + my - 1], j <- [c' .. c' + mx - 1]]
    in ((h * mx,w * my),concatMap f ix)

bitarray_to_bitindices :: Bitarray -> Bitindices
bitarray_to_bitindices (dm,v) =
    let v' = zip [0..] (map (zip [0..]) v)
        f i (j,b) = if b then Just (i,j) else Nothing
        g (i,r) = mapMaybe (f i) r
    in (dm,concatMap g v')

bitindices_to_bitarray :: Bitindices -> Bitarray
bitindices_to_bitarray ((h,w),ix) =
    let f r c = (r,c) `elem` ix
        g r = map (f r) [0 .. w - 1]
    in ((h,w),map g [0 .. h - 1])

bitindices_leading_edges :: Bitindices -> Bitindices
bitindices_leading_edges (dm,ix) =
    let f (r,c) = c == 0 || (r,c - 1) `notElem` ix
    in (dm,filter f ix)

indices_displace :: (Int,Int) -> Indices -> Indices
indices_displace (dx,dy) = let f (r,c) = (r + dx,c + dy) in map f

-- * Show and PP

-- | Function to draw bit given (true,false) or (one,zero) characters.
bit_to_char :: (Char,Char) -> Bit -> Char
bit_to_char (one,zero) x = if x then one else zero

-- | Show 'Bitseq', using @\@@ for 'True' and @.@ for 'False'.
bitseq_show :: Bitseq -> String
bitseq_show = map (bit_to_char ('@','.'))

-- | Show 'Bitarray' using 'bitseq_show'.
bitarray_show :: Bitarray -> String
bitarray_show = unlines . map bitseq_show . snd

bitmap_show :: FiniteBits b => Bitmap b -> String
bitmap_show = bitarray_show . bitmap_to_bitarray

bitindices_show :: Bitindices -> String
bitindices_show = bitarray_show . bitindices_to_bitarray
