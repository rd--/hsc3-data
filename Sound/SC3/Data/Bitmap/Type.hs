-- | Bitmap functions.  Indexes are in (row,column) or (y-descending,x) form.
-- True/1 indicates presence (black) and False/0 absence (white).
module Sound.SC3.Data.Bitmap.Type where

import Data.Bits {- base -}
import Data.Char {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Data.Map as Map {- containers -}

import qualified Music.Theory.List as List {- hmt -}

-- * Dimensions and Indices

-- | Height (number of rows).
type Height = Int

-- | Width (number of columns).
type Width = Int

-- | 'Height' (number of rows) and 'Width' (number of columns).
-- The ordering follows the indexing scheme (ie. (row,column) or (y,x)).
type Dimensions = (Height,Width)

-- | Row index.
type Row = Int

-- | Column index.
type Column = Int

-- | ('Row','Column') index.
type Ix = (Row,Column)

-- | Minima and maxima 'Ix' given 'Dimensions'.
dimensions_to_bounds :: Dimensions -> (Ix,Ix)
dimensions_to_bounds (nr,nc) = ((0,0),(nr - 1,nc - 1))

-- | Type specialised 'fst'.
ix_row :: Ix -> Row
ix_row = fst

-- | Type specialised 'snd'.
ix_column :: Ix -> Column
ix_column = snd

-- | Row-order indices given 'Dimensions'.
--
-- > bm_indices (2,3) == [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]
bm_indices :: Dimensions -> [Ix]
bm_indices (nr,nc) = [(r,c) | r <- [0 .. nr - 1], c <- [0 .. nc - 1]]

-- | Translate 'Ix' to linear (row-order) index.
--
-- > map (ix_to_linear (3,4)) (bm_indices (3,4)) == [0..11]
ix_to_linear :: Dimensions -> Ix -> Int
ix_to_linear (_,nc) (r,c) = r * nc + c

-- | Column-order variant.
--
-- > map (ix_to_linear_co (3,4)) (bm_indices (3,4)) == [0,3,6,9,1,4,7,10,2,5,8,11]
ix_to_linear_co :: Dimensions -> Ix -> Int
ix_to_linear_co (nr,_) (r,c) = c * nr + r

-- | Inverse of 'ix_to_linear'.
--
-- > map (linear_to_ix (2,3)) [0 .. 5] == bm_indices (2,3)
linear_to_ix :: Dimensions -> Int -> Ix
linear_to_ix (_,nc) i = i `divMod` nc

-- > map (linear_to_ix_co (3,4)) [0,3,6,9,1,4,7,10,2,5,8,11]
linear_to_ix_co :: Dimensions -> Int -> Ix
linear_to_ix_co (nr,_) i = i `divMod` nr

indices_displace :: (Int,Int) -> Indices -> Indices
indices_displace (dx,dy) = let f (r,c) = (r + dx,c + dy) in map f

-- | The eight vectors (ie. (dy,dx)) to move to a neighbouring cell, clockwise.
--
-- > length neighbour_vectors_at_1_cw == 8
neighbour_vectors_at_1_cw :: Num n => [(n,n)]
neighbour_vectors_at_1_cw =
    [(0, 1),( 1, 1),( 1,0),( 1,-1)
    ,(0,-1),(-1,-1),(-1,0),(-1, 1)]

-- > length neighbour_vectors_at_2_cw == 16
neighbour_vectors_at_2_cw :: Num n => [(n,n)]
neighbour_vectors_at_2_cw =
    [(0, 2),( 1, 2),( 2, 2),( 2, 1),( 2,0),( 2,-1),( 2,-2),( 1,-2)
    ,(0,-2),(-1,-2),(-2,-2),(-2,-1),(-2,0),(-2, 1),(-2, 2),(-1, 2)]

-- | Given 'Dimensions' and an 'Ix' derive the set of neighbouring indices.
--
-- > map (length . neighbour_indices (3,3)) (bm_indices (3,3)) == [3,5,3,5,8,5,3,5,3]
neighbour_indices :: [(Int,Int)] -> Dimensions -> Ix -> [Ix]
neighbour_indices n_fn (nr,nc) (r,c) =
    let f (dr,dc) =
            let r' = r + dr
                c' = c + dc
            in if r' >= 0 && r' < nr && c' >= 0 && c' < nc
               then Just (r',c')
               else Nothing
    in mapMaybe f n_fn

-- | Predicate to decide if indices neighbours by at most /distance/ moves.
--
-- > map (ix_are_neighbours 1 (0,0)) neighbour_vectors_at_1 == replicate 8 True
-- > map (ix_are_neighbours 2 (0,0)) neighbour_vectors_at_2 == replicate 16 True
ix_are_neighbours :: (Num t,Ord t) => t -> (t,t) -> (t,t) -> Bool
ix_are_neighbours d (r,c) (r',c') = abs (r - r') <= d && abs (c - c') <= d && (r,c) /= (r',c')

-- * Bitarray

-- | Bit, as 0 = 'False' and 1 = 'True'.
type Bit = Bool

-- | Function to draw bit given (true,false) or (one,zero) characters.
bit_to_char :: (Char,Char) -> Bit -> Char
bit_to_char (one,zero) x = if x then one else zero

-- | List of 'Bit's, the first 'Bit' is the leftmost.
--   "Bits are numbered from 0 with bit 0 being the least significant bit." (Data.Bits)
type Bitseq = [Bit]

-- | Show 'Bitseq', using @\@@ for 'True' and @.@ for 'False'.
bitseq_show :: Bitseq -> String
bitseq_show = map (bit_to_char ('@','.'))

-- | Given 'Bits' test the /i/th _least_ significant bit.
bitenc_test_lsb :: Bits b => b -> Int -> Bool
bitenc_test_lsb = testBit

-- | Given 'Bits' value of size /sz/ test the /i/th _most_ significant bit.
bitenc_test_msb :: Bits b => Int -> b -> Int -> Bool
bitenc_test_msb sz x i = testBit x (sz - 1 - i)

-- | Unpack the /n/ _most_ significant elements of a 'FiniteBits' value.
--
-- > bitseq_show (bitseq_msb 4 (0xA0::Data.Word.Word8)) == "@.@."
bitseq_msb :: FiniteBits b => Int -> b -> Bitseq
bitseq_msb n x = let sz = finiteBitSize x in map (bitenc_test_msb sz x) [0 .. n - 1]

-- | Unpack the /n/ _least_ significant elements of a 'FiniteBits' value.
--
-- > bitseq_show (bitseq_lsb 4 (0x05::Data.Word.Word8)) == "@.@."
bitseq_lsb :: FiniteBits b => Int -> b -> Bitseq
bitseq_lsb n x = map (bitenc_test_lsb x) [0 .. n - 1]

-- > bitseq_elem (bitseq_lsb 8 (0x05::Data.Word.Word8)) == [0,2]
bitseq_elem :: (Num n,Enum n) => Bitseq -> [n]
bitseq_elem = mapMaybe (\(ix,b) -> if b then Just ix else Nothing) . zip [0..]

-- | List of rows, each a 'Bitseq', the first is the uppermost.
type Bitarray = (Dimensions,[Bitseq])

bitarray_to_bitindices :: Bitarray -> Bitindices
bitarray_to_bitindices (dm,v) =
    let v' = zip [0..] (map (zip [0..]) v)
        f i (j,b) = if b then Just (i,j) else Nothing
        g (i,r) = mapMaybe (f i) r
    in (dm,concatMap g v')

-- | Show 'Bitarray' using 'bitseq_show'.
bitarray_show :: Bitarray -> String
bitarray_show = unlines . map bitseq_show . snd

-- * Bitindices

-- | List of 'Ix', ie. (r,c).
type Indices = [Ix]

-- | The (row,column) indices for 'True' bits of a 'Bitarray'.
type Bitindices = (Dimensions,Indices)

bitindices_height :: Bitindices -> Height
bitindices_height = fst . fst

bitindices_row :: Bitindices -> Row -> [Column]
bitindices_row (_,d) r = map ix_column (filter ((== r) . ix_row) d)

indices_by_row :: [Ix] -> [(Row,[Column])]
indices_by_row =
    let f x = (ix_row (head x),map ix_column x)
    in map f . List.group_on ix_row . sortOn ix_row

bitindices_rows :: Bitindices -> [[Column]]
bitindices_rows ((nr,_),ix) =
    let f = map snd . List.fill_gaps_ascending [] (0,nr - 1)
    in f (indices_by_row ix)

bitindices_width :: Bitindices -> Width
bitindices_width = snd . fst

bitindices_column :: Bitindices -> Column -> [Row]
bitindices_column b c = map fst (filter ((== c) . snd) (snd b))

indices_by_column :: [Ix] -> [(Column,[Row])]
indices_by_column =
    let f x = (ix_column (head x),map ix_row x)
    in map f . List.group_on ix_column . sortOn ix_column

bitindices_columns :: Bitindices -> [[Row]]
bitindices_columns ((_,nc),ix) =
    let f = map snd . List.fill_gaps_ascending [] (0,nc - 1)
    in f (indices_by_column ix)

-- | Transpose rows and columns.
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

bitindices_to_bitarray :: Bitindices -> Bitarray
bitindices_to_bitarray ((h,w),ix) =
    let f r c = (r,c) `elem` ix
        g r = map (f r) [0 .. w - 1]
    in ((h,w),map g [0 .. h - 1])

bitindices_show :: Bitindices -> String
bitindices_show = bitarray_show . bitindices_to_bitarray

-- * BitPattern

-- | A 'BitPattern' is a list of rows (lines), each line is a bit sequence
-- of /width/ elements encoded using the type parameter. The most
-- significant bit of each line represents the leftmost pixel.
type BitPattern b = (Dimensions,[b])

bitpattern_to_bitarray :: FiniteBits b => BitPattern b -> Bitarray
bitpattern_to_bitarray ((h,w),m) = ((h,w),map (bitseq_msb w) m)

-- | Index into 'BitPattern' at (row,column).
bitpattern_ix :: Bits b => BitPattern b -> (Int,Int) -> Bit
bitpattern_ix (_,m) (i,j) = bitenc_test_msb 8 (m !! i) j

bitpattern_show :: FiniteBits b => BitPattern b -> String
bitpattern_show = bitarray_show . bitpattern_to_bitarray

-- | * BitMap

-- | By convention is sparse, with only 'True' entries.
type BitMap = (Dimensions,Map.Map Ix Bool)

bitmap_get :: BitMap -> Ix -> Bool
bitmap_get (_,m) ix = Map.findWithDefault False ix m

-- | Lookup a sequence of keys in a map, halting when one is present.
--
-- > map_lookup_set (Map.fromList (zip [1..9] ['a'..])) [0,1] == Just (1,'a')
map_lookup_set :: Ord k => Map.Map k a -> [k] -> Maybe (k,a)
map_lookup_set m set =
    case set of
      [] -> Nothing
      k:set' -> case Map.lookup k m of
                  Nothing -> map_lookup_set m set'
                  Just r -> Just (k,r)

-- | Find a neighbour of 'Ix' in 'BitMap'.
bitmap_neighbour_1 :: [(Int,Int)] -> BitMap -> Ix -> Maybe (Ix,Bool)
bitmap_neighbour_1 n_fn (d,m) ix = map_lookup_set m (neighbour_indices n_fn d ix)

bitmap_to_bitindices :: BitMap -> Bitindices
bitmap_to_bitindices (d,m) = (d,map fst (filter snd (Map.toList m)))

bitindices_to_bitmap :: Bitindices -> BitMap
bitindices_to_bitmap (dm,ix) = (dm,Map.fromList (zip ix (repeat True)))

-- * Leading edge

data DIRECTION = RIGHT | LEFT | DOWN | UP deriving (Eq,Show)

direction_pp :: DIRECTION -> String
direction_pp = map toLower . show

-- > map direction_char [RIGHT,LEFT,DOWN,UP] == "rldu"
direction_char :: DIRECTION -> Char
direction_char = head . direction_pp

-- > mapMaybe parse_dir_char "rldu"
parse_dir_char :: Char -> Maybe DIRECTION
parse_dir_char c = lookup c (zip "rldu" [RIGHT,LEFT,DOWN,UP])

parse_dir_char' :: Char -> DIRECTION
parse_dir_char' =
    fromMaybe (error "parse_dir_char: not 'r','l','d' or 'u'") .
    parse_dir_char

leading_edge_f :: DIRECTION -> Dimensions -> (Ix -> Bool) -> Ix -> Bool
leading_edge_f dir (h,w) not_elem_f =
    let f_right (r,c) = c == 0 || not_elem_f (r,c - 1)
        f_left (r,c) = c == w - 1 || not_elem_f (r,c + 1)
        f_down (r,c) = r == 0 || not_elem_f (r - 1,c)
        f_up (r,c) = r == h - 1 || not_elem_f (r + 1,c)
    in case dir of
         UP -> f_up
         DOWN -> f_down
         LEFT -> f_left
         RIGHT -> f_right

bitindices_leading_edges :: DIRECTION -> Bitindices -> Bitindices
bitindices_leading_edges dir (dm,ix) =
    let le_f = leading_edge_f dir dm (`notElem` ix)
    in (dm,filter le_f ix)

bitmap_leading_edges :: DIRECTION -> BitMap -> BitMap
bitmap_leading_edges dir (d,m) =
    let le_f ix' _ = leading_edge_f dir d (\ix -> not (Map.findWithDefault False ix m)) ix'
    in (d,Map.filterWithKey le_f m)
