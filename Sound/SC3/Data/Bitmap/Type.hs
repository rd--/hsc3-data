-- | Bitmap functions.  Indexes are in (row,column) or (y,x) form.  True/1
-- indicates presence (black) and False/0 absence (white).
module Sound.SC3.Data.Bitmap.Type where

import Data.Bits {- base -}
import Data.Char {- base -}
import Data.List.Split {- split -}
import Data.List.Split.Internals {- split -}
import qualified Data.Map as M {- containers -}
import Data.Maybe {- base -}

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

-- | Row-order indices for given 'Dimensions'.
--
-- > bm_indices (2,3) == [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]
bm_indices :: Dimensions -> [Ix]
bm_indices (nr,nc) = [(r,c) | r <- [0 .. nr - 1], c <- [0 .. nc - 1]]

-- | Translate 'Ix' to linear (row-order) index.
--
-- > map (ix_to_linear (3,4)) (bm_indices (3,4)) == [0..11]
ix_to_linear :: Dimensions -> Ix -> Int
ix_to_linear (_,nc) (r,c) = r * nc + c

-- | Inverse of 'ix_to_linear'.
--
-- > map (linear_to_ix (2,3)) [0 .. 5] == bm_indices (2,3)
linear_to_ix :: Dimensions -> Int -> Ix
linear_to_ix (_,nc) i = i `divMod` nc

indices_displace :: (Int,Int) -> Indices -> Indices
indices_displace (dx,dy) = let f (r,c) = (r + dx,c + dy) in map f

on_elem :: Eq a => a -> Splitter a
on_elem e = defaultSplitter { delimiter = Delimiter [(==) e] }

-- > split_at 'a' "abcde"
split_at :: Eq a => a -> [a] -> [[a]]
split_at = split . keepDelimsL . on_elem

-- | Rotate list so that is starts at indicated element.
--
-- > starting_from 'c' "abcde" == Just "cdeab"
starting_from :: Eq a => a -> [a] -> Maybe [a]
starting_from x l =
    case split_at x l of
      [lhs,rhs] -> Just (rhs ++ lhs)
      _ -> Nothing

starting_from_err :: Eq a => a -> [a] -> [a]
starting_from_err x =
    fromMaybe (error "starting_from: non-element") .
    starting_from x

-- | The eight vectors (ie. (dy,dx)) to move to a neighbouring cell,
-- clockwise.
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
type Bitseq = [Bit]

-- | Show 'Bitseq', using @\@@ for 'True' and @.@ for 'False'.
bitseq_show :: Bitseq -> String
bitseq_show = map (bit_to_char ('@','.'))

-- | Given 'Bits' value of size /sz/ test the /i/th _most_ significant
-- bit.
bitenc_test :: Bits a => Int -> a -> Int -> Bool
bitenc_test sz x i = testBit x (sz - 1 - i)

-- | Unpack the /n/ _most_ significant elements of a 'FiniteBits' value.
--
-- > bitseq_show (bitseq 4 (0x90::Word8)) == "@..@"
bitseq :: FiniteBits b => Int -> b -> Bitseq
bitseq n x = let sz = finiteBitSize x in map (bitenc_test sz x) [0 .. n - 1]

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

-- | List of 'Ix'.
type Indices = [Ix]

-- | The (row,column) indices for 'True' bits of a 'Bitarray'.
type Bitindices = (Dimensions,Indices)

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
bitpattern_to_bitarray ((h,w),m) = ((h,w),map (bitseq w) m)

-- | Index into 'BitPattern' at (row,column).
bitpattern_ix :: Bits b => BitPattern b -> (Int,Int) -> Bit
bitpattern_ix (_,m) (i,j) = bitenc_test 8 (m !! i) j

bitpattern_show :: FiniteBits b => BitPattern b -> String
bitpattern_show = bitarray_show . bitpattern_to_bitarray

-- | * BitMap

-- | By convention is sparse, with only 'True' entries.
type BitMap = (Dimensions,M.Map Ix Bool)

bitmap_get :: BitMap -> Ix -> Bool
bitmap_get (_,m) ix = M.findWithDefault False ix m

-- | Lookup a sequence of keys in a map, halting when one is present.
--
-- > map_lookup_set (M.fromList (zip [1..9] ['a'..])) [0,1] == Just (1,'a')
map_lookup_set :: Ord k => M.Map k a -> [k] -> Maybe (k,a)
map_lookup_set m set =
    case set of
      [] -> Nothing
      k:set' -> case M.lookup k m of
                  Nothing -> map_lookup_set m set'
                  Just r -> Just (k,r)

-- | Find a neighbour of 'Ix' in 'BitMap'.
bitmap_neighbour_1 :: [(Int,Int)] -> BitMap -> Ix -> Maybe (Ix,Bool)
bitmap_neighbour_1 n_fn (d,m) ix = map_lookup_set m (neighbour_indices n_fn d ix)

bitmap_to_bitindices :: BitMap -> Bitindices
bitmap_to_bitindices (d,m) = (d,map fst (filter snd (M.toList m)))

bitindices_to_bitmap :: Bitindices -> BitMap
bitindices_to_bitmap (dm,ix) = (dm,M.fromList (zip ix (repeat True)))

-- * Leading edge

data DIRECTION = RIGHT | LEFT | DOWN | UP deriving (Eq,Show)

direction_pp :: DIRECTION -> String
direction_pp = map toLower . show

-- > mapMaybe parse_dir_char "rldu"
parse_dir_char :: Char -> Maybe DIRECTION
parse_dir_char c = lookup c (zip "rldu" [RIGHT,LEFT,DOWN,UP])

parse_dir_char' :: Char -> DIRECTION
parse_dir_char' =
    fromMaybe (error "parse_dir_char: not 'r','l','d' or 'u'") .
    parse_dir_char

bitindices_leading_edges :: DIRECTION -> Bitindices -> Bitindices
bitindices_leading_edges dir ((h,w),ix) =
    let f_right (r,c) = c == 0 || (r,c - 1) `notElem` ix
        f_left (r,c) = c == w - 1 || (r,c + 1) `notElem` ix
        f_down (r,c) = r == 0 || (r - 1,c) `notElem` ix
        f_up (r,c) = r == h - 1 || (r + 1,c) `notElem` ix
        f = case dir of
              UP -> f_up
              DOWN -> f_down
              LEFT -> f_left
              RIGHT -> f_right
    in ((h,w),filter f ix)
