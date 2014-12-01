-- | Parser for the _Glyph Bitmap Distribution Format_ (BDF) by Adobe.
--
-- Adobe Glyph Bitmap Distribution Format (BDF) Specification, version 2.2.
-- <http://partners.adobe.com/public/developer/en/font/5005.BDF_Spec.pdf>
module Sound.SC3.Data.Bitmap.Font.BDF where

import Data.Bits {- base -}
import Data.Char {- base -}
import Data.List {- base -}
import Data.List.Split {- split -}
import Data.Maybe {- base -}
import Data.Word {- base -}

-- * Bitmaps, Bitvectors and Bitindices

-- | Bit as 0 = 'False' and 1 = 'True'.
type Bit = Bool

-- | List of 'Bit's, the first 'Bit' is the leftmost.
type Bitseq = [Bit]

-- | Word encoded 'Bit' sequence, the current parser only supports
-- fonts with glyphs no wider than eight pixels.
type Bitenc = Word8

-- | A 'Bitmap' is a list of rows (lines), each line is a bit sequence
-- encoded in an 'Int'. The most significant bit of each line
-- represents the leftmost pixel.
type Bitmap = [Bitenc]

-- | Unpacked 'Bitmap', each row is a 'Bitseq'.
type Bitvector = [Bitseq]

-- | The (row,column) indices for 'True' bits of a 'Bitvector'.
type Bitindices = [(Int,Int)]

-- | Given 'Bits' value of size /sz/' test the /i/th most _most_
-- significant bit.
bitenc_test :: Bits a => Int -> a -> Int -> Bool
bitenc_test sz x i = testBit x (sz - 1 - i)

-- | Unpack the /n/ _most_ significant elements of a 'Bits' value.
--
-- > bitseq_show (bitseq 4 (0x90::Word8)) == "@..@"
bitseq :: FiniteBits b => Int -> b -> Bitseq
bitseq n x = let sz = finiteBitSize x in map (bitenc_test sz x) [0 .. n - 1]

-- | Draw bit as @@@ for 'True' and @.@ for 'False'.
bit_to_char :: Bit -> Char
bit_to_char x = if x then '@' else '.'

-- | Show 'Bitseq' using 'bit_to_char'.
bitseq_show :: Bitseq -> String
bitseq_show = map bit_to_char

-- | Show 'Bitvector' using 'bitseq_show'.
bitvector_show :: Bitvector -> String
bitvector_show = unlines . map bitseq_show

bitmap_to_bitvector :: Int -> Bitmap -> Bitvector
bitmap_to_bitvector n = map (bitseq n)

bitmap_show :: Int -> Bitmap -> String
bitmap_show n = bitvector_show . bitmap_to_bitvector n

-- | Index into bitmap at (row,column).
bitmap_ix :: Bitmap -> (Int,Int) -> Bit
bitmap_ix m (i,j) = bitenc_test 8 (m !! i) j

bitvector_to_bitindices :: Bitvector -> Bitindices
bitvector_to_bitindices v =
    let v' = zip [0..] (map (zip [0..]) v)
        f i (j,b) = if b then Just (i,j) else Nothing
        g (i,r) = mapMaybe (f i) r
    in concatMap g v'

-- * Glyphs

-- | Bounding box
data Box = Box (Int,Int,Int,Int) deriving (Eq,Show)

-- | The name of the glyph.
type Name = String

-- | Encoding point (ASCII).
type Encoding = Int

-- | A property is a (key,value) pair.
type Property = (String,String)

-- | A glyph.
data Glyph = Glyph {glyph_name :: Name
                   ,glyph_encoding :: Encoding
                   ,glyph_bbx :: Box
                   ,glyph_bitmap :: Bitmap
                   ,glyph_properties :: [Property]}
             deriving (Eq,Show)

-- | The 'glyph_name' if it is a single character, else 'Nothing'.
glyph_char :: Glyph -> Maybe Char
glyph_char g =
    case glyph_name g of
      [c] -> Just c
      _ -> Nothing

-- | Given the font bounding 'Box', a 'Glyph' and a (row,column)
-- index, lookup the bit at that index.
--
-- nh = normal height, bl = base-line, r = row, c = column
glyph_ix :: Box -> Glyph -> (Int,Int) -> Bit
glyph_ix fb g (r,c) =
    let Box (_,h,dx,dy) = fb
        Box (g_w,g_h,g_dx,g_dy) = glyph_bbx g
        i = r + g_dy - (h - g_h) - dy -- ?
        j = c - g_dx + dx -- ?
    in if i < 0 || i >= g_h || j < 0 || j >= g_w
       then False
       else bitmap_ix (glyph_bitmap g) (i,j)

glyph_bitvector :: Box -> Glyph -> Bitvector
glyph_bitvector fb g =
    let Box (w,h,_,_) = fb
        f i j = glyph_ix fb g (i,j)
    in map (\n -> map (f n) [0..w]) [0..h]

glyph_show :: Glyph -> String
glyph_show g =
    let Box (w,_,_,_) = glyph_bbx g
    in bitmap_show w (glyph_bitmap g)

-- * BDF

type Source = [String]

-- | The header is a list of 'Property's.
type Header = [Property]

type BDF = (Header,[Glyph])

properties :: Source -> [Property]
properties =
    let f r = let (k,v) = span (not . isSpace) r
              in (k,dropWhile isSpace v)
    in map f

property :: [Property] -> String -> String
property p n = maybe "" snd (find ((==) n . fst) p)

parse_bitmap :: Source -> Bitmap
parse_bitmap =
    map (read . ("0x" ++)) .
    takeWhile ((/=) "ENDCHAR") .
    tail .
    dropWhile ((/=) "BITMAP")

bdf_glyph_property_keys :: [String]
bdf_glyph_property_keys =
    ["ENCODING"
    ,"SWIDTH","DWIDTH"
    ,"SWIDTH1","DWIDTH1"
    ,"VVECTOR"
    ,"BBX"]

parse_glyph :: Source -> Glyph
parse_glyph s =
    let pp' = properties s
        pp = filter (flip elem bdf_glyph_property_keys . fst) pp'
        nm = property pp' "STARTCHAR" -- not strictly a property...
        en = read (property pp "ENCODING")
        bb = t4 (map read (words (property pp "BBX")))
        (w,_,_,_) = bb
    in if w > 8
       then error "parse_glyph: width > 8"
       else Glyph nm en (Box bb) (parse_bitmap s) pp

-- splitWhen variant (keeps delimiters at left)
sep_when :: (a -> Bool) -> [a] -> [[a]]
sep_when = split . keepDelimsL . whenElt

bdf_parse :: Source -> BDF
bdf_parse s =
  let (h:g) = sep_when (isPrefixOf "STARTCHAR") s
  in (properties h,map parse_glyph g)

bdf_read :: FilePath -> IO BDF
bdf_read fn = do
  s <- readFile fn
  return (bdf_parse (lines s))

t4 :: [t] -> (t,t,t,t)
t4 l = case l of {[p,q,r,s] -> (p,q,r,s); _ -> error "t4"}

bdf_fontboundingbox :: BDF -> Box
bdf_fontboundingbox (h,_) =
    let f = Box . t4 . map read . words
    in f (property h "FONTBOUNDINGBOX")

bdf_printing_ascii :: BDF -> [Glyph]
bdf_printing_ascii f =
    let c = map fromEnum (filter isPrint (map toEnum [0..127]))
    in mapMaybe (from_encoding f) c

-- * BDF lookup

-- | Given 'Encoding', lookup 'Glyph'.
from_encoding :: BDF -> Encoding -> Maybe Glyph
from_encoding (_,g) e = find ((==) e . glyph_encoding) g

-- | Given 'Name', lookup 'Glyph'.
from_name :: BDF -> Name -> Maybe Glyph
from_name (_,g) e = find ((==) e . glyph_name) g

-- | Given 'Char', lookup 'Glyph'.
from_char :: BDF -> Char -> Maybe Glyph
from_char (_,g) e =
    let f = maybe False ((==) e) . glyph_char
    in find f g

-- * E/I

-- | ('Encoding','Bitindices') representation of a glyph.
type Glyph_EI = (Encoding,Bitindices)

-- | Load 'Glyph_EI' representations of 'bdf_printing_ascii' of font.
bdf_load_ei :: FilePath -> IO [Glyph_EI]
bdf_load_ei fn = do
  f <- bdf_read fn
  let fb = bdf_fontboundingbox f
      cs = bdf_printing_ascii f
      is = map (bitvector_to_bitindices . glyph_bitvector fb) cs
  return (zip (map glyph_encoding cs) is)

-- * PP

-- | Variant of glyph_show that orients the locates the glyph in the 'bdf_fontboundingbox'.
glyph_pp :: BDF -> Glyph -> String
glyph_pp f = bitvector_show . glyph_bitvector (bdf_fontboundingbox f)

bdf_ascii :: FilePath -> IO ()
bdf_ascii nm = do
  f <- bdf_read nm
  mapM_ (putStrLn . glyph_pp f) (bdf_printing_ascii f)

{-
ibm <- bdf_read "/home/rohan/sw/hsc3-data/data/font/ibm.bdf"
crp <- bdf_read "/home/rohan/sw/hsc3-data/data/font/creep.bdf"

bdf_fontboundingbox ibm == Box (8,16,0,-4)
bdf_fontboundingbox crp == Box (7,12,-1,-3)

mapM_ (putStrLn . glyph_show) (snd ibm)
mapM_ (putStrLn . glyph_show) (snd crp)

let ibm_cs = bdf_printing_ascii ibm
let crp_cs = bdf_printing_ascii crp

mapM_ (putStrLn . glyph_pp ibm) ibm_cs
mapM_ (putStrLn . glyph_pp crp) crp_cs

ibm_ei <- bdf_load_ei "/home/rohan/sw/hsc3-data/data/font/ibm.bdf"
length ibm_ei == 95

bdf_ascii "/home/rohan/sw/hsc3-data/data/font/ibm.bdf"
bdf_ascii "/home/rohan/sw/hsc3-data/data/font/creep.bdf"

-}
