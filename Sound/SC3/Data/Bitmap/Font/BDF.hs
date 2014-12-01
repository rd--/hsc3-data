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

-- * Bitmaps, Bitvectors and Bitindices

-- | Bit as 0 = 'False' and 1 = 'True'.
type Bit = Bool

-- | List of 'Bit's, the first 'Bit' is the leftmost.
type Bitseq = [Bit]

-- | A 'Bitmap' is a list of rows (lines), each line is a bit sequence
-- encoded in an 'Int'. The most significant bit of each line
-- represents the leftmost pixel.
type Bitmap = [Int]

-- | Unpacked 'Bitmap', each row is a 'Bitseq'.
type Bitvector = [Bitseq]

-- | The (row,column) indices for 'True' bits of a 'Bitvector'.
type Bitindices = [(Int,Int)]

-- | Unpack the first /n/ elements of a 'Bits' value.
--
-- > bitseq_show (bitseq 8 75) == "@@.@..@.."
bitseq :: (Bits a) => Int -> a -> Bitseq
bitseq n x = map (testBit x) [0 .. n]

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
bitmap_to_bitvector n = map (reverse . bitseq n)

bitmap_show :: Int -> Bitmap -> String
bitmap_show n = bitvector_show . bitmap_to_bitvector n

bitmap_ix :: Bitmap -> (Int,Int) -> Bit
bitmap_ix m (i,j) = testBit (m !! i) j

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

-- | A glyph.
data Glyph = Glyph {glyph_name :: Name
                   ,glyph_encoding :: Encoding
                   ,glyph_bbx :: Box
                   ,glyph_bitmap :: Bitmap}
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
    let Box (_,nh,_,bl) = fb
        Box (_,h,dx,dy) = glyph_bbx g
        i = r + dy - (nh - h - 2) - bl
        j = c + dx
    in if i < 0 || i >= h
       then False
       else bitmap_ix (glyph_bitmap g) (i,j)

glyph_bitvector :: Box -> Glyph -> Bitvector
glyph_bitvector fb g =
    let Box (w,h,dx,dy) = fb
        (c0,c1) = (0,w + dx)
        (r0,r1) = (dy,h + dy)
        f i j = glyph_ix fb g (i,j)
    in map (\n -> map (f n) [c1,c1-1 .. c0]) [r0..r1]

glyph_show :: Glyph -> String
glyph_show g =
    let Box (w,_,dx,_) = glyph_bbx g
    in bitmap_show (w + dx) (glyph_bitmap g)

-- * BDF

type Source = [String]

-- | A property is a (key,value) pair.
type Property = (String,String)

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

parse_glyph :: Source -> Glyph
parse_glyph s =
    let pp = properties s
        nm = property pp "STARTCHAR"
        en = read (property pp "ENCODING")
        bb = t4 (map read (words (property pp "BBX")))
    in Glyph nm en (Box bb) (parse_bitmap s)

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

{-
f <- bdf_read "/home/rohan/uc/sp-muma/lib/font/Nouveau_IBM-15.bdf"
let fb = bdf_fontboundingbox f
fb == Box (8,16,0,-4)
let cs = bdf_printing_ascii f
let Just x = from_char f 'x'
x
putStrLn$ glyph_show x
mapM_ (putStrLn . glyph_show) cs
mapM_ (putStrLn . bitvector_show . glyph_bitvector fb) cs
from_name f "A" >>= return . bitvector_to_bitindices . glyph_bitvector fb

ibm <- bdf_load_ei "/home/rohan/uc/sp-muma/lib/font/Nouveau_IBM-15.bdf"
length ibm == 95
-}
