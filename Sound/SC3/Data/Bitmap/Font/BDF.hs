module Sound.SC3.Data.Bitmap.Font.BDF where

import Data.Bits {- base -}
import Data.Char {- base -}
import Data.List {- base -}
import Data.List.Split {- split -}
import Data.Maybe {- base -}

-- * Bitmaps, Bitvectors and Bitindices

type Bit = Bool
type Bitmap = [Int]
type Bitvector = [[Bit]]
type Bitindices = [(Int,Int)]

bit_seq :: (Bits a) => Int -> a -> [Bit]
bit_seq n x = map (testBit x) [0 .. n]

bit_to_char :: Bit -> Char
bit_to_char x = if x then '@' else '.'

bitvector_show :: Bitvector -> String
bitvector_show = unlines . map (map bit_to_char)

bitmap_to_bitvector :: Int -> Bitmap -> Bitvector
bitmap_to_bitvector n = map (reverse . bit_seq n)

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

data Box = Box [Int] deriving (Eq,Show)
type Name = String
type Encoding = Int
type Property = (String,String)
data Glyph = Glyph {glyph_name :: Name
                   ,glyph_encoding :: Encoding
                   ,glyph_bbx :: Box
                   ,glyph_bitmap :: Bitmap}
             deriving (Eq,Show)

glyph_char :: Glyph -> Maybe Char
glyph_char g =
    case glyph_name g of
      [c] -> Just c
      _ -> Nothing

-- nh = normal height, bl = base-line, r = row, c = column
glyph_ix :: Box -> Glyph -> (Int,Int) -> Bit
glyph_ix fb g (r,c) =
    let Box [_,nh,_,bl] = fb
        Box [_,h,dx,dy] = glyph_bbx g
        i = r + dy - (nh - h - 2) - bl
        j = c + dx
    in if i < 0 || i >= h
       then False
       else bitmap_ix (glyph_bitmap g) (i,j)

glyph_bitvector :: Box -> Glyph -> Bitvector
glyph_bitvector fb g =
    let Box [w,h,dx,dy] = fb
        (c0,c1) = (0,w+dx)
        (r0,r1) = (dy,h+dy)
        f i j = glyph_ix fb g (i,j)
    in map (\n -> map (f n) [c1,c1-1 .. c0]) [r0..r1]

glyph_show :: Glyph -> String
glyph_show g =
    let Box [w,_,dx,_] = glyph_bbx g
    in bitmap_show (w+dx) (glyph_bitmap g)

-- * BDF

type Source = [String]
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
    map (read . ("0x"++)) .
    takeWhile ((/=) "ENDCHAR") .
    tail .
    dropWhile ((/=) "BITMAP")

parse_glyph :: Source -> Glyph
parse_glyph s =
    let pp = properties s
        nm = property pp "STARTCHAR"
        en = read (property pp "ENCODING")
        bb = map read (words (property pp "BBX"))
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

bdf_fontboundingbox :: BDF -> Box
bdf_fontboundingbox (h,_) =
    let f = Box . map read . words
    in f (property h "FONTBOUNDINGBOX")

bdf_printing_ascii :: BDF -> [Glyph]
bdf_printing_ascii f =
    let c = map fromEnum (filter isPrint (map toEnum [0..127]))
    in mapMaybe (from_encoding f) c

-- * BDF lookup

from_encoding :: BDF -> Encoding -> Maybe Glyph
from_encoding (_,g) e = find ((==) e . glyph_encoding) g

from_name :: BDF -> Name -> Maybe Glyph
from_name (_,g) e = find ((==) e . glyph_name) g

from_char :: BDF -> Char -> Maybe Glyph
from_char (_,g) e =
    let f = maybe False ((==) e) . glyph_char
    in find f g

{-
f <- bdf_read "/home/rohan/uc/sp-muma/lib/font/Nouveau_IBM-15.bdf"
let fb = bdf_fontboundingbox f
let cs = bdf_printing_ascii f
mapM_ (putStrLn . glyph_show) cs
mapM_ (putStrLn . bitvector_show . glyph_bitvector fb) cs
from_name f "A" >>= return . bitvector_to_bitindices . glyph_bitvector fb
-}
