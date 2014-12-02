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
import System.FilePath {- filepath -}

-- * Bitmaps, Bitarrays and Bitindices

type Width = Int

type Height = Int

type Dimensions = (Width,Height)

-- | Bit as 0 = 'False' and 1 = 'True'.
type Bit = Bool

-- | List of 'Bit's, the first 'Bit' is the leftmost.
type Bitseq = [Bit]

-- | List of rows, each a 'Bitseq', the first is the uppermost.
type Bitarray = (Dimensions,[Bitseq])

-- | Word encoded 'Bit' sequence, the current parser only supports
-- fonts with glyphs no wider than eight pixels.
type Bitenc = Word8

-- | A 'Bitmap' is a list of rows (lines), each line is a bit sequence
-- of /width/ elements encoded in a 'Bitenc'. The most significant bit
-- of each line represents the leftmost pixel.
type Bitmap = (Dimensions,[Bitenc])

type Row = Int
type Column = Int
type Ix = (Row,Column)
type Indices = [Ix]

-- | The (row,column) indices for 'True' bits of a 'Bitarray'.
type Bitindices = (Dimensions,Indices)

-- | Given 'Bits' value of size /sz/ test the /i/th _most_ significant
-- bit.
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

bit_to_int :: Bit -> Char
bit_to_int x = if x then '1' else '0'

-- | Show 'Bitseq' using 'bit_to_char'.
bitseq_show :: Bitseq -> String
bitseq_show = map bit_to_char

-- | Show 'Bitarray' using 'bitseq_show'.
bitarray_show :: Bitarray -> String
bitarray_show = unlines . map bitseq_show . snd

bitmap_to_bitarray :: Bitmap -> Bitarray
bitmap_to_bitarray ((w,h),m) = ((w,h),map (bitseq w) m)

bitmap_show :: Bitmap -> String
bitmap_show = bitarray_show . bitmap_to_bitarray

-- | Index into bitmap at (row,column).
bitmap_ix :: Bitmap -> (Int,Int) -> Bit
bitmap_ix (_,m) (i,j) = bitenc_test 8 (m !! i) j

bitarray_to_bitindices :: Bitarray -> Bitindices
bitarray_to_bitindices (dm,v) =
    let v' = zip [0..] (map (zip [0..]) v)
        f i (j,b) = if b then Just (i,j) else Nothing
        g (i,r) = mapMaybe (f i) r
    in (dm,concatMap g v')

bitindices_to_bitarray :: Bitindices -> Bitarray
bitindices_to_bitarray ((w,h),ix) =
    let f r c = (r,c) `elem` ix
        g r = map (f r) [0 .. w - 1]
    in ((w,h),map g [0 .. h - 1])

indices_displace :: (Int,Int) -> Indices -> Indices
indices_displace (dx,dy) = let f (r,c) = (r + dx,c + dy) in map f

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

glyph_bitarray :: Box -> Glyph -> Bitarray
glyph_bitarray fb g =
    let Box (w,h,_,_) = fb
        f i j = glyph_ix fb g (i,j)
    in ((w,h),map (\n -> map (f n) [0 .. w - 1]) [0 .. h - 1])

glyph_bitindices :: Box -> Glyph -> Bitindices
glyph_bitindices bx = bitarray_to_bitindices . glyph_bitarray bx

glyph_show :: Glyph -> String
glyph_show = bitmap_show . glyph_bitmap

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

parse_bitmap :: Dimensions -> Source -> Bitmap
parse_bitmap d =
    (\m -> (d,m)) .
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
        (w,h,_,_) = bb
    in if w > 8
       then error "parse_glyph: width > 8"
       else Glyph nm en (Box bb) (parse_bitmap (w,h) s) pp

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
    let c = map fromEnum (filter isPrint (map toEnum [0 .. 127]))
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

from_char_err :: BDF -> Char -> Glyph
from_char_err f c = fromMaybe (error ("from_char: " ++ [c])) (from_char f c)

from_ascii :: Enum a => BDF -> a -> Maybe Glyph
from_ascii bdf ch = from_encoding bdf (fromEnum ch)

from_ascii_err :: BDF -> Char -> Glyph
from_ascii_err f c = fromMaybe (error ("from_ascii: " ++ [c])) (from_ascii f c)

-- * E/I

-- | ('Encoding','Bitindices') representation of a glyph.
type Glyph_EI = (Encoding,Bitindices)

-- | Load 'Glyph_EI' representations of 'bdf_printing_ascii' of font.
bdf_load_ei :: FilePath -> IO [Glyph_EI]
bdf_load_ei fn = do
  f <- bdf_read fn
  let fb = bdf_fontboundingbox f
      cs = bdf_printing_ascii f
      is = map (bitarray_to_bitindices . glyph_bitarray fb) cs
  return (zip (map glyph_encoding cs) is)

-- * PP

-- | Variant of glyph_show that orients the locates the glyph in the 'bdf_fontboundingbox'.
glyph_pp :: BDF -> Glyph -> String
glyph_pp f = bitarray_show . glyph_bitarray (bdf_fontboundingbox f)

bdf_ascii :: FilePath -> IO ()
bdf_ascii nm = do
  f <- bdf_read nm
  mapM_ (putStrLn . glyph_pp f) (bdf_printing_ascii f)

-- * PBM

bitarray_pbm1 :: Bitarray -> String
bitarray_pbm1 ((w,h),a) =
    let ty = "P1"
        dm = show w ++ " " ++ show h
        f = intersperse ' ' . map bit_to_int
    in unlines ([ty,dm] ++ map f a)

bitmap_pbm1 :: Bitmap -> String
bitmap_pbm1 = bitarray_pbm1 . bitmap_to_bitarray

glyph_pbm1 :: BDF -> Glyph -> String
glyph_pbm1 f = bitarray_pbm1 . glyph_bitarray (bdf_fontboundingbox f)

bdf_pbm1 :: BDF -> FilePath -> IO ()
bdf_pbm1 f dir = do
  let wr g = writeFile (dir </> glyph_name g <.> "pbm") (glyph_pbm1 f g)
  mapM_ wr (snd f)

-- * Text

text_bitindices :: BDF -> String -> Bitindices
text_bitindices bdf t =
    let bx = bdf_fontboundingbox bdf
        Box (w,h,_,_) = bx
        l = lines t
        nr = length l
        nc = maximum (map length l)
        c = map (\(ln,i) -> map (\(ch,j) -> ((i * h,j * w),ch)) (zip ln [0..])) (zip l [0..])
        f (sh,ch) = let (_,ix) = glyph_bitindices bx (from_ascii_err bdf ch)
                    in indices_displace sh ix
    in ((nc * w,nr * h),concatMap f (concat c))

text_pbm1 :: BDF -> String -> String
text_pbm1 bdf t =
    let ix = text_bitindices bdf t
    in bitarray_pbm1 (bitindices_to_bitarray ix)

{-
ibm <- bdf_read "/home/rohan/sw/hsc3-data/data/font/ibm.bdf"
crp <- bdf_read "/home/rohan/sw/hsc3-data/data/font/creep.bdf"

let t = unlines [['a' .. 'z'],['A' .. 'Z'],['0' .. '9']]

writeFile "/tmp/ibm.pbm" (text_pbm1 ibm t)
writeFile "/tmp/crp.pbm" (text_pbm1 crp t)

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

bdf_pbm1 ibm "/tmp"
bdf_pbm1 crp "/tmp"

-}
