-- | Parser for the /Glyph Bitmap Distribution Format/ (BDF) by Adobe.
--
-- Adobe Glyph Bitmap Distribution Format (BDF) Specification, version 2.2.
-- <http://partners.adobe.com/public/developer/en/font/5005.BDF_Spec.pdf>
module Sound.Sc3.Data.Bitmap.Font.Bdf where

import Data.Char {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import Data.Word {- base -}
import System.FilePath {- filepath -}

import qualified Data.List.Split as Split {- split -}

import qualified Sound.Sc3.Data.Bitmap.Pbm as Pbm {- hsc3-data -}
import qualified Sound.Sc3.Data.Bitmap.Type as Bitmap  {- hsc3-data -}

-- * Glyphs

-- | Bounding box (w,h,dx,dy)
newtype Box = Box (Int,Int,Int,Int) deriving (Eq,Show)

-- | The name of the glyph.
type Name = String

-- | Encoding point (ASCII).
type Encoding = Int

-- | A property is a (key,value) pair.
type Property = (String,String)

-- | A glyph, encoded as a 'Word8' 'BitPattern' (the current Bdf parser only supports
-- fonts with glyphs no wider than eight pixels).
data Glyph = Glyph {glyph_name :: Name
                   ,glyph_encoding :: Encoding
                   ,glyph_bbox :: Box
                   ,glyph_bitpattern :: Bitmap.BitPattern Word8
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
glyph_ix :: Box -> Glyph -> (Int,Int) -> Bitmap.Bit
glyph_ix fb g (r,c) =
    let Box (_,h,dx,dy) = fb
        Box (g_w,g_h,g_dx,g_dy) = glyph_bbox g
        i = r + g_dy - (h - g_h) - dy -- ?
        j = c - g_dx + dx -- ?
    in not (i < 0 || i >= g_h || j < 0 || j >= g_w) &&
       Bitmap.bitpattern_ix (glyph_bitpattern g) (i,j)

glyph_bitarray :: Box -> Glyph -> Bitmap.Bitarray
glyph_bitarray fb g =
    let Box (w,h,_,_) = fb
        f i j = glyph_ix fb g (i,j)
    in ((h,w),map (\n -> map (f n) [0 .. w - 1]) [0 .. h - 1])

glyph_bitindices :: Box -> Glyph -> Bitmap.Bitindices
glyph_bitindices bx = Bitmap.bitarray_to_bitindices . glyph_bitarray bx

-- * Bdf

type Source = [String]

-- | The header is a list of 'Property's.
type Header = [Property]

type Bdf = (Header,[Glyph])

properties :: Source -> [Property]
properties =
    let f r = let (k,v) = break isSpace r
              in (k,dropWhile isSpace v)
    in map f

property :: [Property] -> String -> String
property p n = maybe "" snd (find ((==) n . fst) p)

parse_bitpattern :: Read b => Bitmap.Dimensions -> Source -> Bitmap.BitPattern b
parse_bitpattern d =
    (\m -> (d,m)) .
    map (read . ("0x" ++)) .
    takeWhile (/= "ENDCHAR") .
    tail .
    dropWhile (/= "BITMAP")

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
       else Glyph nm en (Box bb) (parse_bitpattern (h,w) s) pp

-- splitWhen variant (keeps delimiters at left)
sep_when :: (a -> Bool) -> [a] -> [[a]]
sep_when = Split.split . Split.keepDelimsL . Split.whenElt

bdf_parse :: Source -> Bdf
bdf_parse s =
  let (h:g) = sep_when (isPrefixOf "STARTCHAR") s
  in (properties h,map parse_glyph g)

-- | 'bdf_parse' of 'readFile'
bdf_read :: FilePath -> IO Bdf
bdf_read fn = do
  s <- readFile fn
  return (bdf_parse (lines s))

t4 :: [t] -> (t,t,t,t)
t4 l = case l of {[p,q,r,s] -> (p,q,r,s); _ -> error "t4"}

bdf_fontboundingbox :: Bdf -> Box
bdf_fontboundingbox (h,_) =
    let f = Box . t4 . map read . words
    in f (property h "FONTBOUNDINGBOX")

bdf_printing_ascii :: Bdf -> [Glyph]
bdf_printing_ascii f =
    let c = map fromEnum (filter isPrint (map toEnum [0 .. 127]))
    in mapMaybe (from_encoding f) c

-- * Bdf lookup

-- | Given 'Encoding', lookup 'Glyph'.
from_encoding :: Bdf -> Encoding -> Maybe Glyph
from_encoding (_,g) e = find ((==) e . glyph_encoding) g

-- | Given 'Name', lookup 'Glyph'.
from_name :: Bdf -> Name -> Maybe Glyph
from_name (_,g) e = find ((==) e . glyph_name) g

-- | Given 'Char', lookup 'Glyph'.
from_char :: Bdf -> Char -> Maybe Glyph
from_char (_,g) e =
    let f = (Just e ==) . glyph_char
    in find f g

from_char_err :: Bdf -> Char -> Glyph
from_char_err f c = fromMaybe (error ("from_char: " ++ [c])) (from_char f c)

from_ascii :: Enum a => Bdf -> a -> Maybe Glyph
from_ascii bdf ch = from_encoding bdf (fromEnum ch)

from_ascii_err :: Bdf -> Char -> Glyph
from_ascii_err f c = fromMaybe (error ("from_ascii: " ++ [c])) (from_ascii f c)

-- * E/I

-- | ('Encoding','Bitindices') representation of a glyph.
type Glyph_EI = (Encoding,Bitmap.Bitindices)

-- | Load 'Glyph_EI' representations of 'bdf_printing_ascii' of font.
bdf_load_ei :: FilePath -> IO [Glyph_EI]
bdf_load_ei fn = do
  f <- bdf_read fn
  let fb = bdf_fontboundingbox f
      cs = bdf_printing_ascii f
      is = map (Bitmap.bitarray_to_bitindices . glyph_bitarray fb) cs
  return (zip (map glyph_encoding cs) is)

-- * Show and PP

-- | Variant of 'Show' instance that locates the glyph in the
-- 'bdf_fontboundingbox', ie. 'bitarray_show' of 'glyph_bitarray'.
glyph_pp :: Bdf -> Glyph -> String
glyph_pp f = Bitmap.bitarray_show . glyph_bitarray (bdf_fontboundingbox f)

-- | 'glyph_pp' of all 'bdf_printing_ascii' of font given by 'FilePath'.
bdf_ascii :: FilePath -> IO ()
bdf_ascii nm = do
  f <- bdf_read nm
  mapM_ (putStrLn . glyph_pp f) (bdf_printing_ascii f)

-- * Pbm1

-- | 'Pbm1' of 'Glyph'.
glyph_pbm1 :: Bdf -> Glyph -> Pbm.Pbm1
glyph_pbm1 f = Pbm.bitarray_pbm1 . glyph_bitarray (bdf_fontboundingbox f)

-- | Given 'Bdf' /font/ and directory name, write all glyphs as 'Pbm1'
-- files, the name of the file is given by 'glyph_name'.
bdf_pbm1 :: Bdf -> FilePath -> IO ()
bdf_pbm1 f dir = do
  let wr g = writeFile (dir </> glyph_name g <.> "pbm") (glyph_pbm1 f g)
  mapM_ wr (snd f)

-- * Text

-- | Given a 'Bdf' /font/ and a 'String' /text/, generate a
-- 'Bitindices' value.  All glyphs are equal size, given by
-- 'bdf_fontboundingbox'.
text_bitindices :: Bdf -> String -> Bitmap.Bitindices
text_bitindices bdf t =
    let bx = bdf_fontboundingbox bdf
        Box (w,h,_,_) = bx
        l = lines t
        nr = length l
        nc = maximum (map length l)
        c = zipWith (\ln i -> zipWith (\ch j -> ((i * h,j * w),ch)) ln [0..]) l [0..]
        f (sh,ch) = let (_,ix) = glyph_bitindices bx (from_ascii_err bdf ch)
                    in Bitmap.indices_displace sh ix
    in ((nr * h,nc * w),concatMap f (concat c))

-- | 'Pbm1' of 'text_bitindices'.
text_pbm1 :: Bdf -> String -> Pbm.Pbm1
text_pbm1 bdf = Pbm.bitindices_pbm1 . text_bitindices bdf

{-

ibm <- bdf_read "/home/rohan/sw/hsc3-data/data/font/ibm.bdf"
crp <- bdf_read "/home/rohan/sw/hsc3-data/data/font/creep.bdf"

let t = unlines [['a' .. 'z'],['A' .. 'Z'],['0' .. '9']]

writeFile "/tmp/ibm.pbm" (text_pbm1 ibm t)
writeFile "/tmp/crp.pbm" (text_pbm1 crp t)

bdf_fontboundingbox ibm == Box (8,16,0,-4)
bdf_fontboundingbox crp == Box (7,12,-1,-3)

mapM_ (putStrLn . glyph_pp ibm) (snd ibm)
mapM_ (putStrLn . glyph_pp crp) (snd crp)

let ibm_cs = bdf_printing_ascii ibm
let crp_cs = bdf_printing_ascii crp

mapM_ (putStrLn . glyph_pp ibm) ibm_cs
mapM_ (putStrLn . glyph_pp crp) crp_cs

bdf_ascii "/home/rohan/sw/hsc3-data/data/font/ibm.bdf"
bdf_ascii "/home/rohan/sw/hsc3-data/data/font/creep.bdf"

bdf_pbm1 ibm "/tmp"
bdf_pbm1 crp "/tmp"

ibm_ei <- bdf_load_ei "/home/rohan/sw/hsc3-data/data/font/ibm.bdf"
length ibm_ei == 95
map (length . snd . snd) ibm_ei

-}
