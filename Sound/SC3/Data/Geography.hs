module Sound.SC3.Data.Geography where

import Control.Monad {- base -}
import Data.List {- base -}

import qualified Text.ParserCombinators.Parsec as C {- parsec -}

import qualified Music.Theory.IO as IO {- hmt -}

-- * TYPES

-- | Degree, minutes, seconds.
type DMS = (Int,Int,Int)

-- | Quadrant, degree, minutes, seconds.
type QDMS = (Char,Int,Int,Int)

-- | Synonym for 'Double'.
type R = Double

-- | Quadrant, fractional degree.
type QDEG = (Char,R)

-- | Cartesian point, here (phi,lambda).
type PT = (R,R)

-- | Geographical location.
type G = (PT,[String])

-- | Parser.
type P a = C.GenParser Char () a

-- * CORE

dms_to_degree :: DMS -> R
dms_to_degree (d,m,s) =
    let i = fromIntegral
    in i d + ((i m * 60 + i s) / 3600)

-- | N & E are 'id', W & S are 'negate'.
quadrant_f :: Char -> (R -> R)
quadrant_f q =
    case q of
      'N' -> id
      'E' -> id
      'W' -> negate
      'S' -> negate
      _ -> error "quadrant_to_f"

-- | Translate from 'QDMS' to 'R' degree.
--
-- > qdms_to_degree ('S',37,48,50) == -37.81388888888889
qdms_to_degree :: QDMS -> R
qdms_to_degree (q,d,m,s) = quadrant_f q (dms_to_degree (d,m,s))

degree_to_dms :: R -> DMS
degree_to_dms dgr =
    let i = fromIntegral
        d = abs dgr
        d' = truncate d
        m = 60 * (d - i d')
        m' = truncate m
        s = 60 * (m - i m')
    in (d',m',round s)

-- | Given target quadrants, the inverse of 'qdms_to_degree'.
--
-- > degree_to_qdms ('S','N') (-37.814) == ('S',37,48,50)
degree_to_qdms :: (Char,Char) -> R -> QDMS
degree_to_qdms (l,r) dgr =
    let q = if dgr < 0 then l else r
        (d,m,s) = degree_to_dms dgr
    in (q,d,m,s)

latitude_to_qdms :: R -> QDMS
latitude_to_qdms = degree_to_qdms ('S','N')

longitude_to_qdms :: R -> QDMS
longitude_to_qdms = degree_to_qdms ('W','E')

-- * PARSE

p_int_str :: P String
p_int_str = do
  s <- C.optionMaybe (C.try (C.char '-'))
  n <- C.many1 C.digit
  case s of
    Nothing -> return n
    (Just c) -> return (c : n)

p_int :: P Int
p_int = liftM read p_int_str

p_word :: P String
p_word = C.many1 (C.letter C.<|> C.oneOf "-'") C.<?> "word"

p_phrase :: P [String]
p_phrase = C.sepEndBy1 p_word (C.char ' ')

p_location :: P [[String]]
p_location = do
  let skp = C.skipMany1 (C.char ',' >> C.char ' ')
  C.optional (C.char ' ')
  C.sepEndBy1 p_phrase skp

p_qdms_unicode :: P QDMS
p_qdms_unicode = do
  d <- p_int
  _ <- C.char '°'
  m <- p_int
  _ <- C.char '′'
  s <- p_int
  _ <- C.char '″'
  q <- C.oneOf "NSEW"
  return (q,d,m,s)

p_qdms :: P QDMS
p_qdms = do
  q <- C.oneOf "NSEW"
  _ <- C.char ' '
  d <- p_int
  _ <- C.char ' '
  m <- p_int
  _ <- C.char ' '
  s <- p_int
  return (q,d,m,s)

p_coord_by :: P QDMS -> P PT
p_coord_by p = do
  phi <- fmap qdms_to_degree p
  _ <- C.char ' '
  lambda <- fmap qdms_to_degree p
  return (phi,lambda)

p_coord :: P PT
p_coord = p_coord_by p_qdms

p_geography_by :: P QDMS -> P G
p_geography_by p = do
  nm <- p_location
  _ <- C.char ':'
  _ <- C.char ' '
  (lambda, phi) <- p_coord_by p
  return ((lambda, phi),map unwords nm)

p_geography :: P G
p_geography = p_geography_by p_qdms

p_geographies :: P [G]
p_geographies = do
  xs <- C.sepEndBy1 p_geography C.newline
  _ <- C.eof
  return xs

-- > parse_qdms_unicode "34°16′32″N" == Right ('N',34,16,32)
-- > parse_qdms_unicode "132°18′28″E" == Right ('E',132,18,28)
parse_qdms_unicode :: String -> Either C.ParseError QDMS
parse_qdms_unicode = C.parse p_qdms_unicode "parse_qdms_unicode"

-- > parse_qdms "N 34 16 32" == Just ('N',34,16,32)
parse_qdms :: Monad m => String -> m QDMS
parse_qdms s =
  case C.parse p_qdms "parse_qdms" s of
    Left err -> fail (show err)
    Right g -> return g

-- > parse_coord "N 34 16 32 E 132 18 28" == Right (34.275555555555556,132.30777777777777)
parse_coord :: Monad m => String -> m PT
parse_coord s =
  case C.parse p_coord "parse_coord" s of
    Left err -> fail (show err)
    Right g -> return g

-- > parse_coord_unicode "34°16′32″N 132°18′28″E"
parse_coord_unicode :: String -> Either C.ParseError PT
parse_coord_unicode = C.parse (p_coord_by p_qdms_unicode) "parse_coord_unicode"

-- > parse_geography "_" "Melbourne, Victoria, AU: S 37 48 50 E 144 57 47"
parse_geography :: C.SourceName -> String -> Either C.ParseError G
parse_geography src = C.parse p_geography src

parse_geographies :: C.SourceName -> String -> Either C.ParseError [G]
parse_geographies src = C.parse p_geographies src

-- * EQ

g_match :: [String] -> G -> Bool
g_match q (_,l) =
    let f a = any (\x -> a `isInfixOf` x) l
    in all f q

-- * PP

qdms_pp :: QDMS -> String
qdms_pp (q,d,m,s) = q : ' ' : unwords (map show [d,m,s])

coord_pp :: PT -> String
coord_pp (phi,lambda) =
    let phi' = latitude_to_qdms phi
        lambda' = longitude_to_qdms lambda
    in qdms_pp phi' ++ " " ++ qdms_pp lambda'

g_pp :: G -> String
g_pp (coord,name) = intercalate ", " name ++ coord_pp coord

-- * IO

-- | Read set of 'G' from named @UTF-8@ encoded file.
load_geographies :: String -> IO [G]
load_geographies fn = do
  s <- IO.read_file_utf8 fn
  case parse_geographies fn s of
    (Left err) -> error (show err)
    (Right g) -> return g
