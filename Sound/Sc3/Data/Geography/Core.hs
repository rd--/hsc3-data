module Sound.Sc3.Data.Geography.Core where

import qualified Text.ParserCombinators.Parsec as Parsec {- parsec -}

-- * Types

-- | Degree, minutes, seconds.
type Dms = (Int, Int, Int)

-- | Quadrant, degree, minutes, seconds.
type Qdms = (Char, Int, Int, Int)

-- | Quadrant, fractional degree.
type Qdeg = (Char, Double)

-- | Cartesian point, here (phi, lambda).
type Coord = (Double, Double)

-- | Parser.
type P a = Parsec.GenParser Char () a

-- * Core

dms_to_degree :: Dms -> Double
dms_to_degree (d, m, s) =
  let i = fromIntegral
  in i d + ((i m * 60 + i s) / 3600)

-- | N & E are 'id', W & S are 'negate'.
quadrant_f :: Char -> (Double -> Double)
quadrant_f q =
  case q of
    'N' -> id
    'E' -> id
    'W' -> negate
    'S' -> negate
    _ -> error "quadrant_to_f"

{- | Translate from 'Qdms' to 'R' degree.

>>> qdms_to_degree ('S',37,48,50)
-37.81388888888889
-}
qdms_to_degree :: Qdms -> Double
qdms_to_degree (q, d, m, s) = quadrant_f q (dms_to_degree (d, m, s))

degree_to_dms :: Double -> Dms
degree_to_dms dgr =
  let i = fromIntegral
      d = abs dgr
      d' = truncate d
      m = 60 * (d - i d')
      m' = truncate m
      s = 60 * (m - i m')
  in (d', m', round s)

{- | Given target quadrants, the inverse of 'qdms_to_degree'.

>>> degree_to_qdms ('S','N') (-37.814)
('S',37,48,50)
-}
degree_to_qdms :: (Char, Char) -> Double -> Qdms
degree_to_qdms (l, r) dgr =
  let q = if dgr < 0 then l else r
      (d, m, s) = degree_to_dms dgr
  in (q, d, m, s)

latitude_to_qdms :: Double -> Qdms
latitude_to_qdms = degree_to_qdms ('S', 'N')

longitude_to_qdms :: Double -> Qdms
longitude_to_qdms = degree_to_qdms ('W', 'E')

-- * Parse

p_int_str :: P String
p_int_str = do
  s <- Parsec.optionMaybe (Parsec.try (Parsec.char '-'))
  n <- Parsec.many1 Parsec.digit
  case s of
    Nothing -> return n
    (Just c) -> return (c : n)

p_int :: P Int
p_int = fmap read p_int_str

p_word :: P String
p_word = Parsec.many1 (Parsec.letter Parsec.<|> Parsec.oneOf "-'") Parsec.<?> "word"

p_phrase :: P [String]
p_phrase = Parsec.sepEndBy1 p_word (Parsec.char ' ')

p_location :: P [[String]]
p_location = do
  let skp = Parsec.skipMany1 (Parsec.char ',' >> Parsec.char ' ')
  Parsec.optional (Parsec.char ' ')
  Parsec.sepEndBy1 p_phrase skp

-- | Parser for 'Qdms' with unicode characters @°@, @′@ and @″@.
p_qdms_unicode :: P Qdms
p_qdms_unicode = do
  d <- p_int
  _ <- Parsec.char '°'
  m <- p_int
  _ <- Parsec.char '′'
  s <- p_int
  _ <- Parsec.char '″'
  q <- Parsec.oneOf "NSEW"
  return (q, d, m, s)

-- | Parser for 'Qdms' with leading direction and whitespace.
p_qdms_ws :: P Qdms
p_qdms_ws = do
  q <- Parsec.oneOf "NSEW"
  _ <- Parsec.char ' '
  d <- p_int
  _ <- Parsec.char ' '
  m <- p_int
  _ <- Parsec.char ' '
  s <- p_int
  return (q, d, m, s)

p_coord_by :: P Qdms -> P Coord
p_coord_by p = do
  phi <- fmap qdms_to_degree p
  _ <- Parsec.char ' '
  lambda <- fmap qdms_to_degree p
  return (phi, lambda)

p_coord :: P Coord
p_coord = p_coord_by p_qdms_ws

{- | Run 'p_qdms_unicode'.

>>> parse_qdms_unicode "34°16′32″N"
Right ('N',34,16,32)

>>> parse_qdms_unicode "132°18′28″E"
Right ('E',132,18,28)
-}
parse_qdms_unicode :: String -> Either Parsec.ParseError Qdms
parse_qdms_unicode = Parsec.parse p_qdms_unicode "parse_qdms_unicode"

{- | Run 'p_qdms_ws'.

>>> parse_qdms_ws "N 34 16 32"
Right ('N',34,16,32)
-}
parse_qdms_ws :: String -> Either Parsec.ParseError Qdms
parse_qdms_ws s =
  case Parsec.parse p_qdms_ws "parse_qdms_ws" s of
    Left err -> error (show err)
    Right g -> return g

parse_qdms_ws_err :: String -> Qdms
parse_qdms_ws_err s =
  case parse_qdms_ws s of
    Left err -> error (show err)
    Right g -> g

{- | Run 'p_coord'

>>> parse_coord "N 34 16 32 E 132 18 28"
Right (34.275555555555556,132.30777777777777)
-}
parse_coord :: String -> Either Parsec.ParseError Coord
parse_coord = Parsec.parse p_coord "parse_coord"

parse_coord_err :: String -> Coord
parse_coord_err s =
  case parse_coord s of
    Left err -> error (show err)
    Right g -> g

{- | Run 'p_coord_by' of 'p_qdms_unicode'.

>>> parse_coord_unicode "34°16′32″N 132°18′28″E"
Right (34.275555555555556,132.30777777777777)
-}
parse_coord_unicode :: String -> Either Parsec.ParseError Coord
parse_coord_unicode = Parsec.parse (p_coord_by p_qdms_unicode) "parse_coord_unicode"

-- * Pretty print (Pp)

qdms_pp :: Qdms -> String
qdms_pp (q, d, m, s) = q : ' ' : unwords (map show [d, m, s])

coord_pp :: Coord -> String
coord_pp (phi, lambda) =
  let phi' = latitude_to_qdms phi
      lambda' = longitude_to_qdms lambda
  in qdms_pp phi' ++ " " ++ qdms_pp lambda'
