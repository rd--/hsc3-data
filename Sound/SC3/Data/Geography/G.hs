-- | G
module Sound.SC3.Data.Geography.G where

import Data.List {- base -}

import qualified Text.ParserCombinators.Parsec as C {- parsec -}

import qualified Music.Theory.IO as IO {- hmt -}

import Sound.SC3.Data.Geography.Core


-- | Geographical location.
type G = (PT,[String])

p_geography_by :: P QDMS -> P G
p_geography_by p = do
  nm <- p_location
  _ <- C.char ':'
  _ <- C.char ' '
  (lambda, phi) <- p_coord_by p
  return ((lambda, phi),map unwords nm)

p_geography :: P G
p_geography = p_geography_by p_qdms_ws

p_geographies :: P [G]
p_geographies = do
  xs <- C.sepEndBy1 p_geography C.newline
  _ <- C.eof
  return xs

g_pp :: G -> String
g_pp (coord,name) = intercalate ", " name ++ coord_pp coord

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

-- * IO

-- | Read set of 'G' from named @UTF-8@ encoded file.
--
-- > load_geographies "/home/rohan/rf/pp/geography.text"
load_geographies :: String -> IO [G]
load_geographies fn = do
  s <- IO.read_file_utf8 fn
  case parse_geographies fn s of
    (Left err) -> error (show err)
    (Right g) -> return g
