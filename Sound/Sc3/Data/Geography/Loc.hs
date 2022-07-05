-- | Geographical location
module Sound.Sc3.Data.Geography.Loc where

import Data.List {- base -}

import qualified Text.ParserCombinators.Parsec as C {- parsec -}

import qualified Music.Theory.Io as Io {- hmt-base -}

import Sound.Sc3.Data.Geography.Core {- hsc3-data -}


-- | Geographical location.
type Geo_Loc = (Coord, [String])

p_geography_by :: P Qdms -> P Geo_Loc
p_geography_by p = do
  nm <- p_location
  _ <- C.char ':'
  _ <- C.char ' '
  (lambda, phi) <- p_coord_by p
  return ((lambda, phi),map unwords nm)

p_geography :: P Geo_Loc
p_geography = p_geography_by p_qdms_ws

p_geographies :: P [Geo_Loc]
p_geographies = do
  xs <- C.sepEndBy1 p_geography C.newline
  _ <- C.eof
  return xs

g_pp :: Geo_Loc -> String
g_pp (coord,name) = intercalate ", " name ++ coord_pp coord

-- > parse_geography "_" "Melbourne, Victoria, AU: S 37 48 50 E 144 57 47"
parse_geography :: C.SourceName -> String -> Either C.ParseError Geo_Loc
parse_geography = C.parse p_geography

parse_geographies :: C.SourceName -> String -> Either C.ParseError [Geo_Loc]
parse_geographies = C.parse p_geographies

-- * Eq

g_match :: [String] -> Geo_Loc -> Bool
g_match q (_,l) =
    let f a = any (\x -> a `isInfixOf` x) l
    in all f q

-- * Io

{- | Read set of 'G' from named @UTF-8@ encoded file.

> load_geographies "/home/rohan/rf/pp/geography.text"
-}
load_geographies :: String -> IO [Geo_Loc]
load_geographies fn = do
  s <- Io.read_file_utf8 fn
  case parse_geographies fn s of
    (Left err) -> error (show err)
    (Right g) -> return g
