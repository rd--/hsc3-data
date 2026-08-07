{- | Cif File Format -}
module Sound.Sc3.Data.Chemistry.Cif where

import qualified Data.List {- base -}

import qualified Data.List.Split {- split -}

import qualified Text.ParserCombinators.Parsec as Parsec {- parsec -}

import qualified Music.Theory.List as List {- hmt-base -}

-- | Parser.
type P a = Parsec.GenParser Char () a

data Cif_File =
  Cif_File [Cif_Data]
  deriving (Eq, Show)

data Cif_Data =
  Cif_Data String [Cif_Entry]
  deriving (Eq, Show)

cif_data_entries :: Cif_Data -> [Cif_Entry]
cif_data_entries (Cif_Data _ e) = e

data Cif_Entry =
  Cif_Item String String
  | Cif_Looped_Data [String] [[String]]
  deriving (Eq, Show)

cif_item_value :: Cif_Entry -> String
cif_item_value e =
  case e of
    Cif_Item _ v -> v
    _ -> error "cif_item_value"

cif_looped_data_column :: Cif_Entry -> String -> [String]
cif_looped_data_column e x =
  case e of
    Cif_Looped_Data c m ->
      let Just i = Data.List.elemIndex x c
      in m !! i
    _ -> error "cif_looped_data_column"

cif_looped_data_category :: Cif_Entry -> String
cif_looped_data_category e =
  case e of
    Cif_Looped_Data c _ -> foldl1 List.longestCommonPrefix c
    _ -> error "cif_looped_data_category"

{- | Lookup entry in Cif data.
The key may refer to either an item or to a column in a looped data table.

>>> d <- load_cif_data "/home/rohan/rd/j/2020-02-20/cif/2236556.cif"
>>> cif_lookup d "journal_name_full"
Just (Left "Acta Crystallographica Section E")

>>> cif_lookup d "publ_author_name"
Just (Right ["Mishnev, Anatoly","Zvirgzdins, Alvis","Actins, Andris","Delina, Mara"])
-}
cif_lookup :: Cif_Data -> String -> Maybe (Either String [String])
cif_lookup d x =
  let f e = case e of
              Cif_Item k _ -> x == k
              Cif_Looped_Data c _ -> x `elem` c
      g e = case e of
              Cif_Item _ v -> Left v
              Cif_Looped_Data _ _ -> Right (cif_looped_data_column e x)
  in fmap g (Data.List.find f (cif_data_entries d))

cif_lookup_item :: Cif_Data -> String -> Maybe String
cif_lookup_item d x =
  case cif_lookup d x of
    Just (Left r) -> Just r
    _ -> Nothing

cif_lookup_column :: Cif_Data -> String -> Maybe [String]
cif_lookup_column d x =
  case cif_lookup d x of
    Just (Right r) -> Just r
    _ -> Nothing

-- > load_cif_data "/home/rohan/Downloads/PDB.cif"
-- > load_cif_data "/home/rohan/rd/j/2019-10-08/cif/920204.cif" -- 1135095 1253718 281511 281512 716913 920204
load_cif_file :: FilePath -> IO Cif_File
load_cif_file fn = do
  txt <- readFile fn
  return (p_eval parse_cif_file txt)

{- | Load Cif file having one data block.

>>> d <- load_cif_data "/home/rohan/data/pdb/structure/cif/1CAG.cif"
>>> length (cif_data_entries d)
258
-}
load_cif_data :: FilePath -> IO Cif_Data
load_cif_data fn = do
  Cif_File [d] <- load_cif_file fn
  return d

parse_cif_file :: P Cif_File
parse_cif_file = do
  _ <- p_consumeSpace
  d <- Parsec.many1 parse_cif_data
  return (Cif_File d)

parse_cif_data :: P Cif_Data
parse_cif_data = do
  c <- p_dataBlockCode
  e <- Parsec.many1 parse_cif_entry
  return (Cif_Data c e)

parse_cif_item :: P Cif_Entry
parse_cif_item = do
  (k, v) <- p_dataItem
  return (Cif_Item k v)

parse_cif_looped_data :: P Cif_Entry
parse_cif_looped_data = do
  (k, v) <- p_loopedData
  return (Cif_Looped_Data k v)

parse_cif_entry :: P Cif_Entry
parse_cif_entry = Parsec.choice [parse_cif_item, parse_cif_looped_data]

p_eval :: P t -> String -> t
p_eval p x =
  case Parsec.parse p "" x of
    Left e -> error (show e)
    Right y -> y

{- | Parse data block code (lexeme). 5(a)

>>> p_eval p_dataBlockCode "data_rhinovirus"
"rhinovirus"
-}
p_dataBlockCode :: P String
p_dataBlockCode = do
  _ <- Parsec.string "data_"
  a <- p_lexeme (Parsec.many1 (Parsec.noneOf " \t\r\n"))
  return a

{- | Parse data name (lexeme). 1(a)

>>> p_eval p_dataName "_publication.author_name"
"publication.author_name"
-}
p_dataName :: P String
p_dataName = do
  _ <- Parsec.char '_'
  a <- p_lexeme (Parsec.many (Parsec.noneOf " \t\r\n"))
  return a

reservedWords :: [String]
reservedWords = ["data_", "loop_"]

p_checkReservedWord :: String -> P String
p_checkReservedWord w =
  if w `elem` reservedWords
  then fail "reserved word"
  else return w

{- | Parse data value (lexeme). 1(b)

>>> let p = p_eval p_dataValue
>>> map p ["5.3","'x y'","\"x y\"","A,B,C"]
["5.3","x y","x y","A,B,C"]

> p "_a" -- error

> p "loop_" -- error
-}
p_dataValue :: P String
p_dataValue = Parsec.try (p_lexeme p_delimitedValue >>= p_checkReservedWord)

{- | Parse data item. 1(c)

>>> p_eval p_dataItem "_cell_volume 2310(2)"
("cell_volume","2310(2)")
-}
p_dataItem :: P (String, String)
p_dataItem = do
  a <- p_dataName
  b <- p_dataValue
  return (a, b)

{- | Parse looped data (lexeme). 4

>>> p_eval p_loopedData "loop_ _a _b 1 2 3 4"
(["a","b"],[["1","3"],["2","4"]])
-}
p_loopedData :: P ([String], [[String]])
p_loopedData = do
  _ <- p_lexeme (Parsec.string "loop_")
  a <- Parsec.many1 p_dataName
  b <- Parsec.many1 p_dataValue
  let i = length a
  let j = length b
  if j `mod` i == 0
  then return (a, Data.List.transpose (Data.List.Split.chunksOf i b))
  else Parsec.unexpected "invalid loop data"

p_delimitedValue :: P String
p_delimitedValue =
  Parsec.choice
  [ p_whitespaceDelimitedValue
  , p_quoteDelimitedValue
  , p_apostropheDelimitedValue
  , p_semicolonDelimitedValue
  ]

{- | Parse whitespace delimited value. 2(a)

>>> let p = p_eval p_whitespaceDelimitedValue
>>> map p ["5.3", "6.083(1)e+23", "light-blue", "O'Connor"]
["5.3","6.083(1)e+23","light-blue","O'Connor"]

>>> p "A,B,C"
"A,B,C"
-}
p_whitespaceDelimitedValue :: P String
p_whitespaceDelimitedValue = do
  a <- Parsec.noneOf "_'\";"
  b <- Parsec.many (Parsec.noneOf " \t\r\n")
  return (a : b)

{- | Parse quote delimited value. 2(b)

>>> let f = p_eval p_quoteDelimitedValue
>>> map f ["\"low melting point\"","\"light blue\"","\"Patrick O'Connor\""]
["low melting point","light blue","Patrick O'Connor"]
-}
p_quoteDelimitedValue :: P String
p_quoteDelimitedValue = do
  _ <- Parsec.char '"'
  b <- Parsec.many (Parsec.noneOf "\"\r\n")
  _ <- Parsec.char '"'
  return b

{- | Parse quote delimited value. 2(b)

>>> let f = p_eval p_apostropheDelimitedValue
>>> map f ["'light blue'","'classed as \"unknown\"'"]
["light blue","classed as \"unknown\""]
-}
p_apostropheDelimitedValue :: P String
p_apostropheDelimitedValue = do
  _ <- Parsec.char '\''
  b <- Parsec.many (Parsec.noneOf "'\r\n'")
  _ <- Parsec.char '\''
  return b

{- | Parse semicolon delimited value. 2(d)

>>> p_eval p_semicolonDelimitedValue ";x\ny\n;"
"x\ny"
-}
p_semicolonDelimitedValue :: P String
p_semicolonDelimitedValue = do
  _ <- Parsec.string ";"
  a <- Parsec.manyTill Parsec.anyChar (Parsec.try (Parsec.string "\n;"))
  return a

{- | Parse end of line comment. 6

>>> p_eval p_endOfLineComment "# commentary text\n"
()
-}
p_endOfLineComment :: P ()
p_endOfLineComment = do
  _ <- Parsec.char '#'
  _ <- Parsec.many (Parsec.noneOf "\r\n")
  return ()

-- | Parse non-empty whitespace
p_whitespace :: P ()
p_whitespace = do
  _ <- Parsec.many1 (Parsec.oneOf " \t\r\n")
  return ()

{- | Consume all space, including comments.

>>> p_eval p_consumeSpace " \t\r\n"
()
-}
p_consumeSpace :: P ()
p_consumeSpace = do
  _ <- Parsec.many (Parsec.choice [p_whitespace, p_endOfLineComment])
  return ()

p_lexeme :: P t -> P t
p_lexeme p = do
  a <- p
  _ <- p_consumeSpace
  return a

