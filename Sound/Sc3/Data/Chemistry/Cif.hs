{- | Cif File Format -}
module Sound.Sc3.Data.Chemistry.Cif where

import qualified Text.ParserCombinators.Parsec as Parsec {- parsec -}

-- | Parser.
type P a = Parsec.GenParser Char () a

p_eval :: P t -> String -> t
p_eval p x =
  case Parsec.parse p "" x of
    Left e -> error (show e)
    Right y -> y

{- | Parse data block code. 5(a)

>>> p_eval p_dataBlockCode "data_rhinovirus"
"rhinovirus"
-}
p_dataBlockCode :: P String
p_dataBlockCode = do
  _ <- Parsec.string "data_"
  a <- Parsec.many (Parsec.noneOf " \t\n")
  return a

{- | Parse data name. 1(a)

>>> p_eval p_dataName "_publication.author_name"
"publication.author_name"
-}
p_dataName :: P String
p_dataName = do
  _ <- Parsec.char '_'
  a <- p_lexeme (Parsec.many (Parsec.noneOf " \t\n"))
  return a

-- | Parse data value. 1(b)
p_dataValue :: P String
p_dataValue =
  p_lexeme $ Parsec.choice
  [ p_whitespaceDelimitedValue
  , p_quoteDelimitedValue
  , p_apostropheDelimitedValue
  , p_semicolonDelimitedValue
  ]

{- | Parse data item. 1(c)

>>> p_eval p_dataItem "_cell_volume 2310(2)"
("cell_volume","2310(2)")
-}
p_dataItem :: P (String, String)
p_dataItem = do
  a <- p_dataName
  b <- p_dataValue
  return (a, b)

{- | Parse whitespace delimited value. 2(a)

>>> let f = p_eval p_whitespaceDelimitedValue
>>> map f ["5.3", "6.083(1)e+23", "light-blue", "O'Connor"]
["5.3","6.083(1)e+23","light-blue","O'Connor"]
-}
p_whitespaceDelimitedValue :: P String
p_whitespaceDelimitedValue = do
  a <- Parsec.noneOf "+'\";"
  b <- Parsec.many (Parsec.noneOf " {}[],\n")
  return (a : b)

{- | Parse quote delimited value. 2(b)

>>> let f = p_eval p_quoteDelimitedValue
>>> map f ["\"low melting point\"","\"light blue\"","\"Patrick O'Connor\""]
["low melting point","light blue","Patrick O'Connor"]
-}
p_quoteDelimitedValue :: P String
p_quoteDelimitedValue = do
  _ <- Parsec.char '"'
  b <- Parsec.many (Parsec.noneOf "\"\n")
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
  b <- Parsec.many (Parsec.noneOf "'\n'")
  _ <- p_lexeme (Parsec.char '\'')
  return b

{- | Parse semicolon delimited value. 2(d)

>>> p_eval p_semicolonDelimitedValue "\n;x\ny\n;"
"x\ny"
-}
p_semicolonDelimitedValue :: P String
p_semicolonDelimitedValue = do
  _ <- Parsec.string "\n;"
  a <- Parsec.manyTill Parsec.anyChar (Parsec.try (Parsec.string "\n;"))
  return a

{- | Parse end of line comment. 6

>>> p_eval p_endOfLineComment "# commentary text\n"
()
-}
p_endOfLineComment :: P ()
p_endOfLineComment = do
  _ <- Parsec.char '#'
  _ <- Parsec.many (Parsec.noneOf "\n")
  return ()

-- | Parse non-empty whitespace
p_whitespace :: P ()
p_whitespace = do
  _ <- Parsec.many1 (Parsec.oneOf " \t\n")
  return ()

{- | Consume all space, including comments.

>>> p_eval p_consumeSpace " \t\n"
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

