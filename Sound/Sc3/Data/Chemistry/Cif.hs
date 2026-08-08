-- | Cif File Format
module Sound.Sc3.Data.Chemistry.Cif where

import qualified Data.Char {- base -}
import qualified Data.Either {- base -}
import qualified Data.List {- base -}

import qualified Data.List.Split {- split -}
import qualified Safe {- safe -}

import qualified Text.ParserCombinators.Parsec as Parsec {- parsec -}

import qualified Music.Theory.List as List {- hmt-base -}

-- | Cif names are strings.
type Cif_Name = String

-- | Cif values are strings.
type Cif_Value = String

-- | A Cif file is a sequence of Cif data blocks.
data Cif_File
  = Cif_File
  { cif_file_data :: [Cif_Data]
  }
  deriving (Eq, Show)

-- | A Cif data block is a named sequence of Cif entries.
data Cif_Data
  = Cif_Data
  { cif_data_name :: Cif_Name
  , cif_data_entries :: [Cif_Entry]
  }
  deriving (Eq, Show)

-- | A Cif entry is either a Cif_Item or a Cif_Table.
type Cif_Entry = Either Cif_Item Cif_Table

-- | A Cif item is a (key,value) pair.
data Cif_Item
  = Cif_Item
  { cif_item_name :: Cif_Name
  , cif_item_value :: Cif_Value
  }
  deriving (Eq, Show)

-- | A Cif looped data table is a (column-keys,column-matrix-values) pair.
data Cif_Table
  = Cif_Table
  { cif_table_names :: [Cif_Name]
  , cif_table_column_matrix :: [[Cif_Value]]
  }
  deriving (Eq, Show)

-- | Cif data items.
cif_data_items :: Cif_Data -> [Cif_Item]
cif_data_items = Data.Either.lefts . cif_data_entries

-- | Cif data tables.
cif_data_tables :: Cif_Data -> [Cif_Table]
cif_data_tables = Data.Either.rights . cif_data_entries

-- | Composite of all item and column names.
cif_data_names :: Cif_Data -> [Cif_Name]
cif_data_names d =
  let f e = case e of
        Left (Cif_Item k _) -> [k]
        Right (Cif_Table c _) -> c
  in concatMap f (cif_data_entries d)

-- | Row and column counts for Cif table.
cif_table_shape :: Cif_Table -> (Int, Int)
cif_table_shape (Cif_Table c m) = (length c, length (Safe.headErr m))

-- | True if column count it one.
cif_table_is_column_vector :: Cif_Table -> Bool
cif_table_is_column_vector = (== 1) . snd . cif_table_shape

-- | A column of Cif_Table.
cif_table_column :: Cif_Table -> Cif_Name -> [Cif_Value]
cif_table_column (Cif_Table c m) x =
  case Data.List.elemIndex x c of
    Just i -> m !! i
    Nothing -> error "cif_table_column"

{- | The category of a Cif_Table is the longest common prefix of the column names.

>>> d <- load_cif_data "/home/rohan/data/pdb/structure/cif/1CAG.cif"
>>> map cif_table_category (cif_data_tables d)
["database_2.","pdbx_audit_revision_history.","pdbx_audit_revision_group.","pdbx_audit_revision_category.","pdbx_audit_revision_item.","audit_author.","citation_author.","entity.","pdbx_entity_nonpoly.","entity_poly_seq.","chem_comp.","pdbx_poly_seq_scheme.","pdbx_nonpoly_scheme.","software.","refine_ls_restr.","struct_asym.","struct_ref_seq.","pdbx_struct_assembly_prop.","struct_conf.","struct_conn.","struct_mon_prot_cis.","struct_site.","struct_site_gen.","pdbx_validate_rmsd_angle.","pdbx_validate_torsion.","pdbx_struct_mod_residue.","pdbx_unobs_or_zero_occ_residues.","chem_comp_atom.","chem_comp_bond.","atom_sites_footnote.","atom_type.symbol","atom_site."]

>>> d <- load_cif_data "/home/rohan/data/pdb/ligand/cif/PDB.cif"
>>> map cif_table_category (cif_data_tables d)
["chem_comp_atom.","chem_comp_bond.","pdbx_chem_comp_descriptor.","pdbx_chem_comp_identifier.","pdbx_chem_comp_audit."]

>>> d <- load_cif_data "/home/rohan/rd/j/2019-10-08/cif/1135095.cif"
>>> map cif_table_category (cif_data_tables d)
["citation_","symmetry_equiv_pos_","atom_site_"]

If there are column matrix tables, or if one of the fields uses the category name as a field name, the final entry in the category name will not be a separator, ordinarily an underscore or period.

>>> d <- load_cif_data "/home/rohan/rd/j/2019-10-08/cif/920204.cif"
>>> map cif_table_category (cif_data_tables d)
["citation_","symmetry_equiv_pos_as_xyz","atom_type_","atom_site_","atom_site_aniso_","geom_bond_","geom_angle","geom_hbond_"]

>>> d <- load_cif_data "/home/rohan/rd/j/2020-02-20/cif/2236556.cif"
>>> map cif_table_category (cif_data_tables d)
["publ_author_name","symmetry_equiv_pos_as_xyz","atom_site_","atom_site_aniso_","atom_type_","geom_angle","geom_bond_","geom_hbond_","geom_torsion","cod_related_entry_"]
-}
cif_table_category :: Cif_Table -> Cif_Name
cif_table_category (Cif_Table c _) =
  foldl1 List.longestCommonPrefix c

{- | Lookup entry in Cif data.
The key may refer to either an item or to a column in a looped data table.

>>> d <- load_cif_data "/home/rohan/rd/j/2020-02-20/cif/2236556.cif"
>>> cif_lookup d "journal_name_full"
Just (Left "Acta Crystallographica Section E")

>>> cif_lookup d "publ_author_name"
Just (Right ["Mishnev, Anatoly","Zvirgzdins, Alvis","Actins, Andris","Delina, Mara"])
-}
cif_lookup :: Cif_Data -> Cif_Name -> Maybe (Either Cif_Value [Cif_Value])
cif_lookup d x =
  let f e = case e of
        Left (Cif_Item k _) -> x == k
        Right (Cif_Table c _) -> x `elem` c
      g e = case e of
        Left (Cif_Item _ v) -> Left v
        Right ld -> Right (cif_table_column ld x)
  in fmap g (Data.List.find f (cif_data_entries d))

-- | Lookup item in Cif data.
cif_lookup_item :: Cif_Data -> Cif_Name -> Maybe Cif_Value
cif_lookup_item d x =
  case cif_lookup d x of
    Just (Left r) -> Just r
    _ -> Nothing

-- | Lookup column in Cif data.
cif_lookup_column :: Cif_Data -> Cif_Name -> Maybe [Cif_Value]
cif_lookup_column d x =
  case cif_lookup d x of
    Just (Right r) -> Just r
    _ -> Nothing

-- * Io

{- | Load Cif file

>>> cif <- load_cif_file "/home/rohan/data/pdb/ligand/cif/PDB.cif"
>>> length (cif_file_data cif)
1
-}
load_cif_file :: FilePath -> IO Cif_File
load_cif_file fn = do
  txt <- readFile fn
  return (p_eval parse_cif_file txt)

{- | Load Cif file having one data block.

>>> d <- load_cif_data "/home/rohan/data/pdb/structure/cif/1CAG.cif"
>>> length (cif_data_entries d)
258

>>> length (cif_data_names d)
522
-}
load_cif_data :: FilePath -> IO Cif_Data
load_cif_data fn = do
  Cif_File [d] <- load_cif_file fn
  return d

-- * Parsing

-- | Parser.
type P a = Parsec.GenParser Char () a

-- | Run parser.
p_eval :: P t -> String -> t
p_eval p x =
  case Parsec.parse p "" x of
    Left e -> error (show e)
    Right y -> y

{- | Parse Cif file

>>> p_eval parse_cif_file "data_a _p x data_b _q y"
Cif_File {cif_file_data = [Cif_Data {cif_data_name = "a", cif_data_entries = [Left (Cif_Item {cif_item_name = "p", cif_item_value = "x"})]},Cif_Data {cif_data_name = "b", cif_data_entries = [Left (Cif_Item {cif_item_name = "q", cif_item_value = "y"})]}]}
-}
parse_cif_file :: P Cif_File
parse_cif_file = do
  _ <- p_consumeSpace
  d <- Parsec.many1 parse_cif_data
  return (Cif_File d)

{- | Parse Cif data.

>>> p_eval parse_cif_data "data_a _p x _q y"
Cif_Data {cif_data_name = "a", cif_data_entries = [Left (Cif_Item {cif_item_name = "p", cif_item_value = "x"}),Left (Cif_Item {cif_item_name = "q", cif_item_value = "y"})]}
-}
parse_cif_data :: P Cif_Data
parse_cif_data = do
  c <- p_dataBlockCode
  e <- Parsec.many1 parse_cif_entry
  return (Cif_Data c e)

{- | Parse Cif item.

>>> p_eval parse_cif_item "_p x"
Cif_Item {cif_item_name = "p", cif_item_value = "x"}
-}
parse_cif_item :: P Cif_Item
parse_cif_item = do
  (k, v) <- p_dataItem
  return (Cif_Item k v)

{- | Parse Cif table.

>>> p_eval parse_cif_table "loop_ _p _q a b c d"
Cif_Table {cif_table_names = ["p","q"], cif_table_column_matrix = [["a","c"],["b","d"]]}
-}
parse_cif_table :: P Cif_Table
parse_cif_table = do
  (k, v) <- p_loopedData
  return (Cif_Table k v)

-- | Parse Cif entry
parse_cif_entry :: P Cif_Entry
parse_cif_entry =
  Parsec.choice
  [ fmap Left parse_cif_item
  , fmap Right parse_cif_table]

-- * Parser primitives

{- | Parse data block code (lexeme). 5(a)

>>> p_eval p_dataBlockCode "data_rhinovirus"
"rhinovirus"
-}
p_dataBlockCode :: P Cif_Name
p_dataBlockCode = do
  _ <- Parsec.string "data_"
  a <- p_lexeme (Parsec.many1 (Parsec.noneOf " \t\r\n"))
  return a

{- | Parse data name (lexeme). 1(a)

>>> p_eval p_dataName "_publication.author_name"
"publication.author_name"
-}
p_dataName :: P Cif_Name
p_dataName = do
  _ <- Parsec.char '_'
  a <- p_lexeme (Parsec.many (Parsec.noneOf " \t\r\n"))
  return a

-- | A Cif reserved word is a string.
type Cif_Reserved_Word = String

-- | List of Cif reserved words.
reservedWords :: [Cif_Reserved_Word]
reservedWords = ["data_", "loop_"]

p_checkNotReservedWord :: Cif_Name -> P Cif_Name
p_checkNotReservedWord w =
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
p_dataValue :: P Cif_Value
p_dataValue = Parsec.try (p_lexeme p_delimitedValue >>= p_checkNotReservedWord)

{- | Parse data item. 1(c)

>>> p_eval p_dataItem "_cell_volume 2310(2)"
("cell_volume","2310(2)")
-}
p_dataItem :: P (Cif_Name, Cif_Value)
p_dataItem = do
  a <- p_dataName
  b <- p_dataValue
  return (a, b)

{- | Parse looped data (lexeme). 4

>>> p_eval p_loopedData "loop_ _a _b 1 2 3 4"
(["a","b"],[["1","3"],["2","4"]])
-}
p_loopedData :: P ([Cif_Name], [[Cif_Value]])
p_loopedData = do
  _ <- p_lexeme (Parsec.string "loop_")
  a <- Parsec.many1 p_dataName
  b <- Parsec.many1 p_dataValue
  let i = length a
  let j = length b
  if j `mod` i == 0
    then return (a, Data.List.transpose (Data.List.Split.chunksOf i b))
    else Parsec.unexpected "invalid loop data"

-- | Parse a delimited value.
p_delimitedValue :: P Cif_Value
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
p_whitespaceDelimitedValue :: P Cif_Value
p_whitespaceDelimitedValue = do
  a <- Parsec.noneOf "_'\";"
  b <- Parsec.many (Parsec.noneOf " \t\r\n")
  return (a : b)

{- | Parse quote delimited value. 2(b)

>>> let f = p_eval p_quoteDelimitedValue
>>> map f ["\"low melting point\"","\"light blue\"","\"Patrick O'Connor\""]
["low melting point","light blue","Patrick O'Connor"]
-}
p_quoteDelimitedValue :: P Cif_Value
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
p_apostropheDelimitedValue :: P Cif_Value
p_apostropheDelimitedValue = do
  _ <- Parsec.char '\''
  b <- Parsec.many (Parsec.noneOf "'\r\n'")
  _ <- Parsec.char '\''
  return b

{- | Parse semicolon delimited value. 2(d)

>>> p_eval p_semicolonDelimitedValue ";x\ny\n;"
"x\ny"
-}
p_semicolonDelimitedValue :: P Cif_Value
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

-- | Run p and then consume space.
p_lexeme :: P t -> P t
p_lexeme p = do
  a <- p
  _ <- p_consumeSpace
  return a
