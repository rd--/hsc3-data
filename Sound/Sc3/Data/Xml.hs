-- | Functions over "Text.Xml.Light"
module Sound.Sc3.Data.Xml where

import qualified Data.Maybe {- base -}

import qualified Data.ByteString as ByteString {- bytestring -}
import qualified Text.XML.Light as Xml {- xml -}
import qualified Text.XML.Light.Lexer as Xml.Lexer {- xml -}

-- | Assert list is non-empty.
error_if_null :: String -> [t] -> [t]
error_if_null msg ls = if null ls then error msg else ls

-- | Erroring 'Xml.findChild'
x_get_elem :: String -> Xml.Element -> Xml.Element
x_get_elem nm =
  Data.Maybe.fromMaybe (error ("x_get_elem: " ++ nm))
    . Xml.findChild (Xml.unqual nm)

-- | Erroring 'Xml.findChildren'
x_get_elem_set :: String -> Xml.Element -> [Xml.Element]
x_get_elem_set nm =
  error_if_null ("x_get_elem_set: " ++ nm)
    . Xml.findChildren (Xml.unqual nm)

-- | Descending sequence of 'x_get_elem'
x_get_elem_path :: [String] -> Xml.Element -> Xml.Element
x_get_elem_path p x =
  case p of
    [] -> x
    nm : p1 -> x_get_elem_path p1 (x_get_elem nm x)

-- | Predicate 'Xml.findElement'
x_has_elem :: String -> Xml.Element -> Bool
x_has_elem nm = Data.Maybe.isJust . Xml.findElement (Xml.unqual nm)

-- | Erroring 'Xml.findElement'
x_find_elem :: String -> Xml.Element -> Xml.Element
x_find_elem nm =
  Data.Maybe.fromMaybe (error ("x_find_elem: " ++ nm))
    . Xml.findElement (Xml.unqual nm)

-- | Erroring 'Xml.findAttr'
x_get_attr :: String -> Xml.Element -> String
x_get_attr nm =
  Data.Maybe.fromMaybe (error ("x_get_attr: " ++ nm))
    . Xml.findAttr (Xml.unqual nm)

-- | 'Xml.qName' of 'Xml.elName'
x_elem_name :: Xml.Element -> String
x_elem_name = Xml.qName . Xml.elName

-- | 'Xml.elContent' for simple CData
xml_elem_text_cdata_uniq :: Xml.Element -> String
xml_elem_text_cdata_uniq e =
  case Xml.elContent e of
    [Xml.Text (Xml.CData Xml.CDataText str _)] -> str
    _ -> error "xml_elem_text_cdata_uniq"

-- | Erroring variant of ' Xml.parseXMLDoc'.
xml_parse_err :: Xml.Lexer.XmlSource x => x -> Xml.Element
xml_parse_err = Data.Maybe.fromMaybe (error "xml_parse?") . Xml.parseXMLDoc

-- | 'Xml.parseXMLDoc' of 'B.readFile'.
xml_load :: FilePath -> IO (Maybe Xml.Element)
xml_load = fmap Xml.parseXMLDoc . ByteString.readFile

-- | Erroring variant of 'xml_load'.
xml_load_err :: FilePath -> IO Xml.Element
xml_load_err = fmap (Data.Maybe.fromMaybe (error "xml_load?")) . xml_load
