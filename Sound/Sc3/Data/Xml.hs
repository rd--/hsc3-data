-- | Functions over "Text.Xml.Light"
module Sound.Sc3.Data.Xml where

import Data.Maybe {- base -}

import qualified Data.ByteString as B {- bytestring -}
import qualified Text.XML.Light as X {- xml -}
import qualified Text.XML.Light.Lexer as X {- xml -}

-- | Assert list is non-empty.
error_if_null :: String -> [t] -> [t]
error_if_null msg ls = if null ls then error msg else ls

-- | 'X.findChild'
x_get_elem :: String -> X.Element -> X.Element
x_get_elem nm = fromMaybe (error ("x_get_elem: " ++ nm)) . X.findChild (X.unqual nm)

-- | 'X.findChildren'
x_get_elem_set :: String -> X.Element -> [X.Element]
x_get_elem_set nm = error_if_null ("x_get_elem_set: " ++ nm) . X.findChildren (X.unqual nm)

-- | Descending sequence of 'x_get_elem'
x_get_elem_path :: [String] -> X.Element -> X.Element
x_get_elem_path p x =
  case p of
    [] -> x
    nm : p1 -> x_get_elem_path p1 (x_get_elem nm x)

-- | 'X.findElement'
x_has_elem :: String -> X.Element -> Bool
x_has_elem nm = isJust . X.findElement (X.unqual nm)

-- | 'X.findElement'
x_find_elem :: String -> X.Element -> X.Element
x_find_elem nm = fromMaybe (error ("x_find_elem: " ++ nm)) . X.findElement (X.unqual nm)

-- | 'X.findAttr'
x_get_attr :: String -> X.Element -> String
x_get_attr nm = fromMaybe (error ("x_get_attr: " ++ nm)) . X.findAttr (X.unqual nm)

-- | 'X.qName' of 'X.elName'
x_elem_name :: X.Element -> String
x_elem_name = X.qName . X.elName

-- | 'X.elContent'
xml_elem_text_cdata_uniq :: X.Element -> String
xml_elem_text_cdata_uniq e =
  case X.elContent e of
    [X.Text (X.CData X.CDataText str _)] -> str
    _ -> error "xml_elem_text_cdata_uniq"

-- | Erroring variant of ' X.parseXMLDoc'.
xml_parse_err :: X.XmlSource x => x -> X.Element
xml_parse_err = fromMaybe (error "xml_parse?") . X.parseXMLDoc

-- | 'X.parseXMLDoc' of 'B.readFile'.
xml_load :: FilePath -> IO (Maybe X.Element)
xml_load = fmap X.parseXMLDoc . B.readFile

-- | Erroring variant of 'xml_load'.
xml_load_err :: FilePath -> IO X.Element
xml_load_err = fmap (fromMaybe (error "xml_load?")) . xml_load
