module Sound.SC3.Data.XML where

import Data.Maybe {- base -}

import qualified Data.ByteString as B {- bytestring -}
import qualified Text.XML.Light as X {- xml -}
import qualified Text.XML.Light.Lexer as X {- xml -}

error_if_null :: String -> [t] -> [t]
error_if_null msg ls = if null ls then error msg else ls

x_get_elem :: String -> X.Element -> X.Element
x_get_elem nm = fromMaybe (error "x_get_elem") . X.findChild (X.unqual nm)

x_get_elem_set :: String -> X.Element -> [X.Element]
x_get_elem_set nm = error_if_null "x_get_elem_set" . X.findChildren (X.unqual nm)

x_get_elem_path :: [String] -> X.Element -> X.Element
x_get_elem_path p x =
    case p of
      [] -> x
      nm:p' -> x_get_elem_path p' (x_get_elem nm x)

x_find_elem :: String -> X.Element -> X.Element
x_find_elem nm = fromMaybe (error "x_find_elem") . X.findElement (X.unqual nm)

x_get_attr :: String -> X.Element -> String
x_get_attr nm = fromMaybe (error "x_get_attr") . X.findAttr (X.unqual nm)

x_elem_name :: X.Element -> String
x_elem_name = X.qName . X.elName

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
