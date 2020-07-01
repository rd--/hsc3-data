-- | <https://cccbr.github.io/methods-library/xml/CCCBR_methods.xml.zip>
--
-- <http://www.methods.org.uk/schemas/2007/05/methods.xsd>
--
-- Requires that the CCCBR XML file has had qualifiers at collection element deleted.
module Sound.SC3.Data.XML.CCCBR where

import qualified Text.XML.Light as X {- xml -}

import qualified Music.Theory.Permutations.Morris_1984 as T {- hmt -}

import qualified Sound.SC3.Data.XML as XML {- hsc3-data -}


-- | Load collection.
cccbr_load_collection :: FilePath -> IO X.Element
cccbr_load_collection = XML.xml_load_err

-- | (collection.methodSet).(properties,[method])
cccbr_methods :: X.Element -> [(X.Element,[X.Element])]
cccbr_methods =
  let f e = (XML.x_get_elem "properties" e,XML.x_get_elem_set "method" e)
  in map f . XML.x_get_elem_set "methodSet"

-- | 'cccbr_title' and 'cccbr_geometries' of 'cccbr_load_model'
--
-- > m <- cccbr_load_methods "/home/rohan/data/cccbr/CCCBR_methods.xml"
-- > length m == 816
-- > sum (map (length . snd) m) == 22044
cccbr_load_methods :: FilePath -> IO [(X.Element,[X.Element])]
cccbr_load_methods = fmap cccbr_methods . cccbr_load_collection

-- | ((STAGE,LENGTH-OF-LEAD),(TITLE,PLACE-NOTATION,LEAD-HEAD))
type CCCBR_METHODS = ((Int,Int),[(String,String,[Int])])

-- | Unpack 'CCCBR_METHODS' from XML data.
--
-- > u = map cccbr_methods_unpack m
-- > u !! 815 == ((22,8),[("Little Bob Twenty-two","-1L-14,12",[1,6,4,8,2,10,3,12,5,14,7,16,9,18,11,20,13,22,15,21,17,19])])
cccbr_methods_unpack :: (X.Element,[X.Element]) -> CCCBR_METHODS
cccbr_methods_unpack (p,m) =
  let txt nm = XML.xml_elem_text_cdata_uniq . XML.x_get_elem nm
      int nm = read . txt nm
      lst nm = map T.nchar_to_int . txt nm
      f e = (txt "title" e,txt "notation" e,lst "leadHead" e)
  in ((int "stage" p,int "lengthOfLead" p),map f m)
