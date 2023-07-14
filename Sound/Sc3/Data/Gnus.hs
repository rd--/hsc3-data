module Sound.Sc3.Data.Gnus where

import Data.Function {- base -}
import Data.List.Split {- split -}
import qualified Data.Time as T {- time -}
import System.FilePath {- file-path -}

import qualified Data.ByteString.Char8 as B {- bytestring -}

import qualified Sound.Sc3.Data.Timestamp as Timestamp {- hsc3-data -}

-- | Parse date/time field of overview, required to be very lenient.
parse_time :: String -> T.LocalTime
parse_time s =
    case Timestamp.parse_timestamp s of
      Just t -> t
      Nothing -> Timestamp.parse_time_fmt Timestamp.rfc822_fmt_lenient (take 31 s)

-- | Xref = (folder,message-id)
type Xref = (String,Int)

-- | Allow multiple Xref fields.
xref_parse :: String -> [Xref]
xref_parse s =
    let f x = case splitOn ":" x of
                [nm,ix] -> (nm,read ix)
                _ -> error (show ("xref_parse?",x))
    in case words s of
         "Xref:":_:x -> map f x
         e -> error (show ("xref_parse?",e))

xref_file :: Xref -> FilePath
xref_file (folder,ix) =
    case splitOn "." folder of
      [p,q] -> p </> q </> show ix
      _ -> error "xref_file?"

-- | (ID,Subject,From,Date,Lines,Xref,To)
type Mail_Header = (Int,String,String,T.LocalTime,Int,[Xref],String)

hdr_date :: Mail_Header -> T.LocalTime
hdr_date (_,_,_,date,_,_,_) = date

hdr_year :: Mail_Header -> Int
hdr_year = fromIntegral . (\(y,_,_) -> y) . T.toGregorian . T.localDay . hdr_date

hdr_compare :: Mail_Header -> Mail_Header -> Ordering
hdr_compare = compare `on` hdr_date

{- | Names for overview file fields.

>>> import Data.List
>>> intercalate "," (map snd overview_col)
"ID,Subject,From,Date,Lines,Xref,To"
-}
overview_col :: [(Int,String)]
overview_col = [(0,"ID"),(1,"Subject"),(2,"From"),(3,"Date"),(7,"Lines"),(8,"Xref"),(9,"To")]

overview_parse :: [B.ByteString] -> Mail_Header
overview_parse o =
    let str n = B.unpack (o !! n)
    in (read (str 0)
       ,str 1
       ,str 2
       ,parse_time (str 3)
       ,read (str 7)
       ,xref_parse (str 8)
       ,str 9)

overview_load :: FilePath -> IO [[B.ByteString]]
overview_load fn = do
  s <- B.readFile fn
  let l = B.lines s
      f = B.split '\t'
  return (map f l)
