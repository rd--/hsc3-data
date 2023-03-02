{- | Parse "mime.types" file

Read mime.types:

> db <- loadMimeTypes "/etc/mime.types"
> length db == 2250

Get list of extensions:

> ext = mimeTypesExtensions db
> length ext == 1533

Find extensions that have multiple mime types:

> filter ((> 1) . length . snd) (zip ext (map (lookupMimeTypes db) ext))

Find types that don't have extensions:

> noExt = filter ((== 0) . length . snd) db
> length noExt == 1050

Lookup mime type for extension:

> lookupMimeTypes db "csv" == ["text/csv"]

Lookup extensions for mime-type:

> lookupExtensions db "audio/midi"

Read local mime.types:

> db <- loadMimeTypes "/home/rohan/sw/hsc3-data/data/types/mime.types"
> length db == 36
> lookupMimeType db "midi" == "audio/midi"

-}
module Sound.Sc3.Data.MimeType where

import Data.List {- base -}

type FileExtension = String
type MimeType = String
type MimeTypeEntry = (MimeType, [FileExtension])
type MimeTypes = [MimeTypeEntry]

isEmptyOrComment :: String -> Bool
isEmptyOrComment s = null s || head s == '#'

loadMimeTypes :: FilePath -> IO MimeTypes
loadMimeTypes fn = do
  s <- readFile fn
  let l = filter (not . isEmptyOrComment) (lines s)
      w = map words l
      f te = case te of
               typ:ext -> (typ,ext)
               _ -> error "loadMimeTypes?"
  return (map f w)

lookupMimeTypes :: MimeTypes -> FileExtension -> [MimeType]
lookupMimeTypes db ext = map fst (filter (\(_, e) -> ext `elem` e) db)

lookupMimeType :: MimeTypes -> FileExtension -> MimeType
lookupMimeType db ext =
  case lookupMimeTypes db ext of
    [typ] -> typ
    _ -> error "lookupMimeType?"

mimeTypesExtensions :: MimeTypes -> [FileExtension]
mimeTypesExtensions = nub . sort . concatMap snd

lookupExtensions :: MimeTypes -> MimeType -> [FileExtension]
lookupExtensions db typ = concatMap snd (filter (\(t, _) -> t == typ) db)
