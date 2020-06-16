-- | IFSKit file format <http://larryriddle.agnesscott.org/ifskit/IFShelp/ifsfileformat.html>
module Sound.SC3.Data.Math.IFS where

import Data.CG.Minus.Plain {- hcg-minus -}

-- | Is line NIL or a comment.
ifs_nil_line :: String -> Bool
ifs_nil_line s =
  case s of
    [] -> True
    ';':_ -> True
    _ -> False

-- | Parse data line, probablity entry is optional.
ifs_parse_line :: Read t => String -> (M22 t,V2 t,Maybe t)
ifs_parse_line s =
  case map read (words s) of
    [a,b,c,d,e,f] -> (((a,b),(c,d)),(e,f),Nothing)
    [a,b,c,d,e,f,g] -> (((a,b),(c,d)),(e,f),Just g)
    _ -> error "ifs_parse_line?"

-- | Load IFS file with single entry.
ifs_load_1 :: Read t => FilePath -> IO [(M22 t, V2 t, Maybe t)]
ifs_load_1 fn = do
  l <- fmap (filter (not . ifs_nil_line) . lines) (readFile fn)
  let n = length l
  case (last (words (l !! 0)),l !! (n - 1)) of
    ("{","}") -> return (map ifs_parse_line (take (n - 2) (tail l)))
    _ -> error "ifs_load_1?"

-- | Load IFS file with single entity having no probablity entry.
ifs_load_1_np :: Read t => FilePath -> IO [(M22 t, V2 t)]
ifs_load_1_np = fmap (map (\(m,v,_) -> (m,v))) . ifs_load_1

-- | Type specialised
ifs_load_1_np_f64 :: FilePath -> IO [(M22 Double,V2 Double)]
ifs_load_1_np_f64 = ifs_load_1_np
