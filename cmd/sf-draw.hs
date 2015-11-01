import Control.Monad {- base -}
import qualified Data.Vector.Storable as V {- vector -}
import System.Environment {- base -}

import qualified Sound.File.HSndFile as SF {- hsc3-sf-hsndfile -}
import qualified Sound.SC3 as S {- hsc3 -}
import qualified Sound.SC3.Data.Bitmap.PBM as P {- hsc3-data -}

vec_normalise :: (Fractional b, Ord b, V.Storable b) => V.Vector b -> V.Vector b
vec_normalise v =
    let v' = V.map abs v
        m = V.foldl1 max v'
    in V.map (\e -> S.linlin' e (- m) m (-1) 1) v

vec_ix :: Bool -> Double -> Int -> Int -> V.Vector Double -> Int -> [(Int,Int)]
vec_ix sym h ch nc vec i =
    let v = vec V.! ((i * nc) + ch)
        f x = round (((x + 1) / 2) * (h - 1))
    in if sym then [(f v,i),(f (negate v),i)] else [(f v,i)]

sf_draw :: (Bool,Bool,Bool) -> Int -> Int -> FilePath -> FilePath -> IO ()
sf_draw (nrm,inv,sym) h ch sf_fn pbm_fn = do
  (hdr,Just vec') <- SF.read_vec sf_fn
  let vec = ((if sym then V.map abs else id) .
             (if inv then id else V.map negate) .
             (if nrm then vec_normalise else id)) vec'
      nc = SF.channelCount hdr
      nf = SF.frameCount hdr
      ix = vec_ix sym (fromIntegral h) ch nc vec
  when (ch >= nc) (error "ch >= nc")
  let b = ((h,nf),concatMap ix [0 .. nf - 1])
  P.pbm_write pbm_fn (P.bitindices_to_pbm b)

help :: String
help =
    unwords
    ["sf-draw"
    ,"pbm"
    ,"normalise:bool"
    ,"invert:bool"
    ,"symmetrical:bool"
    ,"pbm-height:int"
    ,"channel:int"
    ,"sound-file"
    ,"pbm-file"]

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["pbm",nrm,inv,sym,h,ch,sf_fn,pbm_fn] ->
        sf_draw (nrm == "t",inv == "t",sym == "t") (read h) (read ch) sf_fn pbm_fn
    _ -> putStrLn help

{-

let sf_fn = "/home/rohan/uc/sp-id/eof/au/tbl/gs/01.tbl.au"
let sf_fn = "/home/rohan/data/audio/gebet.R.1024.au"
let pbm_fn = "/tmp/t.pbm"
sf_draw (False,False,True) 128 0 sf_fn pbm_fn

-}
