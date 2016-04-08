import Control.Monad {- base -}
import qualified Data.Vector.Storable as V {- vector -}
import System.Environment {- base -}

import qualified Sound.File.HSndFile as SF {- hsc3-sf-hsndfile -}
import qualified Sound.SC3 as S {- hsc3 -}
import qualified Sound.SC3.Data.Bitmap.PBM as P {- hsc3-data -}

vec_normalise_1 :: (Fractional b, Ord b, V.Storable b) => V.Vector b -> V.Vector b
vec_normalise_1 v =
    let v' = V.map abs v
        m = V.foldl1 max v'
    in V.map (S.linlin_hs (- m,m) (-1,1)) v

vec_normalise_2 :: (Fractional b, Ord b, V.Storable b) => V.Vector b -> V.Vector b
vec_normalise_2 v =
    let l = V.foldl1 min v
        r = V.foldl1 max v
    in V.map (S.linlin_hs (l,r) (-1,1)) v

vec_normalise_h :: (Fractional b, Ord b, V.Storable b) => Bool -> V.Vector b -> V.Vector b
vec_normalise_h hlf = if hlf then vec_normalise_2 else vec_normalise_1

vec_ix :: Bool -> Double -> Int -> Int -> V.Vector Double -> Int -> [(Int,Int)]
vec_ix sym h ch nc vec i =
    let v = vec V.! ((i * nc) + ch)
        f x = round (((x + 1) / 2) * (h - 1))
    in if sym then [(f v,i),(f (negate v),i)] else [(f v,i)]

sf_draw_plain :: (Bool,Bool,Bool,Bool) -> Int -> Int -> FilePath -> FilePath -> IO ()
sf_draw_plain (nrm,hlf,inv,sym) h ch sf_fn pbm_fn = do
  (hdr,Just vec') <- SF.read_vec sf_fn
  let vec = ((if sym then V.map abs else id) .
             (if inv then id else V.map negate) .
             (if nrm then vec_normalise_h hlf else id)) vec'
      nc = SF.channelCount hdr
      nf = SF.frameCount hdr
      ix = vec_ix sym (fromIntegral h) ch nc vec
  when (ch >= nc) (error "ch >= nc")
  let b = ((h,nf),concatMap ix [0 .. nf - 1])
  P.pbm4_write pbm_fn (P.bitindices_to_pbm b)

sf_draw_table :: (Double,Double) -> Int -> Int -> FilePath -> FilePath -> IO ()
sf_draw_table (l,r) h ch sf_fn pbm_fn = do
  (hdr,Just vec') <- SF.read_vec sf_fn
  let vec = V.map (S.linlin_hs (l,r) (fromIntegral h - 1,0)) vec'
      nc = SF.channelCount hdr
      nf = SF.frameCount hdr
  when (ch >= nc) (error "ch >= nc")
  let b = ((h,nf),zip (map (max 0 . min (h - 1) . floor) (V.toList vec)) [0..])
  P.pbm4_write pbm_fn (P.bitindices_to_pbm b)

help :: [String]
help =
    [unwords
     ["sf-draw","plain","pbm"
     ,"normalise:bool","normalise-mode:bool","invert:bool","symmetrical:bool"
     ,"pbm-height:int","channel:int","sound-file","pbm-file"]
    ,"sf-draw table pbm left:float right:float pbm-height:int channel:int sound-file pbm-file"
    ,"  normalise-mode: f = retain symmetry about zero, t = ignore symmetry"]

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["plain","pbm",nrm,hlf,inv,sym,h,ch,sf_fn,pbm_fn] ->
     sf_draw_plain (nrm == "t",hlf == "t",inv == "t",sym == "t") (read h) (read ch) sf_fn pbm_fn
    ["table","pbm",l,r,h,ch,sf_fn,pbm_fn] ->
     sf_draw_table (read l,read r) (read h) (read ch) sf_fn pbm_fn
    _ -> mapM_ putStrLn help

{-

let sf_fn = "/home/rohan/data/audio/gebet.R.1024.au"
let pbm_fn = "/tmp/t.pbm"
sf_draw (False,False,False,True) 128 0 sf_fn pbm_fn

let sf_fn = "/home/rohan/uc/sp-id/eof/au/tbl/gs/01.tbl.au"
sf_draw (True,True,True,False) 32 0 sf_fn pbm_fn

import System.Process
rawSystem "display" [pbm_fn]
rawSystem "convert" [pbm_fn,pbm_fn ++ ".png"]

-}
