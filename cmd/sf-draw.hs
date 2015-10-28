import Control.Monad {- base -}
import qualified Data.Vector.Storable as V {- vector -}
import System.Environment {- base -}

import qualified Sound.File.HSndFile as SF {- hsc3-sf-hsndfile -}
import qualified Sound.SC3 as S {- hsc3 -}
import qualified Sound.SC3.Data.Bitmap.PBM as P {- hsc3-data -}

vec_normalise :: (Fractional b, Ord b, V.Storable b) => V.Vector b -> V.Vector b
vec_normalise v =
    let l = V.foldl1 min v
        r = V.foldl1 max v
    in V.map (\e -> S.linlin' e l r (-1) 1) v

vec_ix :: Double -> Int -> Int -> V.Vector Double -> Int -> (Int,Int)
vec_ix h ch nc vec i =
    let x = vec V.! ((i * nc) + ch)
    in (round (((negate x + 1) / 2) * (h - 1)),i)

sf_draw :: Bool -> Int -> Int -> FilePath -> FilePath -> IO ()
sf_draw nrm h ch sf_fn pbm_fn = do
  (hdr,Just vec') <- SF.read_vec sf_fn
  let vec = if nrm then vec_normalise vec' else vec'
      nc = SF.channelCount hdr
      nf = SF.frameCount hdr
      ix = vec_ix (fromIntegral h) ch nc vec
  when (ch >= nc) (error "ch >= nc")
  let b = ((h,nf),map ix [0 .. nf - 1])
  P.pbm_write pbm_fn (P.bitindices_to_pbm b)

help :: IO ()
help = putStrLn "sf-draw pbm normalise:bool pbm-height:int channel:int sound-file pbm-file"

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["pbm",nrm,h,ch,sf_fn,pbm_fn] -> sf_draw (nrm == "t") (read h) (read ch) sf_fn pbm_fn
    _ -> help

{-

let sf_fn = "/home/rohan/uc/sp-id/eof/au/tbl/gs/01.tbl.au"
let pbm_fn = "/tmp/t.pbm"
sf_draw True 32 0 sf_fn pbm_fn

-}
