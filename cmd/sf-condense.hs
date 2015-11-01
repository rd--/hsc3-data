import Control.Monad {- base -}
import qualified Data.Vector.Storable as V {- vector -}
import System.Environment {- base -}

import qualified Sound.File.HSndFile as SF {- hsc3-sf-hsndfile -}
import qualified Sound.SC3 as S {- hsc3 -}
import qualified Sound.SC3.Data.Bitmap.PBM as P {- hsc3-data -}

vec_segment :: V.Storable a => V.Vector a -> (Int,Int) -> V.Vector a
vec_segment v (l,r) = V.take (r - l) (V.drop l v)

vec_abs :: (Num a,V.Storable a) => V.Vector a -> V.Vector a
vec_abs = V.map abs

vec_sum :: (Num a,V.Storable a) => V.Vector a -> a
vec_sum = V.foldl (+) 0

vec_summary :: (Fractional a,V.Storable a) => a -> V.Vector a -> (Int,Int) -> a
vec_summary w v (l,r) =
    let v' = vec_segment v (l,r)
    in vec_sum (vec_abs v') / w

read_vec_f32 :: FilePath -> IO (SF.Header,V.Vector Float)
read_vec_f32 fn = do
  (hdr,Just vec) <- SF.read_vec fn
  return (hdr,vec)

-- > win_sz 1024 4000
win_sz :: Integral a => a -> a -> a
win_sz out_nf in_nf =
    case in_nf `divMod` out_nf of
      (n,0) -> n
      (n,_) -> n + 1

-- > gen_win 1024 4000
gen_win :: Integral a => a -> a -> [Maybe (a,a)]
gen_win out_nf in_nf =
    let sz = win_sz out_nf in_nf
        f i = let l = i * sz
                  r = i * sz + sz - 1
              in if r > in_nf then Nothing else Just (l,r)
    in map f [0 .. out_nf - 1]

sf_condense :: Int -> FilePath -> FilePath -> IO ()
sf_condense out_nf in_fn out_fn = do
  (hdr,vec) <- read_vec_f32 in_fn
  let nc = SF.channelCount hdr
      nf = SF.frameCount hdr
  when (nf < out_nf) (error "nf < out_nf")
  when (nc /= 1) (error "nc /= 1")
  SF.write_au_f32_vec out_fn hdr vec
  return ()

help :: IO ()
help = putStrLn "sf-condense frame-count:int input-file output-file"

main :: IO ()
main = do
  a <- getArgs
  case a of
    [nf,in_fn,out_fn] -> sf_condense (read nf) in_fn out_fn
    _ -> help

{-

let sf_fn = "/home/rohan/uc/sp-id/eof/au/tbl/gs/01.tbl.au"
let pbm_fn = "/tmp/t.pbm"
sf_draw True 32 0 sf_fn pbm_fn

-}
