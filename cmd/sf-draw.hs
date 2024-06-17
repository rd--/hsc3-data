import Control.Monad {- base -}
import System.Environment {- base -}

import qualified Data.Vector.Storable as Vector {- vector -}

import qualified Sound.File.HSndFile as Sf.SndFile {- hsc3-sf-hsndfile -}
{- hsc3 -}
import qualified Sound.Sc3.Common.Buffer.Vector as Buffer.Vector {- hsc3 -}
import qualified Sound.Sc3.Common.Math as M
import qualified Sound.Sc3.Data.Bitmap.Pbm as Pbm {- hsc3-data -}

vec_normalise_1 :: (Fractional b, Ord b, Vector.Storable b) => Vector.Vector b -> Vector.Vector b
vec_normalise_1 v =
  let v' = Vector.map abs v
      m = Vector.foldl1 max v'
  in Vector.map (M.linlin_hs (-m, m) (-1, 1)) v

vec_normalise_2 :: (Fractional b, Ord b, Vector.Storable b) => Vector.Vector b -> Vector.Vector b
vec_normalise_2 v =
  let l = Vector.foldl1 min v
      r = Vector.foldl1 max v
  in Vector.map (M.linlin_hs (l, r) (-1, 1)) v

vec_normalise_h :: (Fractional b, Ord b, Vector.Storable b) => Bool -> Vector.Vector b -> Vector.Vector b
vec_normalise_h hlf = if hlf then vec_normalise_2 else vec_normalise_1

vec_ix :: Bool -> Double -> Int -> Int -> Vector.Vector Double -> Int -> [(Int, Int)]
vec_ix sym h ch nc vec i =
  let v = vec Vector.! ((i * nc) + ch)
      f x = round (((x + 1) / 2) * (h - 1))
  in if sym then [(f v, i), (f (negate v), i)] else [(f v, i)]

sf_draw_plain :: (Bool, Bool, Bool, Bool, Bool) -> Int -> Int -> FilePath -> FilePath -> IO ()
sf_draw_plain (nrm, hlf, inv, sym, sc3_wt) h ch sf_fn pbm_fn = do
  (hdr, vec') <- Sf.SndFile.read_vec_f64 sf_fn
  let vec =
        ( (if sym then Vector.map abs else id)
            . (if inv then id else Vector.map negate)
            . (if nrm then vec_normalise_h hlf else id)
            . (if sc3_wt then Buffer.Vector.from_wavetable else id)
        )
          vec'
      nc = Sf.SndFile.channelCount hdr
      nf = Vector.length vec -- not (Sf.SndFile.frameCount hdr) due to Buffer.Vector.from_wavetable
      ix = vec_ix sym (fromIntegral h) ch nc vec
  when (ch >= nc) (error "ch >= nc")
  let b = ((h, nf), concatMap ix [0 .. nf - 1])
  Pbm.pbm4_write pbm_fn (Pbm.bitindices_to_pbm b)

sf_draw_table :: (Double, Double) -> Int -> Int -> FilePath -> FilePath -> IO ()
sf_draw_table (l, r) h ch sf_fn pbm_fn = do
  (hdr, vec') <- Sf.SndFile.read_vec_f64 sf_fn
  let vec = Vector.map (M.linlin_hs (l, r) (fromIntegral h - 1, 0)) vec'
      nc = Sf.SndFile.channelCount hdr
      nf = Sf.SndFile.frameCount hdr
  when (ch >= nc) (error "ch >= nc")
  let b = ((h, nf), zip (map (max 0 . min (h - 1) . floor) (Vector.toList vec)) [0 ..])
  Pbm.pbm4_write pbm_fn (Pbm.bitindices_to_pbm b)

help :: [String]
help =
  [ unwords
      [ "sf-draw"
      , "plain"
      , "pbm"
      , "normalise:bool"
      , "normalise-mode:bool"
      , "invert:bool"
      , "symmetrical:bool"
      , "sc3-wavetable:bool"
      , "pbm-height:int"
      , "channel:int"
      , "sound-file"
      , "pbm-file"
      ]
  , "sf-draw table pbm left:float right:float pbm-height:int channel:int sound-file pbm-file"
  , "  normalise-mode: f = retain symmetry about zero, t = ignore symmetry"
  ]

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["plain", "pbm", nrm, hlf, inv, sym, sc3_wt, h, ch, sf_fn, pbm_fn] ->
      sf_draw_plain
        (nrm == "t", hlf == "t", inv == "t", sym == "t", sc3_wt == "t")
        (read h)
        (read ch)
        sf_fn
        pbm_fn
    ["table", "pbm", l, r, h, ch, sf_fn, pbm_fn] ->
      sf_draw_table (read l, read r) (read h) (read ch) sf_fn pbm_fn
    _ -> mapM_ putStrLn help

{-

let sf_fn = "/home/rohan/data/audio/gebet.R.1024.au"
let pbm_fn = "/tmp/t.pbm"
sf_draw_plain (False,False,False,True,False) 128 0 sf_fn pbm_fn

let sf_fn = "/home/rohan/uc/sp-id/eof/au/tbl/gs/01.tbl.au"
sf_draw_plain (True,True,True,False,False) 32 0 sf_fn pbm_fn

import System.Process {- process -}
rawSystem "display" [pbm_fn]
rawSystem "convert" [pbm_fn,pbm_fn ++ ".png"]

-}
