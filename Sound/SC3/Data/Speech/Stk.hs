-- | <https://github.com/thestk/stk/blob/master/src/Phonemes.cpp>
module Sound.SC3.Data.Speech.Stk where

import Data.List.Split {- split -}

-- | REAL
type R = Double

-- | FORMANT-{FREQ,RADIUS,GAIN}
type PARAM = (R,R,R)

-- | (NAME,{VOICE,NOISE}-GAIN,FORMANT-PARAM × 4)
type PH = (String,(R,R),(PARAM,PARAM,PARAM,PARAM))

-- | Parse PH.
ph_parse :: [String] -> PH
ph_parse w =
  let r2 a b = (read a,read b)
      r3 a b c = (read a,read b,read c)
  in case w of
       [x,y,z,a,b,c,d,e,f,g,h,i,j,k,l] -> (x,r2 y z,(r3 a b c,r3 d e f,r3 g h i,r3 j k l))
       _ -> error "ph_parse"

{- | Load PH from CSV file.

> fn = "/home/rohan/sw/hsc3-data/data/speech/stk.csv"
> r <- ph_load fn
> length r == 32
-}
ph_load :: FilePath -> IO [PH]
ph_load fn = do
  s <- readFile fn
  return (map (ph_parse . splitOn ",") (lines s))

-- | <https://ccrma.stanford.edu/~jos/filters/Resonator_Bandwidth_Terms_Pole.html>
--
-- > bw_to_radius (1 / 48000) 1000 == 0.9366460212365959
bw_to_radius :: Floating a => a -> a -> a
bw_to_radius t bw = exp (-pi * bw * t)

-- | <https://ccrma.stanford.edu/~jos/filters/Resonator_Bandwidth_Terms_Pole.html>
--
-- > radius_to_bw (1 / 48000) 0.9366460212365959 == 1000.0000000000002
radius_to_bw :: Floating a => a -> a -> a
radius_to_bw t r = - (log r / (pi * t))
