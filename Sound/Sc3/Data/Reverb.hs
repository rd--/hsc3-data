-- | Reverb
module Sound.Sc3.Data.Reverb where

-- * Moorer, James "About This Reverberation Business" Computer Music Journal 3/2 pp.13-28 (1979)

-- | R = real
type R = Double

-- | sr = sample-rate
jm_sr :: R
jm_sr = 44100

{- | rt = reverb time (seconds), sq = delay-times (seconds)

>>> let (_,_,dt) = jm_boston_hall
>>> jm_comb_gains 1 dt
([0.7079457843841379,0.6791610781908861,0.6561555441406047,0.6251531078671844,0.6081540530409288,0.5834268265736551],0.634)
-}
jm_comb_gains :: R -> [R] -> ([R], R)
jm_comb_gains rt sq =
  let f dt = 10 ** (-3 * dt * rt)
  in (map f sq, 1 - 0.366 / rt)

-- | (Delay-Times:sec,Delay-Gains:lin,Comb-Delays:sec)
jm_boston_hall :: ([R], [R], [R])
jm_boston_hall =
  ( map (/ jm_sr) [190, 948, 992, 1182, 1191, 1314, 2020, 2139, 2523, 2589, 2624, 2699, 3118, 3122, 3202, 3268, 3321, 3515]
  , [1, 0.841, 0.504, 0.491, 0.379, 0.380, 0.346, 0.289, 0.272, 0.192, 0.193, 0.217, 0.181, 0.180, 0.181, 0.176, 0.142, 0.167, 0.134]
  , map (/ jm_sr) [2205, 2470, 2690, 2999, 3175, 3440]
  )

jm_moorer :: ([R], [R])
jm_moorer =
  ( map (/ jm_sr) [0, 878, 1561, 1715, 1826, 3083, 3510]
  , [1, 1.020, 0.818, 0.635, 0.719, 0.267, 0.242]
  )
