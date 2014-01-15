-- | Perceptual loudness functions.
module Sound.SC3.Data.Math.Loudness where

-- | A-weighting curve multiplier function, ie. for linear magnitude
-- value.  See <http://en.wikipedia.org/wiki/A-weighting>
--
-- > import Sound.SC3.Plot
-- > plotTable1 (map a_weighting_R [20,50 .. 20000])
-- > plot_p2_ln [zip (map log [1..]) (map a_weighting_R [25,50 .. 20000])]
a_weighting_R :: Floating a => a -> a
a_weighting_R f =
    let sq x = x * x
        dot = foldl1 (*)
        n = dot [sq 12200
                ,f ** 4]
        d = dot [sq f + sq 20.6
                ,sqrt ((sq f + sq 107.7) * (sq f + sq 737.9))
                ,sq f + sq 12200]
    in n / d

-- | A-weighting curve Db offset (additive) function, ie. for
-- un-weighted Db readings.
--
-- > plotTable1 (map a_weighting [20,50 .. 20000])
-- > plot_p2_ln [zip (map log [1..]) (map a_weighting [25,50 .. 20000])]
a_weighting :: Floating a => a -> a
a_weighting f = 2 + 20 * logBase 10 (a_weighting_R f)
