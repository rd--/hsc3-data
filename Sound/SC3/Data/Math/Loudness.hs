-- | Perceptual loudness functions.
module Sound.SC3.Data.Math.Loudness where

-- * A Weighting

{- | A-weighting curve multiplier function, ie. for linear magnitude
-- value.  See <http://en.wikipedia.org/wiki/A-weighting>

> import Sound.SC3.Plot

> let {f w = map w [20::Double,50 .. 20000]
>     ;r = [a_weighting_R,b_weighting_R,c_weighting_R,d_weighting_R]}
> in plotTable (map f r)

> let {f w = zip (map log [1..]) (map w [25,50 .. 20000])
>     ;r = [a_weighting_R,b_weighting_R,c_weighting_R,d_weighting_R]}
> in plot_p2_ln (map f r)

-}
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

-- * B Weighting

-- | B-weighting curve multiplier function, ie. for linear magnitude
-- value.  See <http://en.wikipedia.org/wiki/A-weighting>
--
-- > import Sound.SC3.Plot
-- > plotTable1 (map b_weighting_R [20,50 .. 20000])
-- > plot_p2_ln [zip (map log [1..]) (map b_weighting_R [25,50 .. 20000])]
b_weighting_R :: Floating a => a -> a
b_weighting_R f =
    let sq x = x * x
        dot = foldl1 (*)
        n = dot [sq 12200
                ,f ** 3]
        d = dot [sq f + sq 20.6
                ,sqrt (sq f + sq 158.5)
                ,sq f + sq 12200]
    in n / d

-- | B-weighting curve Db offset (additive) function, ie. for
-- un-weighted Db readings.
--
-- > plotTable1 (map b_weighting [20,50 .. 20000])
-- > plot_p2_ln [zip (map log [1..]) (map b_weighting [25,50 .. 20000])]
b_weighting :: Floating a => a -> a
b_weighting f = 0.17 + 20 * logBase 10 (b_weighting_R f)

-- * C Weighting

-- | C-weighting curve multiplier function, ie. for linear magnitude
-- value.  See <http://en.wikipedia.org/wiki/A-weighting>
--
-- > import Sound.SC3.Plot
-- > plotTable1 (map c_weighting_R [20,50 .. 20000])
-- > plot_p2_ln [zip (map log [1..]) (map c_weighting_R [25,50 .. 20000])]
c_weighting_R :: Floating a => a -> a
c_weighting_R f =
    let sq x = x * x
        dot = foldl1 (*)
        n = dot [sq 12200
                ,f ** 2]
        d = dot [sq f + sq 20.6
                ,sq f + sq 12200]
    in n / d

-- | C-weighting curve Db offset (additive) function, ie. for
-- un-weighted Db readings.
--
-- > plotTable1 (map c_weighting [20,50 .. 20000])
-- > plot_p2_ln [zip (map log [1..]) (map c_weighting [25,50 .. 20000])]
c_weighting :: Floating a => a -> a
c_weighting f = 0.06 + 20 * logBase 10 (c_weighting_R f)

-- * D Weighting

-- | /h/ function for D weighting.
d_h_function :: Fractional a => a -> a
d_h_function f =
    let sq x = x * x
        n = sq (1037918.48 - sq f) + (1080768.16 * sq f)
        d = sq (9837328 - sq f) + (11723776 * sq f)
    in n / d

-- | D-weighting curve multiplier function, ie. for linear magnitude
-- value.  See <http://en.wikipedia.org/wiki/A-weighting>
--
-- > import Sound.SC3.Plot
-- > plotTable1 (map d_weighting_R [20,50 .. 20000])
-- > plot_p2_ln [zip (map log [1..]) (map d_weighting_R [25,50 .. 20000])]
d_weighting_R :: Floating a => a -> a
d_weighting_R f =
    let sq x = x * x
        a = f / (6.8966888496476 * (10 ** (-5)))
        b = sqrt (d_h_function f / ((sq f + 79919.29) * (sq f + 1345600)))
    in a * b

-- | D-weighting curve Db offset (additive) function, ie. for
-- un-weighted Db readings.
--
-- > plotTable1 (map d_weighting [20,50 .. 20000])
-- > plot_p2_ln [zip (map log [1..]) (map d_weighting [25,50 .. 20000])]
d_weighting :: Floating a => a -> a
d_weighting f = 20 * logBase 10 (d_weighting_R f)
