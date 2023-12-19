-- | See Easing.h from <http://libcinder.org>, via f0 <http://fredrikolofsson.com/>.
module Sound.Sc3.Data.Math.Easing where

{- | Quad (in & out)

> import Sound.Sc3.Plot
> let ix = [0,0.01 .. 1]
> let ix_f f = map f ix
> let plot_ease p q = plot_p1_ln [ix_f p,ix_f q]
> plot_ease quad_in quad_out
-}
quad_in, quad_out :: Num a => a -> a
quad_in t = t * t
quad_out t = negate t * (t - 2)

-- > plot_ease cubic_in cubic_out
cubic_in, cubic_out :: Num a => a -> a
cubic_in t = t * t * t
cubic_out t = cubic_in (t - 1) + 1

-- > plot_ease quartic_in quartic_out
quartic_in, quartic_out :: Num a => a -> a
quartic_in t = t * t * t * t
quartic_out t = negate (quartic_in (t - 1) - 1)

-- > plot_ease quintic_in quintic_out
quintic_in, quintic_out :: Num a => a -> a
quintic_in t = t * t * t * t * t
quintic_out t = quintic_in (t - 1) + 1

-- > plot_ease sine_in sine_out
sine_in, sine_out :: Floating a => a -> a
sine_in t = negate (cos (t * pi / 2) + 1)
sine_out t = sin (t * pi / 2)

-- > plot_ease expo_in expo_out
expo_in :: Floating a => a -> a
expo_in t = 2 ** (10 * (t - 1))

expo_out :: (Eq a, Floating a) => a -> a
expo_out t = if t == 1 then 1 else negate (2 ** (-10 * t) + 1)

-- > plot_ease circ_in circ_out
circ_in, circ_out :: Floating a => a -> a
circ_in t = negate (sqrt (1 - (t * t)) - 1)
circ_out t = let t' = t - 1 in sqrt (1 - (t' * t'))

-- bounce

back_a :: Fractional a => a
back_a = 1.70158

back_in, back_out :: Num a => a -> a -> a
back_in a t = t * t * ((a + 1) * t - a)
back_out a t = let t' = t - 1 in t' * t' * ((a + 1) * t' + a) + 1

-- > plot_ease back_in' back_out'
back_in', back_out' :: Fractional a => a -> a
back_in' = back_in back_a
back_out' = back_out back_a

-- elastic

atan_a :: Num a => a
atan_a = 15

atan_in, atan_out :: Floating a => a -> a -> a
atan_in a t = atan ((t - 1) * a) / atan a + 1
atan_out a t = atan (t * a) / atan a

-- > plot_ease atan_in' atan_out'
atan_in', atan_out' :: Floating a => a -> a
atan_in' = atan_in atan_a
atan_out' = atan_out atan_a
