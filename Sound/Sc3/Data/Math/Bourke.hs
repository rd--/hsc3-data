-- | <http://paulbourke.net/>
module Sound.Sc3.Data.Math.Bourke where

import Music.Theory.Geometry.Vector {- hmt-base -}

-- * Fractals <http://paulbourke.net/fractals>

-- | 2-element /h/ transform.
h_transform_2 :: Num t => t -> (V2 t -> V2 t) -> V2 t -> V2 t
h_transform_2 h f (x, y) =
  let (x', y') = f (x, y)
  in (x + h * x', y + h * y')

-- | 3-element /h/ transform.
h_transform_3 :: Num t => t -> (V3 t -> V3 t) -> V3 t -> V3 t
h_transform_3 h f (x, y, z) =
  let (x', y', z') = f (x, y, z)
  in (x + h * x', y + h * y', z + h * z')

-- | <http://paulbourke.net/fractals/lorenz/>
lorenz :: Num t => t -> t -> t -> V3 t -> V3 t
lorenz a b c (x, y, z) =
  ( a * (y - x)
  , x * (b - z) - y
  , x * y - c * z
  )

{- | <http://paulbourke.net/fractals/lorenz/>

> import Sound.Sc3.Plot
> l = iterate (lorenz_h 0.01 10 28 (8/3)) (0.1,0.0,0.0)
> plot_p3_ln [take 5000 l]

> l = iterate (lorenz_h 0.01 10 28 (8/3)) (0.1,0.0,0.0)
> f (x,_,z) = (x,z)
> plot_p2_ln [take 15000 (map f l)]
-}
lorenz_h :: Num t => t -> t -> t -> t -> V3 t -> V3 t
lorenz_h h a b c = h_transform_3 h (lorenz a b c)

-- | <http://paulbourke.net/fractals/rossler/>
rossler :: Num t => t -> t -> t -> V3 t -> V3 t
rossler a b c (x, y, z) =
  ( negate y - z
  , x + a * y
  , b + z * (x - c)
  )

{- | <http://paulbourke.net/fractals/rossler/>

> plot_p3_ln [take 5000 (iterate (rossler_h 0.02 0.2 0.2 5.7) (0.1,0,0))]
-}
rossler_h :: Num t => t -> t -> t -> t -> V3 t -> V3 t
rossler_h h a b c = h_transform_3 h (rossler a b c)

{- | <http://paulbourke.net/fractals/peterdejong/>

> vw x = plot_p2_pt [take 12500 x]
> pdj a b c d = vw (iterate (peter_de_jong a b c d) (-0.72,-0.64))

> pdj 1.4 (-2.3) 2.4 (-2.1)
> pdj 2.01 (-2.53) 1.61 (-0.33)
> pdj (-2.7) (-0.09) (-0.86) (-2.2)
> pdj (-2.24) 0.43 (-0.65) (-2.43)
> pdj (-2.0) (-2.0) (-1.2) 2.0
> pdj 1.641 1.902 0.316 1.525
> pdj 0.970 (-1.899) 1.381 (-1.506)
> pdj (-0.827) (-1.637) 1.659 (-0.943)
> pdj (-0.709) 1.638 0.452 1.740
-}
peter_de_jong :: Floating t => t -> t -> t -> t -> V2 t -> V2 t
peter_de_jong a b c d (x, y) =
  ( sin (a * y) - cos (b * x)
  , sin (c * x) - cos (d * y)
  )

{- | <http://paulbourke.net/fractals/clifford/>

> vw x = plot_p2_pt [take 12500 x]
> clf a b c d = vw (iterate (clifford a b c d) (-0.72,-0.64))

> clf (-1.4) (1.6) (1.0) (0.7)
> clf (1.1) (-1.0) (1.0) (1.5) -- not as pb indicates?
> clf (1.6) (-0.6) (-1.2) (1.6)
> clf (1.7) (1.7) (0.6) (1.2)
> clf 1.5 (-1.8) 1.6 0.9
> clf (-1.7) 1.3 (-0.1) (-1.2)
> clf (-1.7) 1.8 (-1.9) (-0.4)
> clf (-1.8) (-2.0) (-0.5) (-0.9)
-}
clifford :: Floating t => t -> t -> t -> t -> V2 t -> V2 t
clifford a b c d (x, y) =
  ( sin (a * y) + c * cos (a * x)
  , sin (b * x) + d * cos (b * y)
  )

-- * Interpolation

-- | <http://paulbourke.net/miscellaneous/interpolation/>
interpolate_linear :: Num a => V2 a -> a -> a
interpolate_linear (y1, y2) mu = (y1 * (1 - mu)) + (y2 * mu)

interpolate_linear_v2 :: Num a => V2 (V2 a) -> a -> V2 a
interpolate_linear_v2 ((x1, y1), (x2, y2)) mu = let f = interpolate_linear in (f (x1, x2) mu, f (y1, y2) mu)

interpolate_linear_v3 :: Num a => V2 (V3 a) -> a -> V3 a
interpolate_linear_v3 ((x1, y1, z1), (x2, y2, z2)) mu =
  let f = interpolate_linear in (f (x1, x2) mu, f (y1, y2) mu, f (z1, z2) mu)

interpolate_cosine :: Floating a => V2 a -> a -> a
interpolate_cosine (y1, y2) mu =
  let mu2 = (1 - cos (mu * pi)) / 2
  in (y1 * (1 - mu2) + y2 * mu2)

interpolate_cubic :: Num a => V4 a -> a -> a
interpolate_cubic (y0, y1, y2, y3) mu =
  let mu2 = mu * mu
      a0 = y3 - y2 - y0 + y1
      a1 = y0 - y1 - a0
      a2 = y2 - y0
      a3 = y1
  in a0 * mu * mu2 + a1 * mu2 + a2 * mu + a3

interpolate_catmull_rom :: Fractional a => V4 a -> a -> a
interpolate_catmull_rom (y0, y1, y2, y3) mu =
  let mu2 = mu * mu
      a0 = (-0.5) * y0 + 1.5 * y1 - 1.5 * y2 + 0.5 * y3
      a1 = y0 - 2.5 * y1 + 2 * y2 - 0.5 * y3
      a2 = (-0.5) * y0 + 0.5 * y2
      a3 = y1
  in a0 * mu * mu2 + a1 * mu2 + a2 * mu + a3

{- | Hermite interpolation

   Tension: 1 is high, 0 normal, -1 is low
   Bias: 0 is even,
         positive is towards first segment,
         negative towards the other
-}
interpolate_hermite :: Fractional a => (a, a) -> V4 a -> a -> a
interpolate_hermite (tension, bias) (y0, y1, y2, y3) mu =
  let mu2 = mu * mu
      mu3 = mu2 * mu
      m0 = ((y1 - y0) * (1 + bias) * (1 - tension) / 2) + ((y2 - y1) * (1 - bias) * (1 - tension) / 2)
      m1 = ((y2 - y1) * (1 + bias) * (1 - tension) / 2) + ((y3 - y2) * (1 - bias) * (1 - tension) / 2)
      a0 = 2 * mu3 - 3 * mu2 + 1
      a1 = mu3 - 2 * mu2 + mu
      a2 = mu3 - mu2
      a3 = (-2) * mu3 + 3 * mu2
  in a0 * y1 + a1 * m0 + a2 * m1 + a3 * y2

{-

import qualified Music.Theory.List as T {- hmt-base -}
import Sound.Sc3.Plot {- hsc3-plot -}

y = [145,60,75,55,155,0,100,25]
m k = take k [0,1 / fromIntegral k ..]
z2 k f = concatMap (\z -> map (f z) (m k)) (T.adj2 1 y)
plot_p1_ln [z2 100 interpolate_linear,z2 100 interpolate_cosine]

y' = [145,145,60,75,55,155,0,100,25,25]
z4 k f = concatMap (\[y1,y2,y3,y4] -> map (f (y1,y2,y3,y4)) (m k)) (T.adj_trunc 4 1 y')
plot_p1_ln [z4 100 interpolate_cubic,z4 100 interpolate_catmull_rom,z4 100 (interpolate_hermite (0,0))]
plot_p1_ln [z4 100 (interpolate_hermite (t,b)) | t <- [0,1], b <- [0,1]]
plot_p1_ln [z4 100 (interpolate_hermite (t,b)) | t <- [-1,0], b <- [-1,0]]
plot_p1_ln [z4 100 (interpolate_hermite (t,b)) | t <- [-1,0,1], b <- [-1,0,1]]
plot_p1_ln [z4 100 (interpolate_hermite (t,b)) | t <- [-0.5,0,0.5], b <- [-0.5,0,0.5]]
plot_p1_ln [z4 100 (interpolate_hermite (t,b)) | t <- [-2,2], b <- [0,0.15]]

-}
