-- | <http://paulbourke.net/miscellaneous/interpolation/>
module Sound.SC3.Data.Math.Interpolation where

{-
import qualified Music.Theory.List as T {- hmt -}
import Sound.SC3.Plot {- hsc3-plot -}

y = [145,60,75,55,155,0,100,25]
m k = take k [0,1 / fromIntegral k ..]
z2 k f = concatMap (\z -> map (f z) (m k)) (T.adj2 1 y)
plot_p1_ln [z2 100 interpolate_linear,z2 100 interpolate_cosine]

y' = [145,145,60,75,55,155,0,100,25,25]
z4 k f = concatMap (\[y1,y2,y3,y4] -> map (f y1 y2 y3 y4) (m k)) (T.adj_trunc 4 1 y')
plot_p1_ln [z4 100 interpolate_cubic,z4 100 interpolate_catmull_rom,z4 100 (interpolate_hermite 0 0)]
plot_p1_ln [z4 100 (interpolate_hermite t b) | t <- [0,1], b <- [0,1]]
plot_p1_ln [z4 100 (interpolate_hermite t b) | t <- [-1,0], b <- [-1,0]]
plot_p1_ln [z4 100 (interpolate_hermite t b) | t <- [-1,0,1], b <- [-1,0,1]]
plot_p1_ln [z4 100 (interpolate_hermite t b) | t <- [-0.5,0,0.5], b <- [-0.5,0,0.5]]
plot_p1_ln [z4 100 (interpolate_hermite t b) | t <- [-2,2], b <- [0,0.15]]
-}
interpolate_linear :: Num a => (a,a) -> a -> a
interpolate_linear (y1,y2) mu = (y1 * (1 - mu)) + (y2 * mu)

interpolate_cosine :: Floating a => (a,a) -> a -> a
interpolate_cosine (y1,y2) mu =
  let mu2 = (1 - cos (mu * pi)) / 2
  in (y1 * (1 - mu2) + y2 * mu2)

interpolate_cubic :: Num a => (a,a,a,a) -> a -> a
interpolate_cubic (y0,y1,y2,y3) mu =
  let mu2 = mu * mu
      a0 = y3 - y2 - y0 + y1
      a1 = y0 - y1 - a0
      a2 = y2 - y0
      a3 = y1
  in a0 * mu * mu2 + a1 * mu2 + a2 * mu + a3

interpolate_catmull_rom :: Fractional a => (a,a,a,a) -> a -> a
interpolate_catmull_rom (y0,y1,y2,y3) mu =
  let mu2 = mu * mu
      a0 = (-0.5) * y0 + 1.5 * y1 - 1.5 * y2 + 0.5 * y3
      a1 = y0 - 2.5 * y1 + 2 * y2 - 0.5 * y3
      a2 = (-0.5) * y0 + 0.5 * y2
      a3 = y1
  in a0 * mu * mu2 + a1 * mu2 + a2 * mu + a3

{-
   Tension: 1 is high, 0 normal, -1 is low
   Bias: 0 is even,
         positive is towards first segment,
         negative towards the other
-}
interpolate_hermite :: Fractional a => a -> a -> (a,a,a,a) -> a -> a
interpolate_hermite tension bias (y0,y1,y2,y3) mu =
 let mu2 = mu * mu
     mu3 = mu2 * mu
     m0 = ((y1 - y0) * (1 + bias) * (1 - tension)/2) + ((y2 - y1) * (1 - bias) * (1 - tension)/2)
     m1 = ((y2 - y1) * (1 + bias) * (1 - tension)/2) + ((y3 - y2) * (1 - bias) * (1 - tension)/2)
     a0 = 2 * mu3 - 3 * mu2 + 1
     a1 = mu3 - 2 * mu2 + mu
     a2 = mu3 - mu2
     a3 = (-2) * mu3 + 3 * mu2
 in a0 * y1 + a1 * m0 + a2 * m1 + a3 * y2
