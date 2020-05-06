module Sound.SC3.Data.Math.Hermite where

-- | 4-point, 3rd-order Hermite (x-form)
hermite_p4_o3_xf :: Fractional t => t -> (t,t,t,t) -> t
hermite_p4_o3_xf x (y0,y1,y2,y3) =
    let c0 = y1;
        c1 = 0.5 * (y2 - y0)
        c2 = y0 - 2.5 * y1 + 2.0 * y2 - 0.5 * y3
        c3 = 1.5 * (y1 - y2) + 0.5 * (y3 - y0)
    in ((c3 * x + c2) * x + c1) * x + c0

-- | Hermite bases, expanded form
--
-- > map hermite_bases_exp [0,1/4,1/2,3/4,1]
hermite_bases_exp :: Num d => d -> (d,d,d,d)
hermite_bases_exp t =
  let cub n = n * n * n
      sqr n = n * n
      h00 = 2 * cub t - 3 * sqr t + 1 -- (1 + 2 * t) * sq (1 - t)
      h10 = cub t - 2 * sqr t + t -- t * sq (1 - t)
      h01 = -2 * cub t + 3 * sqr t -- sq t * (3 - 2 * t)
      h11 = cub t - sqr t -- sq t * (t - 1)
  in (h00,h10,h01,h11)

-- | Hermite bases, factored form.
--
-- > map hermite_bases_fac [0,1/4,1/2,3/4,1]
hermite_bases_fac :: Num d => d -> (d,d,d,d)
hermite_bases_fac t =
  let sqr n = n * n
      h00 = (1 + 2 * t) * sqr (1 - t)
      h10 = t * sqr (1 - t)
      h01 = sqr t * (3 - 2 * t)
      h11 = sqr t * (t - 1)
  in (h00,h10,h01,h11)

hermite :: Num a => (a, a) -> (a, a) -> a -> a
hermite (p0,p1) (m0,m1) t =
  let (h00,h10,h01,h11) = hermite_bases_exp t
  in h00 * p0 + h10 * m0 + h01 * p1 + h11 * m1

{- | <https://en.wikipedia.org/wiki/Kochanek-Bartels_spline>

t = tension = changes the length of the tangent vector
b = bias = primarily changes the direction of the tangent vector
c = continuity = changes the sharpness in change between tangents

Setting each parameter to zero would give a Catmull–Rom spline.
-}
kochanek_bartels_spline_m :: Fractional t => (t,t,t) -> (t,t,t,t) -> (t,t)
kochanek_bartels_spline_m (t,b,c) (p0,p1,p2,p3) =
  let m1 = ((((1 - t) * (1 + b) * (1 + c)) / 2) * (p1 - p0)) +
           ((((1 - t) * (1 - b) * (1 - c)) / 2) * (p2 - p1))

      m2 = ((((1 - t) * (1 + b) * (1 - c)) / 2) * (p2 - p1)) +
           ((((1 - t) * (1 - b) * (1 + c)) / 2) * (p3 - p2))
  in (m1,m2)

kochanek_bartels_spline_h :: Fractional t => (t,t,t) -> [t] -> [((t,t),(t,t))]
kochanek_bartels_spline_h param x =
  let rpt_bnd l = head l : l ++ [last l]
      adj4 l = case l of {p:q:r:s:_ -> (p,q,r,s) : adj4 (tail l) ; _ -> []}
      p4 = adj4 (rpt_bnd x)
      p2 = map (\(_,i,j,_) -> (i,j)) p4
      m = map (kochanek_bartels_spline_m param) p4
  in zip p2 m

hermite_to_bezier :: Fractional n => ((n,n),(n,n)) -> (n,n,n,n)
hermite_to_bezier ((p0,p1),(m0,m1)) = (p0,p0 + (m0 / 3),p1 - (m1 / 3),p1)

kochanek_bartels_spline_b :: Fractional t => (t,t,t) -> [t] -> [(t,t,t,t)]
kochanek_bartels_spline_b param = map hermite_to_bezier . kochanek_bartels_spline_h param

{-

import Sound.SC3.Plot {- hsc3-plot -}

y = [145,60,75,55,155,0,100,25]
x k = take k [0,1 / fromIntegral k ..]
h o = kochanek_bartels_spline_h o y
f k (p,m) = map (hermite p m) (x k)
r o = concatMap (f 50) (h o)
plot_p1_ln [r (0,0,0),r (1,0,0),r (0,1,0),r (0,0,1),r (0,0,0),r (-1,0,0),r (0,-1,0),r (0,0,-1)]

-}
