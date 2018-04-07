module Sound.SC3.Data.Math.Hermite where

-- | 4-point, 3rd-order Hermite (x-form)
hermite :: Fractional t => t -> t -> t -> t -> t -> t
hermite x y0 y1 y2 y3 =
    let c0 = y1;
        c1 = 0.5 * (y2 - y0)
        c2 = y0 - 2.5 * y1 + 2.0 * y2 - 0.5 * y3
        c3 = 1.5 * (y1 - y2) + 0.5 * (y3 - y0)
    in ((c3 * x + c2) * x + c1) * x + c0
