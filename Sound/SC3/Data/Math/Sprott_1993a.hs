-- | <http://sprott.physics.wisc.edu/sa.htm>
module Sound.SC3.Data.Math.Sprott_1993a where

import Data.Maybe {- base -}

-- * Coding Table

-- | Table 2-1. ASCII character set and associated coefficient values (p.28)
sprott_tbl_2_1 :: [(Char,Int,Double)]
sprott_tbl_2_1 =
    let i = [32 .. 127]
        n = map (/ 10) [-45,-44 .. 50]
        c = map toEnum i
    in zip3 c i n

-- | Lookup coeffient at 'sprott_tbl_2_1'.
--
-- > sprott_coef 'M' == Just 0
sprott_coef :: Char -> Maybe Double
sprott_coef c =
    let (ch,_,cf) = unzip3 sprott_tbl_2_1
    in lookup c (zip ch cf)

-- | 'fromJust' of 'sprott_coef'.
sprott_coef_err :: Char -> Double
sprott_coef_err = fromJust . sprott_coef

-- * General forms

-- | Pair each elements with the element /n/ places further along.
--
-- > with_delayed 3 [1..9] == [(1,4),(2,5),(3,6),(4,7),(5,8),(6,9)]
with_delayed :: Int -> [t] -> [(t,t)]
with_delayed n l =
    case l of
      [] -> []
      e0:l' -> case drop n l of
                 [] -> []
                 e1:_ -> (e0,e1) : with_delayed n l'

-- | /n/ '*' /n/.
square :: Num a => a -> a
square x = x * x

-- | General one-dimensional quadratic iterated map (Equation 2A, p.25)
quadratic_1 :: Num a => a -> a -> a -> a -> a
quadratic_1 a1 a2 a3 x = a1 + a2 * x + a3 * square x

-- | List (/l/) variant of 'quadratic_1'.
quadratic_1l :: Num a => [a] -> a -> a
quadratic_1l l =
    case l of
      [a1,a2,a3] -> quadratic_1 a1 a2 a3
      _ -> error "quadratic_1l"

-- | General one-dimensional quintic iterated map (Equation 2E, p.41)
quintic_1 :: Num a => a -> a -> a -> a -> a -> a -> a -> a
quintic_1 a1 a2 a3 a4 a5 a6 x =
    let f :: Num a => Integer -> a -> a
        f k n = if k == 1 then n else n * f (k - 1) n
    in a1 + a2 * x + a3 * f 2 x + a4 * f 3 x + a5 * f 4 x + a6 * f 5 x

-- | List (/l/) variant of 'quintic_1'.
quintic_1l :: Num a => [a] -> a -> a
quintic_1l l =
    case l of
      [a1,a2,a3,a4,a5,a6] -> quintic_1 a1 a2 a3 a4 a5 a6
      _ -> error "quintic_1l"

-- | General two-dimensional iterated quadratic map (Equation 3B, p.53)
quadratic_2 :: Num t => t->t->t->t->t->t->t->t->t->t->t->t->(t,t)->(t,t)
quadratic_2 a1 a2 a3 a4 a5 a6 a7 a8 a9 aA aB aC (x,y) =
    let sq n = n * n
        x' = a1 + a2 * x + a3 * sq x + a4 * x * y + a5 * y + a6 * sq y
        y' = a7 + a8 * x + a9 * sq x + aA * x * y + aB * y + aC * sq y
    in (x',y')

-- | List (/l/) variant of 'quadratic_2'.
quadratic_2l :: Num t => [t] -> (t,t) -> (t,t)
quadratic_2l l =
    case l of
      [a1,a2,a3,a4,a5,a6,a7,a8,a9,aA,aB,aC] ->
        quadratic_2 a1 a2 a3 a4 a5 a6 a7 a8 a9 aA aB aC
      _ -> error "quadratic_2l"

-- | General two-dimensional iterated cubic map (Equation 3F, p.80)
cubic_2 :: Num t => t->t->t->t->t->t->t->t->t->t->t->t->t->t->t->t->t->t->t->t->(t,t)->(t,t)
cubic_2 a1 a2 a3 a4 a5 a6 a7 a8 a9 aA aB aC aD aE aF aG aH aI aJ aK (x,y) =
    let sq n = n * n
        cb n = n * n * n
        x' = a1 + a2 * x + a3 * sq x + a4 * cb x + a5 * sq x * y +
             a6 * x * y + a7 * x * sq y + a8 * y + a9 * sq y + aA * cb y
        y' = aB + aC * x + aD * sq x + aE * cb x + aF * sq x * y +
             aG * x * y + aH * x * sq y + aI * y + aJ * sq y + aK * cb y
    in (x',y')

-- | List (/l/) variant of 'cubic_2'.
cubic_2l :: Num t => [t] -> (t,t) -> (t,t)
cubic_2l l =
    case l of
      [a1,a2,a3,a4,a5,a6,a7,a8,a9,aA,aB,aC,aD,aE,aF,aG,aH,aI,aJ,aK] ->
        cubic_2 a1 a2 a3 a4 a5 a6 a7 a8 a9 aA aB aC aD aE aF aG aH aI aJ aK
      _ -> error "cubic_2l"


-- | General three-dimensional iterated quadratic map (Equation 4A, p.147)
quadratic_3 :: Num t => t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> (t, t, t) -> (t, t, t)
quadratic_3 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10
            a11 a12 a13 a14 a15 a16 a17 a18 a19 a20
            a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 (x,y,z) =
    let sq n = n * n
        x' = a1 + a2 * x + a3 * sq x + a4 * x * y + a5 * x * z +
             a6 * y + a7 * sq y + a8 * y * z + a9 * z + a10 * sq z
        y' = a11 + a12 * x + a13 * sq x + a14 * x * y + a15 * x * z +
             a16 * y + a17 * sq y + a18 * y * z + a19 * z + a20 * sq z
        z' = a21 + a22 * x + a23 * sq x + a24 * x * y + a25 * x * z +
             a26 * y + a27 * sq y + a28 * y * z + a29 * z + a30 * sq z
    in (x',y',z')

-- | List (/l/) variant of 'quadratic_3'.
quadratic_3l :: Num t => [t] -> (t, t, t) -> (t, t, t)
quadratic_3l l =
    case l of
      [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,
       a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,
       a21,a22,a23,a24,a25,a26,a27,a28,a29,a30] ->
        quadratic_3 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10
                a11 a12 a13 a14 a15 a16 a17 a18 a19 a20
                a21 a22 a23 a24 a25 a26 a27 a28 a29 a30
      _ -> error "quadratic_3l"

-- * Projections

-- | 'minimum' and 'maximum'.
minmax :: Ord t => [t] -> (t,t)
minmax l = (minimum l,maximum l)

-- | Projection onto sphere (Equation 3G, p.107)
sphere_proj :: (Floating t,Ord t) => [(t,t)] -> [(t,t)]
sphere_proj l =
    let (xs,ys) = unzip l
        (x0,x1) = minmax xs
        (y0,y1) = minmax ys
        xa = (x0 + x1) / 2
        ya = (y0 + y1) / 2
        tt = pi / (x1 - x0)
        pt = pi / (y1 - y0)
        prj (x,y) = let th = tt * (x1 - x)
                        ph = pt * (y1 - y)
                    in (xa + 0.36 * (x1 - x0) * cos th * sin ph
                       ,ya + 0.5 * (y1 - y0) * cos ph)
    in map prj l

-- * Co-efficient CODES

type Code = String
type Annotation = String

{-| One-dimensional codes, in /quintic/ form.

> import Sound.SC3.Plot {- hsc3-plot -}

> let code = map sprott_coef_err (fst (quintic_1_codes !! 4))
> in plot_p2_pt [with_delayed 5 (take 12500 (iterate (quintic_1l code) 0.1))]

-}
quintic_1_codes :: [(Code,Annotation)]
quintic_1_codes =
    [("Mu%MMM","Fig 1-4")
    ,("XBHMMM","Fig 2-1")
    ,("BDUMMM","Fig 2-2")
    ,("CAVMMM","Fig 2-3")
    ,("XDAMMM","Fig 2-4")
    ,("ZEZKMM","Fig 2-5")
    ,("BLCTXM","Fig 2-6")
    ,("UTXJEM","Fig 2-7")
    ,("BOGIZI","Fig 2-8")
    ,("FBIEVV","Fig 2-9")
    ,("OOYRIL","Fig 2-10")]


{-| Two-dimensional codes, in /quadratic/ form.

> import Sound.SC3.Plot {- hsc3-plot -}

> let code = map sprott_coef_err (fst (quadratic_2_codes !! 16))
> in plot_p2_pt [take 25000 (iterate (quadratic_2l code) (0.1,0))]

-}
quadratic_2_codes :: [(Code,Annotation)]
quadratic_2_codes =
    [("WM?MPMMWMMMM","Fig 3-1")
    ,("AGHNFODVNJCP","Fig 3-2")
    ,("BCQAFMFVPXKQ","Fig 3-3")
    ,("DSYUECINGQNV","Fig 3-4")
    ,("ELXAPXMPQOBT","Fig 3-5")
    ,("EYYMKTUMXUVC","Fig 3-6")
    ,("JTTSMBOGLLQF","Fig 3-7")
    ,("NNMJRCTVVTYG","Fig 3-8")
    ,("OUGFJKDHSAJU","Fig 3-9")
    ,("QKOCSIDVTPGY","Fig 3-10")
    ,("QLOIARXYGHAJ","Fig 3-11")
    ,("TJUBWEDNRORR","Fig 3-12")
    ,("TSILUNDQSIFA","Fig 3-13")
    ,("UEBJLCDISIIQ","Fig 3-14")
    ,("VDUOTLRBKTJD","Fig 3-15")
    ,("WLKWPSMOGIGS","Fig 3-16")
    ,("ZPMSGCNFRENG","Fig 3-17")
    ,("MVWMGCMaMaRM","Fig 8-6 (Tinkerbell)")
    ,("AEUBNVIAHERQ","SELECTED.DIC #1 (p. 583)")
    ,("AHSVIGTJKOTB","SELECTED.DIC #1")]

-- > let cf = map sprott_coef_err "IRPGVTFIDGCSXMFPKIDJ"
-- > let cf = map sprott_coef_err "ISMHQCHPDFKFBKEALIFD"
-- > let cf = map sprott_coef_err "JYCBMNFNYOEPYUGHHESU"
-- > let cf = map sprott_coef_err "NUYLCURDUHQUQMRZQWQB"
-- > plot_p2_pt [take 25000 (iterate (cubic_2l cf) (0.1,0))]

-- > let cf = map sprott_coef_err "FUXRRRUIRDYKDUBPHHHOMOBRIRBINCS"
-- > plot_p2_pt [take 25000 (iterate (quartic_2l cf) (0.1,0))]

-- > let cf = map sprott_coef_err "CSRKVVQLGFFS"
-- > let cf = map sprott_coef_err "CVQKGHQTPHTE"
-- > let cf = map sprott_coef_err "KPNERVOTBYCM"
-- > let cf = map sprott_coef_err "UWACXDQIGKHF"
-- > plotPoints [sphere_proj (take 25000 (iterate (quadratic_2l cf) (0.1,0)))]

-- > let cf = map sprott_coef_err "CMMMEWHXRMMM"
-- > plotPoints [take 25000 (iterate (quadratic_2l cf) (0.1,0))]

-- > let cf = map sprott_coef_err "JKRADSXGDBHIJTQJJDICEJKYSTXFNU"
-- > let cf = map sprott_coef_err "LURCEGOHOIQFJKBSNYGSNRUKKIKIHW"
-- > let cf = map sprott_coef_err "NRRXLCEYLFHYAPFSTPHHJMYRYJFBNM"
-- > let cf = map sprott_coef_err "WDWOGDGWGORJOBTUHFQBPRNTCBYQHP"
-- > plot_p3_pt [take 15000 (iterate (quadratic_3l cf) (0.1,0,0))]

-- * Specialised forms

-- | Sprott p.9 (Equation 1C)
--
-- > import Sound.SC3.Plot {- hsc3-plot -}
-- > plot_p2_pt [with_delayed 1 (take 5000 (iterate (logistic 4) 0.05))]
logistic :: Num a => a -> a -> a
logistic r x = r * x * (1 - x)

-- | Figure 3-1. The Hénon map (p.52)
--
-- > plot_p2_pt [take 5000 (iterate (henon (-1.4) 0.3) (0.1,0))]
henon :: Num t => t -> t -> (t, t) -> (t, t)
henon a b (x,y) = (1 + a * square x + b * y, x)
