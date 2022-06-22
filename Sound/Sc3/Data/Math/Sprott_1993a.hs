-- | Julien C. Sprott "Strange Attractors: Creating Patterns in Chaos" 1993
--   <http://sprott.physics.wisc.edu/sa.htm>
module Sound.Sc3.Data.Math.Sprott_1993a where

import Data.Maybe {- base -}

import Sound.Sc3.Plot {- hsc3-plot -}

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

-- | General one-dimensional quadratic iterated map (Equation 2A, p.25)
quadratic_1 :: Num a => a -> a -> a -> a -> a
quadratic_1 a1 a2 a3 x = a1 + a2 * x + a3 * x * x

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

-- | Generalised one-dimensional iterated map.
general_1l :: Num a => [a] -> Maybe (a -> a)
general_1l l =
     case length l of
       3 -> Just (quadratic_1l l)
       4 -> Just (quintic_1l (l ++ [0,0]))
       5 -> Just (quintic_1l (l ++ [0]))
       6 -> Just (quintic_1l l)
       _ -> Nothing

-- | General two-dimensional iterated quadratic map (Equation 3B, p.53)
quadratic_2 :: Num t => t->t->t->t->t->t->t->t->t->t->t->t->(t,t)->(t,t)
quadratic_2 a1 a2 a3 a4 a5 a6 a7 a8 a9 aA aB aC (x,y) =
    let x' = a1 + a2 * x + a3 * x * x + a4 * x * y + a5 * y + a6 * y * y
        y' = a7 + a8 * x + a9 * x * x + aA * x * y + aB * y + aC * y * y
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


-- | Generalised two-dimensional iterated map.
general_2l :: Num t => [t] -> Maybe ((t,t) -> (t,t))
general_2l l =
    case length l of
      12 -> Just (quadratic_2l l)
      20 -> Just (cubic_2l l)
      _ -> Nothing

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

-- | Generalised three-dimensional iterated map.
general_3l :: Num t => [t] -> Maybe ((t,t,t) -> (t,t,t))
general_3l l =
    case length l of
      30 -> Just (quadratic_3l l)
      _ -> Nothing

-- * Projections

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

-- * CALC

-- | Calculate one-dimensional code, /m/ is delay, /n/ is iteration degree, /i/ is initial value.
calc_code_1 :: Int -> Int -> Double -> Code -> [(Double, Double)]
calc_code_1 m n i c =
    case c of
      c0:c' -> if c0 `elem` "ABCD"
               then case general_1l (map sprott_coef_err c') of
                      Just f -> with_delayed m (take n (iterate f i))
                      Nothing -> error "calc_code_1: ill-formed coef"
               else error "calc_code_1: not type {A,B,C,D}"
      _ -> error "calc_code_1: ill-formed code"

-- | Calculate two-dimensional code, /n/ is iteration degree, /i/ is initial value.
calc_code_2 :: Int -> (Double, Double) -> Code -> [(Double, Double)]
calc_code_2 n i c =
    case c of
      c0:c' -> if c0 `elem` "EF"
               then case general_2l (map sprott_coef_err c') of
                      Just f -> take n (iterate f i)
                      Nothing -> error "calc_code_2: ill-formed coef"
               else error "calc_code_2: not type {E,F}"
      _ -> error "calc_code_2: ill-formed code"

-- | Plot three-dimensional code, /n/ is iteration degree, /i/ is initial value.
calc_code_3 :: Int -> (Double,Double,Double) -> Code -> [(Double, Double, Double)]
calc_code_3 n i c =
    case c of
      c0:c' -> if c0 `elem` "I"
               then case general_3l (map sprott_coef_err c') of
                      Just f -> take n (iterate f i)
                      Nothing -> error "calc_code_3: ill-formed coef"
               else error "calc_code_3: not type {I}"
      _ -> error "calc_code_3: ill-formed code"

-- * Code Plotting

-- | 'plot_p2_pt' of 'calc_code_1'
plot_code_1 :: Int -> Int -> Double -> (Code,Annotation) -> IO ()
plot_code_1 m n i (c,_) = plot_p2_pt [calc_code_1 m n i c]

-- | 'plot_p2_pt' of 'calc_code_2'.  If /sph/ run 'sphere_proj'.
plot_code_2 :: Bool -> Int -> (Double,Double) -> (Code,Annotation) -> IO ()
plot_code_2 sph n i (c,_) =
  let prj = if sph then sphere_proj else id
  in plot_p2_pt [prj (calc_code_2 n i c)]

-- | 'plot_p3_pt' of 'calc_code_3'
plot_code_3 :: Int -> (Double,Double,Double) -> (Code,Annotation) -> IO ()
plot_code_3 n i (c,_) = plot_p3_pt [calc_code_3 n i c]

-- * Co-efficient codes.

-- | A code is a string.  The set of book codes are at:
-- <http://sprott.physics.wisc.edu/fractals/bookdisk/BOOKFIGS.DIC> and
-- there is a further set of codes at:
-- <http://sprott.physics.wisc.edu/fractals/bookdisk/SELECTED.DIC>
type Code = String
type Annotation = String

-- | One-dimensional codes.
--
-- > plot_code_1 5 12500 0.1 (codes_1 !! 0)
codes_1 :: [(Code,Annotation)]
codes_1 =
    [("AMu%","Fig 1-4")
    ,("AXBH","Fig 2-1")
    ,("ABDU","Fig 2-2")
    ,("ACAV","Fig 2-3")
    ,("AXDA","Fig 2-4")
    ,("BZEZK","Fig 2-5")
    ,("CBLCTX","Fig 2-6")
    ,("CUTXJE","Fig 2-7")
    ,("DBOGIZI","Fig 2-8")
    ,("DFBIEVV","Fig 2-9")
    ,("DOOYRIL","Fig 2-10")]

-- | Two-dimensional codes.
--
-- > plot_code_2 False 8500 (0.1,0) (codes_2 !! 25)
codes_2 :: [(Code,Annotation)]
codes_2 =
    [("EWM?MPMMWMMMM","Fig 3-1") -- 0
    ,("EAGHNFODVNJCP","Fig 3-2")
    ,("EBCQAFMFVPXKQ","Fig 3-3")
    ,("EDSYUECINGQNV","Fig 3-4")
    ,("EELXAPXMPQOBT","Fig 3-5")
    ,("EEYYMKTUMXUVC","Fig 3-6")
    ,("EJTTSMBOGLLQF","Fig 3-7")
    ,("ENNMJRCTVVTYG","Fig 3-8")
    ,("EOUGFJKDHSAJU","Fig 3-9")
    ,("EQKOCSIDVTPGY","Fig 3-10")
    ,("EQLOIARXYGHAJ","Fig 3-11") -- 10
    ,("ETJUBWEDNRORR","Fig 3-12")
    ,("ETSILUNDQSIFA","Fig 3-13")
    ,("EUEBJLCDISIIQ","Fig 3-14")
    ,("EVDUOTLRBKTJD","Fig 3-15")
    ,("EWLKWPSMOGIGS","Fig 3-16")
    ,("EZPMSGCNFRENG","Fig 3-17")
    ,("FIRPGVTFIDGCSXMFPKIDJ","Fig 3-18")
    ,("FISMHQCHPDFKFBKEALIFD","Fig 3-19")
    ,("FJYCBMNFNYOEPYUGHHESU","Fig 3-20")
    ,("FNUYLCURDUHQUQMRZQWQB","Fig 3-24") -- 20
    ,("ECSRKVVQLGFFS","Fig 3-42")
    ,("ECVQKGHQTPHTE","Fig 3-43")
    ,("EKPNERVOTBYCM","Fig 3-44")
    ,("EUWACXDQIGKHF","Fig 3-45")
    ,("ECMMMEWHXRMMM","Fig 3-58")
    ,("EMVWMGCMaMaRM","Fig 8-6 (Tinkerbell)")
    ,("EAEUBNVIAHERQ","SELECTED.DIC #1 (p. 583)")
    ,("EAHSVIGTJKOTB","SELECTED.DIC #1")
    ]

-- | Three-dimensional codes.
--
-- > plot_code_3 8500 (0.1,0,0) (codes_3 !! 3)
codes_3 :: [(Code,Annotation)]
codes_3 =
    [("IJKRADSXGDBHIJTQJJDICEJKYSTXFNU","Fig 4-1")
    ,("ILURCEGOHOIQFJKBSNYGSNRUKKIKIHW","Fig 4-2")
    ,("INRRXLCEYLFHYAPFSTPHHJMYRYJFBNM","Fig 4-4")
    ,("IWDWOGDGWGORJOBTUHFQBPRNTCBYQHP","Fig 4-8")]

-- > cf = map sprott_coef_err "FUXRRRUIRDYKDUBPHHHOMOBRIRBINCS"
-- > plot_p2_pt [take 25000 (iterate (quartic_2l cf) (0.1,0))]

-- * Specialised forms

-- | Sprott p.9 (Equation 1C)
--
-- > plot_p2_pt [with_delayed 1 (take 5000 (iterate (logistic 4) 0.05))]
logistic :: Num a => a -> a -> a
logistic r x = r * x * (1 - x)

-- | Figure 3-1. The Hénon map (p.52)
--
-- > plot_p2_pt [take 5000 (iterate (henon (-1.4) 0.3) (0.1,0))]
henon :: Num t => t -> t -> (t, t) -> (t, t)
henon a b (x,y) = (1 + a * x * x + b * y, x)
