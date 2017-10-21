module Sound.SC3.Data.Math.Bouwkamp where

import Control.Monad {- base -}
import Data.List {- base -}
import System.Exit {- base -}
import System.Process {- process -}

import qualified Text.ParserCombinators.Parsec as P {- parsec -}

import qualified Data.CG.Minus as CG {- hcg-minus -}
import qualified Data.CG.Minus.Colour.RYB as RYB {- hcg-minus -}
import qualified Graphics.PS as PS {- hps -}

type Pt = (Int,Int)
type Sz = Int

-- | (upper-left,size)
type Sq = (Pt,Sz)

sq_lower_left :: Sq -> Pt
sq_lower_left ((x,y),sz) = (x,y + sz)

sq_lower_right :: Sq -> Pt
sq_lower_right ((x,y),sz) = (x + sz,y + sz)

sq_corners_cw :: Sq -> [Pt]
sq_corners_cw ((x,y),z) = [(x,y),(x + z,y),(x + z,y + z),(x,y +z)]

sq_ln :: Sq -> [CG.Ln Int]
sq_ln sq =
    let f (x,y) = CG.Pt (fromIntegral x) (fromIntegral y)
        [p0,p1,p2,p3] = map f (sq_corners_cw sq)
    in zipWith CG.Ln [p0,p1,p2,p3] [p1,p2,p3,p0]

sq_contains :: Sq -> Pt -> Bool
sq_contains sq (x,y) =
    let ((x0,y0),_) = sq
        (x1,y1) = sq_lower_right sq
    in x >= x0 && x < x1 && y >= y0 && y < y1

sq_on_edge :: Sq -> Pt -> Bool
sq_on_edge sq (x,y) =
    let ((x0,y0),_) = sq
        (x1,y1) = sq_lower_right sq
    in ((x == x0 || x == x1) && (y >= y0 && y < y1)) ||
       ((y == y0 || y == y1) && (x >= x0 && x < x1))

uppermost_leftmost :: Pt -> Pt -> Ordering
uppermost_leftmost (x0,y0) (x1,y1) =
    case compare y0 y1 of
      EQ -> compare x0 x1
      r -> r

-- > next_slot (place_row (0,0) [50,35,27]) == (85,27)
next_slot :: [Sq] -> Pt
next_slot sq =
    let f = minimumBy uppermost_leftmost .
            filter (\p -> not (any id (map (\e -> sq_contains e p) sq))) .
            map sq_lower_left
    in f sq

-- > place_row (85,27) [8,19] == [((85,27),8),((93,27),19)]
place_row :: Pt -> [Sz] -> [Sq]
place_row (x,y) r =
    case r of
      [] -> []
      r0:r' -> ((x,y),r0) : place_row (x + r0,y) r'

place_square_f :: [Sq] -> Pt -> [[Sz]] -> [Sq]
place_square_f pl pt sq =
    case sq of
      [] -> pl
      sq0:sq' -> let pl' = pl ++ place_row pt sq0
                     pt' = next_slot pl'
                 in place_square_f pl' pt' sq'

-- > place_square sq_21_112
place_square :: [[Sz]] -> [Sq]
place_square = place_square_f [] (0,0)

-- * ASCII

sq_ascii :: (Int,Int) -> [Sq] -> [String]
sq_ascii (w,h) sq =
    let f r c = if any (\e -> sq_on_edge e (c,r)) sq then '.' else ' '
        g r = map (f r) [0 .. w]
    in map g [0 .. h]

-- * PS

to_pt :: Int -> Pt -> CG.Pt Double
to_pt h (x,y) = CG.Pt (fromIntegral x) (fromIntegral (h - y))

gen_poly :: Int -> [Sq] -> [PS.Path]
gen_poly h = let wr = PS.polygon . map (to_pt h) . sq_corners_cw in map wr

gen_clr :: Int -> [PS.GS]
gen_clr = map (\(r,g,b) -> PS.defaultGS (PS.RGB r g b)) . drop 2 . RYB.rgb_colour_gen . (+ 2)

gen_eps :: Maybe [PS.GS] -> Int -> String -> [Sq] -> IO ExitCode
gen_eps m_clr sz nm sq = do
  let p = gen_poly sz sq
      gs = PS.greyGS 0.0
      gs' = gs {PS.gs_line_width = 0.1}
      i = case m_clr of
            Just clr -> PS.translate 4 4 $ foldl1 PS.Over $ zipWith PS.Fill clr $ p
            Nothing -> PS.translate 4 4 $ PS.Stroke gs' $ foldl1 PS.Join $ p
      fn ty = nm ++ "." ++ ty
  PS.eps (fn "eps") (PS.BBox 0 0 (sz + 8) (sz + 8)) i
  eps_to_svg (fn "eps") (fn "pdf") (fn "svg")

eps_to_pdf :: String -> String -> IO ExitCode
eps_to_pdf i o = rawSystem "ps2pdf" ["-dEPSCrop",i,o]

pdf_to_svg :: String -> String -> IO ExitCode
pdf_to_svg i o = rawSystem "pdf2svg" [i,o]

eps_to_svg :: String -> String -> String -> IO ExitCode
eps_to_svg i o1 o2 = eps_to_pdf i o1 >> pdf_to_svg o1 o2

-- * CSV

ln_entry :: Show a => CG.Ln a -> String
ln_entry ln =
    let ((x0,y0),(x1,y1)) = CG.ln_pt' ln
    in intercalate "," (map show [x0,y0,x1,y1])

gen_csv :: FilePath -> [Sq] -> IO ()
gen_csv nm = writeFile nm . unlines . concatMap (map ln_entry . sq_ln)

-- * Type

type Bouwkamp_Code = (Int, Int, Int, [[Int]])

-- * Parser

type P a = P.GenParser Char () a

p_comma :: P Char
p_comma = P.char ','

p_int :: P Int
p_int = liftM read (P.many1 P.digit)

p_int_list :: P [Int]
p_int_list = P.sepEndBy1 p_int p_comma

p_int_paren_list :: P [Int]
p_int_paren_list = do
  _ <- P.char '('
  r <- p_int_list
  _ <- P.char ')'
  return r

p_space :: P Char
p_space = P.char ' '

p_bouwkamp :: P Bouwkamp_Code
p_bouwkamp = do
  n <- p_int
  _ <- p_space
  w <- p_int
  _ <- p_space
  h <- p_int
  _ <- p_space
  l <- P.many1 p_int_paren_list
  return (n,w,h,l)

bouwkamp_parse_err :: String -> Bouwkamp_Code
bouwkamp_parse_err s =
    case P.parse p_bouwkamp "p_bouwkamp" s of
      Left err -> error (show err)
      Right r -> r
