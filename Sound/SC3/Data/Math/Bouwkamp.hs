module Sound.SC3.Data.Math.Bouwkamp where

import Control.Monad {- base -}
import Data.Colour.SRGB {- colour -}
import Data.List {- base -}
import System.Exit {- base -}
import System.Process {- process -}
import Text.Printf {- base -}

import qualified Language.Dot as D {- language-dot -}
import qualified Text.ParserCombinators.Parsec as P {- parsec -}

import qualified Data.CG.Minus as CG {- hcg-minus -}
import qualified Data.CG.Minus.Colour.RYB as RYB {- hcg-minus -}
import qualified Data.CG.Minus.Picture as CG {- hcg-minus -}
import qualified Render.CG.Minus.Picture as CG {- hcg-minus-cairo -}
import qualified Music.Theory.List as T {- hmt -}

type Pt = (Int,Int)
type Sz = Int

-- | (upper-left,size)
type Sq = (Pt,Sz)

sq_lower_left :: Sq -> Pt
sq_lower_left ((x,y),sz) = (x,y + sz)

sq_lower_right :: Sq -> Pt
sq_lower_right ((x,y),sz) = (x + sz,y + sz)

sq_corners_cw :: Sq -> [Pt]
sq_corners_cw ((x,y),sz) = [(x,y),(x + sz,y),(x + sz,y + sz),(x,y + sz)]

sq_ln :: Sq -> [CG.Ln Int]
sq_ln sq =
    let f (x,y) = CG.Pt (fromIntegral x) (fromIntegral y)
        [p0,p1,p2,p3] = map f (sq_corners_cw sq)
    in zipWith CG.Ln [p0,p1,p2,p3] [p1,p2,p3,p0]

sq_ul_lr :: Sq -> (Pt,Pt)
sq_ul_lr sq =
    let ((x0,y0),_) = sq
        (x1,y1) = sq_lower_right sq
    in ((x0,y0),(x1,y1))

-- | Does 'Pt' lie within 'Sq', inclusive of upper & left, exclusive of lower & right.
sq_contains_xlr :: Sq -> Pt -> Bool
sq_contains_xlr sq (x,y) =
    let ((x0,y0),(x1,y1)) = sq_ul_lr sq
    in x >= x0 && x < x1 && y >= y0 && y < y1

-- | Does 'Pt' lie upon an edge of 'Sq'.
sq_on_edge :: Sq -> Pt -> Bool
sq_on_edge sq (x,y) =
    let ((x0,y0),(x1,y1)) = sq_ul_lr sq
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
            filter (\p -> not (any id (map (\e -> sq_contains_xlr e p) sq))) .
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

-- * PDF

to_pt :: Int -> Pt -> CG.Pt Double
to_pt h (x,y) = CG.Pt (fromIntegral x) (fromIntegral (h - y))

gen_poly :: Int -> [Sq] -> [[CG.Pt Double]]
gen_poly h = let f = map (to_pt h) . sq_corners_cw in map f

gen_clr :: Int -> [CG.Ca]
gen_clr = map (\(r,g,b) -> CG.rgba_to_ca (r,g,b,1)) . drop 2 . RYB.rgb_colour_gen . (+ 2)

gen_pdf :: Maybe [CG.Ca] -> Int -> String -> [Sq] -> IO ExitCode
gen_pdf m_clr sz nm sq = do
  let p = gen_poly sz sq
      black_pen = CG.Pen 0.1 (CG.rgba_to_ca (0,0,0,1)) CG.no_dash
      i = case m_clr of
            Just clr_seq -> zipWith (CG.polygon_f) clr_seq p
            Nothing -> map (CG.polygon_l black_pen) p
      fn ty = nm ++ "." ++ ty
  CG.picture_to_pdf 4 (fn "pdf") i
  pdf_to_svg (fn "pdf") (fn "svg")

eps_to_pdf :: String -> String -> IO ExitCode
eps_to_pdf i o = rawSystem "ps2pdf" ["-dEPSCrop",i,o]

pdf_to_svg :: String -> String -> IO ExitCode
pdf_to_svg i o = rawSystem "pdf2svg" [i,o]

eps_to_svg :: String -> String -> String -> IO ExitCode
eps_to_svg i o1 o2 = eps_to_pdf i o1 >> pdf_to_svg o1 o2

-- * CSV

ln_entry :: Show a => CG.Ln a -> String
ln_entry ln =
    let ((x0,y0),(x1,y1)) = CG.ln_elem ln
    in intercalate "," (map show [x0,y0,x1,y1])

gen_csv :: FilePath -> [Sq] -> IO ()
gen_csv nm = writeFile nm . unlines . concatMap (map ln_entry . sq_ln)

-- * Type

type Bouwkamp_Code = (Int, Int, Int, [[Int]])

bc_to_sq :: Bouwkamp_Code -> [Sq]
bc_to_sq (_,_,_,sz) = place_square sz

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

-- * Analysis

bc_vertices :: [Sq] -> [Pt]
bc_vertices = nub . sort . concatMap sq_corners_cw

bc_pt_connects :: Pt -> [Sq] -> [Sq]
bc_pt_connects pt =
    let f sq = sq_on_edge sq pt
    in filter f

-- > let r = [[],[],[('a','b')],[('a','b'),('a','c'),('b','c')]]
-- > in map all_pairs_asc ["","a","ab","abc"] == r
all_pairs_asc :: Ord t => [t] -> [(t,t)]
all_pairs_asc l = [(p,q) | p <- l, q <- l, p < q]

bc_connection_graph :: [Sq] -> [((Sq,Sq),[Pt])]
bc_connection_graph sq =
    let v = bc_vertices sq
        f pt = bc_pt_connects pt sq
        g (pt,ls) = map (\(p,q) -> ((p,q),pt)) (all_pairs_asc ls)
        e = sort (concatMap g (zip v (map f v)))
    in T.collate_adjacent e

-- * Dot

gen_hex_clr :: Int -> [String]
gen_hex_clr = map sRGB24show . drop 2 . RYB.colour_gen . (+ 2)

dot_attr_str :: String -> String -> D.Attribute
dot_attr_str k v = D.AttributeSetValue (D.NameId k) (D.StringId v)

str_to_node_id :: String -> D.NodeId
str_to_node_id k = D.NodeId (D.NameId k) Nothing

dot_node :: String -> [D.Attribute] -> D.Statement
dot_node k = D.NodeStatement (str_to_node_id k)

dot_uedge :: String -> String -> [D.Attribute] -> D.Statement
dot_uedge p q =
    let p' = D.ENodeId D.NoEdge (str_to_node_id p)
        q' = D.ENodeId D.UndirectedEdge (str_to_node_id q)
    in D.EdgeStatement [p',q']

bc_connection_graph_dot :: Bool -> [Sq] -> ([D.Statement],[D.Statement])
bc_connection_graph_dot opt sq_set =
    let sq_nm,sq_txt :: Sq -> String
        sq_nm ((x,y),sz) = printf "sq_%d_%d_%d" x y sz
        sq_txt (pt,sz) = if opt then printf "%s□%d" (pt_pp pt) sz else show sz
        pt_pp :: Pt -> String
        pt_pp (x,y) = printf "%d,%d" x y
        clr_tbl = zip sq_set (gen_hex_clr (length sq_set))
        n_pp sq = dot_node (sq_nm sq) [dot_attr_str "label" (sq_txt sq)
                                      ,dot_attr_str "style" "filled"
                                      ,dot_attr_str "fillcolor" (T.lookup_err sq clr_tbl)]
        embrace s = "{" ++ s ++ "}"
        pt_set_pp = if opt then embrace . intercalate "∘" . map pt_pp else const ""
        e_pp ((p,q),e) = dot_uedge (sq_nm p) (sq_nm q) [dot_attr_str "label" (pt_set_pp e)]
    in (map n_pp sq_set,map e_pp (bc_connection_graph sq_set))

dot_ugraph :: [D.Statement] -> D.Graph
dot_ugraph = D.Graph D.StrictGraph D.UndirectedGraph Nothing

dot_graph_attr :: [D.Attribute] -> D.Statement
dot_graph_attr = D.AttributeStatement D.GraphAttributeStatement

dot_node_attr :: [D.Attribute] -> D.Statement
dot_node_attr = D.AttributeStatement D.NodeAttributeStatement

bc_connection_graph_dot_wr :: Bool -> [D.Statement] -> FilePath -> [Sq] -> IO ()
bc_connection_graph_dot_wr opt x fn sq = do
  let (n,e) = bc_connection_graph_dot opt sq
  writeFile fn (D.renderDot (dot_ugraph (x ++ n ++ e)))

