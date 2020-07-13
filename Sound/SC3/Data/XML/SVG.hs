-- | Load linearised path data from SVG files.
module Sound.SC3.Data.XML.SVG where

import Data.List {- base -}
import Data.Maybe {- base -}
import qualified System.IO.Unsafe as Unsafe {- base -}
import qualified Text.XML.Light as X {- xml -}

import qualified Graphics.SVG.ReadPath as P {- SVGPath -}

import Data.CG.Minus.Plain {- hcg-minus -}
import Data.CG.Minus.Types {- hcg-minus -}
import Data.CG.Minus.Core {- hcg-minus -}

import qualified Music.Theory.Tuple as T {- hmt -}

import qualified Sound.SC3.Plot as Plot {- hsc3-plot -}

import qualified Sound.SC3.Data.XML as XML {- hsc3-data -}

-- | Real number, synonym for 'Double'.
type R = Double

-- | Make 'X.QName' with @svg@ 'X.qURI'.
--
-- > svg_name "path"
svg_name :: String -> X.QName
svg_name nm =
    X.blank_name
         {X.qName = nm
         ,X.qURI = Just "http://www.w3.org/2000/svg"}

svg_load :: FilePath -> IO X.Element
svg_load fn = do
  txt <- readFile fn
  case X.parseXMLDoc txt of
    Nothing -> error "svg_load"
    Just e -> return e

-- | svg:viewBox
svg_viewbox :: X.Element -> V4 Double
svg_viewbox = T.t4_from_list . map read . words . XML.x_get_attr "viewBox"

-- | svg:width
svg_width :: X.Element -> Double
svg_width = read . XML.x_get_attr "width"

-- | svg:height
svg_height :: X.Element -> Double
svg_height = read . XML.x_get_attr "height"

-- | Line elements that are direct children of e.
svg_line_elem :: X.Element -> [X.Element]
svg_line_elem e = X.findElements (svg_name "line") e

svg_line_coord :: X.Element -> V2 (V2 Double)
svg_line_coord e =
  let f nm = read (XML.x_get_attr nm e)
  in ((f "x1",f "y1"),(f "x2",f "y2"))

-- * PATH

-- | Non-IO variant of 'P.pathFromString'.
pathFromString_unsafe :: String -> Either String [P.PathCommand]
pathFromString_unsafe = Right . Unsafe.unsafePerformIO . P.pathFromString

-- | 'pathFromString_unsafe' with 'error' on failed parse.
parse_path :: String -> [P.PathCommand]
parse_path str =
    case pathFromString_unsafe str of
      Left err -> error err
      Right cmd -> cmd

-- | 'parse_path' at /path/ elements of XML 'String'.
svg_read_path_d :: String -> [[P.PathCommand]]
svg_read_path_d s =
    let p = case X.parseXMLDoc s of
              Nothing -> error "svg_read_path_d: no parse"
              Just e -> X.findElements (svg_name "path") e
        d = mapMaybe (X.findAttr (X.unqual "d")) p
    in map parse_path d

-- | 'svg_read_path_d' of 'readFile'.
svg_load_paths :: FilePath -> IO [[P.PathCommand]]
svg_load_paths = fmap svg_read_path_d . readFile

-- | 'Ls' variant of 'P.commandsToPoints'.
--   (dx,dy) is the size of a pixel and is used for rasterisation.
subpaths_to_ls :: (R,R) -> [P.PathCommand] -> [Ls R]
subpaths_to_ls (dx,dy) r =
    case P.commandsToPoints r (dx,dy) (0,0) of
      [] -> error "subpaths_to_ls: no sub-paths"
      p -> map (Ls . map mk_pt) p

-- | 'subpaths_to_ls' of 'svg_load_paths'
svg_load_ls :: (R, R) -> FilePath -> IO [Ls R]
svg_load_ls rs = fmap (concatMap (subpaths_to_ls rs)) . svg_load_paths

-- | 'plot_p2_ln' of 'pt_xy'.
plot_ls :: Plot.PNum t => [Ls t] -> IO ()
plot_ls = Plot.plot_p2_ln . map (map pt_xy . ls_elem)

{- | Convert a relative 'P.PathCommand' to the absolute form, giving also the absolute end point.

In SVG absolute commands are upper case letters and relative commands are lower case.

Commands are:
M = move-to,
Z = close (line-to-initial-point),
L = line-to,
H = horizontal-line,
V = vertical-line,
C = cubic-bézier-curve,
S = C-reflected
Q = quadratic-curve
T = Q-reflected
A = elliptic-arc

-}
pathcommand_to_abs :: (R,R) -> P.PathCommand -> ((R,R),P.PathCommand)
pathcommand_to_abs (x,y) cmd =
    case cmd of
      P.M_abs (x',y') -> let r = (x',y') in (r,P.M_abs r)
      P.M_rel (x',y') -> let r = (x+x',y+y') in (r,P.M_abs r)
      P.Z -> ((x,y),P.Z)
      P.L_abs (x',y') -> let r = (x',y') in (r,P.L_abs r)
      P.L_rel (x',y') -> let r = (x+x',y+y') in (r,P.L_abs r)
      P.H_abs x' -> let r = (x',y) in (r,P.H_abs x')
      P.H_rel x' -> let x'' = x+x' ; r = (x'',y) in (r,P.H_abs x'')
      P.V_abs y' -> let r = (x,y') in (r,P.V_abs y')
      P.V_rel y' -> let y'' = y+y' ; r = (x,y'') in (r,P.V_abs y'')
      P.C_abs (x1,y1,x2,y2,x',y') -> ((x',y'),P.C_abs (x1,y1,x2,y2,x',y'))
      P.C_rel (x1,y1,x2,y2,x',y') -> ((x+x',y+y'),P.C_abs (x+x1,y+y1,x+x2,y+y2,x+x',y+y'))
      P.S_abs (x2,y2,x',y') -> ((x',y'),P.S_abs (x2,y2,x',y'))
      P.S_rel (x2,y2,x',y') -> ((x+x',y+y'),P.S_abs (x+x2,y+y2,x+x',y+y'))
      P.Q_abs (x1,y1,x',y') -> ((x',y'),P.Q_abs (x1,y1,x',y'))
      P.Q_rel (x1,y1,x',y') -> ((x+x',y+y'),P.Q_abs (x+x1,y+y1,x+x',y+y'))
      P.T_abs (x',y') -> let r = (x',y') in (r,P.T_abs r)
      P.T_rel (x',y') -> let r = (x+x',y+y') in (r,P.T_abs r)
      P.A_abs -> ((x,y),P.A_abs) -- error "A_abs"
      P.A_rel -> ((x,y),P.A_rel) -- error "A_rel"

-- | Convert a sequence of 'P.PathCommand' to absolute form given
-- starting point.
path_to_abs :: (R,R) -> [P.PathCommand] -> ((R,R),[P.PathCommand])
path_to_abs = mapAccumL pathcommand_to_abs

-- | Extend bounds considering /only/ endpoints.
pathcommand_extend_ep_bounds :: ((R,R),(R,R)) -> P.PathCommand -> ((R,R),(R,R))
pathcommand_extend_ep_bounds b cmd =
    let ((x0,y0),(x1,y1)) = b
        ext (x,y) = ((min x x0,min y y0),(max x x1,max y y1))
    in case cmd of
      P.M_abs p -> ext p
      P.Z -> b
      P.L_abs p -> ext p
      P.H_abs x -> ext (x,y0)
      P.V_abs y -> ext (x0,y)
      P.C_abs (_,_,_,_,x,y) -> ext (x,y)
      P.S_abs (_,_,x,y) -> ext (x,y)
      P.Q_abs (_,_,x,y) -> ext (x,y)
      P.T_abs p -> ext p
      P.A_abs -> b
      _ -> error "pathcommand_extend_bounds: relative cmd"

path_ep_bounds :: ((R,R),(R,R)) -> [P.PathCommand] -> ((R,R),(R,R))
path_ep_bounds = foldl pathcommand_extend_ep_bounds

path_ep_bounds_o :: [P.PathCommand] -> ((R,R),(R,R))
path_ep_bounds_o = path_ep_bounds ((0,0),(0,0))

is_z :: P.PathCommand -> Bool
is_z cmd = case cmd of {P.Z -> True;_ -> False}

m_abs_xy :: P.PathCommand -> Maybe (R,R)
m_abs_xy cmd = case cmd of {P.M_abs (x,y) -> Just (x,y);_ -> Nothing}
