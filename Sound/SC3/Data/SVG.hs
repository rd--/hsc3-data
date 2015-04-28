-- | Load linearised path data from SVG files.
module Sound.SC3.Data.SVG where

import Data.List {- base -}
import Data.Maybe {- base -}
import System.IO.Unsafe {- base -}
import qualified Text.XML.Light as X {- xml -}

import Graphics.SVG.ReadPath as P {- SVGPath -}

import Data.CG.Minus {- hcg-minus -}
import Sound.SC3.Plot {- hsc3-plot -}

-- | Make 'X.QName' with @svg@ 'X.qURI'.
svg_name :: String -> X.QName
svg_name nm =
    X.blank_name
         {X.qName = nm
         ,X.qURI = Just "http://www.w3.org/2000/svg"}

pathFromString' :: String -> Either String [PathCommand]
pathFromString' = Right . unsafePerformIO . P.pathFromString

parse_path :: String -> [P.PathCommand]
parse_path str =
    case pathFromString' str of
      Left err -> error err
      Right cmd -> cmd

svg_read_path_d :: String -> [[P.PathCommand]]
svg_read_path_d s =
    let p = case X.parseXMLDoc s of
              Nothing -> error "svg_read_path_d: no parse"
              Just e -> X.findElements (svg_name "path") e
        d = mapMaybe (X.findAttr (X.unqual "d")) p
    in map parse_path d

subpaths_to_ls :: (R,R) -> [P.PathCommand] -> [Ls R]
subpaths_to_ls (dx,dy) r =
    case P.commandsToPoints r (dx,dy) (0,0) of
      [] -> error "subpaths_to_ls: no sub-paths"
      p -> map (map pt') p

svg_load_ls :: (R, R) -> FilePath -> IO [Ls R]
svg_load_ls rs fn = do
  s <- readFile fn
  let d = svg_read_path_d s
      p = concatMap (subpaths_to_ls rs) d
  return p

-- | 'plotCoord' of 'pt_xy'.
plot_ls :: PNum t => [Ls t] -> IO ()
plot_ls = plotCoord . map (map pt_xy)

-- | Convert a relative 'P.PathCommand' to the absolute form, giving
-- also the absolute end point.
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
