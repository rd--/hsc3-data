-- | Load linearised path data from SVG files.
module Sound.SC3.Data.SVG where

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

subpaths_to_ls :: (Double,Double) -> [P.PathCommand] -> [Ls Double]
subpaths_to_ls (dx,dy) r =
    case P.commandsToPoints r (dx,dy) (0,0) of
      [] -> error "subpaths_to_ls: no sub-paths"
      p -> map (map pt') p

svg_load_ls :: (Double, Double) -> FilePath -> IO [Ls Double]
svg_load_ls rs fn = do
  s <- readFile fn
  let d = svg_read_path_d s
      p = concatMap (subpaths_to_ls rs) d
  return p

plot_ls :: PNum t => [Ls t] -> IO ()
plot_ls = plotCoord . map (map pt_xy)
