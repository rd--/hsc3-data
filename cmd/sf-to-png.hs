import qualified Data.Vector.Storable as V {- vector -}
import System.Environment {- base -}

import qualified Sound.File.HSndFile as SF {- hsc3-sf-hsndfile -}
import qualified Sound.Sc3.Data.Image.Plain as I {- hsc3-data -}

sf_to_png :: (Double -> Double) -> FilePath -> FilePath -> IO ()
sf_to_png op sf_fn png_fn = do
  (hdr,Just vec) <- SF.read_vec sf_fn
  let png = I.img_from_vec_co (SF.frameCount hdr,SF.channelCount hdr) (V.map op vec)
  I.img_write_png png_fn png

help :: IO ()
help = putStrLn "sf-to-png {bw|gs} sound-file png-file"

md_to_fn :: String -> (Double -> Double)
md_to_fn md =
    case md of
      "bw" -> (1 -)
      "gs" -> id
      _ -> error "md = {bw|gs}"

main :: IO ()
main = do
  a <- getArgs
  case a of
    [md,sf_fn,png_fn] -> sf_to_png (md_to_fn md) sf_fn png_fn
    _ -> help

{-

:set -XScopedTypeVariables
let fn = "/home/rohan/desert/01.bw.extract.png.au"
(hdr,Just (vec :: V.Vector Float)) <- SF.read_vec fn
ERROR: out of memory (requested 17783848960 bytes)

-}
