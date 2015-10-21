import qualified Data.Vector.Storable as V {- vector -}
import System.Environment {- base -}
import System.FilePath {- filepath -}

import qualified Sound.File.HSndFile as SF {- hsc3-sf-hsndfile -}
import Sound.SC3.Data.Image.Plain {- hsc3-data -}

sf_to_img :: (Double -> Double) -> FilePath -> IO ()
sf_to_img op fn = do
  (hdr,Just vec) <- SF.read_vec fn
  let img = img_from_vec_co (SF.frameCount hdr,SF.channelCount hdr) (V.map op vec)
  img_write_png (fn <.> "png") img

help :: IO ()
help = putStrLn "sf-to-img {bw|gs} file-name"

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
    [md,fn] -> sf_to_img (md_to_fn md) fn
    _ -> help

{-

let fn = "/home/rohan/desert/01.bw.extract.png.au"
(hdr,Just vec) <- SF.read_vec fn
ERROR: out of memory (requested 17783848960 bytes)

-}
