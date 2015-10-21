import System.Environment {- base -}
import System.FilePath {- filepath -}

import qualified Sound.File.NeXT as AU {- hsc3-sf -}
import Sound.SC3.Data.Image.Plain {- hsc3-data -}

au_to_img :: (Double -> Double) -> FilePath -> IO ()
au_to_img op fn = do
  (hdr,ch) <- AU.read fn
  let img = img_from_gs (AU.frameCount hdr,AU.channelCount hdr) (map (map op) ch)
  img_write_png (fn <.> "png") img

help :: IO ()
help = putStrLn "au-to-img {bw|gs} file-name"

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
    [md,fn] -> au_to_img (md_to_fn md) fn
    _ -> help
