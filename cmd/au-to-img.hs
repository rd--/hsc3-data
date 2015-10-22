import qualified Data.Vector.Storable as V {- vector -}
import System.Environment {- base -}
import System.FilePath {- filepath -}

import qualified Sound.File.NeXT as AU {- hsc3-sf -}
import qualified Sound.File.NeXT.Vector as AU {- hsc3-data -}
import Sound.SC3.Data.Image.Plain {- hsc3-data -}

-- > au_to_img (1 -) "/home/rohan/sw/hsc3-sf/au/mc-12-2000.au"
au_to_img :: (Float -> Float) -> FilePath -> IO ()
au_to_img op fn = do
  (hdr,vec) <- AU.read_vec_f32 fn
  let img = img_from_vec_co (AU.frameCount hdr,AU.channelCount hdr) (V.map op vec)
  img_write_png (fn <.> "png") img

help :: IO ()
help = putStrLn "au-to-img {bw|gs} file-name"

md_to_fn :: String -> (Float -> Float)
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
