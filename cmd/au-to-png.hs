import qualified Data.Vector.Storable as V {- vector -}
import System.Environment {- base -}

import qualified Sound.File.NeXT as AU {- hsc3-sf -}
import qualified Sound.File.NeXT.Vector as AU {- hsc3-data -}
import Sound.SC3.Data.Image.Plain {- hsc3-data -}

-- > let fn = "/home/rohan/sw/hsc3-sf/au/mc-4-16.au"
-- > au_to_img (1 -) fn (fn ++ ".png")
au_to_img :: (Float -> Float) -> FilePath -> FilePath -> IO ()
au_to_img op au_fn png_fn = do
  (hdr,vec) <- AU.au_read_f32_vec au_fn
  let img = img_from_vec_co (AU.frameCount hdr,AU.channelCount hdr) (V.map op vec)
  img_write_png png_fn img

help :: IO ()
help = putStrLn "au-to-img {bw|gs} au-file png-file"

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
    [md,au_fn,png_fn] -> au_to_img (md_to_fn md) au_fn png_fn
    _ -> help
