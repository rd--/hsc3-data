import System.Environment {- base -}
import System.FilePath {- filepath -}

import qualified Sound.File.NeXT as AU {- hsc3-sf -}
import Sound.SC3.Data.Image.Plain {- hsc3-data -}

au_to_img :: FilePath -> IO ()
au_to_img fn = do
  (hdr,ch) <- AU.read fn
  let img = img_from_gs (AU.frameCount hdr,AU.channelCount hdr) ch
  img_write_png (fn <.> "png") img

main :: IO ()
main = do
  a <- getArgs
  case a of
    [fn] -> au_to_img fn
    _ -> putStrLn "au-to-img file-name"
