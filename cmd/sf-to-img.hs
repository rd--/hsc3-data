import System.Environment {- base -}
import System.FilePath {- filepath -}

import qualified Sound.File.HSndFile as SF {- hsc3-sf-hsndfile -}
import Sound.SC3.Data.Image.Plain {- hsc3-data -}

sf_to_img :: FilePath -> IO ()
sf_to_img fn = do
  (hdr,Just vec) <- SF.read_vec fn
  let img = img_from_vec_co (SF.frameCount hdr,SF.channelCount hdr) vec
  img_write_png (fn <.> "png") img

main :: IO ()
main = do
  a <- getArgs
  case a of
    [fn] -> sf_to_img fn
    _ -> putStrLn "sf-to-img file-name"

{-

let fn = "/home/rohan/desert/01.bw.extract.png.au"
(hdr,Just vec) <- SF.read_vec fn
ERROR: out of memory (requested 17783848960 bytes)

-}
