import System.Environment {- base -}

import qualified Sound.File.HSndFile as SF {- hsc3-sf-hsndfile -}
import Sound.SC3.Data.Image.Plain {- hsc3-data -}

-- > sf_to_img "/tmp/mc-12-2000.au"
-- > sf_to_img "/home/rohan/in/image/d.0.jpg.au"
sf_to_img :: FilePath -> IO ()
sf_to_img fn = do
  (hdr,Just vec) <- SF.read_vec fn
  let img = img_from_vec_co (SF.frameCount hdr,SF.channelCount hdr) vec
  img_write_png (fn ++ ".png") img

help :: IO ()
help = putStrLn "sf-to-img file-name"

main :: IO ()
main = do
  a <- getArgs
  case a of
    [fn] -> sf_to_img fn
    _ -> help
