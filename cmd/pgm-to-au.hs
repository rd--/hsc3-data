import System.Environment {- base -}

import qualified Sound.File.NeXT as AU {- hsc3-sf -}
import qualified Sound.File.NeXT.Vector as AU {- hsc3-data -}
import qualified Sound.SC3.Data.Image.PGM as I {- hsc3-data -}

pgm_to_au :: FilePath -> FilePath -> IO ()
pgm_to_au pgm_fn au_fn = do
  img <- I.pgm_load_0 pgm_fn
  let (nr,nc) = I.pgm_dimensions img
      img' = I.pgm_to_pgmf img
      hdr = AU.Header nc AU.Float 1 nr
      v = I.pgmf_to_vec (nr,nc) img'
  AU.write_f32_vec au_fn (hdr,v)

main :: IO ()
main = do
  a <- getArgs
  case a of
    [pgm_fn,au_fn] -> pgm_to_au pgm_fn au_fn
    _ -> putStrLn "pgm-to-au pgm-file au-file"
