import System.Environment {- base -}

import qualified Sound.File.NeXT as AU {- hsc3-sf -}

import Sound.SC3.Data.Image.PGM {- hsc3-data -}

-- > let fn = "/home/rohan/sw/hsc3-sf/au/mc-4-16.au"
-- > au_to_pgm 8 fn (fn ++ ".pgm")
au_to_pgm :: Int -> FilePath -> FilePath -> IO ()
au_to_pgm depth au_fn pgm_fn = do
  (hdr,vec) <- AU.au_read_f32_vec au_fn
  let dm = (AU.channelCount hdr,AU.frameCount hdr)
      img = pgmf_from_vec_co dm vec
  pgm5_save_0 pgm_fn (pgmf_to_pgm depth img)

main :: IO ()
main = do
  a <- getArgs
  case a of
    [depth,au_fn,pgm_fn] -> au_to_pgm (read depth) au_fn pgm_fn
    _ -> putStrLn "au-to-pgm au-file pgm-file"
