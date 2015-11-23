module Sound.SC3.Data.Buffer where

import System.Directory {- directory -}
import System.FilePath {- filepath -}

import Sound.OSC {- hosc -}
import Sound.SC3 {- hsc3 -}

import qualified Sound.File.NeXT as F {- hsc3-sf -}

-- | Message to send data to scsynth via temporary audio file.
b_tmp_allocRead :: (Floating n,Real n) => Int -> [n] -> IO Message
b_tmp_allocRead nid d = do
  tmp <- getTemporaryDirectory
  let nc = 1
      sr = 1
      h = F.Header (length d) F.Float sr nc
      nm = tmp </> "b_tmp_allocRead" <.> show nid <.> "au"
  F.au_write nm h [d]
  return (b_allocRead nid nm 0 0)
