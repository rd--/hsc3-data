-- | 'b_allocRead' variant, writing data to temporary file.
module Sound.SC3.Data.Buffer where

import System.Directory {- directory -}
import System.FilePath {- filepath -}

import Sound.Osc {- hosc -}
import Sound.SC3 {- hsc3 -}

import qualified Sound.File.NeXT as SF {- hsc3-sf -}

-- | Message to send data to scsynth via temporary audio file.
b_tmp_allocRead :: Real n => Int -> [n] -> IO Message
b_tmp_allocRead nid d = do
  tmp <- getTemporaryDirectory
  let nc = 1
      sr = 1
      h = SF.SF_Header (length d) SF.Float sr nc
      nm = tmp </> "b_tmp_allocRead" <.> show nid <.> "au"
  SF.au_write nm h [d]
  return (b_allocRead nid nm 0 0)
