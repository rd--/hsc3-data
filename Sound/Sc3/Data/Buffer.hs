-- | 'b_allocRead' variant, writing data to temporary file.
module Sound.Sc3.Data.Buffer where

import System.Directory {- directory -}
import System.FilePath {- filepath -}

import Sound.Osc {- hosc -}
import Sound.Sc3 {- hsc3 -}

import qualified Sound.File.Next as Sf {- hsc3-sf -}

-- | Message to send data to scsynth via temporary audio file.
b_tmp_allocRead :: Real n => Int -> [n] -> IO Message
b_tmp_allocRead nid d = do
  tmp <- getTemporaryDirectory
  let nc = 1
      sr = 1
      h = Sf.Sf_Header (length d) Sf.Float sr nc
      nm = tmp </> "b_tmp_allocRead" <.> show nid <.> "au"
  Sf.au_write nm h [d]
  return (b_allocRead nid nm 0 0)
