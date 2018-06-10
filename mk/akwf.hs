import Sound.SC3.Data.WT.AKWF {- hsc3-data -}

import qualified WWW.Minus.Fetch as WWW {- www-minus -}

akfw_get_png_grp :: FilePath -> AKWF_GRP -> IO ()
akfw_get_png_grp dir = mapM_ WWW.url_fetch . akfw_get_png_cmd dir

akwf_get_png_all :: FilePath -> IO ()
akwf_get_png_all dir = mapM_ (akfw_get_png_grp dir . akwf_lookup_err . fst) akwf_grp

{-

akwf_get_png_all "/home/rohan/data/audio/wt/akwf/png"

let ix_fn = "/home/rohan/sw/hsc3-data/md/akwf.md"
writeFile ix_fn (unlines (akfw_png_ix_all "sw/hsc3-data/png/akwf"))

-}
