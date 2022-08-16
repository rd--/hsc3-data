-- | Simple markdown utilities
module Sound.Sc3.Data.Text.Markdown where

import Safe {- safe -}

import Music.Theory.List {- hmt-base -}

-- | Is a line a top-level heading.
md_is_h1 :: String -> Bool
md_is_h1 = (== "#") . headDef "" . words

-- | Split a markdown text (given as a sequence of lines) into top-level sections.
md_split_at_h1 :: [String] -> [[String]]
md_split_at_h1 = filter (not . null) . split_when_keeping_left md_is_h1

-- | Read a markdown file and write each top-level section separately.
md_extract_h1 :: FilePath -> ([String] -> FilePath) -> IO ()
md_extract_h1 in_file gen_fn = do
  in_ln <- fmap lines (readFile in_file)
  let h1_ln = md_split_at_h1 in_ln
      out_files = map (gen_fn . tail . words . head) h1_ln
      wr_ln (fn, ln) = writeFile fn (unlines ln)
  mapM_ wr_ln (zip out_files h1_ln)
