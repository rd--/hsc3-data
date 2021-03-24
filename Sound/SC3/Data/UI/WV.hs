-- | WV = WebView
module Sound.SC3.Data.UI.WV where

import System.Process {- process -}

-- | wv = web-view, uri = uniform-resource-identifier, w = width, h = height, rs = resizable
ui_hsc3_wv_uri :: String -> (Int,Int) -> Bool -> IO ()
ui_hsc3_wv_uri uri (w,h) rs = callProcess "hsc3-wv" [uri,show w,show h,show rs]

-- | fn = file-name
--
-- > ui_hsc3_wv_fn "/tmp/t.html" (100,100) False
ui_hsc3_wv_fn :: FilePath -> (Int,Int) -> Bool -> IO ()
ui_hsc3_wv_fn fn = ui_hsc3_wv_uri ("file://" ++ fn)
