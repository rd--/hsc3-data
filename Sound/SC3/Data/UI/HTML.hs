module Sound.SC3.Data.UI.HTML where

import Data.List {- base -}
import Text.Printf {- base -}

import System.Process {- process -}

-- | h = height:n-line ; w = width:n-char ; z = uid ; lbl = label
ui_lbl_html :: Int -> Int -> Int -> String -> String
ui_lbl_html h w z lbl =
  printf
  "<select id=\"L%d\" style=\"width:%dem\" size=\"%d\" disabled><option>%s</option></select>"
  z w h lbl

-- | k = initial-ix ; e = enumeration
ui_enum_html :: Int -> Int -> Int -> Int -> [String] -> [String]
ui_enum_html h w z k e =
  let sel = printf "<select id=\"P%d\" style=\"width:%dem\" size=\"%d\">" z w h
      opt (ix,txt) = printf
                     "<option value=\"%d\"%s>%s</option>"
                     ix (if ix == k then " selected" else "") txt
  in sel : map opt (zip [0 ..] e) ++ ["</select>"]

data UI_Elem = UI_Label Int Int String | UI_Enum Int Int Int [String] | UI_LineBreak
  deriving (Eq,Show)

ui_elem_html :: Int -> UI_Elem -> [String]
ui_elem_html h e =
  case e of
    UI_Label wd z txt -> [ui_lbl_html h wd z txt]
    UI_Enum wd z k lst -> ui_enum_html h wd z k lst
    UI_LineBreak -> ["<br />"]

-- | Enumerations given as (char-wd,enum-lst) pairs.
--   Null entries are interpreted as line-breaks.
--   Unary entries are interpreted as labels.
ui_plain_to_elem :: [(Int,[String])] -> [UI_Elem]
ui_plain_to_elem tbl =
  let acc_f (l,p) (wd,e) =
        case e of
          [] -> ((l,p),(-1,(wd,e)))
          [_] -> ((l+1,p),(l,(wd,e)))
          _ -> ((l,p+1),(p,(wd,e)))
      (_,lst) = mapAccumL acc_f (0,0) tbl
      ui_f (z,(wd,e)) =
        case e of
          [] -> UI_LineBreak
          [s] -> UI_Label wd z s
          _ -> UI_Enum wd z 0 e
  in map ui_f lst

-- | Derive widths for plain data.
ui_plain_derive_width :: [[String]] -> [(Int,[String])]
ui_plain_derive_width = map (\e -> (maximum (map length e),e))

-- | Plain text input.
ui_text_to_elem :: [[String]] -> [UI_Elem]
ui_text_to_elem = ui_plain_to_elem . ui_plain_derive_width

-- | CSS font=monospace bg=black fg=white border=off scrollbar=off
ui_css :: [String]
ui_css =
  [""
  ,"body {"
  ,"  background-color:black;"
  ,"  font-family: monospace;"
  ,"}"
  ,"select {"
  ,"  background-color:black;"
  ,"  color:white;"
  ,"  font-family: monospace;"
  ,"  border: 0;"
  ,"  -webkit-appearance:none;"
  ,"}"
  ,"select::-webkit-scrollbar {"
  ,"  display: none;"
  ,"}"
  ]

-- | JS osc=[/P,d1,d2] ctl-edit=send-osc recv-osc=ctl-set
ui_js :: Int -> [String]
ui_js ws_p =
  [""
  ,"window.onload = function () {"
  ,"  var d = window.document;"
  ,"  var ws = new WebSocket('ws://localhost:" ++ show ws_p ++ "/');"
  ,"  var sel = d.getElementsByTagName('select');"
  ,"  for(i = 0; i < sel.length; i++) {"
  ,"    sel[i].addEventListener('change', (e) => {"
  ,"      var k = e.target.getAttribute('id');"
  ,"      var addr = ['/',k.charAt(0)].join('');"
  ,"      var d1 = parseInt(k.slice(1));"
  ,"      var d2 = parseInt(e.target.value);"
  ,"      ws.send(JSON.stringify([addr,d1,d2]));"
  ,"      console.log(k,d2);"
  ,"    });"
  ,"  };"
  ,"  ws.onmessage = function(e) {"
  ,"    var m = JSON.parse(e.data);"
  ,"    var k = [m[0].charAt(1),m[1].toString()].join('');"
  ,"    var sel = d.getElementById(k);"
  ,"    sel.value = m[2];"
  ,"  };"
  ,"};"
  ]

-- | HTML pre-amble ws_p=websocket-port nm=document-title css=ui_css js=ui_js
ui_html_pre :: Int -> String -> [String]
ui_html_pre ws_p nm =
  [""
  ,"<!DOCTYPE html>"
  ,"<html lang=\"en\">"
  ,"<head>"
  ,"<meta charset=\"utf-8\"/>"
  ,"<title>",nm,"</title>"
  ,"<style>",unlines ui_css,"</style>"
  ,"<script>",unlines (ui_js ws_p),"</script>"
  ,"</head>"
  ,"<body>"
  ,"<form>"
  ]

-- | HTML post-able
ui_html_post :: [String]
ui_html_post =
  [""
  ,"</form>"
  ,"</body>"
  ,"</html>"
  ]

-- | Generate HTML for UI.
ui_html :: Int -> Int -> String -> [UI_Elem] -> [String]
ui_html ws_p h nm lst =
  concat [ui_html_pre ws_p nm
         ,concatMap (ui_elem_html h) lst
         ,ui_html_post]

-- | 'writeFile' of 'ui_html'.
--
-- > txt = [["R1"],words "A B C",[],["R2"],words "D E F",words "G H I"]
-- > ui_html_wr 9160 1 "/tmp/t.html" "test" (ui_text_to_elem txt)
ui_html_wr :: Int -> Int -> FilePath -> String -> [UI_Elem] -> IO ()
ui_html_wr ws_p h fn nm lst = writeFile fn (unlines (ui_html ws_p h nm lst))

ui_hsc3_wv_uri :: String -> (Int,Int) -> Bool -> IO ()
ui_hsc3_wv_uri uri (w,h) rs = callProcess "hsc3-wv" [uri,show w,show h,show rs]

ui_hsc3_wv_fn :: FilePath -> (Int,Int) -> Bool -> IO ()
ui_hsc3_wv_fn fn = ui_hsc3_wv_uri ("file://" ++ fn)
