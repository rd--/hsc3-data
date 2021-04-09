-- | UI HTML.  Very simple UI consisting only of text enumeration inputs.
module Sound.SC3.Data.UI.HTML where

import Control.Concurrent {- base -}
import Data.List {- base -}
import Text.Printf {- base -}

import Sound.SC3.Data.UI.WS as WS
import Sound.SC3.Data.UI.WV as WV

-- | UI element types.
data UI_Elem
  = UI_Label Int Int String -- ^ width:n-char uid text
  | UI_Enum Int Int Int [String] -- ^ width:n-char uid initial-ix texts
  | UI_LineBreak
  deriving (Eq,Show)

-- | h = height:n-line ; w = width:n-char ; z = uid (label index); lbl = label
ui_lbl_html :: Int -> Int -> Int -> String -> String
ui_lbl_html h w z =
  printf
  "<select id=\"L%d\" style=\"width:%dem\" size=\"%d\" disabled><option>%s</option></select>"
  z w h

-- | k = initial-ix ; e = enumeration
ui_enum_html :: Int -> Int -> Int -> Int -> [String] -> [String]
ui_enum_html h w z k e =
  let sel = printf "<select id=\"P%d\" style=\"width:%dem\" size=\"%d\">" z w h
      opt ix txt = printf
                   "<option value=\"%d\"%s>%s</option>"
                   ix (if ix == k then " selected" else "") txt
  in sel : zipWith opt [0 ..] e ++ ["</select>"]

-- | Generate HTML for UI_Elem.
ui_elem_html :: Int -> UI_Elem -> [String]
ui_elem_html h e =
  case e of
    UI_Label wd z txt -> [ui_lbl_html h wd z txt]
    UI_Enum wd z k lst -> ui_enum_html h wd z k lst
    UI_LineBreak -> ["<br />"]

{- | Enumerations given as (width:n-char,enum:[string]) pairs.
     Null entries are interpreted as line-breaks.
     Unary entries are interpreted as labels.
     Label and Enum elements are assigned distinct identifier sequences, each of the form [0..].

> ui_plain_to_elem (zip (repeat 1) (map words ["x","a b","C D E"]))
-}
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

-- | JS msg="p d1 d2" ctl-edit=send-msg recv-osc=ctl-set
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
  ,"      var p = k.charAt(0);"
  ,"      var d1 = k.slice(1,k.length);"
  ,"      var d2 = e.target.value;"
  ,"      ws.send([p,d1,d2].join(' '));"
  ,"    });"
  ,"  };"
  ,"  ws.onmessage = function(e) {"
  ,"    var m = e.data.split(' ');"
  ,"    var k = [m[0],m[1]].join('');"
  ,"    var sel = d.getElementById(k);"
  ,"    sel.value = Number.parseInt(m[2]);"
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
--
-- h = height:n-line
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

{- | 'ui_html_wr' of 'ui_plain_to_elem'.
     Run 'WV.ui_hsc3_wv_fn' to run UI and 'WS.ws_reader' to process UI input.
     When UI process ends, kill input receive process.

> pln = ui_plain_derive_width (map words ["x","a b","c d e"])
> ui_html_proc 9160 3 "/tmp/x.html" "x" pln () (\(_,(i,j)) -> print (i,j)) (960,600) True
-}
ui_html_proc :: Int -> Int -> FilePath -> String -> [(Int,[String])] -> st -> WS.WS_RECV_F st -> (Int, Int) -> Bool -> IO ()
ui_html_proc ws_p h html_fn nm pln st f dm rs = do
  ui_html_wr ws_p h html_fn nm (ui_plain_to_elem pln)
  th <- forkIO (WS.ws_reader ws_p st f)
  WV.ui_hsc3_wv_fn html_fn dm rs
  killThread th
