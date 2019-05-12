module Sound.SC3.Data.UI.HTML where

import Data.List {- base -}
import Text.Printf {- base -}

-- | wd = char-width ; z = uid
ui_lbl_html :: Int -> Int -> String -> String
ui_lbl_html wd z txt =
  printf
  "<select id=\"L%d\" style=\"width:%dem\" disabled><option>%s</option></select>"
  z wd txt

-- | wd = char-width ; z = uid ; k = initial-ix ; e = enumeration
ui_enum_html :: Int -> Int -> Int -> [String] -> [String]
ui_enum_html wd z k e =
  let sel = printf "<select id=\"P%d\" style=\"width:%dem\">" z wd
      opt (ix,txt) = printf
                     "<option value=\"%d\"%s>%s</option>"
                     ix (if ix == k then " selected" else "") txt
  in sel : map opt (zip [0 ..] e) ++ ["</select>"]

data UI_Elem = UI_Label Int Int String | UI_Enum Int Int Int [String] | UI_LineBreak
  deriving (Eq,Show)

ui_elem_html :: UI_Elem -> [String]
ui_elem_html e =
  case e of
    UI_Label wd z txt -> [ui_lbl_html wd z txt]
    UI_Enum wd z k lst -> ui_enum_html wd z k lst
    UI_LineBreak -> ["<br />"]

type UI_Plain = [String]

ui_plain_to_elem :: [[UI_Plain]] -> [UI_Elem]
ui_plain_to_elem tbl =
  let acc_f (l,p) e = case e of
                        [] -> ((l,p),(-1,e))
                        [_] -> ((l+1,p),(l,e))
                        _ -> ((l,p+1),(p,e))
      (_,lst) = mapAccumL acc_f (0,0) (intercalate [[]] tbl)
      ui_f (z,e) =
        case e of
          [] -> UI_LineBreak
          [s] -> UI_Label (length s) z s
          _ -> UI_Enum (maximum (map length e)) z 0 e
  in map ui_f lst

ui_css :: [String]
ui_css =
  ["body {"
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
  ]

ui_js :: [String]
ui_js =
  ["window.onload = function () {"
  ,"    var d = window.document;"
  ,"    var ws = new WebSocket('ws://localhost:9160/');"
  ,"    var sel = d.getElementsByTagName('select');"
  ,"    for(i = 0; i < sel.length; i++) {"
  ,"        sel[i].addEventListener('change', (e) => {"
  ,"            var k = e.target.getAttribute('id');"
  ,"            var addr = ['/',k.charAt(0)].join('');"
  ,"            var d1 = parseInt(k.slice(1));"
  ,"            var d2 = parseInt(e.target.value);"
  ,"            ws.send(JSON.stringify([addr,d1,d2]));"
  ,"            console.log(k,d2);"
  ,"        });"
  ,"    };"
  ,"};"
  ]

ui_html_pre :: String -> [String]
ui_html_pre nm =
  ["<!DOCTYPE html>"
  ,"<html lang=\"en\">"
  ,"<head>"
  ,"<title>",nm,"</title>"
  ,"<style>",unlines ui_css,"</style>"
  ,"<script>",unlines ui_js,"</script>"
  ,"</head>"
  ,"<body>"
  ,"<form>"]

ui_html_post :: [String]
ui_html_post =
  ["</form>"
  ,"</body>"
  ,"</html>"]

ui_html :: String -> [UI_Elem] -> [String]
ui_html nm lst =
  concat [ui_html_pre nm
         ,concatMap ui_elem_html lst
         ,ui_html_post]

-- > pln = [[["R1"],words "A B C"],[],[["R2"],words "D E F",words "G H I"]]
-- > ui_html_wr "/tmp/t.html" "test" (ui_plain_to_elem pln)
ui_html_wr :: FilePath -> String -> [UI_Elem] -> IO ()
ui_html_wr fn nm lst = writeFile fn (unlines (ui_html nm lst))
