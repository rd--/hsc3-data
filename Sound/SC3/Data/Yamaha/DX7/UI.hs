-- | DX7 / UI (HTML)
module Sound.SC3.Data.Yamaha.DX7.UI where

import Data.List {- base -}

import Sound.SC3.UI.Enum.HTML {- hsc3-ui -}

import Sound.SC3.Data.Yamaha.DX7
import Sound.SC3.Data.Yamaha.DX7.PP
import Sound.SC3.Data.Yamaha.DX7.DX7II

u8_to_int :: U8 -> Int
u8_to_int = fromIntegral

dx7_parameter_ui :: Int -> DX7_Parameter -> Int -> UI_Elem
dx7_parameter_ui wd p k =
  UI_Enum wd (u8_to_int (dx7_parameter_ix p)) k (dx7_parameter_enum_usr p)

dx7_op_parameter_ui :: U8 -> [U8] -> [UI_Elem]
dx7_op_parameter_ui k p =
  zipWith3
  dx7_parameter_ui
  dx7_op_char_count
  (dx7_rewrite_op_dx7_parameter_tbl k)
  (map u8_to_int p)

dx7_sh_parameter_ui :: [U8] -> [UI_Elem]
dx7_sh_parameter_ui p =
  zipWith3
  dx7_parameter_ui
  dx7_sh_char_count
  dx7_sh_parameter_tbl
  (map u8_to_int p)

dx7_op_hdr_ui :: [UI_Elem]
dx7_op_hdr_ui =
  zipWith3 UI_Label dx7_op_char_count [0..] (map fst dx7ii_op_parameter_names)

dx7_sh_hdr_ui :: [UI_Elem]
dx7_sh_hdr_ui =
  zipWith3 UI_Label dx7_sh_char_count [dx7_op_nparam..] (map fst dx7ii_sh_parameter_names)

dx7_ui_br :: [[UI_Elem]] -> [UI_Elem]
dx7_ui_br = intercalate [UI_LineBreak]

dx7_voice_ui :: DX7_Voice -> [UI_Elem]
dx7_voice_ui vc = do
  let [op6,op5,op4,op3,op2,op1,sh,_] = dx7_voice_grp vc
      vc_nm = dx7_voice_name '?' vc
      nm = UI_Label 10 145 vc_nm
      op_ui = zipWith dx7_op_parameter_ui [6,5,4,3,2,1] [op6,op5,op4,op3,op2,op1]
    in dx7_ui_br (concat [[[nm,UI_LineBreak]
                          ,dx7_op_hdr_ui]
                         ,op_ui
                         ,[[UI_LineBreak]
                          ,dx7_sh_hdr_ui
                          ,dx7_sh_parameter_ui sh]])

-- > dx7_voice_ui_wr "/tmp/t.html" dx7_init_voice
dx7_voice_ui_wr :: Int -> Int -> FilePath -> DX7_Voice -> IO ()
dx7_voice_ui_wr ws_p h fn vc = ui_html_wr ws_p h fn (dx7_voice_name '?' vc) (dx7_voice_ui vc)
