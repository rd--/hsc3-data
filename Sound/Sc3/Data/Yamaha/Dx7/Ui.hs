-- | Dx7 / Ui (Html)
module Sound.Sc3.Data.Yamaha.Dx7.Ui where

import Data.List {- base -}

import Sound.Sc3.Ui.Enum.Html {- hsc3-ui -}

import Sound.Sc3.Data.Yamaha.Dx7
import Sound.Sc3.Data.Yamaha.Dx7.Pp
import Sound.Sc3.Data.Yamaha.Dx7.Dx7ii

u8_to_int :: U8 -> Int
u8_to_int = fromIntegral

dx7_parameter_ui :: Int -> Dx7_Parameter -> Int -> Ui_Elem
dx7_parameter_ui wd p k =
  Ui_Enum wd (u8_to_int (dx7_parameter_ix p)) k (dx7_parameter_enum_usr p)

dx7_op_parameter_ui :: U8 -> [U8] -> [Ui_Elem]
dx7_op_parameter_ui k p =
  zipWith3
  dx7_parameter_ui
  dx7_op_char_count
  (dx7_rewrite_op_dx7_parameter_tbl k)
  (map u8_to_int p)

dx7_sh_parameter_ui :: [U8] -> [Ui_Elem]
dx7_sh_parameter_ui p =
  zipWith3
  dx7_parameter_ui
  dx7_sh_char_count
  dx7_sh_parameter_tbl
  (map u8_to_int p)

dx7_op_hdr_ui :: [Ui_Elem]
dx7_op_hdr_ui =
  zipWith3 Ui_Label dx7_op_char_count [0..] (map fst dx7ii_op_parameter_names)

dx7_sh_hdr_ui :: [Ui_Elem]
dx7_sh_hdr_ui =
  zipWith3 Ui_Label dx7_sh_char_count [dx7_op_nparam..] (map fst dx7ii_sh_parameter_names)

dx7_ui_br :: [[Ui_Elem]] -> [Ui_Elem]
dx7_ui_br = intercalate [Ui_LineBreak]

dx7_voice_ui :: Dx7_Voice -> [Ui_Elem]
dx7_voice_ui vc = do
  let [op6,op5,op4,op3,op2,op1,sh,_] = dx7_voice_grp vc
      vc_nm = dx7_voice_name '?' vc
      nm = Ui_Label 10 145 vc_nm
      op_ui = zipWith dx7_op_parameter_ui [6,5,4,3,2,1] [op6,op5,op4,op3,op2,op1]
    in dx7_ui_br (concat [[[nm,Ui_LineBreak]
                          ,dx7_op_hdr_ui]
                         ,op_ui
                         ,[[Ui_LineBreak]
                          ,dx7_sh_hdr_ui
                          ,dx7_sh_parameter_ui sh]])

-- > dx7_voice_ui_wr 9160 1 "/tmp/t.html" dx7_init_voice
dx7_voice_ui_wr :: Int -> Int -> FilePath -> Dx7_Voice -> IO ()
dx7_voice_ui_wr ws_p h fn vc = ui_html_wr ws_p h fn (dx7_voice_name '?' vc) (dx7_voice_ui vc)
