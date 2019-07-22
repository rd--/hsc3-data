-- | Peterson/Barney (1952) data set giving formant locations.
module Sound.SC3.Data.Speech.PB where

pb_vc_tbl :: [(Int,String)]
pb_vc_tbl = zip [1..] (words "M F C")

pb_ph_tbl :: [(Int,String)]
pb_ph_tbl = zip [1..] (words "IY IH EH AE AH AA AO UH UW ER")

-- | (VOICE-ID,SPEAKER-ID,PHONEME-ID,(F0,F1,F2,F3))
type PB_ENT = (Int,Int,Int,(Int,Int,Int,Int))

pb_parse :: [String] -> PB_ENT
pb_parse l =
  case l of
    [vc,k,ph,_,f0,f1,f2,f3] -> (read vc,read k,read ph,(read f0,read f1,read f2,read f3))
    _ -> error "pb_parse?"
