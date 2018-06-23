-- | FOO-YC20 <https://github.com/sampov2/foo-yc20>
module Sound.SC3.Data.Yamaha.YC20 where

type U8 = Int

-- | FOO-YC20 controls, left-to-right sequence.
--
-- > length foo_yc20_cc_control_table == 23
foo_yc20_cc_control_table :: [(U8,String)]
foo_yc20_cc_control_table =
  [(50,"PITCH")
  ,(07,"VOLUME")
  ,(51,"BASS VOLUME")

  ,(52,"REALISM")

  ,(12,"VIBRATO") -- DEPTH
  ,(13,"VIB SPEED")

  ,(14,"BASS 16'") -- notched
  ,(15,"BASS 8'") -- notched
  ,(23,"MAN BASS") -- value < 64 = off, otherwise on

  ,(02,"I 16'") -- notched
  ,(03,"I 8'") -- notched
  ,(04,"I 4'") -- notched
  ,(05,"I 2+2/3'") -- notched
  ,(06,"I 2'") -- notched
  ,(08,"I 1+3/5'") -- notched
  ,(09,"I 1'") -- notched

  ,(16,"BALANCE") -- sections I and II

  ,(17,"BRIGHT")
  ,(18,"II 16'") -- notched
  ,(19,"II 8'") -- notched
  ,(20,"II 4'") -- notched
  ,(21,"II 2'") -- notched

  ,(22,"PERCUSSIVE") -- notched

  ]

foo_yc20_realism_modes :: [String]
foo_yc20_realism_modes =
  ["UNREALISTIC"
  ,"OSCILLATOR DETUNE"
  ,"PERCUSSION MANUAL BLEED"
  ,"DRAWBAR BLEED"]

foo_yc20_cc16 :: [[U8]]
foo_yc20_cc16 =
  [[14,15, 2,3,4,5,6,8,9, 16, 17,18,19,20,21, 22]]

