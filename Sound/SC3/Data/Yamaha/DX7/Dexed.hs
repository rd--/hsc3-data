-- | <https://github.com/asb2m10/dexed>
module Sound.SC3.Data.Yamaha.DX7.Dexed where

import Data.List {- base -}

{-

CtrlDX::CtrlDX > printf("CtrlDX {name=\"%s\", steps=%d, offset=%d, display_value=(%d)}\n"
                        ,name.toStdString().c_str(),steps,offset,displayValue);

-}

data CtrlDX = CtrlDX {name :: String, steps :: Int, offset :: Int, display_value :: Int}

-- > length dexed_param == 145
-- > sort (map offset dexed_param) == [0 .. 144]
dexed_param :: [CtrlDX]
dexed_param =
    [CtrlDX {name="ALGORITHM", steps=32, offset=134, display_value=1}
    ,CtrlDX {name="FEEDBACK", steps=8, offset=135, display_value=0}
    ,CtrlDX {name="OSC KEY SYNC", steps=2, offset=136, display_value=0}
    ,CtrlDX {name="LFO SPEED", steps=100, offset=137, display_value=0}
    ,CtrlDX {name="LFO DELAY", steps=100, offset=138, display_value=0}
    ,CtrlDX {name="LFO PM DEPTH", steps=100, offset=139, display_value=0}
    ,CtrlDX {name="LFO AM DEPTH", steps=100, offset=140, display_value=0}
    ,CtrlDX {name="LFO KEY SYNC", steps=2, offset=141, display_value=0}
    ,CtrlDX {name="LFO WAVE", steps=6, offset=142, display_value=0}
    ,CtrlDX {name="MIDDLE C", steps=49, offset=144, display_value=0}
    ,CtrlDX {name="P MODE SENS.", steps=8, offset=143, display_value=0}
    ,CtrlDX {name="PITCH EG RATE 1", steps=100, offset=126, display_value=0}
    ,CtrlDX {name="PITCH EG RATE 2", steps=100, offset=127, display_value=0}
    ,CtrlDX {name="PITCH EG RATE 3", steps=100, offset=128, display_value=0}
    ,CtrlDX {name="PITCH EG RATE 4", steps=100, offset=129, display_value=0}
    ,CtrlDX {name="PITCH EG LEVEL 1", steps=100, offset=130, display_value=0}
    ,CtrlDX {name="PITCH EG LEVEL 2", steps=100, offset=131, display_value=0}
    ,CtrlDX {name="PITCH EG LEVEL 3", steps=100, offset=132, display_value=0}
    ,CtrlDX {name="PITCH EG LEVEL 4", steps=100, offset=133, display_value=0}
    ,CtrlDX {name="OP1 EG RATE 1", steps=100, offset=105, display_value=0}
    ,CtrlDX {name="OP1 EG RATE 2", steps=100, offset=106, display_value=0}
    ,CtrlDX {name="OP1 EG RATE 3", steps=100, offset=107, display_value=0}
    ,CtrlDX {name="OP1 EG RATE 4", steps=100, offset=108, display_value=0}
    ,CtrlDX {name="OP1 EG LEVEL 1", steps=100, offset=109, display_value=0}
    ,CtrlDX {name="OP1 EG LEVEL 2", steps=100, offset=110, display_value=0}
    ,CtrlDX {name="OP1 EG LEVEL 3", steps=100, offset=111, display_value=0}
    ,CtrlDX {name="OP1 EG LEVEL 4", steps=100, offset=112, display_value=0}
    ,CtrlDX {name="OP1 OUTPUT LEVEL", steps=100, offset=121, display_value=0}
    ,CtrlDX {name="OP1 MODE", steps=2, offset=122, display_value=0}
    ,CtrlDX {name="OP1 F COARSE", steps=32, offset=123, display_value=0}
    ,CtrlDX {name="OP1 F FINE", steps=100, offset=124, display_value=0}
    ,CtrlDX {name="OP1 OSC DETUNE", steps=15, offset=125, display_value=(-7)}
    ,CtrlDX {name="OP1 BREAK POINT", steps=100, offset=113, display_value=0}
    ,CtrlDX {name="OP1 L SCALE DEPTH", steps=100, offset=114, display_value=0}
    ,CtrlDX {name="OP1 R SCALE DEPTH", steps=100, offset=115, display_value=0}
    ,CtrlDX {name="OP1 L KEY SCALE", steps=4, offset=116, display_value=0}
    ,CtrlDX {name="OP1 R KEY SCALE", steps=4, offset=117, display_value=0}
    ,CtrlDX {name="OP1 RATE SCALING", steps=8, offset=118, display_value=0}
    ,CtrlDX {name="OP1 A MOD SENS.", steps=4, offset=119, display_value=0}
    ,CtrlDX {name="OP1 KEY VELOCITY", steps=8, offset=120, display_value=0}
    ,CtrlDX {name="OP2 EG RATE 1", steps=100, offset=84, display_value=0}
    ,CtrlDX {name="OP2 EG RATE 2", steps=100, offset=85, display_value=0}
    ,CtrlDX {name="OP2 EG RATE 3", steps=100, offset=86, display_value=0}
    ,CtrlDX {name="OP2 EG RATE 4", steps=100, offset=87, display_value=0}
    ,CtrlDX {name="OP2 EG LEVEL 1", steps=100, offset=88, display_value=0}
    ,CtrlDX {name="OP2 EG LEVEL 2", steps=100, offset=89, display_value=0}
    ,CtrlDX {name="OP2 EG LEVEL 3", steps=100, offset=90, display_value=0}
    ,CtrlDX {name="OP2 EG LEVEL 4", steps=100, offset=91, display_value=0}
    ,CtrlDX {name="OP2 OUTPUT LEVEL", steps=100, offset=100, display_value=0}
    ,CtrlDX {name="OP2 MODE", steps=2, offset=101, display_value=0}
    ,CtrlDX {name="OP2 F COARSE", steps=32, offset=102, display_value=0}
    ,CtrlDX {name="OP2 F FINE", steps=100, offset=103, display_value=0}
    ,CtrlDX {name="OP2 OSC DETUNE", steps=15, offset=104, display_value=(-7)}
    ,CtrlDX {name="OP2 BREAK POINT", steps=100, offset=92, display_value=0}
    ,CtrlDX {name="OP2 L SCALE DEPTH", steps=100, offset=93, display_value=0}
    ,CtrlDX {name="OP2 R SCALE DEPTH", steps=100, offset=94, display_value=0}
    ,CtrlDX {name="OP2 L KEY SCALE", steps=4, offset=95, display_value=0}
    ,CtrlDX {name="OP2 R KEY SCALE", steps=4, offset=96, display_value=0}
    ,CtrlDX {name="OP2 RATE SCALING", steps=8, offset=97, display_value=0}
    ,CtrlDX {name="OP2 A MOD SENS.", steps=4, offset=98, display_value=0}
    ,CtrlDX {name="OP2 KEY VELOCITY", steps=8, offset=99, display_value=0}
    ,CtrlDX {name="OP3 EG RATE 1", steps=100, offset=63, display_value=0}
    ,CtrlDX {name="OP3 EG RATE 2", steps=100, offset=64, display_value=0}
    ,CtrlDX {name="OP3 EG RATE 3", steps=100, offset=65, display_value=0}
    ,CtrlDX {name="OP3 EG RATE 4", steps=100, offset=66, display_value=0}
    ,CtrlDX {name="OP3 EG LEVEL 1", steps=100, offset=67, display_value=0}
    ,CtrlDX {name="OP3 EG LEVEL 2", steps=100, offset=68, display_value=0}
    ,CtrlDX {name="OP3 EG LEVEL 3", steps=100, offset=69, display_value=0}
    ,CtrlDX {name="OP3 EG LEVEL 4", steps=100, offset=70, display_value=0}
    ,CtrlDX {name="OP3 OUTPUT LEVEL", steps=100, offset=79, display_value=0}
    ,CtrlDX {name="OP3 MODE", steps=2, offset=80, display_value=0}
    ,CtrlDX {name="OP3 F COARSE", steps=32, offset=81, display_value=0}
    ,CtrlDX {name="OP3 F FINE", steps=100, offset=82, display_value=0}
    ,CtrlDX {name="OP3 OSC DETUNE", steps=15, offset=83, display_value=(-7)}
    ,CtrlDX {name="OP3 BREAK POINT", steps=100, offset=71, display_value=0}
    ,CtrlDX {name="OP3 L SCALE DEPTH", steps=100, offset=72, display_value=0}
    ,CtrlDX {name="OP3 R SCALE DEPTH", steps=100, offset=73, display_value=0}
    ,CtrlDX {name="OP3 L KEY SCALE", steps=4, offset=74, display_value=0}
    ,CtrlDX {name="OP3 R KEY SCALE", steps=4, offset=75, display_value=0}
    ,CtrlDX {name="OP3 RATE SCALING", steps=8, offset=76, display_value=0}
    ,CtrlDX {name="OP3 A MOD SENS.", steps=4, offset=77, display_value=0}
    ,CtrlDX {name="OP3 KEY VELOCITY", steps=8, offset=78, display_value=0}
    ,CtrlDX {name="OP4 EG RATE 1", steps=100, offset=42, display_value=0}
    ,CtrlDX {name="OP4 EG RATE 2", steps=100, offset=43, display_value=0}
    ,CtrlDX {name="OP4 EG RATE 3", steps=100, offset=44, display_value=0}
    ,CtrlDX {name="OP4 EG RATE 4", steps=100, offset=45, display_value=0}
    ,CtrlDX {name="OP4 EG LEVEL 1", steps=100, offset=46, display_value=0}
    ,CtrlDX {name="OP4 EG LEVEL 2", steps=100, offset=47, display_value=0}
    ,CtrlDX {name="OP4 EG LEVEL 3", steps=100, offset=48, display_value=0}
    ,CtrlDX {name="OP4 EG LEVEL 4", steps=100, offset=49, display_value=0}
    ,CtrlDX {name="OP4 OUTPUT LEVEL", steps=100, offset=58, display_value=0}
    ,CtrlDX {name="OP4 MODE", steps=2, offset=59, display_value=0}
    ,CtrlDX {name="OP4 F COARSE", steps=32, offset=60, display_value=0}
    ,CtrlDX {name="OP4 F FINE", steps=100, offset=61, display_value=0}
    ,CtrlDX {name="OP4 OSC DETUNE", steps=15, offset=62, display_value=(-7)}
    ,CtrlDX {name="OP4 BREAK POINT", steps=100, offset=50, display_value=0}
    ,CtrlDX {name="OP4 L SCALE DEPTH", steps=100, offset=51, display_value=0}
    ,CtrlDX {name="OP4 R SCALE DEPTH", steps=100, offset=52, display_value=0}
    ,CtrlDX {name="OP4 L KEY SCALE", steps=4, offset=53, display_value=0}
    ,CtrlDX {name="OP4 R KEY SCALE", steps=4, offset=54, display_value=0}
    ,CtrlDX {name="OP4 RATE SCALING", steps=8, offset=55, display_value=0}
    ,CtrlDX {name="OP4 A MOD SENS.", steps=4, offset=56, display_value=0}
    ,CtrlDX {name="OP4 KEY VELOCITY", steps=8, offset=57, display_value=0}
    ,CtrlDX {name="OP5 EG RATE 1", steps=100, offset=21, display_value=0}
    ,CtrlDX {name="OP5 EG RATE 2", steps=100, offset=22, display_value=0}
    ,CtrlDX {name="OP5 EG RATE 3", steps=100, offset=23, display_value=0}
    ,CtrlDX {name="OP5 EG RATE 4", steps=100, offset=24, display_value=0}
    ,CtrlDX {name="OP5 EG LEVEL 1", steps=100, offset=25, display_value=0}
    ,CtrlDX {name="OP5 EG LEVEL 2", steps=100, offset=26, display_value=0}
    ,CtrlDX {name="OP5 EG LEVEL 3", steps=100, offset=27, display_value=0}
    ,CtrlDX {name="OP5 EG LEVEL 4", steps=100, offset=28, display_value=0}
    ,CtrlDX {name="OP5 OUTPUT LEVEL", steps=100, offset=37, display_value=0}
    ,CtrlDX {name="OP5 MODE", steps=2, offset=38, display_value=0}
    ,CtrlDX {name="OP5 F COARSE", steps=32, offset=39, display_value=0}
    ,CtrlDX {name="OP5 F FINE", steps=100, offset=40, display_value=0}
    ,CtrlDX {name="OP5 OSC DETUNE", steps=15, offset=41, display_value=(-7)}
    ,CtrlDX {name="OP5 BREAK POINT", steps=100, offset=29, display_value=0}
    ,CtrlDX {name="OP5 L SCALE DEPTH", steps=100, offset=30, display_value=0}
    ,CtrlDX {name="OP5 R SCALE DEPTH", steps=100, offset=31, display_value=0}
    ,CtrlDX {name="OP5 L KEY SCALE", steps=4, offset=32, display_value=0}
    ,CtrlDX {name="OP5 R KEY SCALE", steps=4, offset=33, display_value=0}
    ,CtrlDX {name="OP5 RATE SCALING", steps=8, offset=34, display_value=0}
    ,CtrlDX {name="OP5 A MOD SENS.", steps=4, offset=35, display_value=0}
    ,CtrlDX {name="OP5 KEY VELOCITY", steps=8, offset=36, display_value=0}
    ,CtrlDX {name="OP6 EG RATE 1", steps=100, offset=0, display_value=0}
    ,CtrlDX {name="OP6 EG RATE 2", steps=100, offset=1, display_value=0}
    ,CtrlDX {name="OP6 EG RATE 3", steps=100, offset=2, display_value=0}
    ,CtrlDX {name="OP6 EG RATE 4", steps=100, offset=3, display_value=0}
    ,CtrlDX {name="OP6 EG LEVEL 1", steps=100, offset=4, display_value=0}
    ,CtrlDX {name="OP6 EG LEVEL 2", steps=100, offset=5, display_value=0}
    ,CtrlDX {name="OP6 EG LEVEL 3", steps=100, offset=6, display_value=0}
    ,CtrlDX {name="OP6 EG LEVEL 4", steps=100, offset=7, display_value=0}
    ,CtrlDX {name="OP6 OUTPUT LEVEL", steps=100, offset=16, display_value=0}
    ,CtrlDX {name="OP6 MODE", steps=2, offset=17, display_value=0}
    ,CtrlDX {name="OP6 F COARSE", steps=32, offset=18, display_value=0}
    ,CtrlDX {name="OP6 F FINE", steps=100, offset=19, display_value=0}
    ,CtrlDX {name="OP6 OSC DETUNE", steps=15, offset=20, display_value=(-7)}
    ,CtrlDX {name="OP6 BREAK POINT", steps=100, offset=8, display_value=0}
    ,CtrlDX {name="OP6 L SCALE DEPTH", steps=100, offset=9, display_value=0}
    ,CtrlDX {name="OP6 R SCALE DEPTH", steps=100, offset=10, display_value=0}
    ,CtrlDX {name="OP6 L KEY SCALE", steps=4, offset=11, display_value=0}
    ,CtrlDX {name="OP6 R KEY SCALE", steps=4, offset=12, display_value=0}
    ,CtrlDX {name="OP6 RATE SCALING", steps=8, offset=13, display_value=0}
    ,CtrlDX {name="OP6 A MOD SENS.", steps=4, offset=14, display_value=0}
    ,CtrlDX {name="OP6 KEY VELOCITY", steps=8, offset=15, display_value=0}
    ]

-- > import qualified Sound.SC3.Data.Yamaha.DX7 as DX7
-- > mapM_ print $ zip dexed_param_dx7 DX7.dx7_parameter_tbl
dexed_param_dx7 :: [(Int, String, Int)]
dexed_param_dx7 =
    let p = map (\(i,c) -> (offset c,name c,i)) (zip [0..] dexed_param)
    in sortOn (\(i,_,_) -> i) p
