-- | <https://github.com/asb2m10/dexed>
module Sound.SC3.Data.Yamaha.DX7.Dexed where

import Data.List {- base -}
import Data.Maybe {- base -}
import Data.Word {- base -}

import qualified Data.Map as M {- containers -}

-- * CTRL

-- | Dexed control, idx=index.
data Ctrl =
  Ctrl {label :: String, idx :: Int}
  deriving (Eq,Show)

{- | Dexed control parameters.

The first four param (0-3) are not DX7 parameters, as are the six operator "SWITCH" parameters.

> > for(i=0;i < ctrl.size(); i++) {
> >   printf("Ctrl {label=\"%s\", idx=%d}\n",
> >     ctrl[i]->label.toStdString().c_str(),
> >     ctrl[i]->idx);
> > }

> map idx dexed_param == [0 .. 154]
> 155 - (4 + 6) == 145
-}
dexed_param :: [Ctrl]
dexed_param =
    [Ctrl {label="Cutoff", idx=0} -- non-DX7
    ,Ctrl {label="Resonance", idx=1} -- non-DX7
    ,Ctrl {label="Output", idx=2} -- non-DX7
    ,Ctrl {label="MASTER TUNE ADJ", idx=3} -- non-DX7
    ,Ctrl {label="ALGORITHM", idx=4}
    ,Ctrl {label="FEEDBACK", idx=5}
    ,Ctrl {label="OSC KEY SYNC", idx=6}
    ,Ctrl {label="LFO SPEED", idx=7}
    ,Ctrl {label="LFO DELAY", idx=8}
    ,Ctrl {label="LFO PM DEPTH", idx=9}
    ,Ctrl {label="LFO AM DEPTH", idx=10}
    ,Ctrl {label="LFO KEY SYNC", idx=11}
    ,Ctrl {label="LFO WAVE", idx=12}
    ,Ctrl {label="MIDDLE C", idx=13}
    ,Ctrl {label="P MODE SENS.", idx=14}
    ,Ctrl {label="PITCH EG RATE 1", idx=15}
    ,Ctrl {label="PITCH EG RATE 2", idx=16}
    ,Ctrl {label="PITCH EG RATE 3", idx=17}
    ,Ctrl {label="PITCH EG RATE 4", idx=18}
    ,Ctrl {label="PITCH EG LEVEL 1", idx=19}
    ,Ctrl {label="PITCH EG LEVEL 2", idx=20}
    ,Ctrl {label="PITCH EG LEVEL 3", idx=21}
    ,Ctrl {label="PITCH EG LEVEL 4", idx=22}
    ,Ctrl {label="OP1 EG RATE 1", idx=23}
    ,Ctrl {label="OP1 EG RATE 2", idx=24}
    ,Ctrl {label="OP1 EG RATE 3", idx=25}
    ,Ctrl {label="OP1 EG RATE 4", idx=26}
    ,Ctrl {label="OP1 EG LEVEL 1", idx=27}
    ,Ctrl {label="OP1 EG LEVEL 2", idx=28}
    ,Ctrl {label="OP1 EG LEVEL 3", idx=29}
    ,Ctrl {label="OP1 EG LEVEL 4", idx=30}
    ,Ctrl {label="OP1 OUTPUT LEVEL", idx=31}
    ,Ctrl {label="OP1 MODE", idx=32}
    ,Ctrl {label="OP1 F COARSE", idx=33}
    ,Ctrl {label="OP1 F FINE", idx=34}
    ,Ctrl {label="OP1 OSC DETUNE", idx=35}
    ,Ctrl {label="OP1 BREAK POINT", idx=36}
    ,Ctrl {label="OP1 L SCALE DEPTH", idx=37}
    ,Ctrl {label="OP1 R SCALE DEPTH", idx=38}
    ,Ctrl {label="OP1 L KEY SCALE", idx=39}
    ,Ctrl {label="OP1 R KEY SCALE", idx=40}
    ,Ctrl {label="OP1 RATE SCALING", idx=41}
    ,Ctrl {label="OP1 A MOD SENS.", idx=42}
    ,Ctrl {label="OP1 KEY VELOCITY", idx=43}
    ,Ctrl {label="OP1 SWITCH", idx=44} -- non-DX7
    ,Ctrl {label="OP2 EG RATE 1", idx=45}
    ,Ctrl {label="OP2 EG RATE 2", idx=46}
    ,Ctrl {label="OP2 EG RATE 3", idx=47}
    ,Ctrl {label="OP2 EG RATE 4", idx=48}
    ,Ctrl {label="OP2 EG LEVEL 1", idx=49}
    ,Ctrl {label="OP2 EG LEVEL 2", idx=50}
    ,Ctrl {label="OP2 EG LEVEL 3", idx=51}
    ,Ctrl {label="OP2 EG LEVEL 4", idx=52}
    ,Ctrl {label="OP2 OUTPUT LEVEL", idx=53}
    ,Ctrl {label="OP2 MODE", idx=54}
    ,Ctrl {label="OP2 F COARSE", idx=55}
    ,Ctrl {label="OP2 F FINE", idx=56}
    ,Ctrl {label="OP2 OSC DETUNE", idx=57}
    ,Ctrl {label="OP2 BREAK POINT", idx=58}
    ,Ctrl {label="OP2 L SCALE DEPTH", idx=59}
    ,Ctrl {label="OP2 R SCALE DEPTH", idx=60}
    ,Ctrl {label="OP2 L KEY SCALE", idx=61}
    ,Ctrl {label="OP2 R KEY SCALE", idx=62}
    ,Ctrl {label="OP2 RATE SCALING", idx=63}
    ,Ctrl {label="OP2 A MOD SENS.", idx=64}
    ,Ctrl {label="OP2 KEY VELOCITY", idx=65}
    ,Ctrl {label="OP2 SWITCH", idx=66} -- non-DX7
    ,Ctrl {label="OP3 EG RATE 1", idx=67}
    ,Ctrl {label="OP3 EG RATE 2", idx=68}
    ,Ctrl {label="OP3 EG RATE 3", idx=69}
    ,Ctrl {label="OP3 EG RATE 4", idx=70}
    ,Ctrl {label="OP3 EG LEVEL 1", idx=71}
    ,Ctrl {label="OP3 EG LEVEL 2", idx=72}
    ,Ctrl {label="OP3 EG LEVEL 3", idx=73}
    ,Ctrl {label="OP3 EG LEVEL 4", idx=74}
    ,Ctrl {label="OP3 OUTPUT LEVEL", idx=75}
    ,Ctrl {label="OP3 MODE", idx=76}
    ,Ctrl {label="OP3 F COARSE", idx=77}
    ,Ctrl {label="OP3 F FINE", idx=78}
    ,Ctrl {label="OP3 OSC DETUNE", idx=79}
    ,Ctrl {label="OP3 BREAK POINT", idx=80}
    ,Ctrl {label="OP3 L SCALE DEPTH", idx=81}
    ,Ctrl {label="OP3 R SCALE DEPTH", idx=82}
    ,Ctrl {label="OP3 L KEY SCALE", idx=83}
    ,Ctrl {label="OP3 R KEY SCALE", idx=84}
    ,Ctrl {label="OP3 RATE SCALING", idx=85}
    ,Ctrl {label="OP3 A MOD SENS.", idx=86}
    ,Ctrl {label="OP3 KEY VELOCITY", idx=87}
    ,Ctrl {label="OP3 SWITCH", idx=88} -- non-DX7
    ,Ctrl {label="OP4 EG RATE 1", idx=89}
    ,Ctrl {label="OP4 EG RATE 2", idx=90}
    ,Ctrl {label="OP4 EG RATE 3", idx=91}
    ,Ctrl {label="OP4 EG RATE 4", idx=92}
    ,Ctrl {label="OP4 EG LEVEL 1", idx=93}
    ,Ctrl {label="OP4 EG LEVEL 2", idx=94}
    ,Ctrl {label="OP4 EG LEVEL 3", idx=95}
    ,Ctrl {label="OP4 EG LEVEL 4", idx=96}
    ,Ctrl {label="OP4 OUTPUT LEVEL", idx=97}
    ,Ctrl {label="OP4 MODE", idx=98}
    ,Ctrl {label="OP4 F COARSE", idx=99}
    ,Ctrl {label="OP4 F FINE", idx=100}
    ,Ctrl {label="OP4 OSC DETUNE", idx=101}
    ,Ctrl {label="OP4 BREAK POINT", idx=102}
    ,Ctrl {label="OP4 L SCALE DEPTH", idx=103}
    ,Ctrl {label="OP4 R SCALE DEPTH", idx=104}
    ,Ctrl {label="OP4 L KEY SCALE", idx=105}
    ,Ctrl {label="OP4 R KEY SCALE", idx=106}
    ,Ctrl {label="OP4 RATE SCALING", idx=107}
    ,Ctrl {label="OP4 A MOD SENS.", idx=108}
    ,Ctrl {label="OP4 KEY VELOCITY", idx=109}
    ,Ctrl {label="OP4 SWITCH", idx=110} -- non-DX7
    ,Ctrl {label="OP5 EG RATE 1", idx=111}
    ,Ctrl {label="OP5 EG RATE 2", idx=112}
    ,Ctrl {label="OP5 EG RATE 3", idx=113}
    ,Ctrl {label="OP5 EG RATE 4", idx=114}
    ,Ctrl {label="OP5 EG LEVEL 1", idx=115}
    ,Ctrl {label="OP5 EG LEVEL 2", idx=116}
    ,Ctrl {label="OP5 EG LEVEL 3", idx=117}
    ,Ctrl {label="OP5 EG LEVEL 4", idx=118}
    ,Ctrl {label="OP5 OUTPUT LEVEL", idx=119}
    ,Ctrl {label="OP5 MODE", idx=120}
    ,Ctrl {label="OP5 F COARSE", idx=121}
    ,Ctrl {label="OP5 F FINE", idx=122}
    ,Ctrl {label="OP5 OSC DETUNE", idx=123}
    ,Ctrl {label="OP5 BREAK POINT", idx=124}
    ,Ctrl {label="OP5 L SCALE DEPTH", idx=125}
    ,Ctrl {label="OP5 R SCALE DEPTH", idx=126}
    ,Ctrl {label="OP5 L KEY SCALE", idx=127}
    ,Ctrl {label="OP5 R KEY SCALE", idx=128}
    ,Ctrl {label="OP5 RATE SCALING", idx=129}
    ,Ctrl {label="OP5 A MOD SENS.", idx=130}
    ,Ctrl {label="OP5 KEY VELOCITY", idx=131}
    ,Ctrl {label="OP5 SWITCH", idx=132} -- non-DX7
    ,Ctrl {label="OP6 EG RATE 1", idx=133}
    ,Ctrl {label="OP6 EG RATE 2", idx=134}
    ,Ctrl {label="OP6 EG RATE 3", idx=135}
    ,Ctrl {label="OP6 EG RATE 4", idx=136}
    ,Ctrl {label="OP6 EG LEVEL 1", idx=137}
    ,Ctrl {label="OP6 EG LEVEL 2", idx=138}
    ,Ctrl {label="OP6 EG LEVEL 3", idx=139}
    ,Ctrl {label="OP6 EG LEVEL 4", idx=140}
    ,Ctrl {label="OP6 OUTPUT LEVEL", idx=141}
    ,Ctrl {label="OP6 MODE", idx=142}
    ,Ctrl {label="OP6 F COARSE", idx=143}
    ,Ctrl {label="OP6 F FINE", idx=144}
    ,Ctrl {label="OP6 OSC DETUNE", idx=145}
    ,Ctrl {label="OP6 BREAK POINT", idx=146}
    ,Ctrl {label="OP6 L SCALE DEPTH", idx=147}
    ,Ctrl {label="OP6 R SCALE DEPTH", idx=148}
    ,Ctrl {label="OP6 L KEY SCALE", idx=149}
    ,Ctrl {label="OP6 R KEY SCALE", idx=150}
    ,Ctrl {label="OP6 RATE SCALING", idx=151}
    ,Ctrl {label="OP6 A MOD SENS.", idx=152}
    ,Ctrl {label="OP6 KEY VELOCITY", idx=153}
    ,Ctrl {label="OP6 SWITCH", idx=154} -- non-DX7
    ]

-- | The DEXED MASTER_TUNE_ADJ parameter maps input of (0,1) to cents re-tuning of (-50,+50).
--
-- > map dexed_master_tune_adj_cents [-100,-50,0,50,100] == [0,0,0.5,1,1]
dexed_master_tune_adj_cents :: (Ord p, Fractional p) => p -> p
dexed_master_tune_adj_cents x = if x < (-50) then 0 else if x > 50 then 1 else (x + 50) / 100

-- * CTRLDX

-- | Dexed data for DX7 parameter.
data CtrlDX =
  CtrlDX {name :: String -- ^ Name
         ,steps :: Int -- ^ Resolution
         ,offset :: Word8 -- ^ DX7 index offset (?)
         ,display_value :: Int -- ^ Display value
         }
  deriving (Eq,Show)

{- | Dexed DX7 parameters.

> > printf("CtrlDX {name=\"%s\", steps=%d, offset=%d, display_value=(%d)}\n",
> >   name.toStdString().c_str(),steps,offset,displayValue);

> length dexed_dx7_param == 145
> sort (map offset dexed_dx7_param) == [0 .. 144]

-}
dexed_dx7_param :: [CtrlDX]
dexed_dx7_param =
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

-- | Lookup 'dexed_dx7_param' by name.
--
-- > map (\c -> (idx c,dexed_dx7_param_by_name (label c))) dexed_param
dexed_dx7_param_by_name :: String -> Maybe CtrlDX
dexed_dx7_param_by_name nm = find ((== nm) . name) dexed_dx7_param

-- | DEXED parameter list in the form (DX7-IX,DEXED-IX,DX7-NAME).
--
-- > import qualified Sound.SC3.Data.Yamaha.DX7 as DX7
-- > mapM_ print $ zip dexed_dx7_param_dx7 DX7.dx7_parameter_tbl
dexed_dx7_param_dx7 :: [(Word8, Int, String)]
dexed_dx7_param_dx7 =
    let f (i,c) = case c of
                    Nothing -> Nothing
                    Just dx -> Just (offset dx,i,name dx)
        p = mapMaybe (\c -> f (idx c,dexed_dx7_param_by_name (label c))) dexed_param
    in sortOn (\(i,_,_) -> i) p

-- | Table from DX7 parameter index to DEXED parameter index.
--
-- > sort (map fst dx7_to_dexed_tbl) == [0 .. 144]
-- > sort (map snd dx7_to_dexed_tbl) == [4 .. 153] \\ [44,66,88,110,132]
dx7_to_dexed_tbl :: [(Word8, Int)]
dx7_to_dexed_tbl = map (\(i,j,_) -> (i,j)) dexed_dx7_param_dx7

-- | 'M.fromList' of 'dx7_to_dexed_tbl'.
dx7_to_dexed_map :: M.Map Word8 Int
dx7_to_dexed_map = M.fromList dx7_to_dexed_tbl

-- | 'M.lookup' of 'dx7_to_dexed_tbl', ie. given DX7 index lookup DEXED index.
--
-- > map dx7_to_dexed [123,125,134,144] == [33,35,4,13]
-- > dx7_to_dexed (DX7.dx7_parameter_index "ALGORITHM #") == 4
dx7_to_dexed :: Word8 -> Int
dx7_to_dexed = fromMaybe (error "dx7_to_dexed") . flip M.lookup dx7_to_dexed_map

-- | Reverse lookup of 'dx7_to_dexed_tbl', ie. given DEXED index lookup DX7 index.
--
-- > map dexed_to_dx7 [4]
dexed_to_dx7 :: Int -> Word8
dexed_to_dx7 x = fst (fromMaybe (error "dexed_to_dx7") (find ((== x) . snd) dx7_to_dexed_tbl))

-- * NON-DX7

-- | 'Ctrl' that are not DX7 parameters.
--
-- > length non_dx7_param == 10
-- > length dexed_param - length dexed_dx7_param == 10
non_dx7_param :: [Ctrl]
non_dx7_param =
    let f (c,dx) = case dx of
                    Nothing -> Just c
                    Just _ -> Nothing
    in mapMaybe (\c -> f (c,dexed_dx7_param_by_name (label c))) dexed_param

-- * INIT

-- | INIT data from <DexedAudioProcessor::resetToInitVoice>.
--   This differs from the dx7_init_data in one place (KBD LEV SCL BRK PT) for each operator.
--
-- > map DX7.dx7_parameter_name [8,29,50,71,92,113]
-- > dexed_init_voice DX7.dx7_init_voice
dexed_init_voice :: Num t => [t] -> [t]
dexed_init_voice =
  let brk_pt_set = take 6 [8,8 + 21 ..]
      f ix p = if ix `elem` brk_pt_set then 0 else p
  in zipWith f [0..]

{-

http://rd.slavepianos.org/sw/rju/cmd/jack-lxvst.cpp

import Sound.OSC.FD {- hosc -}
fd <- openUDP "127.0.0.1" 57210
let p_set ix val = sendMessage fd (Message "/param" [int32 ix,Float val])
p_set 3 0.5 -- MASTER TUNE ADJ
let p_set_dx7 ix = p_set (dx7_to_dexed ix)
p_set_dx7 123 1 -- OP 1 OSC FREQ COARSE
p_set_dx7 125 1 -- OP 1 OSC DETUNE
p_set_dx7 134 0.5 -- ALGORITHM #
p_set_dx7 144 0.5 -- TRANSPOSE

-}
