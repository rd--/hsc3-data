-- | <https://github.com/asb2m10/dexed>
module Sound.SC3.Data.Yamaha.DX7.Dexed where

import Data.List {- base -}
import Data.Maybe {- base -}
import Data.Word {- base -}

import qualified Data.Map as M {- containers -}

-- * CTRL VST / NON-DX7

-- | Dexed control, idx=index.
data Ctrl =
  Ctrl {label :: String, idx :: Int}
  deriving (Eq,Show)

-- | Lookup control sequence by label.
ctrl_by_label :: String -> [Ctrl] -> Maybe Int
ctrl_by_label nm = fmap idx . find ((== nm) . label)

{- | Dexed non-DX7 control parameters.

Dexed has 155 VST parameters, of which 145 are DX7 parameters and 10 are non-DX7 parameters.

The numbering and naming of the parameters is NOT EQUAL to the DX7 schema.

> length non_dx7_param == 10

> > for(i=0;i < ctrl.size(); i++) {
> >   printf("Ctrl {label=\"%s\", idx=%d}\n",
> >     ctrl[i]->label.toStdString().c_str(),
> >     ctrl[i]->idx);
> > }

-}
dexed_non_dx7_param :: [Ctrl]
dexed_non_dx7_param =
  [Ctrl {label="Cutoff", idx=0} -- non-DX7
  ,Ctrl {label="Resonance", idx=1} -- non-DX7
  ,Ctrl {label="Output", idx=2} -- non-DX7
  ,Ctrl {label="MASTER TUNE ADJ", idx=3} -- non-DX7
  ,Ctrl {label="OP1 SWITCH", idx=44} -- non-DX7
  ,Ctrl {label="OP2 SWITCH", idx=66} -- non-DX7
  ,Ctrl {label="OP3 SWITCH", idx=88} -- non-DX7
  ,Ctrl {label="OP4 SWITCH", idx=110} -- non-DX7
  ,Ctrl {label="OP5 SWITCH", idx=132} -- non-DX7
  ,Ctrl {label="OP6 SWITCH", idx=154} -- non-DX7
  ]

-- | Lookup DEXED index for non-DX7 parameter by name.
dexed_non_dx7_param_by_name :: String -> Maybe Int
dexed_non_dx7_param_by_name nm = ctrl_by_label nm dexed_non_dx7_param

-- | Erroring variant.
--
-- > dexed_non_dx7_param_by_name_err "MASTER TUNE ADJ"
dexed_non_dx7_param_by_name_err :: String -> Int
dexed_non_dx7_param_by_name_err = fromMaybe (error "non_dx7_param?") . dexed_non_dx7_param_by_name

-- | Dexed default values for NON-DX7 parameters, as (IX,N) pairs.
--
-- > length non_dx7_defaults == 10
non_dx7_defaults :: [(Int, Double)]
non_dx7_defaults = [(0,1),(1,0),(2,1),(3,0.5),(44,1),(66,1),(88,1),(110,1),(132,1),(154,1)]

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
         ,dx7_ix :: Word8 -- ^ DX7 parameter number
         ,display_value :: Int -- ^ Display value
         ,dexed_ix :: Int -- ^ DEXED VST parameter index
         }
  deriving (Eq,Show)

{- | Dexed DX7 parameters.

> > printf("CtrlDX {name=\"%s\", steps=%d, dx7_ix=%d, display_value=(%d)}\n",
> >   name.toStdString().c_str(),steps,dx7_ix,displayValue);

> length dexed_dx7_param == 145
> sort (map dx7_ix dexed_dx7_param) == [0 .. 144]

-}
dexed_dx7_param :: [CtrlDX]
dexed_dx7_param =
  [CtrlDX {name = "ALGORITHM", steps = 32, dx7_ix = 134, display_value = 1, dexed_ix = 4}
  ,CtrlDX {name = "FEEDBACK", steps = 8, dx7_ix = 135, display_value = 0, dexed_ix = 5}
  ,CtrlDX {name = "OSC KEY SYNC", steps = 2, dx7_ix = 136, display_value = 0, dexed_ix = 6}
  ,CtrlDX {name = "LFO SPEED", steps = 100, dx7_ix = 137, display_value = 0, dexed_ix = 7}
  ,CtrlDX {name = "LFO DELAY", steps = 100, dx7_ix = 138, display_value = 0, dexed_ix = 8}
  ,CtrlDX {name = "LFO PM DEPTH", steps = 100, dx7_ix = 139, display_value = 0, dexed_ix = 9}
  ,CtrlDX {name = "LFO AM DEPTH", steps = 100, dx7_ix = 140, display_value = 0, dexed_ix = 10}
  ,CtrlDX {name = "LFO KEY SYNC", steps = 2, dx7_ix = 141, display_value = 0, dexed_ix = 11}
  ,CtrlDX {name = "LFO WAVE", steps = 6, dx7_ix = 142, display_value = 0, dexed_ix = 12}
  ,CtrlDX {name = "MIDDLE C", steps = 49, dx7_ix = 144, display_value = 0, dexed_ix = 13}
  ,CtrlDX {name = "P MODE SENS.", steps = 8, dx7_ix = 143, display_value = 0, dexed_ix = 14}
  ,CtrlDX {name = "PITCH EG RATE 1", steps = 100, dx7_ix = 126, display_value = 0, dexed_ix = 15}
  ,CtrlDX {name = "PITCH EG RATE 2", steps = 100, dx7_ix = 127, display_value = 0, dexed_ix = 16}
  ,CtrlDX {name = "PITCH EG RATE 3", steps = 100, dx7_ix = 128, display_value = 0, dexed_ix = 17}
  ,CtrlDX {name = "PITCH EG RATE 4", steps = 100, dx7_ix = 129, display_value = 0, dexed_ix = 18}
  ,CtrlDX {name = "PITCH EG LEVEL 1", steps = 100, dx7_ix = 130, display_value = 0, dexed_ix = 19}
  ,CtrlDX {name = "PITCH EG LEVEL 2", steps = 100, dx7_ix = 131, display_value = 0, dexed_ix = 20}
  ,CtrlDX {name = "PITCH EG LEVEL 3", steps = 100, dx7_ix = 132, display_value = 0, dexed_ix = 21}
  ,CtrlDX {name = "PITCH EG LEVEL 4", steps = 100, dx7_ix = 133, display_value = 0, dexed_ix = 22}
  ,CtrlDX {name = "OP1 EG RATE 1", steps = 100, dx7_ix = 105, display_value = 0, dexed_ix = 23}
  ,CtrlDX {name = "OP1 EG RATE 2", steps = 100, dx7_ix = 106, display_value = 0, dexed_ix = 24}
  ,CtrlDX {name = "OP1 EG RATE 3", steps = 100, dx7_ix = 107, display_value = 0, dexed_ix = 25}
  ,CtrlDX {name = "OP1 EG RATE 4", steps = 100, dx7_ix = 108, display_value = 0, dexed_ix = 26}
  ,CtrlDX {name = "OP1 EG LEVEL 1", steps = 100, dx7_ix = 109, display_value = 0, dexed_ix = 27}
  ,CtrlDX {name = "OP1 EG LEVEL 2", steps = 100, dx7_ix = 110, display_value = 0, dexed_ix = 28}
  ,CtrlDX {name = "OP1 EG LEVEL 3", steps = 100, dx7_ix = 111, display_value = 0, dexed_ix = 29}
  ,CtrlDX {name = "OP1 EG LEVEL 4", steps = 100, dx7_ix = 112, display_value = 0, dexed_ix = 30}
  ,CtrlDX {name = "OP1 OUTPUT LEVEL", steps = 100, dx7_ix = 121, display_value = 0, dexed_ix = 31}
  ,CtrlDX {name = "OP1 MODE", steps = 2, dx7_ix = 122, display_value = 0, dexed_ix = 32}
  ,CtrlDX {name = "OP1 F COARSE", steps = 32, dx7_ix = 123, display_value = 0, dexed_ix = 33}
  ,CtrlDX {name = "OP1 F FINE", steps = 100, dx7_ix = 124, display_value = 0, dexed_ix = 34}
  ,CtrlDX {name = "OP1 OSC DETUNE", steps = 15, dx7_ix = 125, display_value = -7, dexed_ix = 35}
  ,CtrlDX {name = "OP1 BREAK POINT", steps = 100, dx7_ix = 113, display_value = 0, dexed_ix = 36}
  ,CtrlDX {name = "OP1 L SCALE DEPTH", steps = 100, dx7_ix = 114, display_value = 0, dexed_ix = 37}
  ,CtrlDX {name = "OP1 R SCALE DEPTH", steps = 100, dx7_ix = 115, display_value = 0, dexed_ix = 38}
  ,CtrlDX {name = "OP1 L KEY SCALE", steps = 4, dx7_ix = 116, display_value = 0, dexed_ix = 39}
  ,CtrlDX {name = "OP1 R KEY SCALE", steps = 4, dx7_ix = 117, display_value = 0, dexed_ix = 40}
  ,CtrlDX {name = "OP1 RATE SCALING", steps = 8, dx7_ix = 118, display_value = 0, dexed_ix = 41}
  ,CtrlDX {name = "OP1 A MOD SENS.", steps = 4, dx7_ix = 119, display_value = 0, dexed_ix = 42}
  ,CtrlDX {name = "OP1 KEY VELOCITY", steps = 8, dx7_ix = 120, display_value = 0, dexed_ix = 43}
  ,CtrlDX {name = "OP2 EG RATE 1", steps = 100, dx7_ix = 84, display_value = 0, dexed_ix = 45}
  ,CtrlDX {name = "OP2 EG RATE 2", steps = 100, dx7_ix = 85, display_value = 0, dexed_ix = 46}
  ,CtrlDX {name = "OP2 EG RATE 3", steps = 100, dx7_ix = 86, display_value = 0, dexed_ix = 47}
  ,CtrlDX {name = "OP2 EG RATE 4", steps = 100, dx7_ix = 87, display_value = 0, dexed_ix = 48}
  ,CtrlDX {name = "OP2 EG LEVEL 1", steps = 100, dx7_ix = 88, display_value = 0, dexed_ix = 49}
  ,CtrlDX {name = "OP2 EG LEVEL 2", steps = 100, dx7_ix = 89, display_value = 0, dexed_ix = 50}
  ,CtrlDX {name = "OP2 EG LEVEL 3", steps = 100, dx7_ix = 90, display_value = 0, dexed_ix = 51}
  ,CtrlDX {name = "OP2 EG LEVEL 4", steps = 100, dx7_ix = 91, display_value = 0, dexed_ix = 52}
  ,CtrlDX {name = "OP2 OUTPUT LEVEL", steps = 100, dx7_ix = 100, display_value = 0, dexed_ix = 53}
  ,CtrlDX {name = "OP2 MODE", steps = 2, dx7_ix = 101, display_value = 0, dexed_ix = 54}
  ,CtrlDX {name = "OP2 F COARSE", steps = 32, dx7_ix = 102, display_value = 0, dexed_ix = 55}
  ,CtrlDX {name = "OP2 F FINE", steps = 100, dx7_ix = 103, display_value = 0, dexed_ix = 56}
  ,CtrlDX {name = "OP2 OSC DETUNE", steps = 15, dx7_ix = 104, display_value = -7, dexed_ix = 57}
  ,CtrlDX {name = "OP2 BREAK POINT", steps = 100, dx7_ix = 92, display_value = 0, dexed_ix = 58}
  ,CtrlDX {name = "OP2 L SCALE DEPTH", steps = 100, dx7_ix = 93, display_value = 0, dexed_ix = 59}
  ,CtrlDX {name = "OP2 R SCALE DEPTH", steps = 100, dx7_ix = 94, display_value = 0, dexed_ix = 60}
  ,CtrlDX {name = "OP2 L KEY SCALE", steps = 4, dx7_ix = 95, display_value = 0, dexed_ix = 61}
  ,CtrlDX {name = "OP2 R KEY SCALE", steps = 4, dx7_ix = 96, display_value = 0, dexed_ix = 62}
  ,CtrlDX {name = "OP2 RATE SCALING", steps = 8, dx7_ix = 97, display_value = 0, dexed_ix = 63}
  ,CtrlDX {name = "OP2 A MOD SENS.", steps = 4, dx7_ix = 98, display_value = 0, dexed_ix = 64}
  ,CtrlDX {name = "OP2 KEY VELOCITY", steps = 8, dx7_ix = 99, display_value = 0, dexed_ix = 65}
  ,CtrlDX {name = "OP3 EG RATE 1", steps = 100, dx7_ix = 63, display_value = 0, dexed_ix = 67}
  ,CtrlDX {name = "OP3 EG RATE 2", steps = 100, dx7_ix = 64, display_value = 0, dexed_ix = 68}
  ,CtrlDX {name = "OP3 EG RATE 3", steps = 100, dx7_ix = 65, display_value = 0, dexed_ix = 69}
  ,CtrlDX {name = "OP3 EG RATE 4", steps = 100, dx7_ix = 66, display_value = 0, dexed_ix = 70}
  ,CtrlDX {name = "OP3 EG LEVEL 1", steps = 100, dx7_ix = 67, display_value = 0, dexed_ix = 71}
  ,CtrlDX {name = "OP3 EG LEVEL 2", steps = 100, dx7_ix = 68, display_value = 0, dexed_ix = 72}
  ,CtrlDX {name = "OP3 EG LEVEL 3", steps = 100, dx7_ix = 69, display_value = 0, dexed_ix = 73}
  ,CtrlDX {name = "OP3 EG LEVEL 4", steps = 100, dx7_ix = 70, display_value = 0, dexed_ix = 74}
  ,CtrlDX {name = "OP3 OUTPUT LEVEL", steps = 100, dx7_ix = 79, display_value = 0, dexed_ix = 75}
  ,CtrlDX {name = "OP3 MODE", steps = 2, dx7_ix = 80, display_value = 0, dexed_ix = 76}
  ,CtrlDX {name = "OP3 F COARSE", steps = 32, dx7_ix = 81, display_value = 0, dexed_ix = 77}
  ,CtrlDX {name = "OP3 F FINE", steps = 100, dx7_ix = 82, display_value = 0, dexed_ix = 78}
  ,CtrlDX {name = "OP3 OSC DETUNE", steps = 15, dx7_ix = 83, display_value = -7, dexed_ix = 79}
  ,CtrlDX {name = "OP3 BREAK POINT", steps = 100, dx7_ix = 71, display_value = 0, dexed_ix = 80}
  ,CtrlDX {name = "OP3 L SCALE DEPTH", steps = 100, dx7_ix = 72, display_value = 0, dexed_ix = 81}
  ,CtrlDX {name = "OP3 R SCALE DEPTH", steps = 100, dx7_ix = 73, display_value = 0, dexed_ix = 82}
  ,CtrlDX {name = "OP3 L KEY SCALE", steps = 4, dx7_ix = 74, display_value = 0, dexed_ix = 83}
  ,CtrlDX {name = "OP3 R KEY SCALE", steps = 4, dx7_ix = 75, display_value = 0, dexed_ix = 84}
  ,CtrlDX {name = "OP3 RATE SCALING", steps = 8, dx7_ix = 76, display_value = 0, dexed_ix = 85}
  ,CtrlDX {name = "OP3 A MOD SENS.", steps = 4, dx7_ix = 77, display_value = 0, dexed_ix = 86}
  ,CtrlDX {name = "OP3 KEY VELOCITY", steps = 8, dx7_ix = 78, display_value = 0, dexed_ix = 87}
  ,CtrlDX {name = "OP4 EG RATE 1", steps = 100, dx7_ix = 42, display_value = 0, dexed_ix = 89}
  ,CtrlDX {name = "OP4 EG RATE 2", steps = 100, dx7_ix = 43, display_value = 0, dexed_ix = 90}
  ,CtrlDX {name = "OP4 EG RATE 3", steps = 100, dx7_ix = 44, display_value = 0, dexed_ix = 91}
  ,CtrlDX {name = "OP4 EG RATE 4", steps = 100, dx7_ix = 45, display_value = 0, dexed_ix = 92}
  ,CtrlDX {name = "OP4 EG LEVEL 1", steps = 100, dx7_ix = 46, display_value = 0, dexed_ix = 93}
  ,CtrlDX {name = "OP4 EG LEVEL 2", steps = 100, dx7_ix = 47, display_value = 0, dexed_ix = 94}
  ,CtrlDX {name = "OP4 EG LEVEL 3", steps = 100, dx7_ix = 48, display_value = 0, dexed_ix = 95}
  ,CtrlDX {name = "OP4 EG LEVEL 4", steps = 100, dx7_ix = 49, display_value = 0, dexed_ix = 96}
  ,CtrlDX {name = "OP4 OUTPUT LEVEL", steps = 100, dx7_ix = 58, display_value = 0, dexed_ix = 97}
  ,CtrlDX {name = "OP4 MODE", steps = 2, dx7_ix = 59, display_value = 0, dexed_ix = 98}
  ,CtrlDX {name = "OP4 F COARSE", steps = 32, dx7_ix = 60, display_value = 0, dexed_ix = 99}
  ,CtrlDX {name = "OP4 F FINE", steps = 100, dx7_ix = 61, display_value = 0, dexed_ix = 100}
  ,CtrlDX {name = "OP4 OSC DETUNE", steps = 15, dx7_ix = 62, display_value = -7, dexed_ix = 101}
  ,CtrlDX {name = "OP4 BREAK POINT", steps = 100, dx7_ix = 50, display_value = 0, dexed_ix = 102}
  ,CtrlDX {name = "OP4 L SCALE DEPTH", steps = 100, dx7_ix = 51, display_value = 0, dexed_ix = 103}
  ,CtrlDX {name = "OP4 R SCALE DEPTH", steps = 100, dx7_ix = 52, display_value = 0, dexed_ix = 104}
  ,CtrlDX {name = "OP4 L KEY SCALE", steps = 4, dx7_ix = 53, display_value = 0, dexed_ix = 105}
  ,CtrlDX {name = "OP4 R KEY SCALE", steps = 4, dx7_ix = 54, display_value = 0, dexed_ix = 106}
  ,CtrlDX {name = "OP4 RATE SCALING", steps = 8, dx7_ix = 55, display_value = 0, dexed_ix = 107}
  ,CtrlDX {name = "OP4 A MOD SENS.", steps = 4, dx7_ix = 56, display_value = 0, dexed_ix = 108}
  ,CtrlDX {name = "OP4 KEY VELOCITY", steps = 8, dx7_ix = 57, display_value = 0, dexed_ix = 109}
  ,CtrlDX {name = "OP5 EG RATE 1", steps = 100, dx7_ix = 21, display_value = 0, dexed_ix = 111}
  ,CtrlDX {name = "OP5 EG RATE 2", steps = 100, dx7_ix = 22, display_value = 0, dexed_ix = 112}
  ,CtrlDX {name = "OP5 EG RATE 3", steps = 100, dx7_ix = 23, display_value = 0, dexed_ix = 113}
  ,CtrlDX {name = "OP5 EG RATE 4", steps = 100, dx7_ix = 24, display_value = 0, dexed_ix = 114}
  ,CtrlDX {name = "OP5 EG LEVEL 1", steps = 100, dx7_ix = 25, display_value = 0, dexed_ix = 115}
  ,CtrlDX {name = "OP5 EG LEVEL 2", steps = 100, dx7_ix = 26, display_value = 0, dexed_ix = 116}
  ,CtrlDX {name = "OP5 EG LEVEL 3", steps = 100, dx7_ix = 27, display_value = 0, dexed_ix = 117}
  ,CtrlDX {name = "OP5 EG LEVEL 4", steps = 100, dx7_ix = 28, display_value = 0, dexed_ix = 118}
  ,CtrlDX {name = "OP5 OUTPUT LEVEL", steps = 100, dx7_ix = 37, display_value = 0, dexed_ix = 119}
  ,CtrlDX {name = "OP5 MODE", steps = 2, dx7_ix = 38, display_value = 0, dexed_ix = 120}
  ,CtrlDX {name = "OP5 F COARSE", steps = 32, dx7_ix = 39, display_value = 0, dexed_ix = 121}
  ,CtrlDX {name = "OP5 F FINE", steps = 100, dx7_ix = 40, display_value = 0, dexed_ix = 122}
  ,CtrlDX {name = "OP5 OSC DETUNE", steps = 15, dx7_ix = 41, display_value = -7, dexed_ix = 123}
  ,CtrlDX {name = "OP5 BREAK POINT", steps = 100, dx7_ix = 29, display_value = 0, dexed_ix = 124}
  ,CtrlDX {name = "OP5 L SCALE DEPTH", steps = 100, dx7_ix = 30, display_value = 0, dexed_ix = 125}
  ,CtrlDX {name = "OP5 R SCALE DEPTH", steps = 100, dx7_ix = 31, display_value = 0, dexed_ix = 126}
  ,CtrlDX {name = "OP5 L KEY SCALE", steps = 4, dx7_ix = 32, display_value = 0, dexed_ix = 127}
  ,CtrlDX {name = "OP5 R KEY SCALE", steps = 4, dx7_ix = 33, display_value = 0, dexed_ix = 128}
  ,CtrlDX {name = "OP5 RATE SCALING", steps = 8, dx7_ix = 34, display_value = 0, dexed_ix = 129}
  ,CtrlDX {name = "OP5 A MOD SENS.", steps = 4, dx7_ix = 35, display_value = 0, dexed_ix = 130}
  ,CtrlDX {name = "OP5 KEY VELOCITY", steps = 8, dx7_ix = 36, display_value = 0, dexed_ix = 131}
  ,CtrlDX {name = "OP6 EG RATE 1", steps = 100, dx7_ix = 0, display_value = 0, dexed_ix = 133}
  ,CtrlDX {name = "OP6 EG RATE 2", steps = 100, dx7_ix = 1, display_value = 0, dexed_ix = 134}
  ,CtrlDX {name = "OP6 EG RATE 3", steps = 100, dx7_ix = 2, display_value = 0, dexed_ix = 135}
  ,CtrlDX {name = "OP6 EG RATE 4", steps = 100, dx7_ix = 3, display_value = 0, dexed_ix = 136}
  ,CtrlDX {name = "OP6 EG LEVEL 1", steps = 100, dx7_ix = 4, display_value = 0, dexed_ix = 137}
  ,CtrlDX {name = "OP6 EG LEVEL 2", steps = 100, dx7_ix = 5, display_value = 0, dexed_ix = 138}
  ,CtrlDX {name = "OP6 EG LEVEL 3", steps = 100, dx7_ix = 6, display_value = 0, dexed_ix = 139}
  ,CtrlDX {name = "OP6 EG LEVEL 4", steps = 100, dx7_ix = 7, display_value = 0, dexed_ix = 140}
  ,CtrlDX {name = "OP6 OUTPUT LEVEL", steps = 100, dx7_ix = 16, display_value = 0, dexed_ix = 141}
  ,CtrlDX {name = "OP6 MODE", steps = 2, dx7_ix = 17, display_value = 0, dexed_ix = 142}
  ,CtrlDX {name = "OP6 F COARSE", steps = 32, dx7_ix = 18, display_value = 0, dexed_ix = 143}
  ,CtrlDX {name = "OP6 F FINE", steps = 100, dx7_ix = 19, display_value = 0, dexed_ix = 144}
  ,CtrlDX {name = "OP6 OSC DETUNE", steps = 15, dx7_ix = 20, display_value = -7, dexed_ix = 145}
  ,CtrlDX {name = "OP6 BREAK POINT", steps = 100, dx7_ix = 8, display_value = 0, dexed_ix = 146}
  ,CtrlDX {name = "OP6 L SCALE DEPTH", steps = 100, dx7_ix = 9, display_value = 0, dexed_ix = 147}
  ,CtrlDX {name = "OP6 R SCALE DEPTH", steps = 100, dx7_ix = 10, display_value = 0, dexed_ix = 148}
  ,CtrlDX {name = "OP6 L KEY SCALE", steps = 4, dx7_ix = 11, display_value = 0, dexed_ix = 149}
  ,CtrlDX {name = "OP6 R KEY SCALE", steps = 4, dx7_ix = 12, display_value = 0, dexed_ix = 150}
  ,CtrlDX {name = "OP6 RATE SCALING", steps = 8, dx7_ix = 13, display_value = 0, dexed_ix = 151}
  ,CtrlDX {name = "OP6 A MOD SENS.", steps = 4, dx7_ix = 14, display_value = 0, dexed_ix = 152}
  ,CtrlDX {name = "OP6 KEY VELOCITY", steps = 8, dx7_ix = 15, display_value = 0, dexed_ix = 153}]

-- | Lookup 'dexed_dx7_param' by name.
--
-- > dexed_dx7_param_by_name "MIDDLE C"
dexed_dx7_param_by_name :: String -> Maybe CtrlDX
dexed_dx7_param_by_name nm = find ((== nm) . name) dexed_dx7_param

-- | DEXED parameter list in the form (DX7-IX,DEXED-IX,DX7-NAME).
--
-- > import qualified Sound.SC3.Data.Yamaha.DX7 as DX7
-- > mapM_ print $ zip dexed_dx7_param_dx7 DX7.dx7_parameter_tbl
dexed_dx7_param_dx7 :: [(Word8, Int, String)]
dexed_dx7_param_dx7 = sort (map (\c -> (dx7_ix c,dexed_ix c,name c)) dexed_dx7_param)

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
-- > map dexed_to_dx7 [4] == [134]
dexed_to_dx7 :: Int -> Word8
dexed_to_dx7 x = fst (fromMaybe (error "dexed_to_dx7") (find ((== x) . snd) dx7_to_dexed_tbl))

-- * TRANSLATE

-- | Translate 145 element DX7 parameter sequence to 155 element DEXED sequence.
dx7_voice_to_dexed_param :: [Word8] -> [Double]
dx7_voice_to_dexed_param vc =
  let word8_to_double = fromIntegral
      u8_at l n = l !! (fromIntegral n)
      f k = case lookup k non_dx7_defaults of
              Just v -> v
              Nothing -> word8_to_double (vc `u8_at` dexed_to_dx7 k) / 99.0
  in map f [0 .. 154]

-- * INIT

-- | INIT data from <DexedAudioProcessor::resetToInitVoice>.
--   This differs from the dx7_init_data in one place (KBD LEV SCL BRK PT) for each operator.
--
-- > map DX7.dx7_parameter_name [8,29,50,71,92,113]
-- > dexed_init_voice DX7.dx7_init_voice
dexed_init_voice :: Num t => [t] -> [t]
dexed_init_voice =
  let brk_pt_set = take 6 [8,8 + 21 ..]
      f k p = if k `elem` brk_pt_set then 0 else p
  in zipWith f [0::Int ..]

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
