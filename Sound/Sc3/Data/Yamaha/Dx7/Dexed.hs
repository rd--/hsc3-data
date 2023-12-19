-- | <https://github.com/asb2m10/dexed>
module Sound.Sc3.Data.Yamaha.Dx7.Dexed where

import Data.List {- base -}
import Data.Maybe {- base -}
import Data.Word {- base -}

import qualified Data.Map as M {- containers -}

-- * Ctrl Vst / Non-Dx7

-- | Dexed control, idx=index.
data Ctrl = Ctrl {label :: String, idx :: Int}
  deriving (Eq, Show)

-- | Lookup control sequence by label.
ctrl_by_label :: String -> [Ctrl] -> Maybe Int
ctrl_by_label nm = fmap idx . find ((== nm) . label)

{- | Dexed non-Dx7 control parameters.

Dexed has 155 VST parameters, of which 145 are Dx7 parameters and 10 are non-Dx7 parameters.

The numbering and naming of the parameters is NOT EQUAL to the Dx7 schema.

> length non_dx7_param == 10

> > for(i=0;i < ctrl.size(); i++) {
> >   printf("Ctrl {label=\"%s\", idx=%d}\n",
> >     ctrl[i]->label.toStdString().c_str(),
> >     ctrl[i]->idx);
> > }
-}
dexed_non_dx7_param :: [Ctrl]
dexed_non_dx7_param =
  [ Ctrl {label = "Cutoff", idx = 0} -- non-Dx7
  , Ctrl {label = "Resonance", idx = 1} -- non-Dx7
  , Ctrl {label = "Output", idx = 2} -- non-Dx7 -- VOL
  , Ctrl {label = "MASTER TUNE ADJ", idx = 3} -- non-Dx7
  , Ctrl {label = "OP1 SWITCH", idx = 44} -- non-Dx7
  , Ctrl {label = "OP2 SWITCH", idx = 66} -- non-Dx7
  , Ctrl {label = "OP3 SWITCH", idx = 88} -- non-Dx7
  , Ctrl {label = "OP4 SWITCH", idx = 110} -- non-Dx7
  , Ctrl {label = "OP5 SWITCH", idx = 132} -- non-Dx7
  , Ctrl {label = "OP6 SWITCH", idx = 154} -- non-Dx7
  ]

-- | Lookup Dexed index for non-Dx7 parameter by name.
dexed_non_dx7_param_by_name :: String -> Maybe Int
dexed_non_dx7_param_by_name nm = ctrl_by_label nm dexed_non_dx7_param

{- | Erroring variant.

> dexed_non_dx7_param_by_name_err "MASTER TUNE ADJ"
-}
dexed_non_dx7_param_by_name_err :: String -> Int
dexed_non_dx7_param_by_name_err = fromMaybe (error "non_dx7_param?") . dexed_non_dx7_param_by_name

{- | Dexed default values for NON-Dx7 parameters, as (IX,N) pairs.

> length non_dx7_defaults == 10
-}
non_dx7_defaults :: [(Int, Double)]
non_dx7_defaults = [(0, 1), (1, 0), (2, 1), (3, 0.5), (44, 1), (66, 1), (88, 1), (110, 1), (132, 1), (154, 1)]

{- | The Dexed MASTER_TUNE_ADJ parameter maps input of (0,1) to cents re-tuning of (-50,+50).

> map dexed_master_tune_adj_cents [-100,-50,0,50,100] == [0,0,0.5,1,1]
-}
dexed_master_tune_adj_cents :: (Ord p, Fractional p) => p -> p
dexed_master_tune_adj_cents x = if x < (-50) then 0 else if x > 50 then 1 else (x + 50) / 100

-- * CtrlDx

-- | Dexed data for Dx7 parameter.
data CtrlDx = CtrlDx
  { name :: String
  -- ^ Name
  , steps :: Int
  -- ^ Resolution
  , dx7_ix :: Word8
  -- ^ Dx7 parameter number
  , display_value :: Int
  -- ^ Display value
  , dexed_ix :: Int
  -- ^ Dexed VST parameter index
  }
  deriving (Eq, Show)

{- | Dexed Dx7 parameters.

> > printf("CtrlDx {name=\"%s\", steps=%d, dx7_ix=%d, display_value=(%d)}\n",
> >   name.toStdString().c_str(),steps,dx7_ix,displayValue);

> length dexed_dx7_param == 145
> sort (map dx7_ix dexed_dx7_param) == [0 .. 144]
-}
dexed_dx7_param :: [CtrlDx]
dexed_dx7_param =
  [ CtrlDx {name = "ALGORITHM", steps = 32, dx7_ix = 134, display_value = 1, dexed_ix = 4}
  , CtrlDx {name = "FEEDBACK", steps = 8, dx7_ix = 135, display_value = 0, dexed_ix = 5}
  , CtrlDx {name = "OSC KEY SYNC", steps = 2, dx7_ix = 136, display_value = 0, dexed_ix = 6}
  , CtrlDx {name = "LFO SPEED", steps = 100, dx7_ix = 137, display_value = 0, dexed_ix = 7}
  , CtrlDx {name = "LFO DELAY", steps = 100, dx7_ix = 138, display_value = 0, dexed_ix = 8}
  , CtrlDx {name = "LFO PM DEPTH", steps = 100, dx7_ix = 139, display_value = 0, dexed_ix = 9}
  , CtrlDx {name = "LFO AM DEPTH", steps = 100, dx7_ix = 140, display_value = 0, dexed_ix = 10}
  , CtrlDx {name = "LFO KEY SYNC", steps = 2, dx7_ix = 141, display_value = 0, dexed_ix = 11}
  , CtrlDx {name = "LFO WAVE", steps = 6, dx7_ix = 142, display_value = 0, dexed_ix = 12}
  , CtrlDx {name = "MIDDLE C", steps = 49, dx7_ix = 144, display_value = 0, dexed_ix = 13}
  , CtrlDx {name = "P MODE SENS.", steps = 8, dx7_ix = 143, display_value = 0, dexed_ix = 14}
  , CtrlDx {name = "PITCH EG RATE 1", steps = 100, dx7_ix = 126, display_value = 0, dexed_ix = 15}
  , CtrlDx {name = "PITCH EG RATE 2", steps = 100, dx7_ix = 127, display_value = 0, dexed_ix = 16}
  , CtrlDx {name = "PITCH EG RATE 3", steps = 100, dx7_ix = 128, display_value = 0, dexed_ix = 17}
  , CtrlDx {name = "PITCH EG RATE 4", steps = 100, dx7_ix = 129, display_value = 0, dexed_ix = 18}
  , CtrlDx {name = "PITCH EG LEVEL 1", steps = 100, dx7_ix = 130, display_value = 0, dexed_ix = 19}
  , CtrlDx {name = "PITCH EG LEVEL 2", steps = 100, dx7_ix = 131, display_value = 0, dexed_ix = 20}
  , CtrlDx {name = "PITCH EG LEVEL 3", steps = 100, dx7_ix = 132, display_value = 0, dexed_ix = 21}
  , CtrlDx {name = "PITCH EG LEVEL 4", steps = 100, dx7_ix = 133, display_value = 0, dexed_ix = 22}
  , CtrlDx {name = "OP1 EG RATE 1", steps = 100, dx7_ix = 105, display_value = 0, dexed_ix = 23}
  , CtrlDx {name = "OP1 EG RATE 2", steps = 100, dx7_ix = 106, display_value = 0, dexed_ix = 24}
  , CtrlDx {name = "OP1 EG RATE 3", steps = 100, dx7_ix = 107, display_value = 0, dexed_ix = 25}
  , CtrlDx {name = "OP1 EG RATE 4", steps = 100, dx7_ix = 108, display_value = 0, dexed_ix = 26}
  , CtrlDx {name = "OP1 EG LEVEL 1", steps = 100, dx7_ix = 109, display_value = 0, dexed_ix = 27}
  , CtrlDx {name = "OP1 EG LEVEL 2", steps = 100, dx7_ix = 110, display_value = 0, dexed_ix = 28}
  , CtrlDx {name = "OP1 EG LEVEL 3", steps = 100, dx7_ix = 111, display_value = 0, dexed_ix = 29}
  , CtrlDx {name = "OP1 EG LEVEL 4", steps = 100, dx7_ix = 112, display_value = 0, dexed_ix = 30}
  , CtrlDx {name = "OP1 OUTPUT LEVEL", steps = 100, dx7_ix = 121, display_value = 0, dexed_ix = 31}
  , CtrlDx {name = "OP1 MODE", steps = 2, dx7_ix = 122, display_value = 0, dexed_ix = 32}
  , CtrlDx {name = "OP1 F COARSE", steps = 32, dx7_ix = 123, display_value = 0, dexed_ix = 33}
  , CtrlDx {name = "OP1 F FINE", steps = 100, dx7_ix = 124, display_value = 0, dexed_ix = 34}
  , CtrlDx {name = "OP1 OSC DETUNE", steps = 15, dx7_ix = 125, display_value = -7, dexed_ix = 35}
  , CtrlDx {name = "OP1 BREAK POINT", steps = 100, dx7_ix = 113, display_value = 0, dexed_ix = 36}
  , CtrlDx {name = "OP1 L SCALE DEPTH", steps = 100, dx7_ix = 114, display_value = 0, dexed_ix = 37}
  , CtrlDx {name = "OP1 R SCALE DEPTH", steps = 100, dx7_ix = 115, display_value = 0, dexed_ix = 38}
  , CtrlDx {name = "OP1 L KEY SCALE", steps = 4, dx7_ix = 116, display_value = 0, dexed_ix = 39}
  , CtrlDx {name = "OP1 R KEY SCALE", steps = 4, dx7_ix = 117, display_value = 0, dexed_ix = 40}
  , CtrlDx {name = "OP1 RATE SCALING", steps = 8, dx7_ix = 118, display_value = 0, dexed_ix = 41}
  , CtrlDx {name = "OP1 A MOD SENS.", steps = 4, dx7_ix = 119, display_value = 0, dexed_ix = 42}
  , CtrlDx {name = "OP1 KEY VELOCITY", steps = 8, dx7_ix = 120, display_value = 0, dexed_ix = 43}
  , CtrlDx {name = "OP2 EG RATE 1", steps = 100, dx7_ix = 84, display_value = 0, dexed_ix = 45}
  , CtrlDx {name = "OP2 EG RATE 2", steps = 100, dx7_ix = 85, display_value = 0, dexed_ix = 46}
  , CtrlDx {name = "OP2 EG RATE 3", steps = 100, dx7_ix = 86, display_value = 0, dexed_ix = 47}
  , CtrlDx {name = "OP2 EG RATE 4", steps = 100, dx7_ix = 87, display_value = 0, dexed_ix = 48}
  , CtrlDx {name = "OP2 EG LEVEL 1", steps = 100, dx7_ix = 88, display_value = 0, dexed_ix = 49}
  , CtrlDx {name = "OP2 EG LEVEL 2", steps = 100, dx7_ix = 89, display_value = 0, dexed_ix = 50}
  , CtrlDx {name = "OP2 EG LEVEL 3", steps = 100, dx7_ix = 90, display_value = 0, dexed_ix = 51}
  , CtrlDx {name = "OP2 EG LEVEL 4", steps = 100, dx7_ix = 91, display_value = 0, dexed_ix = 52}
  , CtrlDx {name = "OP2 OUTPUT LEVEL", steps = 100, dx7_ix = 100, display_value = 0, dexed_ix = 53}
  , CtrlDx {name = "OP2 MODE", steps = 2, dx7_ix = 101, display_value = 0, dexed_ix = 54}
  , CtrlDx {name = "OP2 F COARSE", steps = 32, dx7_ix = 102, display_value = 0, dexed_ix = 55}
  , CtrlDx {name = "OP2 F FINE", steps = 100, dx7_ix = 103, display_value = 0, dexed_ix = 56}
  , CtrlDx {name = "OP2 OSC DETUNE", steps = 15, dx7_ix = 104, display_value = -7, dexed_ix = 57}
  , CtrlDx {name = "OP2 BREAK POINT", steps = 100, dx7_ix = 92, display_value = 0, dexed_ix = 58}
  , CtrlDx {name = "OP2 L SCALE DEPTH", steps = 100, dx7_ix = 93, display_value = 0, dexed_ix = 59}
  , CtrlDx {name = "OP2 R SCALE DEPTH", steps = 100, dx7_ix = 94, display_value = 0, dexed_ix = 60}
  , CtrlDx {name = "OP2 L KEY SCALE", steps = 4, dx7_ix = 95, display_value = 0, dexed_ix = 61}
  , CtrlDx {name = "OP2 R KEY SCALE", steps = 4, dx7_ix = 96, display_value = 0, dexed_ix = 62}
  , CtrlDx {name = "OP2 RATE SCALING", steps = 8, dx7_ix = 97, display_value = 0, dexed_ix = 63}
  , CtrlDx {name = "OP2 A MOD SENS.", steps = 4, dx7_ix = 98, display_value = 0, dexed_ix = 64}
  , CtrlDx {name = "OP2 KEY VELOCITY", steps = 8, dx7_ix = 99, display_value = 0, dexed_ix = 65}
  , CtrlDx {name = "OP3 EG RATE 1", steps = 100, dx7_ix = 63, display_value = 0, dexed_ix = 67}
  , CtrlDx {name = "OP3 EG RATE 2", steps = 100, dx7_ix = 64, display_value = 0, dexed_ix = 68}
  , CtrlDx {name = "OP3 EG RATE 3", steps = 100, dx7_ix = 65, display_value = 0, dexed_ix = 69}
  , CtrlDx {name = "OP3 EG RATE 4", steps = 100, dx7_ix = 66, display_value = 0, dexed_ix = 70}
  , CtrlDx {name = "OP3 EG LEVEL 1", steps = 100, dx7_ix = 67, display_value = 0, dexed_ix = 71}
  , CtrlDx {name = "OP3 EG LEVEL 2", steps = 100, dx7_ix = 68, display_value = 0, dexed_ix = 72}
  , CtrlDx {name = "OP3 EG LEVEL 3", steps = 100, dx7_ix = 69, display_value = 0, dexed_ix = 73}
  , CtrlDx {name = "OP3 EG LEVEL 4", steps = 100, dx7_ix = 70, display_value = 0, dexed_ix = 74}
  , CtrlDx {name = "OP3 OUTPUT LEVEL", steps = 100, dx7_ix = 79, display_value = 0, dexed_ix = 75}
  , CtrlDx {name = "OP3 MODE", steps = 2, dx7_ix = 80, display_value = 0, dexed_ix = 76}
  , CtrlDx {name = "OP3 F COARSE", steps = 32, dx7_ix = 81, display_value = 0, dexed_ix = 77}
  , CtrlDx {name = "OP3 F FINE", steps = 100, dx7_ix = 82, display_value = 0, dexed_ix = 78}
  , CtrlDx {name = "OP3 OSC DETUNE", steps = 15, dx7_ix = 83, display_value = -7, dexed_ix = 79}
  , CtrlDx {name = "OP3 BREAK POINT", steps = 100, dx7_ix = 71, display_value = 0, dexed_ix = 80}
  , CtrlDx {name = "OP3 L SCALE DEPTH", steps = 100, dx7_ix = 72, display_value = 0, dexed_ix = 81}
  , CtrlDx {name = "OP3 R SCALE DEPTH", steps = 100, dx7_ix = 73, display_value = 0, dexed_ix = 82}
  , CtrlDx {name = "OP3 L KEY SCALE", steps = 4, dx7_ix = 74, display_value = 0, dexed_ix = 83}
  , CtrlDx {name = "OP3 R KEY SCALE", steps = 4, dx7_ix = 75, display_value = 0, dexed_ix = 84}
  , CtrlDx {name = "OP3 RATE SCALING", steps = 8, dx7_ix = 76, display_value = 0, dexed_ix = 85}
  , CtrlDx {name = "OP3 A MOD SENS.", steps = 4, dx7_ix = 77, display_value = 0, dexed_ix = 86}
  , CtrlDx {name = "OP3 KEY VELOCITY", steps = 8, dx7_ix = 78, display_value = 0, dexed_ix = 87}
  , CtrlDx {name = "OP4 EG RATE 1", steps = 100, dx7_ix = 42, display_value = 0, dexed_ix = 89}
  , CtrlDx {name = "OP4 EG RATE 2", steps = 100, dx7_ix = 43, display_value = 0, dexed_ix = 90}
  , CtrlDx {name = "OP4 EG RATE 3", steps = 100, dx7_ix = 44, display_value = 0, dexed_ix = 91}
  , CtrlDx {name = "OP4 EG RATE 4", steps = 100, dx7_ix = 45, display_value = 0, dexed_ix = 92}
  , CtrlDx {name = "OP4 EG LEVEL 1", steps = 100, dx7_ix = 46, display_value = 0, dexed_ix = 93}
  , CtrlDx {name = "OP4 EG LEVEL 2", steps = 100, dx7_ix = 47, display_value = 0, dexed_ix = 94}
  , CtrlDx {name = "OP4 EG LEVEL 3", steps = 100, dx7_ix = 48, display_value = 0, dexed_ix = 95}
  , CtrlDx {name = "OP4 EG LEVEL 4", steps = 100, dx7_ix = 49, display_value = 0, dexed_ix = 96}
  , CtrlDx {name = "OP4 OUTPUT LEVEL", steps = 100, dx7_ix = 58, display_value = 0, dexed_ix = 97}
  , CtrlDx {name = "OP4 MODE", steps = 2, dx7_ix = 59, display_value = 0, dexed_ix = 98}
  , CtrlDx {name = "OP4 F COARSE", steps = 32, dx7_ix = 60, display_value = 0, dexed_ix = 99}
  , CtrlDx {name = "OP4 F FINE", steps = 100, dx7_ix = 61, display_value = 0, dexed_ix = 100}
  , CtrlDx {name = "OP4 OSC DETUNE", steps = 15, dx7_ix = 62, display_value = -7, dexed_ix = 101}
  , CtrlDx {name = "OP4 BREAK POINT", steps = 100, dx7_ix = 50, display_value = 0, dexed_ix = 102}
  , CtrlDx {name = "OP4 L SCALE DEPTH", steps = 100, dx7_ix = 51, display_value = 0, dexed_ix = 103}
  , CtrlDx {name = "OP4 R SCALE DEPTH", steps = 100, dx7_ix = 52, display_value = 0, dexed_ix = 104}
  , CtrlDx {name = "OP4 L KEY SCALE", steps = 4, dx7_ix = 53, display_value = 0, dexed_ix = 105}
  , CtrlDx {name = "OP4 R KEY SCALE", steps = 4, dx7_ix = 54, display_value = 0, dexed_ix = 106}
  , CtrlDx {name = "OP4 RATE SCALING", steps = 8, dx7_ix = 55, display_value = 0, dexed_ix = 107}
  , CtrlDx {name = "OP4 A MOD SENS.", steps = 4, dx7_ix = 56, display_value = 0, dexed_ix = 108}
  , CtrlDx {name = "OP4 KEY VELOCITY", steps = 8, dx7_ix = 57, display_value = 0, dexed_ix = 109}
  , CtrlDx {name = "OP5 EG RATE 1", steps = 100, dx7_ix = 21, display_value = 0, dexed_ix = 111}
  , CtrlDx {name = "OP5 EG RATE 2", steps = 100, dx7_ix = 22, display_value = 0, dexed_ix = 112}
  , CtrlDx {name = "OP5 EG RATE 3", steps = 100, dx7_ix = 23, display_value = 0, dexed_ix = 113}
  , CtrlDx {name = "OP5 EG RATE 4", steps = 100, dx7_ix = 24, display_value = 0, dexed_ix = 114}
  , CtrlDx {name = "OP5 EG LEVEL 1", steps = 100, dx7_ix = 25, display_value = 0, dexed_ix = 115}
  , CtrlDx {name = "OP5 EG LEVEL 2", steps = 100, dx7_ix = 26, display_value = 0, dexed_ix = 116}
  , CtrlDx {name = "OP5 EG LEVEL 3", steps = 100, dx7_ix = 27, display_value = 0, dexed_ix = 117}
  , CtrlDx {name = "OP5 EG LEVEL 4", steps = 100, dx7_ix = 28, display_value = 0, dexed_ix = 118}
  , CtrlDx {name = "OP5 OUTPUT LEVEL", steps = 100, dx7_ix = 37, display_value = 0, dexed_ix = 119}
  , CtrlDx {name = "OP5 MODE", steps = 2, dx7_ix = 38, display_value = 0, dexed_ix = 120}
  , CtrlDx {name = "OP5 F COARSE", steps = 32, dx7_ix = 39, display_value = 0, dexed_ix = 121}
  , CtrlDx {name = "OP5 F FINE", steps = 100, dx7_ix = 40, display_value = 0, dexed_ix = 122}
  , CtrlDx {name = "OP5 OSC DETUNE", steps = 15, dx7_ix = 41, display_value = -7, dexed_ix = 123}
  , CtrlDx {name = "OP5 BREAK POINT", steps = 100, dx7_ix = 29, display_value = 0, dexed_ix = 124}
  , CtrlDx {name = "OP5 L SCALE DEPTH", steps = 100, dx7_ix = 30, display_value = 0, dexed_ix = 125}
  , CtrlDx {name = "OP5 R SCALE DEPTH", steps = 100, dx7_ix = 31, display_value = 0, dexed_ix = 126}
  , CtrlDx {name = "OP5 L KEY SCALE", steps = 4, dx7_ix = 32, display_value = 0, dexed_ix = 127}
  , CtrlDx {name = "OP5 R KEY SCALE", steps = 4, dx7_ix = 33, display_value = 0, dexed_ix = 128}
  , CtrlDx {name = "OP5 RATE SCALING", steps = 8, dx7_ix = 34, display_value = 0, dexed_ix = 129}
  , CtrlDx {name = "OP5 A MOD SENS.", steps = 4, dx7_ix = 35, display_value = 0, dexed_ix = 130}
  , CtrlDx {name = "OP5 KEY VELOCITY", steps = 8, dx7_ix = 36, display_value = 0, dexed_ix = 131}
  , CtrlDx {name = "OP6 EG RATE 1", steps = 100, dx7_ix = 0, display_value = 0, dexed_ix = 133}
  , CtrlDx {name = "OP6 EG RATE 2", steps = 100, dx7_ix = 1, display_value = 0, dexed_ix = 134}
  , CtrlDx {name = "OP6 EG RATE 3", steps = 100, dx7_ix = 2, display_value = 0, dexed_ix = 135}
  , CtrlDx {name = "OP6 EG RATE 4", steps = 100, dx7_ix = 3, display_value = 0, dexed_ix = 136}
  , CtrlDx {name = "OP6 EG LEVEL 1", steps = 100, dx7_ix = 4, display_value = 0, dexed_ix = 137}
  , CtrlDx {name = "OP6 EG LEVEL 2", steps = 100, dx7_ix = 5, display_value = 0, dexed_ix = 138}
  , CtrlDx {name = "OP6 EG LEVEL 3", steps = 100, dx7_ix = 6, display_value = 0, dexed_ix = 139}
  , CtrlDx {name = "OP6 EG LEVEL 4", steps = 100, dx7_ix = 7, display_value = 0, dexed_ix = 140}
  , CtrlDx {name = "OP6 OUTPUT LEVEL", steps = 100, dx7_ix = 16, display_value = 0, dexed_ix = 141}
  , CtrlDx {name = "OP6 MODE", steps = 2, dx7_ix = 17, display_value = 0, dexed_ix = 142}
  , CtrlDx {name = "OP6 F COARSE", steps = 32, dx7_ix = 18, display_value = 0, dexed_ix = 143}
  , CtrlDx {name = "OP6 F FINE", steps = 100, dx7_ix = 19, display_value = 0, dexed_ix = 144}
  , CtrlDx {name = "OP6 OSC DETUNE", steps = 15, dx7_ix = 20, display_value = -7, dexed_ix = 145}
  , CtrlDx {name = "OP6 BREAK POINT", steps = 100, dx7_ix = 8, display_value = 0, dexed_ix = 146}
  , CtrlDx {name = "OP6 L SCALE DEPTH", steps = 100, dx7_ix = 9, display_value = 0, dexed_ix = 147}
  , CtrlDx {name = "OP6 R SCALE DEPTH", steps = 100, dx7_ix = 10, display_value = 0, dexed_ix = 148}
  , CtrlDx {name = "OP6 L KEY SCALE", steps = 4, dx7_ix = 11, display_value = 0, dexed_ix = 149}
  , CtrlDx {name = "OP6 R KEY SCALE", steps = 4, dx7_ix = 12, display_value = 0, dexed_ix = 150}
  , CtrlDx {name = "OP6 RATE SCALING", steps = 8, dx7_ix = 13, display_value = 0, dexed_ix = 151}
  , CtrlDx {name = "OP6 A MOD SENS.", steps = 4, dx7_ix = 14, display_value = 0, dexed_ix = 152}
  , CtrlDx {name = "OP6 KEY VELOCITY", steps = 8, dx7_ix = 15, display_value = 0, dexed_ix = 153}
  ]

{- | Lookup 'dexed_dx7_param' by name.

> dexed_dx7_param_by_name "MIDDLE C"
-}
dexed_dx7_param_by_name :: String -> Maybe CtrlDx
dexed_dx7_param_by_name nm = find ((== nm) . name) dexed_dx7_param

{- | Dexed parameter list in the form (Dx7-IX,Dexed-IX,Dx7-NAME).

> import qualified Sound.Sc3.Data.Yamaha.Dx7 as Dx7
> mapM_ print $ zip dexed_dx7_param_dx7 Dx7.dx7_parameter_tbl
-}
dexed_dx7_param_dx7 :: [(Word8, Int, String)]
dexed_dx7_param_dx7 = sort (map (\c -> (dx7_ix c, dexed_ix c, name c)) dexed_dx7_param)

{- | Table from Dx7 parameter index to Dexed parameter index.

> sort (map fst dx7_to_dexed_tbl) == [0 .. 144]
> sort (map snd dx7_to_dexed_tbl) == [4 .. 153] \\ [44,66,88,110,132]
-}
dx7_to_dexed_tbl :: [(Word8, Int)]
dx7_to_dexed_tbl = map (\(i, j, _) -> (i, j)) dexed_dx7_param_dx7

-- | 'M.fromList' of 'dx7_to_dexed_tbl'.
dx7_to_dexed_map :: M.Map Word8 Int
dx7_to_dexed_map = M.fromList dx7_to_dexed_tbl

{- | 'M.lookup' of 'dx7_to_dexed_tbl', ie. given Dx7 index lookup Dexed index.

> map dx7_to_dexed [123,125,134,144] == [33,35,4,13]
> dx7_to_dexed (Dx7.dx7_parameter_index "ALGORITHM #") == 4
-}
dx7_to_dexed :: Word8 -> Int
dx7_to_dexed = fromMaybe (error "dx7_to_dexed") . flip M.lookup dx7_to_dexed_map

{- | Reverse lookup of 'dx7_to_dexed_tbl', ie. given Dexed index lookup Dx7 index.

> map dexed_to_dx7 [4] == [134]
-}
dexed_to_dx7 :: Int -> Word8
dexed_to_dx7 x = fst (fromMaybe (error "dexed_to_dx7") (find ((== x) . snd) dx7_to_dexed_tbl))

-- * Translate

-- | Translate 145 element Dx7 parameter sequence to 155 element Dexed sequence.
dx7_voice_to_dexed_param :: [Word8] -> [Double]
dx7_voice_to_dexed_param vc =
  let word8_to_double = fromIntegral
      u8_at l n = l !! fromIntegral n
      f k = case lookup k non_dx7_defaults of
        Just v -> v
        Nothing -> word8_to_double (vc `u8_at` dexed_to_dx7 k) / 99.0
  in map f [0 .. 154]

-- * Init

{- | Init data from <DexedAudioProcessor::resetToInitVoice>.
  This differs from the dx7_init_data in one place (KBD LEV SCL BRK PT) for each operator.

> map Dx7.dx7_parameter_name [8,29,50,71,92,113]
> dexed_init_voice Dx7.dx7_init_voice
-}
dexed_init_voice :: Num t => [t] -> [t]
dexed_init_voice =
  let brk_pt_set = take 6 [8, 8 + 21 ..]
      f k p = if k `elem` brk_pt_set then 0 else p
  in zipWith f [0 :: Int ..]

{-

http://rd.slavepianos.org/sw/rju/cmd/jack-lxvst.cpp

import Sound.Osc.Fd {- hosc -}
fd <- openUDP "127.0.0.1" 57210
let p_set ix val = sendMessage fd (Message "/param" [int32 ix,Float val])
p_set 3 0.5 -- MASTER TUNE ADJ
let p_set_dx7 ix = p_set (dx7_to_dexed ix)
p_set_dx7 123 1 -- OP 1 OSC FREQ COARSE
p_set_dx7 125 1 -- OP 1 OSC DETUNE
p_set_dx7 134 0.5 -- ALGORITHM #
p_set_dx7 144 0.5 -- TRANSPOSE

-}
