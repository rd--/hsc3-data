-- | Yamaha DX7
--
-- <http://homepages.abdn.ac.uk/mth192/pages/dx7/sysex-format.txt>
-- <https://sourceforge.net/u/tedfelix/dx7dump/ci/master/tree/dx7dump.cpp>
module Sound.SC3.Data.Yamaha.DX7 where

import Data.List.Split {- base -}
import Text.Printf {- base -}

import qualified Music.Theory.Read as T {- hmt -}

yamaha_id :: Num n => n
yamaha_id = 0x43

usr_str_tbl :: [(String,String)]
usr_str_tbl =
    [("BOOL","OFF;ON")
    ,("CURVE","-LIN;-EXP;+EXP;+LIN")
    ,("LFO-WAVE","TRIANGLE;SAWTOOTH-DOWN;SAWTOOTH-UP;SQUARE;SINE;SAMPLE-AND-HOLD")
    ,("MODE","RATIO;FIXED")]

-- (DX7-IX,NAME,STEPS,USR_DIFF,USR_STR)
type Parameter = (Int,String,Int,Int,String)

parameter_range :: Parameter -> (Int,Int)
parameter_range (_,_,n,_,_) = (0,n - 1)

parameter_range_usr :: Parameter -> (Int,Int)
parameter_range_usr (_,_,n,d,_) = (d,d + n - 1)

-- > length operator_parameter_template == 21
operator_parameter_template :: [Parameter]
operator_parameter_template =
    [(00,"EG RATE 1",100,0,"")
    ,(01,"EG RATE 2",100,0,"")
    ,(02,"EG RATE 3",100,0,"")
    ,(03,"EG RATE 4",100,0,"")
    ,(04,"EG LEVEL 1",100,0,"")
    ,(05,"EG LEVEL 2",100,0,"")
    ,(06,"EG LEVEL 3",100,0,"")
    ,(07,"EG LEVEL 4",100,0,"")
    ,(08,"KBD LEV SCL BRK PT",100,0,"")
    ,(09,"KBD LEV SCL LFT DEPTH",100,0,"")
    ,(10,"KBD LEV SCL RHT DEPTH",100,0,"")
    ,(11,"KBD LEV SCL LFT CURVE",4,0,"-LIN;-EXP;+EXP;+LIN")
    ,(12,"KBD LEV SCL RHT CURVE",4,0,"-LIN;-EXP;+EXP;+LIN")
    ,(13,"KBD RATE SCALING",8,0,"")
    ,(14,"AMP MOD SENSITIVITY",4,0,"")
    ,(15,"KEY VEL SENSITIVITY",8,0,"")
    ,(16,"OPERATOR OUTPUT LEVEL",100,0,"")
    ,(17,"OSC MODE",2,0,"RATIO;FIXED")
    ,(18,"OSC FREQ COARSE",32,0,"")
    ,(19,"OSC FREQ FINE",100,0,"")
    ,(20,"OSC DETUNE",15,-7,"-7 - +7")
    ]

gen_operator_parameter_tbl :: Int -> [Parameter]
gen_operator_parameter_tbl n =
    let n' = 6 - n
        f (ix,nm,stp,usr_diff,usr_str) =
            let ix' = ix + (21 * n')
                nm' = "OP " ++ show n ++ " " ++ nm
            in (ix',nm',stp,usr_diff,usr_str)
    in map f operator_parameter_template

-- > length operator_parameter_tbl == 126
operator_parameter_tbl :: [Parameter]
operator_parameter_tbl = concatMap gen_operator_parameter_tbl [6,5 .. 1]

-- > length parameter_tbl_rem == 30
parameter_tbl_rem :: [Parameter]
parameter_tbl_rem =
    [(126,"PITCH EG RATE 1",100,0,"")
    ,(127,"PITCH EG RATE 2",100,0,"")
    ,(128,"PITCH EG RATE 3",100,0,"")
    ,(129,"PITCH EG RATE 4",100,0,"")
    ,(130,"PITCH EG LEVEL 1",100,0,"")
    ,(131,"PITCH EG LEVEL 2",100,0,"")
    ,(132,"PITCH EG LEVEL 3",100,0,"")
    ,(133,"PITCH EG LEVEL 4",100,0,"")
    ,(134,"ALGORITHM #",32,1,"")
    ,(135,"FEEDBACK",8,0,"")
    ,(136,"OSCILLATOR SYNC",2,0,"OFF;ON")
    ,(137,"LFO SPEED",100,0,"")
    ,(138,"LFO DELAY",100,0,"")
    ,(139,"LFO PITCH MOD DEPTH",100,0,"")
    ,(140,"LFO AMP MOD DEPTH",100,0,"")
    ,(141,"LFO SYNC",2,0,"OFF;ON")
    ,(142,"LFO WAVEFORM",6,0,"TR;SD;SU;SQ;SI;SH")
    ,(143,"PITCH MOD SENSITIVITY",8,0,"")
    ,(144,"TRANSPOSE",49,0,"12=C2")
    ,(145,"VOICE NAME CHAR 1",128,0,"ASCII")
    ,(146,"VOICE NAME CHAR 2",128,0,"ASCII")
    ,(147,"VOICE NAME CHAR 3",128,0,"ASCII")
    ,(148,"VOICE NAME CHAR 4",128,0,"ASCII")
    ,(149,"VOICE NAME CHAR 5",128,0,"ASCII")
    ,(150,"VOICE NAME CHAR 6",128,0,"ASCII")
    ,(151,"VOICE NAME CHAR 7",128,0,"ASCII")
    ,(152,"VOICE NAME CHAR 8",128,0,"ASCII")
    ,(153,"VOICE NAME CHAR 9",128,0,"ASCII")
    ,(154,"VOICE NAME CHAR 10",128,0,"ASCII")
    ,(155,"OPERATOR ON/OFF",2,0,"BIT5=OP1 - BIT0=OP6") -- NOT STORED IN VOICE DATA
    ]

-- > length dx7_parameter_tbl == 156
dx7_parameter_tbl :: [Parameter]
dx7_parameter_tbl = operator_parameter_tbl ++ parameter_tbl_rem

dx7_parameter_pp :: [Int] -> [String]
dx7_parameter_pp =
    let f (p,x) =
            let (ix,nm,stp,d,u) = p
                x' = if u == "ASCII"
                     then ['\'',toEnum x,'\'']
                     else case splitOn ";" u of
                            [_] -> show (x + d)
                            e -> e !! x
            in if x >= stp
               then error (show ("dx7_parameter_pp",x,p))
               else printf "%03d: %s = %s" ix nm x'
    in map f . zip dx7_parameter_tbl

function_parameters_tbl :: [Parameter]
function_parameters_tbl =
    [(64,"MODE CHANGE",2,0,"POLY;MONO")
    ,(65,"PITCH BEND RANGE",13,0,"")
    ,(66,"PITCH BEND STEP",13,0,"")
    ,(67,"PORTAMENTO MODE ",2,0,"RETAIN;FOLLOW")
    ,(68,"PORTAMENTO GLISS",2,0,"")
    ,(69,"PORTAMENTO TIME ",100,0,"")
    ,(70,"MOD WHEEL RANGE ",100,0,"")
    ,(71,"MOD WHEEL ASSIGN",8,0,"PITCH;AMP;EG-BIAS")
    ,(72,"FOOT CONTROL RANGE",100,0,"")
    ,(73,"FOOT CONTROL ASSIGN",8,0,"")
    ,(74,"BREATH CONT RANGE",100,0,"")
    ,(75,"BREATH CONT ASSIGN",8,0,"")
    ,(76,"AFTERTOUCH RANGE",100,0,"")
    ,(77,"AFTERTOUCH ASSIGN",8,0,"")]

load_dx7_sysex_hex :: (Eq t,Num t) => FilePath -> IO [t]
load_dx7_sysex_hex fn = do
  s <- readFile fn
  case splitAt 4960 (words s) of
    (h,[]) -> return (map (\x -> T.read_hex_byte x) h)
    _ -> error "load_dx7_sysex_hex"

{-
h <- load_dx7_sysex_hex "/home/rohan/data/yamaha/DX7/Dexed_01.syx.hex" :: IO [Int]
let p = chunksOf 155 h
mapM_ (putStrLn . unlines . dx7_parameter_pp) p
-}
