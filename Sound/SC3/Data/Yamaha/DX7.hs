-- | Yamaha DX7
--
-- <http://homepages.abdn.ac.uk/mth192/pages/dx7/sysex-format.txt>
-- <https://sourceforge.net/u/tedfelix/dx7dump/ci/master/tree/dx7dump.cpp>
module Sound.SC3.Data.Yamaha.DX7 where

yamaha_id :: Num n => n
yamaha_id = 0x43

usr_str :: [(String,String)]
usr_str =
    [("BOOL","OFF;ON")
    ,("CURVE","-LIN;-EXP;+EXP;+LIN")
    ,("LFO-WAVE","TRIANGLE;SAWTOOTH-DOWN;SAWTOOTH-UP;SQUARE;SINE;SAMPLE-AND-HOLD")
    ,("MODE","RATIO;FIXED")]

type Parameter = (Int,String,(Int,Int),String)

-- > length operator_parameter_template == 21
operator_parameter_template :: [Parameter]
operator_parameter_template =
    [(00,"EG RATE 1",(0,99),"")
    ,(01,"EG RATE 2",(0,99),"")
    ,(02,"EG RATE 3",(0,99),"")
    ,(03,"EG RATE 4",(0,99),"")
    ,(04,"EG LEVEL 1",(0,99),"")
    ,(05,"EG LEVEL 2",(0,99),"")
    ,(06,"EG LEVEL 3",(0,99),"")
    ,(07,"EG LEVEL 4",(0,99),"")
    ,(08,"KBD LEV SCL BRK PT",(0,99),"")
    ,(09,"KBD LEV SCL LFT DEPTH",(0,99),"")
    ,(10,"KBD LEV SCL RHT DEPTH",(0,99),"")
    ,(11,"KBD LEV SCL LFT CURVE",(0,3),"-LIN;-EXP;+EXP;+LIN")
    ,(12,"KBD LEV SCL RHT CURVE",(0,3),"-LIN;-EXP;+EXP;+LIN")
    ,(13,"KBD RATE SCALING",(0,7),"")
    ,(14,"AMP MOD SENSITIVITY",(0,3),"")
    ,(15,"KEY VEL SENSITIVITY",(0,7),"")
    ,(16,"OPERATOR OUTPUT LEVEL",(0,99),"")
    ,(17,"OSC MODE",(0,1),"RATIO;FIXED")
    ,(18,"OSC FREQ COARSE",(0,31),"")
    ,(19,"OSC FREQ FINE",(0,99),"")
    ,(20,"OSC DETUNE",(0,14),"-7 - +7")
    ]

gen_operator_parameter_tbl :: Int -> [Parameter]
gen_operator_parameter_tbl n =
    let n' = 6 - n
        f (ix,nm,rng,usr) = (ix + (21 * n'),"OP " ++ show n ++ " " ++ nm,rng,usr)
    in map f operator_parameter_template

-- > length operator_parameter_tbl == 126
operator_parameter_tbl :: [Parameter]
operator_parameter_tbl = concatMap gen_operator_parameter_tbl [6,5 .. 1]

-- > length parameter_tbl_rem == 30
parameter_tbl_rem :: [Parameter]
parameter_tbl_rem =
    [(126,"PITCH EG RATE 1",(0,99),"")
    ,(127,"PITCH EG RATE 2",(0,99),"")
    ,(128,"PITCH EG RATE 3",(0,99),"")
    ,(129,"PITCH EG RATE 4",(0,99),"")
    ,(130,"PITCH EG LEVEL 1",(0,99),"")
    ,(131,"PITCH EG LEVEL 2",(0,99),"")
    ,(132,"PITCH EG LEVEL 3",(0,99),"")
    ,(133,"PITCH EG LEVEL 4",(0,99),"")
    ,(134,"ALGORITHM #",(0,31),"")
    ,(135,"FEEDBACK",(0,7),"")
    ,(136,"OSCILLATOR SYNC",(0,1),"")
    ,(137,"LFO SPEED",(0,99),"")
    ,(138,"LFO DELAY",(0,99),"")
    ,(139,"LFO PITCH MOD DEPTH",(0,99),"")
    ,(140,"LFO AMP MOD DEPTH",(0,99),"")
    ,(141,"LFO SYNC",(0,1),"")
    ,(142,"LFO WAVEFORM",(0,5),"TR;SD;SU;SQ;SI;SH")
    ,(143,"PITCH MOD SENSITIVITY",(0,7),"")
    ,(144,"TRANSPOSE",(0,48),"12=C2")
    ,(145,"VOICE NAME CHAR 1",(0,127),"ASCII")
    ,(146,"VOICE NAME CHAR 2",(0,127),"ASCII")
    ,(147,"VOICE NAME CHAR 3",(0,127),"ASCII")
    ,(148,"VOICE NAME CHAR 4",(0,127),"ASCII")
    ,(149,"VOICE NAME CHAR 5",(0,127),"ASCII")
    ,(150,"VOICE NAME CHAR 6",(0,127),"ASCII")
    ,(151,"VOICE NAME CHAR 7",(0,127),"ASCII")
    ,(152,"VOICE NAME CHAR 8",(0,127),"ASCII")
    ,(153,"VOICE NAME CHAR 9",(0,127),"ASCII")
    ,(154,"VOICE NAME CHAR 10",(0,127),"ASCII")
    ,(155,"OPERATOR ON/OFF",(0,1),"BIT5=OP1 - BIT0=OP6") -- NOT STORED IN VOICE DATA
    ]

-- > length dx7_parameter_tbl == 156
dx7_parameter_tbl :: [Parameter]
dx7_parameter_tbl = operator_parameter_tbl ++ parameter_tbl_rem

function_parameters_tbl :: [Parameter]
function_parameters_tbl =
    [(64,"MODE CHANGE",(0,1),"POLY;MONO")
    ,(65,"PITCH BEND RANGE",(0,12),"")
    ,(66,"PITCH BEND STEP",(0,12),"")
    ,(67,"PORTAMENTO MODE ",(0,1),"RETAIN;FOLLOW")
    ,(68,"PORTAMENTO GLISS",(0,1),"")
    ,(69,"PORTAMENTO TIME ",(0,99),"")
    ,(70,"MOD WHEEL RANGE ",(0,99),"")
    ,(71,"MOD WHEEL ASSIGN",(0,7),"PITCH;AMP;EG-BIAS")
    ,(72,"FOOT CONTROL RANGE",(0,99),"")
    ,(73,"FOOT CONTROL ASSIGN",(0,7),"")
    ,(74,"BREATH CONT RANGE",(0,99),"")
    ,(75,"BREATH CONT ASSIGN",(0,7),"")
    ,(76,"AFTERTOUCH RANGE",(0,99),"")
    ,(77,"AFTERTOUCH ASSIGN",(0,7),"")]
