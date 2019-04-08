-- | Yamaha DX7
--
--  GS-1 FM = 1981, DX1 & DX7 & DX9 = 1983, TX816 = 1984, DX7-IID = 1986, DX7S = 1987
--
-- <https://github.com/asb2m10/dexed/blob/master/Documentation/sysex-format.txt>
-- <https://sourceforge.net/u/tedfelix/dx7dump/ci/master/tree/dx7dump.cpp>
module Sound.SC3.Data.Yamaha.DX7 where

import Control.Monad {- base -}
import Data.Bits {- base -}
import Data.Int {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import Data.Word {- base -}
import Text.Printf {- base -}

import qualified Data.ByteString as B {- bytestring -}
import qualified Data.List.Split as Split {- split -}
import qualified Safe {- safe -}
import qualified System.Process as Process {- process -}

import qualified Music.Theory.Byte as Byte {- hmt -}
import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Math.Convert as T {- hmt -}

import qualified Sound.Midi.Common as M {- midi-osc -}

-- | Unsigned 8-bit word.
type U8 = Word8

-- | Signed 8-bit integer.
type I8 = Int8

-- | Number of per-operator parameters.  The DX7 has six operators.
dx7_op_nparam :: Num n => n
dx7_op_nparam = 21

-- | Number of shared (non-operator) parameters.
dx7_sh_nparam :: Num n => n
dx7_sh_nparam = 19

-- | Number of voice parameters.
--
-- > dx7_nparam == 145
-- > dx7_nparam + dx7_name_nchar == 155
dx7_nparam :: Num n => n
dx7_nparam = 6 * dx7_op_nparam + dx7_sh_nparam

-- | Voice parameter data (# = 145 = dx7_nparam)
type DX7_Param = [U8]

-- | Equality ignoring indicated indices.
dx7_param_eq_ignoring :: [U8] -> DX7_Param -> DX7_Param -> Bool
dx7_param_eq_ignoring = T.list_eq_ignoring_indices

-- | Replace each (ix,value) at 'DX7_Param'.
dx7_param_set :: [(U8,U8)] -> DX7_Param -> DX7_Param
dx7_param_set = T.list_set_indices

-- | Type-specialised 'Byte.word8_at'.
dx7_param_at :: DX7_Param -> U8 -> U8
dx7_param_at = Byte.word8_at

-- | Number of bytes for voice name.
dx7_name_nchar :: Num n => n
dx7_name_nchar = 10

-- | Number of voice parameters.
--
-- > dx7_nvoice == 155
-- > dx7_nvoice - dx7_name_nchar == 145
-- > dx7_nvoice * 32 == 4960
dx7_nvoice :: Num n => n
dx7_nvoice = dx7_nparam + dx7_name_nchar

-- | Is /x/ in (32,126)?
--
-- > map fromEnum "\t\n " == [9,10,32]
dx7_ascii_verify :: U8 -> U8
dx7_ascii_verify x = if x < 32 || x > 126 then error "ASCII?" else x

-- | Replace out-of-range ASCII U8 with the ASCII code for /c/.
--
-- > map (dx7_ascii_correct '?') [9,10,32] == [63,63,32]
dx7_ascii_correct :: Char -> U8 -> U8
dx7_ascii_correct c x = if x < 32 || x > 126 then dx7_ascii_verify (Byte.char_to_word8 c) else x

-- | Map ASCII U8 to 'Char'.
--
-- > map (dx7_ascii_char '?') [32 .. 126]
dx7_ascii_char :: Char -> U8 -> Char
dx7_ascii_char c = Byte.word8_to_char . dx7_ascii_correct c

-- | Given 145-byte 'DX7_Param' sequence and a 10 ASCII character name, make 'DX7_Voice'.
dx7_param_to_dx7_voice :: String -> DX7_Param -> DX7_Voice
dx7_param_to_dx7_voice nm =
  if length nm /= 10
  then error "dx7_param_to_dx7_voice"
  else flip (++) (map Byte.char_to_word8 nm)

-- | Select the 145 parameter bytes from a 155-byte 'DX7_Voice'.
--   ie. delete the 10 ASCII character voice-name.
dx7_voice_param :: DX7_Voice -> DX7_Param
dx7_voice_param = take 145

-- | Voice data (# = 155 = dx7_nvoice)
type DX7_Voice = [U8]

-- | Collect any out-of-range parameter data as (IX,VALUE,(MIN,MAX)) triples.
dx7_voice_out_of_range :: DX7_Voice -> [(U8,U8,(U8,U8))]
dx7_voice_out_of_range d =
  let rng = map dx7_parameter_range dx7_parameter_tbl
      chk (ix,n,(l,r)) = if n >= l && n <= r then Nothing else Just (ix,n,(l,r))
  in mapMaybe chk (zip3 [0..154] d rng)

-- | Re-write any out-of-range parameter data to be within range.
dx7_voice_param_correct :: DX7_Voice -> DX7_Voice
dx7_voice_param_correct d =
  let rng = map dx7_parameter_range dx7_parameter_tbl
      clp (n,(l,r)) = if n < l then l else if n > r then r else n
  in map clp (zip d rng)

-- | Check the voice has 'dx7_nvoice' bytes, and perhaps that all parameter data is in range.
dx7_voice_verify :: Bool -> DX7_Voice -> Bool
dx7_voice_verify chk_rng d = length d == dx7_nvoice && (not chk_rng || dx7_voice_out_of_range d == [])

-- | Error if any voice fails to verify.
dx7_voice_set_verify :: Bool -> [DX7_Voice] -> IO ()
dx7_voice_set_verify chk_rng v = when (any not (map (dx7_voice_verify chk_rng) v)) (error "dx7_voice?")

-- | Voice operators, in sequence 6,5,4,3,2,1 (# = 6 x 21 = 126)
dx7_voice_op_params :: DX7_Voice -> [[U8]]
dx7_voice_op_params = Split.chunksOf dx7_op_nparam . take (dx7_op_nparam * 6)

-- | Voice shared parameters (# = 19, IX = 126-144)
--
-- > putStrLn $ unlines $ map dx7_parameter_name [126 .. 144]
dx7_voice_sh_params :: DX7_Voice -> [U8]
dx7_voice_sh_params = take dx7_sh_nparam . drop (dx7_op_nparam * 6)

-- | DX7 INIT operator, /x/ is the output level (# = 21)
dx7_init_op :: U8 -> [U8]
dx7_init_op x = [99,99,99,99,99,99,99,0,39,0,0,0,0,0,0,0,x,0,1,0,7]

-- | DX7 INIT PITCH EG (# = 8; IX = 126-133)
--
-- > putStrLn $ unlines $ map dx7_parameter_name [126 .. 133]
dx7_init_pitch_eg :: [U8]
dx7_init_pitch_eg = [99,99,99,99,50,50,50,50]

-- | DX7 INIT LFO (# = 6; IX = 137-142)
--
-- > putStrLn $ unlines $ map dx7_parameter_name [137 .. 142]
dx7_init_lfo :: [U8]
dx7_init_lfo = [35,0,0,0,1,0]

-- | DX7 INIT VOICE, from DX7-II CART 64-B.
--
-- > dx7_voice_verify True dx7_init_voice == True
-- > dx7_voice_name dx7_init_voice == "INIT VOICE"
-- > (minimum dx7_init_voice,maximum dx7_init_voice) == (0,99)
dx7_init_voice :: DX7_Voice
dx7_init_voice =
  let op_6_2 = concat (replicate 5 (dx7_init_op 0))
      op_1 = dx7_init_op 99
      sh = dx7_init_pitch_eg ++ [0,0,1] ++ dx7_init_lfo ++ [3,24]
      nm = dx7_name_encode '?' "INIT VOICE"
  in concat [op_6_2,op_1,sh,nm]

{-
-- | Type-specialised 'B.pack'.
dx7_param_pack :: DX7_Param -> B.ByteString
dx7_param_pack = B.pack

-- | Type-specialised 'B.unpack'.
dx7_param_unpack :: B.ByteString -> DX7_Param
dx7_param_unpack = B.unpack

-- | Type-specialised 'B.pack'.
dx7_voice_pack :: DX7_Voice -> B.ByteString
dx7_voice_pack = B.pack

-- | Type-specialised 'B.unpack'.
dx7_voice_unpack :: B.ByteString -> DX7_Voice
dx7_voice_unpack = B.unpack
-}

-- * DX7 / BANK

-- | Sequence of 32 voices (32 * 155 = 4960)
type DX7_Bank = [DX7_Voice]

-- | Check there are 32 voices and each run 'dx7_voice_verify' at each.
dx7_bank_verify :: Bool -> DX7_Bank -> Bool
dx7_bank_verify chk_rng b = length b == 32 && all id (map (dx7_voice_verify chk_rng) b)

-- | Make bank from /v/, if there are less than 32 voices extend with 'dx7_init_voice'.
--   It is an error for there to be more than 32 voices.
dx7_voices_to_bank :: [DX7_Voice] -> DX7_Bank
dx7_voices_to_bank v =
  case compare (length v) 32 of
    LT -> take_extending_with dx7_init_voice 32 v
    EQ -> v
    GT -> error "dx7_voices_to_bank: >32?"

-- | Make bank from /v/ starting at index /i/, extend if required, ignore trailing voices.
dx7_bank_from :: Int -> [DX7_Voice] -> DX7_Bank
dx7_bank_from i = take_extending_with dx7_init_voice 32 . drop i

-- | Yamaha manufacturer ID.
--
-- > import Music.Theory.Bits {- hmt -}
-- > gen_bitseq_pp 8 yamaha_id == "01000011"
--
-- > :set -XBinaryLiterals
-- > (yamaha_id == 67,yamaha_id == 0x43,yamaha_id == 0b01000011)
yamaha_id :: U8
yamaha_id = 0x43

-- | DX7 checksum function.
dx7_checksum :: [U8] -> U8
dx7_checksum d = (complement (sum (map ((.&.) 0x7F) d)) + 1) .&. 0x7F

usr_str_tbl :: [(String,String)]
usr_str_tbl =
    [("BOOL","OFF;ON")
    ,("CURVE","-LIN;-EXP;+EXP;+LIN")
    ,("LFO-WAVE","TRIANGLE;SAWTOOTH-DOWN;SAWTOOTH-UP;SQUARE;SINE;SAMPLE-AND-HOLD")
    ,("MODE","RATIO;FIXED")]

-- | (DX7-IX,NAME,STEPS,USR_DIFF,USR_STR)
--
-- All parameters except the NAME data are in the range (0,STEPS-1).
-- NAME data is in (32,126), ie. the ASCII printable characters.
type DX7_Parameter = (U8,String,U8,I8,String)

dx7_parameter_ix :: DX7_Parameter -> U8
dx7_parameter_ix (n,_,_,_,_) = n

dx7_parameter_nm :: DX7_Parameter -> String
dx7_parameter_nm (_,nm,_,_,_) = nm

dx7_parameter_range :: DX7_Parameter -> (U8,U8)
dx7_parameter_range (ix,_,n,_,_) = if ix < 145 then (0,n - 1) else (32,126)

dx7_parameter_range_usr :: DX7_Parameter -> (I8,I8)
dx7_parameter_range_usr (ix,_,n,d,_) = if ix < 125 then (d,d + T.word8_to_int8 n - 1) else (32,126)

-- | Normalise parameter value to be in (0,1).
-- Parameter values are at most in 0-99, excepting characters in voice name data.
--
-- > let p = dx7_op_parameter_tbl !! 20
-- > map (dx7_parameter_value_normalise p) [0 .. 14]
dx7_parameter_value_normalise :: DX7_Parameter -> U8 -> Float
dx7_parameter_value_normalise (_,_,n,_,_) x = T.word8_to_float x / T.word8_to_float (n - 1)

-- | Template for six FM operators.
--
-- > length dx7_op_parameter_tbl == dx7_op_nparam
dx7_op_parameter_tbl :: [DX7_Parameter]
dx7_op_parameter_tbl =
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
    ,(20,"OSC DETUNE",15,-7,"-7 - +7") -- 0x14
    ]

-- | Rewrite 'dx7_op_parameter_tbl' for operator /n/.
dx7_rewrite_op_dx7_parameter_tbl :: U8 -> [DX7_Parameter]
dx7_rewrite_op_dx7_parameter_tbl n =
    let n' = 6 - n
        f (ix,nm,stp,usr_diff,usr_str) =
            let ix' = ix + (dx7_op_nparam * n')
                nm' = "OP " ++ show n ++ " " ++ nm
            in (ix',nm',stp,usr_diff,usr_str)
    in map f dx7_op_parameter_tbl

-- | Group Structure (group-name,field-names,indices)
type Group_Structure = (String,String,[U8])

-- | Operator group structure, 4;4;5;4
operator_group_structure :: [Group_Structure]
operator_group_structure =
    [("EG RATE","1;2;3;4",[0..3])
    ,("EG LEVEL","1;2;3;4",[4..7])
    ,("KBD LEV SCL","BRK-PT;LFT-DEPTH;RHT-DEPTH;LFT-CURVE;RHT-CURVE",[8..12])
    ,("OSC","MODE;FREQ-COARSE;FREQ-FINE;DETUNE",[17..20])
    ]

-- | Six operators, descending order, one-indexed.
--
-- > length dx7_op6_dx7_parameter_tbl == 6 * dx7_op_nparam
dx7_op6_dx7_parameter_tbl :: [DX7_Parameter]
dx7_op6_dx7_parameter_tbl = concatMap dx7_rewrite_op_dx7_parameter_tbl [6,5 .. 1]

-- | Remainder (non-operator) of parameter table.
--
-- > length dx7_sh_parameter_tbl == dx7_sh_nparam
dx7_sh_parameter_tbl :: [DX7_Parameter]
dx7_sh_parameter_tbl =
    [(126,"PITCH EG RATE 1",100,0,"")
    ,(127,"PITCH EG RATE 2",100,0,"")
    ,(128,"PITCH EG RATE 3",100,0,"")
    ,(129,"PITCH EG RATE 4",100,0,"")
    ,(130,"PITCH EG LEVEL 1",100,0,"")
    ,(131,"PITCH EG LEVEL 2",100,0,"")
    ,(132,"PITCH EG LEVEL 3",100,0,"")
    ,(133,"PITCH EG LEVEL 4",100,0,"")
    ,(134,"ALGORITHM #",32,1,"") -- 0x86
    ,(135,"FEEDBACK",8,0,"")
    ,(136,"OSCILLATOR SYNC",2,0,"OFF;ON")
    ,(137,"LFO SPEED",100,0,"")
    ,(138,"LFO DELAY",100,0,"")
    ,(139,"LFO PITCH MOD DEPTH",100,0,"")
    ,(140,"LFO AMP MOD DEPTH",100,0,"")
    ,(141,"LFO SYNC",2,0,"OFF;ON")
    ,(142,"LFO WAVEFORM",6,0,"TR;SD;SU;SQ;SI;SH")
    ,(143,"PITCH MOD SENSITIVITY",8,0,"")
    ,(144,"TRANSPOSE",49,0,"12=C2")]

-- | Voice name data is ASCII.
dx7_name_dx7_parameter_tbl :: [DX7_Parameter]
dx7_name_dx7_parameter_tbl =
    [(145,"VOICE NAME CHAR 01",128,0,"ASCII")
    ,(146,"VOICE NAME CHAR 02",128,0,"ASCII")
    ,(147,"VOICE NAME CHAR 03",128,0,"ASCII")
    ,(148,"VOICE NAME CHAR 04",128,0,"ASCII")
    ,(149,"VOICE NAME CHAR 05",128,0,"ASCII")
    ,(150,"VOICE NAME CHAR 06",128,0,"ASCII")
    ,(151,"VOICE NAME CHAR 07",128,0,"ASCII")
    ,(152,"VOICE NAME CHAR 08",128,0,"ASCII")
    ,(153,"VOICE NAME CHAR 09",128,0,"ASCII")
    ,(154,"VOICE NAME CHAR 10",128,0,"ASCII")]

-- | NOT STORED IN VOICE DATA
dx7_opstatus_param :: DX7_Parameter
dx7_opstatus_param = (155,"OPERATOR ON/OFF",2,0,"BIT5=OP1 - BIT0=OP6")

rem_group_structure :: [Group_Structure]
rem_group_structure =
    [("PITCH EG RATE","1;2;3;4",[126..129])
    ,("PITCH EG LEVEL","1;2;3;4",[130..133])
    ,("LFO","SPEED;DELAY;PITCH-MOD-DEPTH;AMP-MOD-DEPTH;SYNC;WAVEFORM",[137..142])
    ,("VOICE NAME CHAR",";;;;;;;;;",[145..154])]

-- | All DX7 parameters.
--
-- > length dx7_parameter_tbl == dx7_nvoice
-- > map dx7_parameter_range_usr dx7_parameter_tbl
-- > map dx7_parameter_range dx7_parameter_tbl
dx7_parameter_tbl :: [DX7_Parameter]
dx7_parameter_tbl =
  concat [dx7_op6_dx7_parameter_tbl
         ,dx7_sh_parameter_tbl
         ,dx7_name_dx7_parameter_tbl]

-- | Lookup parameter name given index.
--
-- > dx7_parameter_name 0x14 == "OP 6 OSC DETUNE"
-- > dx7_parameter_name 0x50 == "OP 3 OSC MODE"
-- > dx7_parameter_name 0x86 == "ALGORITHM #"
-- > dx7_parameter_name 0x90 == "TRANSPOSE"
dx7_parameter_name :: U8 -> String
dx7_parameter_name n =
    fromMaybe (error "dx7_parameter_name") $
    fmap dx7_parameter_nm $
    find ((== n) . dx7_parameter_ix) dx7_parameter_tbl

-- | Lookup parameter index given name.
--
-- > dx7_parameter_index "ALGORITHM #" == 0x86
-- > dx7_parameter_index "OP 6 OSC DETUNE" == 0x14
dx7_parameter_index :: String -> U8
dx7_parameter_index nm =
    fromMaybe (error "dx7_parameter_ix") $
    fmap dx7_parameter_ix $
    find ((== nm) . dx7_parameter_nm) dx7_parameter_tbl

-- | Pretty print value give 'DX7_Parameter'.
dx7_parameter_value_pp :: DX7_Parameter -> U8 -> String
dx7_parameter_value_pp p x =
  let (_,_,stp,d,u) = p
      (x_clip,err) = if x >= stp
                     then (stp - 1,printf "(ERROR BYTE=0x%02X)" x)
                     else (x,"")
      r = if u == "ASCII"
          then ['\'',dx7_ascii_char '?' x_clip,'\'']
          else case Split.splitOn ";" u of
                 [_] -> printf "%02d" (T.word8_to_int8 x_clip + d)
                 e -> Byte.word8_at e x_clip
  in r ++ err

dx7_parameter_pp :: Bool -> DX7_Parameter -> U8 -> String
dx7_parameter_pp with_ix p x =
  let (ix,nm,_,_,_) = p
  in if with_ix
     then printf "%03d: %s = %s" ix nm (dx7_parameter_value_pp p x)
     else printf "%s = %s" nm (dx7_parameter_value_pp p x)

dx7_parameter_set_pp :: DX7_Parameter -> [U8] -> String
dx7_parameter_set_pp p x =
  let (_,nm,_,_,_) = p
  in printf "%s = %s" nm (intercalate "," (map (dx7_parameter_value_pp p) x))

-- | Print complete parameter sequence.
dx7_parameter_seq_pp :: DX7_Voice -> [String]
dx7_parameter_seq_pp = zipWith (dx7_parameter_pp True) dx7_parameter_tbl

-- | Group 6-operators, shared params and name.
--
-- > dx7_voice_grp dx7_init_voice
dx7_voice_grp :: DX7_Voice -> [[U8]]
dx7_voice_grp p =
  let ix = concat [replicate 6 dx7_op_nparam :: [Int],[dx7_sh_nparam,dx7_name_nchar]]
  in Split.splitPlaces ix p

-- | DX7_Voice pretty-printer.
--
-- > dx7_voice_pp dx7_init_voice
dx7_voice_pp :: DX7_Voice -> [String]
dx7_voice_pp p =
  let p_grp = dx7_voice_grp p
  in concat [zipWith (dx7_parameter_pp False) dx7_sh_parameter_tbl (Byte.word8_at p_grp 6)
            ,zipWith dx7_parameter_set_pp dx7_op_parameter_tbl (transpose (take 6 p_grp))]

dx7_function_parameters_tbl :: [DX7_Parameter]
dx7_function_parameters_tbl =
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

-- * Voice

-- | Extract 10 character voice name from 'DX7_Voice'.
dx7_voice_name :: Char -> DX7_Voice -> String
dx7_voice_name c v = map (dx7_ascii_char c . Byte.word8_at v) [145 .. 154]

-- | Encode ASCII name to U8 sequence.
dx7_name_encode :: Char -> String -> [U8]
dx7_name_encode c = map (dx7_ascii_correct c . Byte.char_to_word8)

-- * DX7 VOICE DATA LIST

-- | Arrangement of parameters on printed DX7 VOICE DATA LIST.
--
-- > sort (concatMap (\(_,_,ix) -> ix) dx7_voice_data_list) == [0 .. 20] ++ [126 .. 144]
dx7_voice_data_list :: [(String, [String], [U8])]
dx7_voice_data_list =
  [(""
   ,["ALGORITHM"],[134])
  ,(""
   ,["FEEDBACK"],[135])
  ,("LFO"
   ,["WAVE","SPEED","DELAY","PMD","AMD","SYNC"],[142,137,138,139,140,141])
  ,("MOD SENSITIVITY"
   ,["PITCH","AMPLITUDE"],[143,14])
  ,("OSCILLATOR"
   ,["MOD","SYNC","FREQ. COARSE","FREQ. FINE","DETUNE"],17:136:[18 .. 20])
  ,("EG"
   ,["R1","R2","R3","R4","L1","L2","L3","L4"],[0 .. 7])
  ,("KEYBOARD LEVEL SCALING"
   ,["BREAK POINT","CURVE L","CURVE R","DEPTH L","DEPTH R"],[8,11,12,9,10])
  ,("",["K.BOARD RATE SCALING"],[13])
  ,("OPERATOR"
   ,["OUTPUT LEVEL","VELO SENS"],[16,15])
  ,("PITCH EG"
   ,["R1","R2","R3","R4","L1","L2","L3","L4"],[126 .. 133])
  ,(""
   ,["KEY TRANSPOSE"],[144])]

-- | Plain text voice data list.
--
-- > putStrLn$unlines$ dx7_voice_data_list_pp (replicate 155 0)
dx7_voice_data_list_pp :: DX7_Voice -> [String]
dx7_voice_data_list_pp d =
  let op_ix_set n = [n, n + dx7_op_nparam .. n + dx7_op_nparam * 5]
      op_ix_pp n = map
                   (dx7_parameter_value_pp (Byte.word8_at dx7_op_parameter_tbl n))
                   (map (Byte.word8_at d) (op_ix_set n))
      is_op_ix n = n < 126
      ix_val n =
        if is_op_ix n
        then intercalate "," (op_ix_pp n)
        else dx7_parameter_value_pp (Byte.word8_at dx7_sh_parameter_tbl (n - 126)) (Byte.word8_at d n)
      pp z nm ix = concat [z,nm,"=",ix_val ix]
      f (grp,nm,ix) =
        if null grp
        then zipWith (pp "") nm ix
        else grp : zipWith (pp "  ") nm ix
      vc_nm = "NAME=" ++ dx7_voice_name '?' d
  in vc_nm : concatMap f dx7_voice_data_list


-- * SYSEX Message: FORMAT=0: Voice Data (163-BYTES)

{-
     11110000  F0   STATUS BYTE - START SYSEX
     0iiiiiii  43   YAMAHA
     0sssnnnn  00   SUB-STATUS = 0x00 & CHANNEL NUMBER
     0fffffff  00   FORMAT NUMBER = 0
     0bbbbbbb  01   BYTE COUNT MS
     0bbbbbbb  1B   BYTE COUNT LS = 155
     0ddddddd  **   DATA BYTE 1
     ...
     0ddddddd  **   DATA BYTE 155
     0eeeeeee  **   CHECKSUM (OF 155 DATA BYTES)
     11110111  F7   STATUS - END SYSEX
-}

-- | A sysex message is a sequence of U8.
type DX7_SYSEX = [U8]

-- | 6-element FMT=0 sysex header.
--
-- > dx7_fmt0_sysex_hdr 0 == [0xF0,0x43,0x00,0x00,0x01,0x1b]
dx7_fmt0_sysex_hdr :: U8 -> [U8]
dx7_fmt0_sysex_hdr ch = [0xF0,0x43,0x00 + ch,0x00,0x01,0x1b]

-- | Generate 163-element DX7 VOICE (FORMAT=0) sysex message.
--
-- > 6 + 155 + 2 == 163
dx7_fmt0_sysex_encode :: Bool -> U8 -> DX7_Voice -> DX7_SYSEX
dx7_fmt0_sysex_encode chk_rng ch d =
  if dx7_voice_verify chk_rng d
  then dx7_fmt0_sysex_hdr ch ++ d ++ [dx7_checksum d,0xF7]
  else error "dx7_fmt0_sysex_encode?"

-- | Select 155-element sub-sequence (6 - 161)
dx7_fmt0_sysex_decode :: DX7_SYSEX -> DX7_Voice
dx7_fmt0_sysex_decode = take 155 . drop 6

-- | Add nil voice-name to 'DX7_Param' and encode sysex.
dx7_param_to_fmt0_sysex :: DX7_Param -> DX7_SYSEX
dx7_param_to_fmt0_sysex p = dx7_fmt0_sysex_encode True 0 (dx7_param_to_dx7_voice "----------" p)

-- * B: SYSEX Message: FORMAT=9: Bank Data (32 VOICES, 4104 BYTES)

{-
     11110000  F0   STATUS BYTE - START SYSEX
     0iiiiiii  43   YAMAHA ID
     0sssnnnn  00   SUB-STATUS = 0x00 & CHANNEL NUMBER
     0fffffff  09   FORMAT NUMBER = 9
     0bbbbbbb  20   BYTE COUNT MS
     0bbbbbbb  00   BYTE COUNT LS = 4096
     0ddddddd  **   DATA BYTE 1
     ....
     0ddddddd  **   DATA BYTE 4096
     0eeeeeee  **   CHECKSUM (OF 4096 DATA BYTES)
     11110111  F7   STATUS - END SYSEX
-}

-- | 6-byte header sequence for FORMAT=9 DX7 sysex data.
dx7_fmt9_sysex_hdr :: U8 -> [U8]
dx7_fmt9_sysex_hdr ch = [0xF0,0x43,0x00 + ch,0x09,0x20,0x00]

-- | Given 4096-element bit-packed data sequence, generate 4104-element FORMAT=9 sysex message data.
dx7_fmt9_sysex_gen :: U8 -> [U8] -> [U8]
dx7_fmt9_sysex_gen ch dat = dx7_fmt9_sysex_hdr ch ++ dat ++ [dx7_checksum dat,0xF7]

-- | Select bytes (6 - 4102) of 'U8' sequence.
dx7_fmt9_sysex_dat :: [U8] -> [U8]
dx7_fmt9_sysex_dat = take 4096 . drop 6

-- | 'M.u8_load'.
dx7_read_u8 :: FilePath -> IO [U8]
dx7_read_u8 = M.u8_load

{- | Verify U8 sequence is FORMAT=9 DX7 sysex data.
     Verification data is (sysex-length,sysex-header,checksum,end-of-sysex)

> let fn = "/home/rohan/sw/hsc3-data/data/yamaha/dx7/rom/DX7-ROM1A.syx"
> d <- dx7_read_u8 rom1a_fn
> dx7_fmt9_sysex_verify 0 d == (True,True,True,True)

-}
dx7_fmt9_sysex_verify :: U8 -> DX7_SYSEX -> (Bool, Bool, Bool, Bool)
dx7_fmt9_sysex_verify ch d =
  let hdr = take 6 d
      dat = take 4096 (drop 6 d)
      chk = Safe.at d (6 + 4096)
      eof = Safe.at d (6 + 4096 + 1)
  in (length d == 6 + 4096 + 2
     ,hdr == dx7_fmt9_sysex_hdr ch
     ,chk == dx7_checksum dat
     ,eof == 0xF7)

-- | Run 'dx7_fmt9_sysex_verify' and lift /syx/ to 'Maybe'.
dx7_fmt9_sysex_validate :: DX7_SYSEX -> Maybe DX7_SYSEX
dx7_fmt9_sysex_validate syx =
  if dx7_fmt9_sysex_verify 0 syx /= (True,True,True,True)
  then Nothing
  else Just syx

-- | 'error' if /syx/ is not valid, else 'id'.
dx7_fmt9_sysex_validate_err :: String -> DX7_SYSEX -> DX7_SYSEX
dx7_fmt9_sysex_validate_err err =
  fromMaybe (error ("dx7_fmt9_sysex_validate: " ++ err)) .
  dx7_fmt9_sysex_validate

{- | Load FORMAT=9 sysex file as 4104-element U8 sequence and run verification.
     See 'dx7_fmt9_sysex_verify'.
-}
dx7_read_fmt9_sysex :: FilePath -> IO (Maybe DX7_SYSEX)
dx7_read_fmt9_sysex = fmap dx7_fmt9_sysex_validate  . dx7_read_u8

-- | Erroring variant.
dx7_read_fmt9_sysex_err :: FilePath -> IO DX7_SYSEX
dx7_read_fmt9_sysex_err = fmap (dx7_fmt9_sysex_validate_err "dx7_read_fmt9_sysex?")  . dx7_read_u8

-- | Write FORMAT=9 4104-element U8 sequence to file.
dx7_write_fmt9_sysex :: FilePath -> DX7_SYSEX -> IO ()
dx7_write_fmt9_sysex fn = M.u8_store fn . dx7_fmt9_sysex_validate_err "dx7_write_fmt9_sysex?"

-- | Unpack bit-packed 'U8' sequence to sequence of 'DX7_Voice' (see cmd/dx7-unpack.c).
--   Input size must be a multiple of 128, output size will be a multiple of 155.
dx7_unpack_bitpacked_u8 :: [U8] -> IO [DX7_Voice]
dx7_unpack_bitpacked_u8 p = do
  when ((length p `rem` 128) /= 0) (error ("dx7_unpack_bitpacked_u8? " ++ show p))
  q <- Process.readProcess "hsc3-dx7-unpack" ["unpack"] (Byte.byte_seq_hex_pp False p)
  let r = Byte.read_hex_byte_seq q
  when ((length r `rem` 155) /= 0) (error ("dx7_unpack_bitpacked_u8? " ++ q))
  return (Split.chunksOf 155 r)

-- | Decode FORMAT=9 SYSEX message.
--   IO because the bit un-packing is done by an external process.
dx7_fmt9_sysex_decode :: [U8] -> IO DX7_Bank
dx7_fmt9_sysex_decode = dx7_unpack_bitpacked_u8 . dx7_fmt9_sysex_dat

{- | Read binary FORMAT=9 sysex file and unpack voice data.

> let fn = "/home/rohan/sw/hsc3-data/data/yamaha/dx7/rom/DX7-ROM1A.syx"
> d <- dx7_load_fmt9_sysex_err fn
> dx7_bank_verify True d
> mapM_ (putStrLn . dx7_voice_name '?') d
> mapM_ (putStrLn . unlines . dx7_parameter_seq_pp) d
> mapM_ (putStrLn . unlines . dx7_voice_pp) d
> mapM_ (putStrLn . unlines . dx7_voice_data_list_pp) d

> dx7_fmt0_sysex_encode True 0x0 (d !! 0)

-}
dx7_load_fmt9_sysex_err :: FilePath -> IO DX7_Bank
dx7_load_fmt9_sysex_err fn = do
  sysex <- dx7_read_fmt9_sysex_err fn
  dx7_unpack_bitpacked_u8 (dx7_fmt9_sysex_dat sysex)

{- | Try and load 'DX7_Voice' data from named file.

Will read exact multiples of:

128 (BITPACKED-VOICE),
155 (UN-BITPACKED-VOICE),
163 (FORMAT=0),
4104 (FORMAT=9)

To truncate a long FORMAT=9 sysex file use:
xxd -u -p -c 4104 -l 4104 x.syx | xxd -r -p > y.syx

-}
dx7_load_sysex_try :: FilePath -> IO (Maybe [DX7_Voice])
dx7_load_sysex_try fn = do
  x <- dx7_read_u8 fn
  let n = length x
      is_mult m = n `rem` m == 0
      decode_syx = dx7_unpack_bitpacked_u8 . dx7_fmt9_sysex_dat
  case n of
    1650 -> return Nothing -- DX7-II PERF SYSEX
    _ ->
      if is_mult 128
      then fmap Just (dx7_unpack_bitpacked_u8 x)
      else if is_mult 155
           then return (Just (Split.chunksOf 155 x))
           else if is_mult 163
                then return (Just (map dx7_fmt0_sysex_decode (Split.chunksOf 163 x)))
                else if n `rem` 4104 == 0
                     then fmap (Just . concat) (mapM decode_syx (Split.chunksOf 4104 x))
                     else return Nothing

-- | Encode 'DX7_Bank' to channel /ch/ 'DX7_SYSEX'.
dx7_fmt9_sysex_encode :: U8 -> DX7_Bank -> IO DX7_SYSEX
dx7_fmt9_sysex_encode ch bnk = do
  let dat = concat bnk
      dat_str = Byte.byte_seq_hex_pp False dat ++ "\n"
  when (length dat /= 4960) (error "dx7_fmt9_sysex_encode")
  syx <- Process.readProcess "hsc3-dx7-unpack" ["pack"] dat_str
  return (dx7_fmt9_sysex_gen ch (Byte.read_hex_byte_seq syx))

{- | Write binary DX7 FORMAT=9 sysex file.

> let fn = "/home/rohan/sw/hsc3-data/data/yamaha/dx7/rom/DX7-ROM1A.syx"
> d <- dx7_load_fmt9_sysex_err fn
> dx7_store_fmt9_sysex "/tmp/dx7.syx" 0 d
> Process.rawSystem "cmp" ["-l",fn,"/tmp/dx7.syx"]

-}
dx7_store_fmt9_sysex :: FilePath -> U8 -> DX7_Bank -> IO ()
dx7_store_fmt9_sysex fn ch bnk = do
  syx <- dx7_fmt9_sysex_encode ch bnk
  B.writeFile fn (B.pack syx)

-- * C: SYSEX MESSAGE: Parameter Change

{-
     11110000  F0   STATUS BYTE = START SYSEX
     0iiiiiii  43   ID = YAMAHA
     0sssnnnn  10   SUB-STATUS = 0x10 & CHANNEL NUMBER
     0gggggpp  **   PARAMETER GROUP (0=VOICE, 2=FUNCTION)
     0ppppppp  **   PARAMETER #
     0ddddddd  **   DATA BYTE
     11110111  F7   STATUS BYTE = END SYSEX

> gen_bitseq_pp 8 (0x10::U8) == "00010000"
-}

-- | Number of bytes in DX7 param change sysex messages.
dx7_param_change_sysex_n :: Num n => n
dx7_param_change_sysex_n = 7

-- | DX7 (group,sub-group) to byte.
--
-- > gen_bitseq_pp 8 (dx7_group_join (0,0)) == "00000000"
-- > gen_bitseq_pp 8 (dx7_group_join (2,0)) == "00001000"
-- > gen_bitseq_pp 8 (dx7_group_join (6,0)) == "00011000"
-- > dx7_group_join (6,0) == 0x18
dx7_group_join :: (U8,U8) -> U8
dx7_group_join (g1,g2) = shiftL g1 2 + g2

-- | Generate DX7 parameter change sysex.
--
-- > dx7_parameter_change_sysex 0 (0x06,0) dx7_microtune_octave [1,2,3]
dx7_parameter_change_sysex :: U8 -> (U8,U8) -> U8 -> [U8] -> [U8]
dx7_parameter_change_sysex ch grp p d = [0xF0,0x43,0x10 + ch,dx7_group_join grp,p] ++ d ++ [0xF7]

{- | Construct DX7 param change sysex.  Variant of 'dx7_parameter_change_sysex'.

Arguments are:
channel (0 indexed),
parameter-index (0-155),
parameter-value (ix dependent, 0-127)

The parameter-index is partly encoded in the sub-group.
If the parameter-index is below 0x80, the sub-group is 0x00, else it is 0x01.
In the latter case you parameter-byte holds the parameter-index minus 0x80.

> let ix = dx7_parameter_index
> dx7_param_change_sysex 0x00 (ix "ALGORITHM #") 0x18 == [0xF0,0x43,0x10,0x01,0x06,0x18,0xF7]
> dx7_param_change_sysex 0x00 (ix "OP 6 OSC DETUNE") 0x07 == [0xF0,0x43,0x10,0x00,0x14,0x07,0xF7]
-}
dx7_param_change_sysex :: U8 -> U8 -> U8 -> DX7_SYSEX
dx7_param_change_sysex ch param_ix param_data =
  let sub = if param_ix >= 0x80 then 0x01 else 0x00
      num = param_ix - if param_ix >= 0x80 then 0x80 else 0x00
  in dx7_parameter_change_sysex ch (0x00,sub) num [param_data]

-- * E: Function Parameters: (GROUP=2)

{-
64         MONO/POLY MODE CHANGE  0-1      O=POLY
65         PITCH BEND RANGE       0-12
66           "    "   STEP        0-12
67         PORTAMENTO MODE        0-1      0=RETAIN 1=FOLLOW
68              "     GLISS       0-1
69              "     TIME        0-99
70         MOD WHEEL RANGE        0-99
71          "    "   ASSIGN       0-7     b0: pitch,  b1:amp, b2: EG bias
72         FOOT CONTROL RANGE     0-99
73          "     "     ASSIGN    0-7           "
74         BREATH CONT RANGE      0-99
75           "     "   ASSIGN     0-7           "
76         AFTERTOUCH RANGE       0-99
77             "      ASSIGN      0-7           "
-}

-- * Microtune Parameter Change Message (DX7s DX7II)

-- | Constant for per-octave mode (ie. tune all pitch-classes equally).
--
-- > gen_bitseq_pp 8 dx7_microtune_octave == "01111101"
dx7_microtune_octave :: U8
dx7_microtune_octave = 0x7D

-- | Constant for full gamut mode (ie. tune all notes distinctly).
--
-- > gen_bitseq_pp 8 dx7_microtune_full == "01111110"
dx7_microtune_full :: U8
dx7_microtune_full = 0x7E

-- | DX7 tuning units are 64 steps per semi-tone.
--
-- > map dx7_tuning_units_to_cents [0,16,32,48,64] == [0,25,50,75,100]
dx7_tuning_units_to_cents :: U8 -> U8
dx7_tuning_units_to_cents x = floor (T.word8_to_double x * (100 / 64))

{- | Microtune parameter change sysex

md = 0x7D | 0x7E
d1 = key number (0-11 for Octave, 0-127 for Full)
d2 = midi note number (13 - 108)
d3 = fine tuning (0 - 63)

The fine tuning parameter is in Yamaha tuning units away from note d2 in 12 ET.
There are 64 tuning units per half step, 12 * 64 = 768 per octave.

If d3 < 33, it is displayed on the LCD as a positive offset from note d2.
Otherwise, it is be displayed as a negative offset -31 to -1 from note d2 + 1.

> let r = [0xF0,0x43,0x10,0x18,0x7E,0x3C,0x3C,0x00,0xF7]
> dx7_microtune_parameter_change_sysex 0 dx7_microtune_full 60 60 0 == r

-}
dx7_microtune_parameter_change_sysex :: U8 -> U8 -> U8 -> U8 -> U8 -> [U8]
dx7_microtune_parameter_change_sysex ch md d1 d2 d3 =
  dx7_parameter_change_sysex ch (6,0) md [d1,d2,d3]

-- | Shift right by four places.
--
-- > map dx7_substatus [0x10,0x20] == [0x01,0x02]
dx7_substatus :: U8 -> U8
dx7_substatus = flip shiftR 4

-- * REQUEST SYSEX

-- | Make data request sysex message, /n/ = channel, /k/ equal request type.
dx7_data_request_sysex :: U8 -> U8 -> DX7_SYSEX
dx7_data_request_sysex n k = [0xF0,0x43,0x20 + n,k,0xF7]

-- | Request voice edit buffer as FORMAT=0 sysex.
--
-- > dx7_data_request_sysex_fmt0 0 == [0xF0,0x43,0x20,0x00,0xF7]
dx7_data_request_sysex_fmt0 :: U8 -> DX7_SYSEX
dx7_data_request_sysex_fmt0 n = dx7_data_request_sysex n 0x00

-- | Request 32-voice data as FORMAT=9 sysex.
dx7_data_request_sysex_fmt9 :: U8 -> DX7_SYSEX
dx7_data_request_sysex_fmt9 n = dx7_data_request_sysex n 0x09

-- * TEXT/HEX

-- | Read sequence of unpacked 155 voice-data parameters from text file.
--   U8 are encoded as two-character hexadecimal numbers (00-FF).
dx7_load_hex :: FilePath -> IO [DX7_Voice]
dx7_load_hex fn = do
  d <- Byte.load_hex_byte_seq fn
  let chk_rng = False
  dx7_voice_set_verify chk_rng d
  return d

{- | Write sequence of unpacked 155 voice-data parameters to text file.

> let fn = "/home/rohan/sw/hsc3-data/data/yamaha/dx7/rom/DX7-ROM1A.syx"
> d <- dx7_load_fmt9_sysex_err fn
> dx7_store_hex True "/tmp/dx7.hex.text" d

> t <- dx7_load_hex "/tmp/dx7.hex.text"
> (length d,length t,d == t) == (32,32,True)

-}
dx7_store_hex :: Bool -> FilePath -> [DX7_Voice] -> IO ()
dx7_store_hex chk_rng fn v = do
  dx7_voice_set_verify chk_rng v
  Byte.store_hex_byte_seq fn v

-- | Variant that runs 'dx7_voice_param_correct' on /v/.
dx7_store_hex_correct :: FilePath -> [DX7_Voice] -> IO ()
dx7_store_hex_correct fn = dx7_store_hex True fn . map dx7_voice_param_correct

-- * TEXT/CSV

-- | Encode 'DX7_Voice' as CSV entry.
--   The first column is the voice name as an ASCII string, followed by the parameters in sequence.
--   Parameters are written as decimal integers.
dx7_voice_to_csv :: DX7_Voice -> String
dx7_voice_to_csv v =
  let nm = map (\c -> if c == ',' then '_' else c) . dx7_voice_name '?'
  in intercalate "," (nm v : map show (dx7_voice_param v))

-- | Decode 'DX7_Voice' from CSV entry.
dx7_voice_from_csv :: String -> DX7_Voice
dx7_voice_from_csv str =
  case Split.splitOn "," str of
    nm:p -> dx7_param_to_dx7_voice nm (map read p)
    _ -> error "dx7_voice_from_csv?"

{- | Write sequence of 'DX7_Voice' to CSV file.

> let fn = "/home/rohan/sw/hsc3-data/data/yamaha/dx7/rom/DX7-ROM1A.syx"
> d <- dx7_load_fmt9_sysex_err fn
> dx7_store_csv True "/tmp/dx7.csv" d
> r <- dx7_load_csv "/tmp/dx7.csv"
> r == d

-}
dx7_store_csv :: Bool -> FilePath -> [DX7_Voice] -> IO ()
dx7_store_csv chk_rng fn v = do
  dx7_voice_set_verify chk_rng v
  writeFile fn (unlines (map dx7_voice_to_csv v))

-- | Read sequence of 'DX7_Voice' from CSV file.
dx7_load_csv :: FilePath -> IO [DX7_Voice]
dx7_load_csv fn = do
  s <- readFile fn
  let v = map dx7_voice_from_csv (lines s)
      chk_rng = False
  dx7_voice_set_verify chk_rng v
  return v

-- * UTIL

-- | Take /n/ from /l/ extending with /z/ if required.
--
-- > take_extending_with '-' 10 "string" == "string----"
take_extending_with :: t -> Int -> [t] -> [t]
take_extending_with z n l = take n (l ++ repeat z)
