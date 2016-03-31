-- | Roland Corporation.
-- /Roland MIDI Linear Synthesiser Programmer PG-1000 Owner's Manual/.
-- Hamamatsu, JP, 1987.
-- <http://cdn.roland.com/assets/media/pdf/PG-1000_OM.pdf>
module Sound.SC3.Data.Roland.D50 where

import Data.Bits {- base -}
import Data.Char {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import Numeric {- base -}

bits_pp :: [Bool] -> String
bits_pp = map (\b -> if b then '1' else '0')

gen_bitseq :: FiniteBits b => Int -> b -> [Bool]
gen_bitseq n x = if finiteBitSize x < n then error "gen_bitseq" else map (testBit x) (reverse [0 .. n - 1])

gen_bitseq_pp :: FiniteBits b => Int -> b -> String
gen_bitseq_pp n = bits_pp . gen_bitseq n

byte_hex_pp :: (Integral i, Show i) => i -> String
byte_hex_pp n =
    case showHex n "" of
      [c] -> ['0',toUpper c]
      [c,d] -> map toUpper [c,d]
      _ -> error "byte_hex_pp"

byte_seq_hex_pp :: (Integral i, Show i) => [i] -> String
byte_seq_hex_pp = unwords . map byte_hex_pp

-- > gen_bitseq_pp 8 sysex_status == "11110000"
sysex_status :: Int
sysex_status = 0xF0

-- > gen_bitseq_pp 8 sysex_end == "11110111"
sysex_end :: Int
sysex_end = 0xF7

-- > gen_bitseq_pp 8 roland_id == "01000001"
roland_id :: Int
roland_id = 0x41

-- > gen_bitseq_pp 8 d50_id == "00010100"
d50_id :: Int
d50_id = 0x14

-- > gen_bitseq_pp 8 dti_cmd_id
dti_cmd_id :: Int
dti_cmd_id = 0x12

rqi_cmd_id :: Int
rqi_cmd_id = 0x11

-- | The checksum is a derived from the address (three bytes) and the data bytes (for the D-50 always one byte).
-- <ftp://ftp.monash.edu.au/pub/midi/DOC/Roland-checksum>
--
-- > roland_checksum [0x40,0x00,0x04,0x64] == 0x58
roland_checksum :: (Ord n,Num n) => [n] -> n
roland_checksum =
    let f n w =
            case w of
              [] -> 0x80 - n
              x:w' -> let n' = n + x in f (if n' > 0x80 then n' - 0x80 else n') w'
    in f 0

t3_to_list :: (t, t, t) -> [t]
t3_to_list (p,q,r) = [p,q,r]

t3_from_list :: [t] -> (t, t, t)
t3_from_list l =
    case l of
      [p,q,r] -> (p,q,r)
      _ -> error "t3_from_list"

-- | MSB -> LSB
--
-- > bits_21_sep 128 == (0x00,0x01,0x00)
-- > bits_21_sep 192 == (0x00,0x01,0x40)
-- > bits_21_sep 320 == (0x00,0x02,0x40)
-- > bits_21_sep 421 == (0x00,0x03,0x25)
bits_21_sep :: (Num t, Bits t) => t -> (t,t,t)
bits_21_sep a = (shiftR a 14 .&. 0x7F,shiftR a 7 .&. 0x7F,a .&. 0x7F)

bits_21_sep_l :: (Num t, Bits t) => t -> [t]
bits_21_sep_l = t3_to_list . bits_21_sep

-- > bits_21_join (0x00,0x01,0x00) == 128
-- > bits_21_join (0x00,0x01,0x40) == 192
-- > bits_21_join (0x00,0x02,0x40) == 320
-- > bits_21_join (0x00,0x03,0x19) == 409
-- > bits_21_join (0x00,0x03,0x25) == 421
bits_21_join :: (Num t, Bits t) => (t,t,t) -> t
bits_21_join (p,q,r) = shiftL p 14 .|. shiftL q 7 .|. r

-- > byte_seq_hex_pp (gen_d50_data_request_sysex 0 0 421) == "F0 41 00 14 11 00 00 00 00 03 25 58 F7"
gen_d50_data_request_sysex :: Int -> Int -> Int -> [Int]
gen_d50_data_request_sysex ch a sz =
    let a' = bits_21_sep_l a
        sz' = bits_21_sep_l sz
        chk = roland_checksum (a' ++ sz')
    in concat [[sysex_status,roland_id,ch,d50_id,rqi_cmd_id]
              ,a',sz'
              ,[chk,sysex_end]]

-- > byte_seq_hex_pp (gen_d50_data_set_sysex 0 1 [50]) == "F0 41 00 14 12 00 00 01 32 4D F7"
-- > byte_seq_hex_pp (gen_d50_data_set_sysex 0 409 [0x10]) == "F0 41 00 14 12 00 03 19 10 54 F7"
gen_d50_data_set_sysex :: Int -> Int -> [Int] -> [Int]
gen_d50_data_set_sysex ch a d =
    let a' = bits_21_sep_l a
        chk = roland_checksum (a' ++ d)
    in concat [[sysex_status,roland_id,ch,d50_id,dti_cmd_id]
              ,a',d
              ,[chk,sysex_end]]


-- > let nm = (Patch,"Lower Tone Fine Tune")
-- > in fmap byte_seq_hex_pp (gen_d50_data_set_sysex_nm 0 nm [0x10]) == Just "F0 41 00 14 12 00 03 19 10 54 F7"
gen_d50_data_set_sysex_nm ch nm d =
    let f a = gen_d50_data_set_sysex ch a d
    in fmap f (named_parameter_to_address nm)

{-

3. EXCLUSIVE COMMUNICATION

3.1 Request (One way) RQ1 11H
( Transmitted only )

Byte Description

a 1111 0000 Exclusive status
b 0100 0001 Roland—ID #
c 0000 nnnn Device—ID # = MIDI basic channel. (0 - 15) where nnnn + 1 = channel #
d 0001 0100 Model—ID # (D-50)
e 0001 0001 Command-ID # (RQI)
f 0aaa aaaa Address MSB          *3—l
g 0bbb bbb  Address
h 0ccc cccc Address LSB
i 0ddd dddd Size MSB             *3-1
j 0eee ecee Size
k 0fff ffff Size LSB
l 0ggg gggg Checksum
m 1111 0111 End of System Exclusive (EOX)

Summed value of the all bytes between Command-ID and EOX
must he 00H (7 bits). It doesn't include Command-ID and
EOX.

*3—1 PG-1OOO transmits this command only when the Parameter Request
button is pushed.  The following values of Address and Size are
transmitted.

Address : [ 00-00-00 ]
Size : [ 00-03-25 ] ( 421bytes )

3.2 Data set (One way) DT1 12H *3—2
( Transmitted and Recognized )

Byte        Description
a 1111 0000 Exclusive status
b 0100 0001 Roland—ID #
c 0000 nnnn Device—ID # = MIDI basic channel. (0 - 15)
d 0001 0100 Model-ID # (D-50)
e 0001 0010 Command-ID # ( DTI )
f 0aaa aaaa Address MSB                 *3—3,5
g 0bbb bbbb Address
h 0ccc cccc Address LSB
i 0ddd dddd Data                        *3-4,5
...
j 0eee eeee Checksum
k 1111 0111 End of System Exclusive

Notes :

*3—3 If aaaaaaa - ccccccc doesn't indicate the address of the
tone parameter or the patch factor, the message will be
ignored.

*3—4 The received value that exceeds the valid range of the parameter
will be ignored.

When the Manual button is pushed, all the parameter values (knob's
positions on the panel) of the Partial, Common and Patch will be
transmitted.

*3-5 See section 4 ( ADDRESS MAPPING OF PARAMETERS AND REMOTE FUNCTION)

4. ADDRESS MAPPING OF PARAMETERS AND REMOTE FUNCTION

4.1 Parameter base address (Top address)

[ 00-00—00 ] Upper Partial 1 (   0 -  53 )
[ 00—00-40 ] Upper Partial 2 (  64 — 117 )
[ 00-01-00 ] Upper Common    ( 128 - 175 )
[ 00—01—40 ] Lower Partial 1 ( 192 - 245 )
[ 00—02—00 ] Lower Partial 2 ( 256 — 309 )
[ 00—02-40 ] Lower Common    ( 320 - 367 )
[ 00-03—00 ] Patch           ( 384 - 420 )

-}

data Tone = Upper | Lower deriving (Eq,Show)
data Partial_Ix = One | Two deriving (Eq,Show)
data Parameter_Type = Partial Tone Partial_Ix | Common Tone | Patch deriving (Eq,Show)

parameter_type_seq :: [Parameter_Type]
parameter_type_seq =
    [Partial Upper One,Partial Upper Two,Common Upper
    ,Partial Lower One,Partial Lower Two,Common Lower
    ,Patch]

partial_ix_pp :: Partial_Ix -> String
partial_ix_pp ix =
    case ix of
      One -> "1"
      Two -> "2"

parameter_type_pp :: Parameter_Type -> String
parameter_type_pp ty =
    case ty of
      Partial tn ix -> unwords [show tn,"Partial",partial_ix_pp ix]
      Common tn -> unwords [show tn,"Common"]
      Patch -> "Patch"

parameter_type_base_address_t3 :: Num t => Parameter_Type -> (t,t,t)
parameter_type_base_address_t3 ty =
    case ty of
      Partial Upper One -> (0x00,0x00,0x00)
      Partial Upper Two -> (0x00,0x00,0x40)
      Common Upper -> (0x00,0x01,0x00)
      Partial Lower One -> (0x00,0x01,0x40)
      Partial Lower Two -> (0x00,0x02,0x00)
      Common Lower -> (0x00,0x02,0x40)
      Patch -> (0x00,0x03,0x00)

parameter_type_base_address :: (Bits t,Num t) => Parameter_Type -> t
parameter_type_base_address = bits_21_join . parameter_type_base_address_t3

-- | Base address, initial offset, number of parameters.
--
-- > sum (map (\ty -> let (_,_,n) = parameter_type_extent ty in n) parameter_type_seq) == 309
-- > let (b,o,n) = parameter_type_extent (last parameter_type_seq) in b + o + n == 421
parameter_type_extent :: (Bits t,Num t) => Parameter_Type -> (t,t,t)
parameter_type_extent ty =
    case ty of
      Partial _ _ -> (parameter_type_base_address ty,0,54)
      Common _ -> (parameter_type_base_address ty,10,38)
      Patch -> (parameter_type_base_address ty,20,17)

range_pp :: Show a => (a,a) -> String
range_pp (p,q) = show p ++ " - " ++ show q

-- | 4.1 Parameter base address (Top address)
d50_parameter_base_address_tbl :: (Bits i,Num i,Show i) => [(i,String,Parameter_Type,String)]
d50_parameter_base_address_tbl =
    let f ty = let (b,o,n) = parameter_type_extent ty
               in (b,parameter_type_pp ty,ty,range_pp (b,b + o + n - 1))
    in map f parameter_type_seq

-- > mapMaybe (\n -> fmap parameter_type_pp (address_to_parameter_type n)) [0 .. 420]
address_to_parameter_type :: (Num a, Ord a, Bits a) => a -> Maybe Parameter_Type
address_to_parameter_type a =
    let f ty = let (b,o,n) = parameter_type_extent ty in a >= b && a < b + o + n
    in find f parameter_type_seq

parameter_type_parameters :: (Enum i,Num i,Show i) => Parameter_Type -> [Parameter i]
parameter_type_parameters ty =
    case ty of
      Partial _ _ -> d50_partial_parameters
      Common _ -> d50_common_parameters
      Patch -> d50_patch_parameters

-- > let ix = [0 .. 420] in zip ix (map address_to_parameter ix)
address_to_parameter :: (Enum i,Num i,Ord i, Bits i,Show i) => i -> Maybe (Parameter_Type,Parameter i)
address_to_parameter a =
    case address_to_parameter_type a of
      Just ty -> let (b,_,_) = parameter_type_extent ty
                     f (i,_,_,_) = i == a - b
                 in fmap (\p -> (ty,p)) (find f (parameter_type_parameters ty))
      Nothing -> Nothing

parameter_lookup :: (Enum i,Num i,Show i) => (Parameter_Type,String) -> Maybe (Parameter_Type,Parameter i)
parameter_lookup (ty,nm) =
    let f (_,nm',_,_) = nm == nm'
    in fmap (\p -> (ty,p)) (find f (parameter_type_parameters ty))

-- > named_parameter_to_address (Patch,"Lower Tone Fine Tune") == Just 409
named_parameter_to_address :: (Bits i,Enum i,Num i,Show i) => (Parameter_Type,String) -> Maybe i
named_parameter_to_address =
    let f (ty,(n,_,_,_)) = let (b,_,_) = parameter_type_extent ty in b + n
    in fmap f . parameter_lookup

{-

4.2 Patch write address
    (Transmitted only)

Transmitted when the Manual Button is pushed twice while holding the Partial Mute button down.

Address Description

[ 00-20-00 ] Patch write function *4-1

*4-l Transmitted a Data byte consisting of two 0011 (2 bytes).

-}

type Parameter i = (i,String,(i,i),String)

-- | 4.3 Partial parameters
--
-- > length d50_partial_parameters == 54
d50_partial_parameters :: Num i => [Parameter i]
d50_partial_parameters =
    [(0,"WG Pitch Coarse",(0,72),"C1 - C7")
    ,(1,"WG Pitch Fine",(0,100),"-50 - +50")
    ,(2,"WG Pitch Keyfollow",(0,16),"-1;-1/2;-4/1;0;1/8;1/4;3/8;1/2;5/6;3/4;7/8;1;5/4;3/2;2;s1;s2")
    ,(3,"WG Mod LFO Mode",(0,3),"OFF;(+);(~);A&L")
    ,(4,"WG Mod P-ENV Mode",(0,2),"OFF;(+);(-)")
    ,(5,"WG Mod Bender Mode",(0,2),"OFF;KF;NORMAL")
    ,(6,"WG Waveform",(0,1),"SQU;SAW")
    ,(7,"WG PCM Wave No.",(0,99),"1 - 100")
    ,(8,"WG Pulse Width",(0,100),"0 - 100")
    ,(9,"WG PW Velocity Range",(0,14),"-7 - +7")
    ,(10,"WG PW LFO Select",(0,5),"+1;-1;+2;-2;+3;-3")
    ,(11,"WG PW LFO Depth",(0,100),"0 - 100")
    ,(12,"WG PW Aftertouch Range",(0,14),"-7 - +7")
    ,(13,"TVF Cutoff Frequency",(0,100),"0 - 100")
    ,(14,"TVF Resonance",(0,30),"0 - 30")
    ,(15,"TVF Keyfollow",(0,14),"-1;-1/2;-4/1;0;1/8;1/4;3/8;1/2;5/8;3/4;7/8;1;5/4;3/2;2")
    ,(16,"TVF Bias Point/Direction",(0,127),"<A1 - <C7;>A1 - >C7")
    ,(17,"TVF Bias Level",(0,14),"-7 - +7")
    ,(18,"TVF ENV Depth",(0,100),"0 - 100")
    ,(19,"TVF ENV Velocity Range",(0,100),"0 - 100")
    ,(20,"TVF ENV Depth Keyfollow",(0,4),"0 - 4")
    ,(21,"TVF ENV Time Keyfollow",(0,10),"0 - 10")
    ,(22,"TVF ENV Time 1",(0,100),"0 - 100")
    ,(23,"TVF ENV Time 2",(0,100),"0 - 100")
    ,(24,"TVF ENV Time 3",(0,100),"0 - 100")
    ,(25,"TVF ENV Time 4",(0,100),"0 - 100")
    ,(26,"TVF ENV Time 5",(0,100),"0 - 100")
    ,(27,"TVF ENV Level 1",(0,100),"0 - 100")
    ,(28,"TVF ENV Level 2",(0,100),"0 - 100")
    ,(29,"TVF ENV Level 3",(0,100),"0 - 100")
    ,(30,"TVF ENV Sustain Level",(0,100),"0 - 100")
    ,(31,"TVF ENV End Level",(0,1),"0;100")
    ,(32,"TVF Mod LFO Select",(0,5),"+1;-1;+2;-2;+3;-3")
    ,(33,"TVF Mod LFO Depth",(0,100),"0 - 100")
    ,(34,"TVF Mod Aftertouch Range",(0,14),"-7 - +7")
    ,(35,"TVA Level",(0,100),"0 - 100")
    ,(36,"TVA Velocity Range",(0,100),"-50 - +50")
    ,(37,"TVA Bias Point Direction",(0,127),"<A1 - <C7;>A1 - >C7")
    ,(38,"TVA Bias Level",(0,12),"-12 - 0")
    ,(39,"TVA ENV Time 1",(0,100),"0 - 100")
    ,(40,"TVA ENV Time 2",(0,100),"0 - 100")
    ,(41,"TVA ENV Time 3",(0,100),"0 - 100")
    ,(42,"TVA ENV Time 4",(0,100),"0 - 100")
    ,(43,"TVA ENV Time 5",(0,100),"0 - 100")
    ,(44,"TVA ENV Level 1",(0,100),"0 - 100")
    ,(45,"TVA ENV Level 2",(0,100),"0 - 100")
    ,(46,"TVA ENV Level 3",(0,100),"0 - 100")
    ,(47,"TVA ENV Sustain Level",(0,100),"0 - 100")
    ,(48,"TVA ENV End Level",(0,1),"0;100")
    ,(49,"TVA ENV T1 Velo Follow",(0,4),"0 - 4")
    ,(50,"TVA ENV Time Keyfollow",(0,4),"0 - 4")
    ,(51,"TVA Mod LFO Select",(0,5),"+1;-1;+2;-2;+3;-3")
    ,(52,"TVA Mod LFO Depth",(0,100),"0 - 100")
    ,(53,"TVA Mod Aftertouch Range",(0,14),"-7 - +7")
    ]

-- | 4.4 Common parameters.
--
-- > length d50_common_pactors == 38
d50_common_factors :: Num i => [Parameter i]
d50_common_factors =
    [(10,"Structure No.",(0,6),"1 - 7")
    ,(11,"P-ENV Velocity Range",(0,2),"0 - 2")
    ,(12,"P-ENV Time Keyfollow",(0,4),"0 - 4")
    ,(13,"P-ENV Time 1",(0,50),"0 - 50")
    ,(14,"P-ENV Time 2",(0,50),"0 - 50")
    ,(15,"P-ENV Time 3",(0,50),"0 - 50")
    ,(16,"P-ENV Time 4",(0,50),"0 - 50")
    ,(17,"P-ENV Level 0",(0,100),"-50 - +50")
    ,(18,"P-ENV Level 1",(0,100),"-50 - +50")
    ,(19,"P-ENV Level 2",(0,100),"-50 - +50")
    ,(20,"P-ENV Sustain Level",(0,100),"-50 - +50")
    ,(21,"P-ENV End Level",(0,100),"-50 - +50")
    ,(22,"Pitch Mod LFO Depth",(0,100),"0 - 100")
    ,(23,"Pitch Mod Lever",(0,100),"0 - 100")
    ,(24,"Pitch Mod Aftertouch",(0,100),"0 - 100")
    ,(25,"LFO-1 Waveform",(0,3),"TRI;SAW;SQU;RND")
    ,(26,"LFO-1 Rate",(0,100),"0 - 100")
    ,(27,"LFO-1 Delay Time",(0,100),"0 - 100")
    ,(28,"LFO-1 Sync",(0,2),"OFF;ON;KEY")
    ,(29,"LFO-2 Waveform",(0,3),"TRI;SAW;SQU;RND")
    ,(30,"LFO-2 Rate",(0,100),"0 - 100")
    ,(31,"LFO-2 Delay Time",(0,100),"0 - 100")
    ,(32,"LFO-2 Sync",(0,1),"OFF;ON")
    ,(33,"LFO-3 Waveform",(0,3),"TRI;SAW;SQU;RND")
    ,(34,"LFO-3 Rate",(0,100),"0 - 100")
    ,(35,"LFO-3 Delay Time",(0,100),"0 - 100")
    ,(36,"LFO-S Sync",(0,1),"OFF;ON")
    ,(37,"Low EQ Frequency",(0,15),"63;75;88;105;125;150;175;210;250;300;350;420;500;600;700;840")
    ,(38,"Low EQ Gain",(0,24),"-12 - +12")
    ,(39,"High EQ Frequency",(0,21),"250;300;350;420;500;600;700;840;1.0;1.2;1.4;1.7;2.0;2.4;2.8;3.4;4.0;4.8;5.7;6.7;8.0;9.5")
    ,(40,"High EQ Q",(0,8),"0.3;0.5;0.7;1.0;1.4;2.0;3.0;4.2;6.0")
    ,(41,"High EQ Gain",(0,24),"-12 - +12")
    ,(42,"Chorus Type",(0,7),"1 - 8")
    ,(43,"Chorus Rate",(0,100),"0 - 100")
    ,(44,"Chorus Depth",(0,100),"0 - 100")
    ,(45,"Chorus Balance",(0,100),"0 - 100")
    ,(46,"Partial Mute",(0,3),"MM;SM;MS;SS")
    ,(47,"Partial Balance",(0,100),"0 - 100")
     ]

d50_char_code_usr :: String
d50_char_code_usr = "' ';'A' - 'Z';'a' - 'z';'1' - '9';'0';'-'"

-- | 'd50_common_factors' preceded by tone name.
d50_common_parameters :: (Enum i,Num i,Show i) => [Parameter i]
d50_common_parameters =
    let ch n = (n,"Tone Name " ++ show (n + 1),(0,60),d50_char_code_usr)
    in map ch [0 .. 9] ++ d50_common_factors

-- | 4.5 Patch Factors.
--
-- > length d50_patch_factors == 17
d50_patch_factors :: Num i => [Parameter i]
d50_patch_factors =
    [(18,"Key Mode",(0,8),"WHOLE;DUAL;SPLIT;SEPARATE;WHOLE-S;DUAL-S;SPLIT-US;SPLIT-LS;SEPARATE-S")
    ,(19,"Split Point",(0,60),"C2 - C7")
    ,(20,"Portamento Mode",(0,2),"U;L;UL")
    ,(21,"Hold Mode",(0,2),"U;L;UL")
    ,(22,"Upper Tone Key Shift",(0,48),"-24 - +24")
    ,(23,"Lower Tone Key Shift",(0,48),"-24 - +24")
    ,(24,"Upper Tone Fine Tune",(0,100),"-50 - +50")
    ,(25,"Lower Tone Fine Tune",(0,100),"-50 - +50")
    ,(26,"Bender Range",(0,12),"0 - 12")
    ,(27,"After Bend Range",(0,24),"-12 - +12")
    ,(28,"Portamento Time",(0,100),"0 - 100")
    ,(29,"Output Mode",(0,3),"1 - 4")
    ,(30,"Reverb Type",(0,31),"1 - 32")
    ,(31,"Reverb Balance",(0,100),"0 - 100")
    ,(32,"Total Volume",(0,100),"0 - 100")
    ,(33,"Tone Balance",(0,100),"0 - 100")
    ,(34,"Chase Mode",(0,2),"UL;ULL;ULU")
    ,(35,"Chase Level",(0,100),"0 - 100")
    ,(36,"Chase Time",(0,100),"0 - 100")
    ]

-- | 'd50_patch_factors' preceded by patch name.
d50_patch_parameters :: (Enum i,Num i,Show i) => [Parameter i]
d50_patch_parameters =
    let ch n = (n,"Patch Name " ++ show (n + 1),(0,60),d50_char_code_usr)
    in map ch [0 .. 17] ++ d50_patch_factors

reads_to_read_precise :: ReadS t -> String -> Maybe t
reads_to_read_precise f s =
    case f s of
      [(r,[])] -> Just r
      _ -> Nothing

reads_to_read_precise_err :: ReadS t -> String -> t
reads_to_read_precise_err f = fromMaybe (error "reads_to_read_precise_err") . reads_to_read_precise f

read_hex_byte :: (Eq t,Num t) => String -> t
read_hex_byte s =
    case s of
      [_,_] -> reads_to_read_precise_err readHex s
      _ -> error "read_hex_byte"

d50_patch_pp :: (Bits i,Num i,Ord i,Show i,Enum i) => [i] -> [String]
d50_patch_pp =
    let hdr = "ADDRESS,PARAMETER TYPE,INDEX,NAME,VALUE,RANGE,USER"
        f (a,v) = case address_to_parameter a of
                    Just (ty,(ix,nm,(l,r),usr)) ->
                        if v < l || v > r
                        then "ERROR: VALUE OUT OF RANGE:" ++ show (a,v,l,r)
                        else intercalate "," [show a,parameter_type_pp ty,show ix,nm,show v,range_pp (l,r),usr]
                    _ -> if v == 0 then "" else "ERROR: VALUE NOT ZERO AT NON-PARAMETER ADDRESS" ++ show (a,v)
    in (hdr :) . map f . zip [0 ..]

-- | Load text file consisting of at least 421 white-space separated two-character hexidecimal byte values.
--
-- > p <- load_d50_text "/home/rohan/uc/invisible/day/d50/d50.hex.text"
-- > writeFile "/home/rohan/uc/invisible/day/d50/d50.csv" (unlines (filter (not . null) (d50_patch_pp p)))
load_d50_text :: (Eq t,Num t) => FilePath -> IO [t]
load_d50_text fn = do
  s <- readFile fn
  return (map (\x -> read_hex_byte x) (take 421 (words s)))

-- | Tone structure number diagram in plain text.
structure_no_text :: (Eq a, Num a) => a -> String
structure_no_text n =
    case n + 1 of
      1 -> "S1 + S2"
      2 -> "S1 + RMOD (S1 + S2)"
      3 -> "P1 + S2"
      4 -> "P1 + RMOD (P1 + S2)"
      5 -> "S1 + RMOD (S1 + P2)"
      6 -> "P1 + P2"
      7 -> "P1 + RMOD (P1 + P2)"
      _ -> error "structure_text"

d50_char_table :: (Enum i,Num i) => [(i,Char)]
d50_char_table =
    let ch = [[' '],['A'..'Z'],['a'..'z'],['1'..'9'],['0','-']]
    in zip [0..] (concat ch)

-- > mapMaybe d50_byte_to_char [9,40,38,27,40,30] == "Inland"
d50_byte_to_char :: (Enum i, Eq i, Num i) => i -> Maybe Char
d50_byte_to_char n = lookup n d50_char_table

