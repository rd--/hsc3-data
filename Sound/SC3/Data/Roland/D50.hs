{- | ROLAND D-50

Roland Corporation.
/Roland MIDI Linear Synthesiser Programmer PG-1000 Owner's Manual/.
Hamamatsu, JP, 1987.
<http://cdn.roland.com/assets/media/pdf/PG-1000_OM.pdf>

Roland Corporation.
/D-50 Service Notes/.
Hamamatsu, JP, 1987.
<http://www.synfo.nl/servicemanuals/Roland/D-50_SERVICE_NOTES.pdf>

-}
module Sound.SC3.Data.Roland.D50 where

import Control.Monad {- base -}
import qualified Data.ByteString as B {- bytestring -}
import Data.Char {- base -}
import Data.List {- base -}
import Data.List.Split {- split -}
import Data.Maybe {- base -}
import Data.Word {- base -}

import qualified Music.Theory.Byte as T {- hmt -}
import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Tuple as T {- hmt -}

import qualified Sound.Midi.Constant as M {- midi-osc -}
import qualified Sound.Midi.Type as M {- midi-osc -}

-- | 8-bit unsigned integer, ie. parameter values, midi data etc.
type U8 = Word8

-- | 8-bit signed integer, ie. USR-OFFSET
type I8 = Int

-- | 24-bit unsigned integer, ie. ADDRESS and related offsets and extents.
type U24 = Int

int_to_u8 :: Int -> U8
int_to_u8 n = if n >= 0 && n <= 255 then fromIntegral n else error "int_to_u8"

u8_to_i8 :: U8 -> I8
u8_to_i8 = fromIntegral

u8_to_u24 :: U8 -> U24
u8_to_u24 = fromIntegral

-- | Parameter address.
type ADDRESS = U24

-- | A patch has two tones, 'Upper' and 'Lower'.
data Tone = Upper | Lower deriving (Eq,Show)

-- | A 'Tone' has two partials, 'One' and 'Two'.
data Partial_Ix = One | Two deriving (Eq,Show)

-- | Parameters are of one of seven types, four 'Partial's
-- (U1,U2,L1,L2), two 'Common' (U,L), or 'Patch'.
data Parameter_Type = Partial Tone Partial_Ix | Common Tone | Patch deriving (Eq,Show)

-- | (INDEX,NAME,STEPS,USR-OFFSET,USR-STRING)
type Parameter = (U24,String,U8,I8,String)

parameter_ix :: Parameter -> U24
parameter_ix (ix,_,_,_,_) = ix

parameter_name :: Parameter -> String
parameter_name (_,nm,_,_,_) = nm

parameter_user_offset :: Parameter -> Int
parameter_user_offset (_,_,_,o,_) = o

-- | (GROUP-NAME,PARAMETER-NAME-SEQ,PARAMETER-IX-SEQ)
type PARAM_GROUP = (String,String,[Int])

-- | D50 SYSEX command ID.
data D50_SYSEX_CMD = RQI_CMD -- ^ REQUEST DATA - ONE WAY
                   | DTI_CMD -- ^ DATA SET - ONE WAY
                   | WSD_CMD -- ^ WANT TO SEND DATA
                   | RQD_CMD -- ^ REQUEST DATA
                   | DAT_CMD -- ^ DATA SET
                   | ACK_CMD -- ^ ACKNOWLEDGE
                   | EOD_CMD -- ^ END OF DATA
                   | ERR_CMD -- ^ COMMUNICATION ERROR
                   | RJC_CMD -- ^ REJECTION
                     deriving (Eq,Show)

-- | DATA-SET COMMAND (DTI|DAT), (CMD,DEVICE-ID,ADDRESS,DATA)
type DSC = (D50_SYSEX_CMD,U8,ADDRESS,[U8])

dsc_address :: DSC -> ADDRESS
dsc_address (_,_,a,_) = a

dsc_data :: DSC -> [U8]
dsc_data (_,_,_,d) = d

-- * COMMON

-- | Unpack 'U24' to three 'U8'.
u24_unpack :: U24 -> [U8]
u24_unpack = map fromIntegral . T.t3_to_list . M.bits_21_sep . fromIntegral

-- | Pack 'U24' from three 'U8'.
u24_pack :: [U8] -> U24
u24_pack = fromIntegral . M.bits_21_join . T.t3_from_list . map fromIntegral

-- | Range as @p - q@.
--
-- > range_pp (1,7) == "1 - 7"
range_pp :: Show a => (a,a) -> String
range_pp (p,q) = show p ++ " - " ++ show q

-- | Segment byte-sequence into SYSEX messages, no verification,
-- ie. seperate before each @0xF0@.
sysex_segment :: [U8] -> [[U8]]
sysex_segment = tail . T.split_before 0xF0

-- * ROLAND

-- | D50 device ID.
--
-- > :set -XBinaryLiterals
-- > (d50_id == 0x14,d50_id == 0b00010100)
d50_id :: U8
d50_id = 0x14

-- | The checksum is a derived from the address (three bytes)
-- and the data bytes.
--
-- <ftp://ftp.monash.edu.au/pub/midi/DOC/Roland-checksum>
--
-- > roland_checksum [0x40,0x00,0x04,0x64] == 0x58
roland_checksum :: [U8] -> U8
roland_checksum =
    let f n w =
            case w of
              [] -> 0x80 - n
              x:w' -> let n' = n + x in f (if n' > 0x80 then n' - 0x80 else n') w'
    in f 0

-- | Translate one-indexed (BANK-NUMBER,PATCH-NUMBER) index to zero linear index.
--
-- > map bank_to_ix [(1,1),(2,3),(7,4),(8,8)] == [0,10,51,63]
bank_to_ix :: Num n => (n,n) -> n
bank_to_ix (p,q) = ((p - 1) * 8) + (q - 1)

-- | Inverse of 'bank_to_ix'.
--
-- > map ix_to_bank [0,10,51,63] == [(1,1),(2,3),(7,4),(8,8)]
ix_to_bank :: Integral i => i -> (i,i)
ix_to_bank n = let (p,q) = n `divMod` 8 in (p + 1,q + 1)

-- * D-50 CMD

-- | Table mapping 'D50_SYSEX_CMD' to it's 'U8' code.
d50_sysex_cmd_tbl :: [(D50_SYSEX_CMD,U8)]
d50_sysex_cmd_tbl =
    [(RQI_CMD,0x11)
    ,(DTI_CMD,0x12)
    ,(WSD_CMD,0x40)
    ,(RQD_CMD,0x41)
    ,(DAT_CMD,0x42)
    ,(ACK_CMD,0x43)
    ,(EOD_CMD,0x45)
    ,(ERR_CMD,0x4E)
    ,(RJC_CMD,0x4F)]

maybe_err :: String -> Maybe a -> a
maybe_err str = fromMaybe (error str)

-- | SYSEX_CMD to 8-Bit identifier.
--
-- > let k = d50_sysex_cmd_id DTI_CMD in (k == 0x12,k == 0b00010010)
-- > let k = d50_sysex_cmd_id RQI_CMD in (k == 0x11,k == 0b00010001)
d50_sysex_cmd_id :: D50_SYSEX_CMD -> U8
d50_sysex_cmd_id cmd = maybe_err "d50_sysex_cmd_id" (lookup cmd d50_sysex_cmd_tbl)

-- | Inverse of 'd50_sysex_cmd_id'.
d50_sysex_cmd_parse :: U8 -> D50_SYSEX_CMD
d50_sysex_cmd_parse i = maybe_err "d50_sysex_cmd_parse" (T.reverse_lookup i d50_sysex_cmd_tbl)

d50_cmd_hdr :: U8 -> D50_SYSEX_CMD -> [U8]
d50_cmd_hdr ch cmd = [M.sysex_status,M.roland_id,ch,d50_id,d50_sysex_cmd_id cmd]

d50_cmd_data_chk :: [U8] -> [U8]
d50_cmd_data_chk dat = dat ++ [roland_checksum dat,M.sysex_end]

d50_addr_sz_cmd :: D50_SYSEX_CMD -> U8 -> ADDRESS -> U24 -> [U8]
d50_addr_sz_cmd cmd ch a sz =
    let dat = u24_unpack a ++ u24_unpack sz
    in concat [d50_cmd_hdr ch cmd,d50_cmd_data_chk dat]

-- | Generate WSD command SYSEX.
d50_wsd_gen :: U8 -> ADDRESS -> U24 -> [U8]
d50_wsd_gen = d50_addr_sz_cmd WSD_CMD

-- | Generate WSD command SYSEX.
d50_rqd_gen :: U8 -> ADDRESS -> U24 -> [U8]
d50_rqd_gen = d50_addr_sz_cmd RQD_CMD

-- | Generate RQI command SYSEX.
--
-- > let r = map T.read_hex_byte (words "F0 41 00 14 11 00 00 00 00 03 40 3D F7")
-- > in d50_rqi_gen 0 0 0x1C0 == r
d50_rqi_gen :: U8 -> ADDRESS -> U24 -> [U8]
d50_rqi_gen = d50_addr_sz_cmd RQI_CMD

-- > d50_cmd_parse (d50_ack_gen 0) == Just (0,0x43,[],0)
d50_ack_gen :: U8 -> [U8]
d50_ack_gen ch = d50_cmd_hdr ch ACK_CMD ++ [M.sysex_end]

-- > d50_cmd_parse (d50_eod_gen 0) == Just (0,0x45,[],0)
d50_eod_gen :: U8 -> [U8]
d50_eod_gen ch = d50_cmd_hdr ch EOD_CMD ++ [M.sysex_end]

-- > d50_cmd_parse (d50_rjc_gen 0) == Just (0,0x4F,[],0)
d50_rjc_gen :: U8 -> [U8]
d50_rjc_gen ch = d50_cmd_hdr ch RJC_CMD ++ [M.sysex_end]

-- | Parse SYSEX to (CH,CMD,DAT,#DAT).
--
-- > d50_cmd_parse (d50_dsc_gen (DTI_CMD,0,1,[50])) == Just (0,18,[0,0,1,50,77],5)
d50_cmd_parse :: [U8] -> Maybe (U8,U8,[U8],U24)
d50_cmd_parse b =
    let b0:b1:b2:b3:b4:b' = b
        dat_n = length b' - 1
        (dat,[eox]) = splitAt dat_n b'
    in if any not [b0 == M.sysex_status,b1 == M.roland_id,b3 == d50_id,eox == M.sysex_end]
       then Nothing
       else Just (b2,b4,dat,dat_n)

-- | Check and remove CHECKSUM from DATA.
--
-- > d50_data_chk ([0,0,1,50,77],5) == Just ([0,0,1,50],4)
d50_data_chk :: ([U8],U24) -> Maybe ([U8],U24)
d50_data_chk (dat,dat_n) =
    let (dat',[ch]) = splitAt (dat_n - 1) dat
    in if roland_checksum dat' == ch
       then Just (dat',dat_n - 1)
       else Nothing

-- | Remove and pack ADDRESS from start of DAT.
--
-- > d50_data_addr ([0,0,1,50],4) == Just (1,[50],1)
d50_data_addr :: ([U8],U24) -> Maybe (U24,[U8],U24)
d50_data_addr (dat,dat_n) =
    case dat of
      a0:a1:a2:dat' -> Just (u24_pack [a0,a1,a2],dat',dat_n - 3)
      _ -> Nothing

-- | Parse DSC (DTI|DAT) SYSEX message.
--
-- > let b = d50_dsc_gen (DTI_CMD,0,1,[50])
-- > d50_dsc_parse b == Just (DTI_CMD,0,1,[50])
d50_dsc_parse :: [U8] -> Maybe DSC
d50_dsc_parse b =
    let Just (ch,cmd,dat,dat_n) = d50_cmd_parse b
        Just (dat',dat_n') = d50_data_chk (dat,dat_n)
        Just (addr,dat'',_dat_n'') = d50_data_addr (dat',dat_n')
        cmd' = d50_sysex_cmd_parse cmd
    in if cmd' == DTI_CMD || cmd' == DAT_CMD
       then Just (cmd',ch,addr,dat'')
       else Nothing

d50_dsc_parse_err :: [U8] -> DSC
d50_dsc_parse_err = fromMaybe (error "d50_dsc_parse") . d50_dsc_parse

-- | Generate DSC (DTI|DAT) SYSEX message.
--
-- > T.byte_seq_hex_pp (d50_dsc_gen (0,1,[50])) == "F0 41 00 14 12 00 00 01 32 4D F7"
-- > T.byte_seq_hex_pp (d50_dsc_gen (0,409,[0x10])) == "F0 41 00 14 12 00 03 19 10 54 F7"
d50_dsc_gen :: DSC -> [U8]
d50_dsc_gen (cmd,ch,a,d) =
    let dat = u24_unpack a ++ d
    in if length d > 256
       then error "d50_dsc_gen: #DATA > 256"
       else concat [d50_cmd_hdr ch cmd,d50_cmd_data_chk dat]

-- | Generate a sequence of DSC messages segmenting data sets longer than 256 elements.
d50_dsc_gen_seq :: DSC -> [[U8]]
d50_dsc_gen_seq (cmd,ch,a,d) =
    if null d
    then []
    else if length d <= 256
         then [d50_dsc_gen (cmd,ch,a,d)]
         else d50_dsc_gen (cmd,ch,a,take 256 d) : d50_dsc_gen_seq (cmd,ch,a + 256,drop 256 d)

-- > let {nm = (Patch,"Lower Tone Fine Tune")
-- >     ;r = "F0 41 00 14 12 00 03 19 10 54 F7"}
-- > in fmap T.byte_seq_hex_pp (d50_gen_dti_nm 0 nm [0x10]) == Just r
d50_gen_dti_nm :: U8 -> (Parameter_Type,String) -> [U8] -> Maybe [U8]
d50_gen_dti_nm ch nm d =
    let f a = d50_dsc_gen (DTI_CMD,ch,a,d)
    in fmap f (named_parameter_to_address nm)

-- * PARTIAL IX

partial_ix_pp :: Partial_Ix -> String
partial_ix_pp ix =
    case ix of
      One -> "1"
      Two -> "2"

-- * PARAMETER TYPE

-- | The parameter type sequence (ie. in ascending ADDRESS order).
parameter_type_seq :: [Parameter_Type]
parameter_type_seq =
    [Partial Upper One,Partial Upper Two,Common Upper
    ,Partial Lower One,Partial Lower Two,Common Lower
    ,Patch]

-- > map parameter_type_pp parameter_type_seq
parameter_type_pp :: Parameter_Type -> String
parameter_type_pp ty =
    case ty of
      Partial tn ix -> unwords [show tn,"Partial",partial_ix_pp ix]
      Common tn -> unwords [show tn,"Common"]
      Patch -> "Patch"

-- * ADDRESS

-- | Base address (offset) for each parameter type (as 7-bit 3-tuple).
parameter_type_base_address_t3 :: Parameter_Type -> (U8,U8,U8)
parameter_type_base_address_t3 ty =
    case ty of
      Partial Upper One -> (0x00,0x00,0x00) -- 0x000 = 000
      Partial Upper Two -> (0x00,0x00,0x40) -- 0x040 = 064
      Common Upper -> (0x00,0x01,0x00)
      Partial Lower One -> (0x00,0x01,0x40) -- 0x0C0 = 192
      Partial Lower Two -> (0x00,0x02,0x00) -- 0x100 = 256
      Common Lower -> (0x00,0x02,0x40)
      Patch -> (0x00,0x03,0x00)

-- | Base address (offset) for each parameter type (as 'ADDRESS').
--
-- > map parameter_type_base_address parameter_type_seq == [0,64,128,192,256,320,384]
-- > [0,64,128,192,256,320,384] == [0x000,0x040,0x080,0x0C0,0x100,0x140,0x180]
parameter_type_base_address :: Parameter_Type -> ADDRESS
parameter_type_base_address = u24_pack . T.t3_to_list . parameter_type_base_address_t3

partial_base_address_seq :: [ADDRESS]
partial_base_address_seq = [0x000,0x040,0x0C0,0x100]

-- | Base address, initial offset, number of parameters.
--
-- > sum (map (\ty -> let (_,_,n) = parameter_type_extent ty in n) parameter_type_seq) == 312
-- > let (b,o,n) = parameter_type_extent (last parameter_type_seq) in b + o + n == 424
parameter_type_extent :: Parameter_Type -> (ADDRESS,U24,U24)
parameter_type_extent ty =
    case ty of
      Partial _ _ -> (parameter_type_base_address ty,0,54)
      Common _ -> (parameter_type_base_address ty,10,38)
      Patch -> (parameter_type_base_address ty,20,20)

-- | Parameter base address (Top address) (4.1)
parameter_type_address_segments :: [(Parameter_Type,(ADDRESS,U24))]
parameter_type_address_segments =
    let f ty = let (b,o,n) = parameter_type_extent ty
               in (ty,(b,b + o + n - 1))
    in map f parameter_type_seq

-- | Split sequence into groups based on 'Parameter_Type' sequence.
parameter_segment :: [t] -> [(Parameter_Type,[t])]
parameter_segment p =
    let f (ty,(b,x)) = (ty,map (p!!) [b .. x])
    in map f parameter_type_address_segments

-- | 4.1 Parameter base address (Top address)
d50_parameter_base_address_tbl :: [(ADDRESS,String,Parameter_Type,String)]
d50_parameter_base_address_tbl =
    let f (ty,(b,x)) = (b,parameter_type_pp ty,ty,range_pp (b,x))
    in map f parameter_type_address_segments

-- | Determine 'Parameter_Type' of value at 'ADDRESS'.
--
-- > mapMaybe (\n -> fmap parameter_type_pp (address_to_parameter_type n)) [0 .. 420]
address_to_parameter_type :: ADDRESS -> Maybe Parameter_Type
address_to_parameter_type a =
    let f ty = let (b,o,n) = parameter_type_extent ty in a >= b && a < b + o + n
    in find f parameter_type_seq

-- > let ix = [0 .. 420] in zip ix (map address_to_parameter ix)
address_to_parameter :: ADDRESS -> Maybe (Parameter_Type,Parameter)
address_to_parameter a =
    case address_to_parameter_type a of
      Just ty -> let (b,_,_) = parameter_type_extent ty
                     f (i,_,_,_,_) = i == a - b
                 in fmap (\p -> (ty,p)) (find f (parameter_type_parameters ty))
      Nothing -> Nothing

parameter_lookup :: (Parameter_Type,String) -> Maybe (Parameter_Type,Parameter)
parameter_lookup (ty,nm) =
    let f (_,nm',_,_,_) = nm == nm'
    in fmap (\p -> (ty,p)) (find f (parameter_type_parameters ty))

-- > named_parameter_to_address (Patch,"Lower Tone Fine Tune") == Just 409
-- > named_parameter_to_address (Patch,"Patch Name 1") == Just 384
-- > named_parameter_to_address (Common Upper,"Tone Name 1") == Just 128
named_parameter_to_address :: (Parameter_Type,String) -> Maybe ADDRESS
named_parameter_to_address =
    let f (ty,(n,_,_,_,_)) = let (b,_,_) = parameter_type_extent ty in b + n
    in fmap f . parameter_lookup

{- | Base address for patch memory /n/ (0,63).

> M.bits_21_join (0x02,0x00,0x00) == 0x8000 -- 32768
> M.bits_21_join (0x02,0x03,0x40) == 0x81C0 -- 33216
> M.bits_21_join (0x03,0x60,0x00) == 0xF000 -- 61440
> 33216 - 32768 == 0x01C0 -- 448
> 32768 + (448 * 64) == 0xF000 -- 61440
> 61440 - 448 == 0xEE40 -- 60992
> M.bits_21_sep 60992 == (0x03,0x5C,0x40)

-}
patch_memory_base :: U8 -> ADDRESS
patch_memory_base n = 32768 + (448 * u8_to_u24 n)

{- | Base address for reverb memory /n/ (0,15)

> M.bits_21_join (0x03,0x60,0x00) == 61440
> M.bits_21_join (0x03,0x62,0x78) == 61816
> M.bits_21_join (0x04,0x0C,0x08) == 67080
> 61816 - 61440 == 376
> 61440 + (376 * (32 - 17)) == 67080

-}
reverb_memory_base :: U8 -> ADDRESS
reverb_memory_base n = 61440 + (376 * u8_to_u24 n)

-- * CHAR

-- | Table mapping byte to ASCII character.
d50_char_table :: [(U8,Char)]
d50_char_table =
    let ch = [[' '],['A'..'Z'],['a'..'z'],['1'..'9'],['0','-','?','?','-']]
    in zip [0..] (concat ch)

-- | Lookup in 'd50_char_table'.
--
-- > mapMaybe d50_byte_to_char [9,40,38,27,40,30] == "Inland"
d50_byte_to_char :: U8 -> Maybe Char
d50_byte_to_char n = lookup n d50_char_table

d50_byte_to_char_err :: U8 -> Char
d50_byte_to_char_err = fromMaybe (error "d50_byte_to_char") . d50_byte_to_char

-- | Reverse lookup in 'd50_char_table'.
--
-- > mapMaybe d50_char_to_byte "Inland" == [9,40,38,27,40,30]
d50_char_to_byte :: Char -> Maybe U8
d50_char_to_byte c = T.reverse_lookup c d50_char_table

-- * USR/STR

pitch_class_seq :: [String]
pitch_class_seq = words "C C# D D# E F F# G G# A A# B"

pitch_seq :: [String]
pitch_seq = [p ++ show o | o <- [1::Int .. 7],p <- pitch_class_seq]

-- | "C1 - C7"
wg_pitch_coarse_usr :: String
wg_pitch_coarse_usr = intercalate ";" (take 73 pitch_seq)

wg_pitch_kf_usr :: String
wg_pitch_kf_usr = "-1;-1/2;-1/4;0;1/8;1/4;3/8;1/2;5/6;3/4;7/8;1;5/4;3/2;2;s1;s2"

wg_pitch_kf_rat :: Fractional n => ([n],[String])
wg_pitch_kf_rat = ([-1,1/2,-1/4,0,1/8,1/4,3/8,1/2,5/6,3/4,7/8,1,5/4,3/2,2],["S1","S2"])

-- > mapMaybe wg_pitch_kf_to_enum [1/8,1/4,1] == [4,5,11]
wg_pitch_kf_to_enum :: (Eq n,Fractional n) => n -> Maybe U8
wg_pitch_kf_to_enum n = fmap int_to_u8 (findIndex (== n) (fst wg_pitch_kf_rat))

wg_pitch_kf_to_enum_err :: (Eq n,Fractional n) => n -> U8
wg_pitch_kf_to_enum_err = fromMaybe (error "wg_pitch_kf_to_enum") . wg_pitch_kf_to_enum

tvf_kf_usr :: String
tvf_kf_usr = "-1;-1/2;-4/1;0;1/8;1/4;3/8;1/2;5/8;3/4;7/8;1;5/4;3/2;2"

-- | "<A1 - <C7;>A1 - >C7"
bias_point_direction_usr :: String
bias_point_direction_usr =
    let p = take 64 (drop 9 pitch_seq)
    in intercalate ";" (map ('<' :) p ++ map ('>' :) p)

eq_lf_usr :: String
eq_lf_usr = "63;75;88;105;125;150;175;210;250;300;350;420;500;600;700;840"

eq_hf_usr :: String
eq_hf_usr =
    "250;300;350;420;500;600;700;840;" ++
    "1.0;1.2;1.4;1.7;2.0;2.4;2.8;3.4;4.0;4.8;5.7;6.7;8.0;9.5"

-- | /USR/ string indicating 'Char' enumeration sequence.
-- "' ';'A' - 'Z';'a' - 'z';'1' - '9';'0';'-'"
d50_char_code_usr :: String
d50_char_code_usr = intersperse ';' (map snd d50_char_table)

key_mode_usr :: String
key_mode_usr = "WHOLE;DUAL;SPLIT;SEPARATE;WHOLE-S;DUAL-S;SPLIT-US;SPLIT-LS;SEPARATE-S"

-- * PP

-- | Tone structure number diagram in plain text.
--
-- > map structure_pp [1 .. 7]
structure_pp :: U8 -> String
structure_pp n =
    case n + 1 of
      1 -> "S1 + S2"
      2 -> "S1 + RMOD (S1 + S2)"
      3 -> "P1 + S2"
      4 -> "P1 + RMOD (P1 + S2)"
      5 -> "S1 + RMOD (S1 + P2)"
      6 -> "P1 + P2"
      7 -> "P1 + RMOD (P1 + P2)"
      _ -> error "structure_text"

-- * PARTIAL

-- | Partial parameters (4.3)
--
-- > length d50_partial_parameters == 54
d50_partial_parameters :: [Parameter]
d50_partial_parameters =
    [(0,"WG Pitch Coarse",73,0,wg_pitch_coarse_usr)
    ,(1,"WG Pitch Fine",101,-50,"-50 - +50")
    ,(2,"WG Pitch Keyfollow",17,0,wg_pitch_kf_usr)
    ,(3,"WG Mod LFO Mode",4,0,"OFF;(+);(~);A&L")
    ,(4,"WG Mod P-ENV Mode",3,0,"OFF;(+);(-)")
    ,(5,"WG Mod Bender Mode",3,0,"OFF;KF;NORMAL")
    ,(6,"WG Waveform",2,0,"SQU;SAW")
    ,(7,"WG PCM Wave No.",100,1,"1 - 100")
    ,(8,"WG Pulse Width",101,0,"0 - 100")
    ,(9,"WG PW Velocity Range",15,-7,"-7 - +7")
    ,(10,"WG PW LFO Select",6,0,"+1;-1;+2;-2;+3;-3")
    ,(11,"WG PW LFO Depth",101,0,"0 - 100")
    ,(12,"WG PW Aftertouch Range",15,-7,"-7 - +7")
    ,(13,"TVF Cutoff Frequency",101,0,"0 - 100")
    ,(14,"TVF Resonance",31,0,"0 - 30")
    ,(15,"TVF Keyfollow",15,0,tvf_kf_usr)
    ,(16,"TVF Bias Point/Direction",128,0,bias_point_direction_usr)
    ,(17,"TVF Bias Level",15,-7,"-7 - +7")
    ,(18,"TVF ENV Depth",101,0,"0 - 100")
    ,(19,"TVF ENV Velocity Range",101,0,"0 - 100")
    ,(20,"TVF ENV Depth Keyfollow",5,0,"0 - 4")
    ,(21,"TVF ENV Time Keyfollow",11,0,"0 - 10")
    ,(22,"TVF ENV Time 1",101,0,"0 - 100")
    ,(23,"TVF ENV Time 2",101,0,"0 - 100")
    ,(24,"TVF ENV Time 3",101,0,"0 - 100")
    ,(25,"TVF ENV Time 4",101,0,"0 - 100")
    ,(26,"TVF ENV Time 5",101,0,"0 - 100")
    ,(27,"TVF ENV Level 1",101,0,"0 - 100")
    ,(28,"TVF ENV Level 2",101,0,"0 - 100")
    ,(29,"TVF ENV Level 3",101,0,"0 - 100")
    ,(30,"TVF ENV Sustain Level",101,0,"0 - 100")
    ,(31,"TVF ENV End Level",2,0,"0;100")
    ,(32,"TVF Mod LFO Select",6,0,"+1;-1;+2;-2;+3;-3")
    ,(33,"TVF Mod LFO Depth",101,0,"0 - 100")
    ,(34,"TVF Mod Aftertouch Range",15,-7,"-7 - +7")
    ,(35,"TVA Level",101,0,"0 - 100")
    ,(36,"TVA Velocity Range",101,-50,"-50 - +50")
    ,(37,"TVA Bias Point/Direction",128,0,bias_point_direction_usr)
    ,(38,"TVA Bias Level",13,-12,"-12 - 0")
    ,(39,"TVA ENV Time 1",101,0,"0 - 100")
    ,(40,"TVA ENV Time 2",101,0,"0 - 100")
    ,(41,"TVA ENV Time 3",101,0,"0 - 100")
    ,(42,"TVA ENV Time 4",101,0,"0 - 100")
    ,(43,"TVA ENV Time 5",101,0,"0 - 100")
    ,(44,"TVA ENV Level 1",101,0,"0 - 100")
    ,(45,"TVA ENV Level 2",101,0,"0 - 100")
    ,(46,"TVA ENV Level 3",101,0,"0 - 100")
    ,(47,"TVA ENV Sustain Level",101,0,"0 - 100")
    ,(48,"TVA ENV End Level",2,0,"0;100")
    ,(49,"TVA ENV T1 Velo Follow",5,0,"0 - 4")
    ,(50,"TVA ENV Time Keyfollow",5,0,"0 - 4")
    ,(51,"TVA Mod LFO Select",6,0,"+1;-1;+2;-2;+3;-3")
    ,(52,"TVA Mod LFO Depth",101,0,"0 - 100")
    ,(53,"TVA Mod Aftertouch Range",15,-7,"-7 - +7")
    ]

-- | Group structure of partial parameters, as in D-50 menu system.
d50_partial_groups :: [PARAM_GROUP]
d50_partial_groups =
    [("WG Pitch","Cors;Fine;KF",[0..2])
    ,("WG Mod","LFO;ENV;Bend",[3..5])
    ,("WG Form","Wave;PCM",[6..7])
    ,("WG PW","PW;Velo;Aftr;LFO;LFOD",[8,9,12,10,11])
    ,("TVF","Freq;Reso;KF;BP;Blvl",[13..17])
    ,("TVF ENV 1","Dpth;Velo;DKF;TKF",[18..21])
    ,("TVF ENV 2","T1;T2;T3;T4;T5",[22..26])
    ,("TVF ENV 3","L1;L2;L3;SusL;EndL",[27..31])
    ,("TVF MOD","LFO;LFOD;Aftr",[32..34])
    ,("TVA","Levl;Velo;BP;Blvl",[35..38])
    ,("TVA ENV 1","T1;T2;T3;T4;T5",[39..43])
    ,("TVA ENV 2","L1;L2;L3;SusL;EndL",[44..48])
    ,("TVA ENV 3","Velo;TKF",[49..50])
    ,("TVA MOD","LFO;LFOD;Aftr",[51..53])
    ]

-- * COMMON

chorus_type_enum :: [String]
chorus_type_enum =
    ["Chorus 1","Chorus 2"
    ,"Flanger1","Flanger2"
    ,"FBChorus"
    ,"Tremolo","C Trem"
    ,"Dimensn"]

chorus_type_usr :: String
chorus_type_usr = intercalate ";" (map (map toUpper . filter (/= ' ')) chorus_type_enum)

-- | Common parameters (4.4).
--
-- > length d50_common_factors == 38
d50_common_factors :: [Parameter]
d50_common_factors =
    [(10,"Structure No.",7,1,"1 - 7")
    ,(11,"P-ENV Velocity Range",3,0,"0 - 2")
    ,(12,"P-ENV Time Keyfollow",5,0,"0 - 4")
    ,(13,"P-ENV Time 1",51,0,"0 - 50")
    ,(14,"P-ENV Time 2",51,0,"0 - 50")
    ,(15,"P-ENV Time 3",51,0,"0 - 50")
    ,(16,"P-ENV Time 4",51,0,"0 - 50")
    ,(17,"P-ENV Level 0",101,-50,"-50 - +50")
    ,(18,"P-ENV Level 1",101,-50,"-50 - +50")
    ,(19,"P-ENV Level 2",101,-50,"-50 - +50")
    ,(20,"P-ENV Sustain Level",101,-50,"-50 - +50")
    ,(21,"P-ENV End Level",101,-50,"-50 - +50")
    ,(22,"Pitch Mod LFO Depth",101,0,"0 - 100")
    ,(23,"Pitch Mod Lever",101,0,"0 - 100")
    ,(24,"Pitch Mod Aftertouch",101,0,"0 - 100")
    ,(25,"LFO-1 Waveform",4,0,"TRI;SAW;SQU;RND")
    ,(26,"LFO-1 Rate",101,0,"0 - 100")
    ,(27,"LFO-1 Delay Time",101,0,"0 - 100")
    ,(28,"LFO-1 Sync",3,0,"OFF;ON;KEY")
    ,(29,"LFO-2 Waveform",4,0,"TRI;SAW;SQU;RND")
    ,(30,"LFO-2 Rate",101,0,"0 - 100")
    ,(31,"LFO-2 Delay Time",101,0,"0 - 100")
    ,(32,"LFO-2 Sync",2,0,"OFF;ON")
    ,(33,"LFO-3 Waveform",4,0,"TRI;SAW;SQU;RND")
    ,(34,"LFO-3 Rate",101,0,"0 - 100")
    ,(35,"LFO-3 Delay Time",101,0,"0 - 100")
    ,(36,"LFO-S Sync",2,0,"OFF;ON")
    ,(37,"Low EQ Frequency",16,0,eq_lf_usr)
    ,(38,"Low EQ Gain",25,-12,"-12 - +12")
    ,(39,"High EQ Frequency",22,0,eq_hf_usr)
    ,(40,"High EQ Q",9,0,"0.3;0.5;0.7;1.0;1.4;2.0;3.0;4.2;6.0")
    ,(41,"High EQ Gain",25,-12,"-12 - +12")
    ,(42,"Chorus Type",8,1,chorus_type_usr)
    ,(43,"Chorus Rate",101,0,"0 - 100")
    ,(44,"Chorus Depth",101,0,"0 - 100")
    ,(45,"Chorus Balance",101,0,"0 - 100")
    ,(46,"Partial Mute",4,0,"MM;SM;MS;SS")
    ,(47,"Partial Balance",101,0,"0 - 100")
     ]

-- | 'd50_common_factors' preceded by tone name.
--
-- > length d50_common_parameters == 48
d50_common_parameters :: [Parameter]
d50_common_parameters =
    let ch n = (n,"Tone Name " ++ show (n + 1),64,0,d50_char_code_usr)
    in map ch [0 .. 9] ++ d50_common_factors

-- | Group structure of common parameters, as in D-50 menu system.
d50_common_groups :: [PARAM_GROUP]
d50_common_groups =
    [("Tone Name Edit",";;;;;;;;;",[0..9])
    ,("Structure","Str;PMut;PBal",[10,46,47])
    ,("P-ENV Edit 1","Velo;TKF",[11..12])
    ,("P-ENV Edit 2","T1;T2;T3;T4",[13..16])
    ,("P-ENV Edit 3","LO;L1;L2;SusL;EndL",[17..21])
    ,("Pitch Mod Edit","LFOD;Levr;Aftr",[22..24])
    ,("LFO-1 Edit","Wave;Rate;Dely;Sync",[25..28])
    ,("LFO-2 Edit","Wave;Rate;Dely;Sync",[29..32])
    ,("LFO-3 Edit","Wave;Rate;Dely;Sync",[33..36])
    ,("EQ Edit","Lf;Lg;Hf;HQ;Hg",[37..41])
    ,("Chorus Edit","Type;Rate;Dpth;Bal",[42..45])
    ]

-- * PATCH

-- | 4.5 Patch Factors.
--
-- > length d50_patch_factors == 22
d50_patch_factors :: [Parameter]
d50_patch_factors =
    [(18,"Key Mode",9,0,key_mode_usr)
    ,(19,"Split Point",61,0,"C2 - C7")
    ,(20,"Portamento Mode",3,0,"U;L;UL")
    ,(21,"Hold Mode",3,0,"U;L;UL")
    ,(22,"Upper Tone Key Shift",49,-24,"-24 - +24")
    ,(23,"Lower Tone Key Shift",49,-24,"-24 - +24")
    ,(24,"Upper Tone Fine Tune",101,-50,"-50 - +50")
    ,(25,"Lower Tone Fine Tune",101,-50,"-50 - +50")
    ,(26,"Bender Range",13,0,"0 - 12")
    ,(27,"After Bend Range",25,-12,"-12 - +12")
    ,(28,"Portamento Time",101,0,"0 - 100")
    ,(29,"Output Mode",4,1,"1 - 4")
    ,(30,"Reverb Type",32,1,"1 - 32")
    ,(31,"Reverb Balance",101,0,"0 - 100")
    ,(32,"Total Volume",101,0,"0 - 100")
    ,(33,"Tone Balance",101,0,"0 - 100")
    ,(34,"Chase Mode",3,0,"UL;ULL;ULU")
    ,(35,"Chase Level",101,0,"0 - 100")
    ,(36,"Chase Time",101,0,"0 - 100")
    ,(37,"Midi Transmit Channel",17,0,"BASIC-CH | 1 - 16")
    ,(38,"Midi Separate Rcv Channel",17,0,"OFF | 1 - 16")
    ,(39,"Midi Transmit Prog. Change",101,0,"OFF | 1 - 100")
    ]

d50_patch_groups :: [PARAM_GROUP]
d50_patch_groups =
    [("Patch Name Edit",";;;;;;;;;;;;;;;;;",[0..17])
    ,("MAIN","KEY-MODE;SPLIT-POINT;TONE-BALANCE",[18,19,33])
    ,("Tone Tune","LKey;UKey;LTune;UTune",[23,22,25,24])
    ,("Control Edit","Bend;AfPB;Port;Port;Hold",[26,27,28,20,21])
    ,("Output Mode Edit","Mode;Rev;Rbal;Vol",[29..32])
    ,("Chase Edit","Mode;Levl;Time",[34..36])
    ]

-- | 'd50_patch_factors' preceded by patch name.
--
-- > length d50_patch_parameters == 37
d50_patch_parameters :: [Parameter]
d50_patch_parameters =
    let ch n = (n,"Patch Name " ++ show (n + 1),64,0,d50_char_code_usr)
    in map ch [0 .. 17] ++ d50_patch_factors

-- * PARAMETER

-- | Complete set of /used/ 'Parameter's.
--
-- > length d50_parameters == 352
-- > nub $ sort $ map parameter_user_offset d50_parameters
-- > putStrLn $ unlines $ map show d50_parameters
d50_parameters :: [Parameter]
d50_parameters =
    let f i = map (\(ix,nm,stp,usr_diff,usr_str) -> (ix + i,nm,stp,usr_diff,usr_str))
        a = map parameter_type_base_address parameter_type_seq
        p = [d50_partial_parameters,d50_partial_parameters,d50_common_parameters
            ,d50_partial_parameters,d50_partial_parameters,d50_common_parameters
            ,d50_patch_parameters]
    in concat (zipWith f a p)

-- | Complete 'Parameter' sequence, including all /unused/ parmaters.
--
-- > length d50_parameters_seq == 448
-- > map parameter_ix d50_parameters_seq == [0 .. 447]
d50_parameters_seq :: [Parameter]
d50_parameters_seq =
    let recur n sq =
            case sq of
              [] -> []
              p:sq' -> if n == parameter_ix p
                       then p : recur (n + 1) sq'
                       else d50_unused_parameter n : recur (n + 1) sq
    in recur 0 d50_parameters ++ map d50_unused_parameter [424..447]

parameter_type_parameters :: Parameter_Type -> [Parameter]
parameter_type_parameters ty =
    case ty of
      Partial _ _ -> d50_partial_parameters
      Common _ -> d50_common_parameters
      Patch -> d50_patch_parameters

-- * PARAMETER

d50_unused_parameter :: U24 -> Parameter
d50_unused_parameter ix = (ix,"UNUSED",1,0,"")

parameter_range :: Parameter -> (U8,U8)
parameter_range (_,_,stp,_,_) = (0,stp - 1)

parameter_range_usr :: Parameter -> (I8,I8)
parameter_range_usr (_,_,stp,diff,_) = (diff,diff + u8_to_i8 stp - 1)

parameter_value_verify :: Parameter -> U8 -> U8
parameter_value_verify (_,_,stp,_,_) x =
    if x < 0 || x > stp
    then error (show ("parameter_value_verify",x,stp))
    else x

parameter_value_usr :: Parameter -> U8 -> String
parameter_value_usr p x =
    let (_,_,stp,usr_diff,usr_str) = p
    in if x < 0 || x > stp
       then error (show ("parameter_value_usr",p,x))
       else case splitOn ";" usr_str of
              [_] -> show (u8_to_i8 x + usr_diff)
              e -> e `genericIndex` x

parameter_csv :: (ADDRESS,U8) -> (Parameter_Type,Parameter) -> String
parameter_csv (a,x) (ty,p) =
    let (ix,nm,_,_,_usr_str) = p
        x' = parameter_value_verify p x
    in intercalate "," [show a,parameter_type_pp ty,show ix,nm
                       ,show x',range_pp (parameter_range p)
                       ,parameter_value_usr p x]

-- | Given sequence of parameter values, generate /CSV/ of patch.
d50_patch_csv :: [U8] -> [String]
d50_patch_csv =
    let hdr = "ADDRESS,PARAMETER-TYPE,INDEX,NAME,VALUE,RANGE,VALUE-USER"
        f (a,v) = case address_to_parameter a of
                    Just p -> parameter_csv (a,v) p
                    _ -> if v == 0
                         then ""
                         else "ERROR: VALUE NOT ZERO AT NON-PARAMETER ADDRESS" ++ show (a,v)
    in (hdr :) . map f . zip [0 ..]

-- | 'PARAM_GROUP' in ADDRESS sequence.
--
-- > maximum (map (\(nm,_,_) -> length nm) (concat d50_group_seq)) == 16
d50_group_seq :: [[PARAM_GROUP]]
d50_group_seq =
    let tn = [d50_partial_groups,d50_partial_groups,d50_common_groups]
    in concat [tn,tn,[d50_patch_groups]]

group_pp :: [(Parameter,U8)] -> PARAM_GROUP -> String
group_pp x_seq (g_nm,p_nm_seq,ix) =
    let f p_nm (p,x) = let x' = parameter_value_usr p x
                       in if null p_nm {- ie. CHAR -} then x' else concat [p_nm,"=",x']
        gr_p = zipWith f (splitOn ";" p_nm_seq) (map (x_seq !!) ix)
    in T.pad_right ' ' 16 g_nm ++ " -> " ++ unwords gr_p

-- | Pretty printer for D-50 patch following group structure (ie. HW screen layout).
--
-- > p <- load_d50_text "/home/rohan/uc/invisible/light/d50/d50.hex.text"
-- > let g = d50_patch_group_pp p
-- > writeFile "/home/rohan/uc/invisible/light/d50/d50.group.text" (unlines g)
d50_patch_group_pp :: [U8] -> [String]
d50_patch_group_pp =
    let f gr pr = "" : parameter_type_pp (fst pr) : map (group_pp (snd pr)) gr
    in concat . zipWith f d50_group_seq . parameter_segment . zip d50_parameters_seq

-- | Patch name
patch_name :: [U8] -> String
patch_name = map d50_byte_to_char_err . take 18 . drop (parameter_type_base_address Patch)

-- | Tone name
--
-- > tone_name Upper p
tone_name :: Tone -> [U8] -> String
tone_name t = map d50_byte_to_char_err . take 10 . drop (parameter_type_base_address (Common t))

patch_name_set :: [U8] -> [String]
patch_name_set p = [patch_name p,tone_name Upper p,tone_name Lower p]

-- * Tuning

-- | Generate DSC/DTI sequence to set the @WG PITCH KF@ parameters of all
-- four partials to indicated ratio.
--
-- > map d50_dsc_gen (d50_wg_pitch_kf_dti (1/4))
d50_wg_pitch_kf_dti :: (Eq n,Fractional n) => n -> [DSC]
d50_wg_pitch_kf_dti r =
    let e = wg_pitch_kf_to_enum_err r
        a_seq = map (+ 2) partial_base_address_seq
        f a = (DTI_CMD,0,a,[e])
    in map f a_seq

-- * I/O

-- | Load text file consisting of 448 (0x1C0) white-space separated
-- two-character hexidecimal byte values, ie. a D50 patch.
--
-- > p <- load_d50_text "/home/rohan/uc/invisible/light/d50/d50.hex.text"
-- > let p' = unlines (filter (not . null) (d50_patch_csv p))
-- > writeFile "/home/rohan/uc/invisible/light/d50/d50.csv" p'
load_d50_text :: FilePath -> IO [U8]
load_d50_text fn = do
  b <- T.load_hex_byte_seq fn
  case splitAt 448 b of
    (h,[]) -> return h
    _ -> error "load_d50_text"

-- | Load binary 'U8' sequence from file.
load_byte_seq :: FilePath -> IO [U8]
load_byte_seq = fmap (map fromIntegral . B.unpack) . B.readFile

{-| Load DTI sequence from D-50 SYSEX file.

> let sysex_fn = "/home/rohan/data/roland-d50/PND50-00.syx"
> b <- load_byte_seq sysex_fn
> let s = sysex_segment b
> d <- d50_load_sysex_dti sysex_fn
> let s' = d50_dsc_gen_seq (DTI_CMD,0,patch_memory_base 0,concatMap dsc_data d)
> s == s'

-}
d50_load_sysex_dti :: FilePath -> IO [DSC]
d50_load_sysex_dti fn = do
  b <- load_byte_seq fn
  let b_n = length b
  when (b_n /= 0x8CD0) (putStrLn "d50_load_sysex: sysex != 0x8CD0 (36048)")
  when (b_n < 0x8CD0) (error "d50_load_sysex: sysex < 0x8CD0 (36048)")
  return (map d50_dsc_parse_err (sysex_segment b))

-- | 'dsc_data' of 'd50_load_sysex_dti'.
d50_load_sysex_u8 :: FilePath -> IO [U8]
d50_load_sysex_u8 fn = do
  dsc <- d50_load_sysex_dti fn
  let dat = concatMap dsc_data dsc
  when (length dat /= 0x8780) (error "d50_load_sysex_u8: length != 0x8780 (34688)")
  return dat

{-| Load patch data (64 patches of 448 bytes) from D-50 SYSEX file.

> let sysex_fn = "/home/rohan/data/roland-d50/PND50-00.syx"
> p <- d50_load_sysex sysex_fn
> putStrLn$unlines$map patch_name p
> putStrLn$unlines$concatMap d50_patch_group_pp p

-}
d50_load_sysex :: FilePath -> IO [[U8]]
d50_load_sysex fn = do
  dat <- d50_load_sysex_u8 fn
  return (take 64 (chunksOf 448 dat))

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

4.2 Patch write address
    (Transmitted only)

Transmitted when the Manual Button is pushed twice while holding the Partial Mute button down.

Address Description

[ 00-20-00 ] Patch write function *4-1

*4-l Transmitted a Data byte consisting of two 0011 (2 bytes).

4.2 Memory Area

[02-00-00] Patch Memory 1-1
[02-03-40] Patch Memory 1-2
...
[03-5C-40] Patch Memory 8-8
[03-60-00] Reverb Data 17
[03-62-78] Reverb Data 18
...
[04-0C-08] Reverb Data 32

-}
