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
import Data.Either {- base -}
import Data.Int {- base -}
import Data.List {- base -}
import qualified Data.List.Split as Split {- split -}
import Data.Maybe {- base -}
import Data.Word {- base -}
import Text.Printf {- base -}

import qualified Music.Theory.Byte as T {- hmt -}
import qualified Music.Theory.Math.Convert as T {- hmt -}
import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Tuple as T {- hmt -}

import qualified Sound.Midi.Common as M {- midi-osc -}
import qualified Sound.Midi.Constant as M {- midi-osc -}

-- * UTIL

-- | 8-bit unsigned integer, ie. parameter values, midi data etc.
type U8 = Word8

-- | 8-bit signed integer, ie. USR-OFFSET
type I8 = Int8

-- | 24-bit unsigned integer, ie. ADDRESS and related offsets and extents.
type U24 = Word32

int_to_u8 :: Int -> U8
int_to_u8 = fromMaybe (error "int_to_u8") . T.int_to_word8_maybe

u24_max :: Num n => n
u24_max = 16777215 -- 2^24 - 1

int_to_u24 :: Int -> U24
int_to_u24 n = if n < 0 || n > u24_max then error "int_to_u24" else fromIntegral n

u8_to_i8 :: U8 -> I8
u8_to_i8 = T.word8_to_int8

u8_to_u24 :: U8 -> U24
u8_to_u24 = T.word8_to_word32

u8_length :: [t] -> U8
u8_length = int_to_u8 . length

u8_at :: [t] -> U8 -> t
u8_at = genericIndex

u24_length :: [t] -> U24
u24_length = int_to_u24 . length

u24_at :: [t] -> U24 -> t
u24_at = genericIndex

u24_drop :: U24 -> [t] -> [t]
u24_drop = genericDrop

u24_split_at :: U24 -> [t] -> ([t], [t])
u24_split_at = genericSplitAt

-- | Unpack 'U24' to 'U8', MSB-LSB.
u24_unpack :: U24 -> [U8]
u24_unpack = T.t3_to_list . M.bits_21_sep_be

-- | Pack 'U24' from three 'U8', MSB-LSB.
--
-- > map u24_pack [[0x02,0x00,0x00],[0x02,0x0F,0x00]] == [0x8000,0x8780]
u24_pack :: [U8] -> U24
u24_pack = M.bits_21_join_be . T.t3_from_list

-- | Range as @p - q@.
--
-- > range_pp (1,7) == "1 - 7"
range_pp :: Show a => (a,a) -> String
range_pp (p,q) = show p ++ " - " ++ show q

-- | Error if 'Nothing', printing /msg/.
maybe_err :: String -> Maybe a -> a
maybe_err msg = fromMaybe (error msg)

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

-- * PARAMETER TYPE

-- | A patch has two tones, 'Upper' and 'Lower'.
data Tone = Upper | Lower deriving (Eq,Show)

-- | Upper -> U, Lower -> L.
d50_tone_sym :: Tone -> String
d50_tone_sym tn = case tn of {Upper -> "U";Lower -> "L"}

-- | A 'Tone' has two partials, 'One' and 'Two'.
data Partial_Ix = One | Two deriving (Eq,Show)

-- | One -> 1, Two -> 2.
d50_partial_ix_sym :: Partial_Ix -> String
d50_partial_ix_sym ix = case ix of {One -> "1";Two -> "2"}

-- | Parameters are of one of seven types.
--   Four 'Partial's (U1,U2,L1,L2), two 'Common' (U,L), or 'Patch'.
data D50_Parameter_Type = Partial Tone Partial_Ix | Common Tone | Patch deriving (Eq,Show)

-- | All 'D50_Parameter_Type' in sequence, ie. in ascending ADDRESS order).
d50_parameter_type_seq :: [D50_Parameter_Type]
d50_parameter_type_seq =
    [Partial Upper One,Partial Upper Two,Common Upper
    ,Partial Lower One,Partial Lower Two,Common Lower
    ,Patch]

-- | Pretty printer for 'D50_Parameter_Type'.
--
-- > map d50_parameter_type_pp d50_parameter_type_seq
d50_parameter_type_pp :: D50_Parameter_Type -> String
d50_parameter_type_pp ty =
  case ty of
    Partial tn ix -> unwords [show tn,"Partial",d50_partial_ix_sym ix]
    Common tn -> unwords [show tn,"Common"]
    Patch -> "Patch"

-- | Short symbolic names for parameter types: U1 U2 U L1 L2 L P.
--
-- > map d50_parameter_type_sym d50_parameter_type_seq
d50_parameter_type_sym :: D50_Parameter_Type -> String
d50_parameter_type_sym ty =
  case ty of
    Partial tn ix -> d50_tone_sym tn ++ d50_partial_ix_sym ix
    Common tn -> d50_tone_sym tn
    Patch -> "P"

-- * PARAMETER

-- | (INDEX,NAME,STEPS,USR-OFFSET,USR-STRING)
type D50_Parameter = (U24,String,U8,I8,String)

d50_parameter_ix :: D50_Parameter -> U24
d50_parameter_ix (ix,_,_,_,_) = ix

d50_parameter_name :: D50_Parameter -> String
d50_parameter_name (_,nm,_,_,_) = nm

d50_parameter_user_offset :: D50_Parameter -> I8
d50_parameter_user_offset (_,_,_,o,_) = o

-- | (GROUP-NAME,PARAMETER-NAME-SEQ,PARAMETER-IX-SEQ)
type PARAM_GROUP = (String,String,[U24])

-- * Patch Bank

-- | Translate one-indexed (BANK-NUMBER,PATCH-NUMBER) index to zero linear index.
--
-- > map bank_to_ix [(1,1),(2,3),(3,1),(4,7),(7,4),(8,8)] == [0,10,16,30,51,63]
bank_to_ix :: Num n => (n,n) -> n
bank_to_ix (p,q) = ((p - 1) * 8) + (q - 1)

-- | Inverse of 'bank_to_ix'.
--
-- > map ix_to_bank [0,10,51,63] == [(1,1),(2,3),(7,4),(8,8)]
ix_to_bank :: Integral i => i -> (i,i)
ix_to_bank n = let (p,q) = n `divMod` 8 in (p + 1,q + 1)

-- * ADDRESS

-- | Parameter address.
type D50_ADDRESS = U24

-- | Parse the address notation used in Roland manuals.
--
-- > d50_addr_parse_t3 "00-02-40" == Just (0x00,0x02,0x40)
d50_addr_parse_t3 :: String -> Maybe (U8,U8,U8)
d50_addr_parse_t3 str =
  case T.read_hex_byte_seq (filter (/= '-') str) of
    [d1,d2,d3] -> Just (d1,d2,d3)
    _ -> Nothing

-- | Encode D50 address, alias for 'M.bits_21_join_be'
--
-- > d50_addr_encode (0x02,0x00,0x00) == 0x8000
d50_addr_encode :: (U8,U8,U8) -> D50_ADDRESS
d50_addr_encode = M.bits_21_join_be

-- | 'd50_addr_encode' of 'd50_addr_parse_t3'
d50_addr_read :: String -> D50_ADDRESS
d50_addr_read = d50_addr_encode . fromMaybe (error "d50_addr_read?") . d50_addr_parse_t3

-- | Show address as 5-element hexadecimal.
d50_addr_pp :: D50_ADDRESS -> String
d50_addr_pp x = printf "%05X" x

{- | Base address (offset) for each parameter type (as 7-bit 3-tuple).

4.1 Parameter base address

[00-00-00] Upper Partial 1 (  0 -  53)
[00-00-40] Upper Partial 2 ( 64 - 117)
[00-01-00] Upper Common    (128 - 175)
[00-01-40] Lower Partial 1 (192 - 245)
[00-02-00] Lower Partial 2 (256 - 309)
[00-02-40] Lower Common    (320 - 367)
[00-03-00] Patch           (384 - 420)

> map (d50_addr_encode . d50_parameter_type_base_address_t3) parameter_type_univ

-}
d50_parameter_type_base_address_t3 :: D50_Parameter_Type -> (U8,U8,U8)
d50_parameter_type_base_address_t3 ty =
    case ty of
      Partial Upper One -> (0x00,0x00,0x00) -- 0x000 = 000
      Partial Upper Two -> (0x00,0x00,0x40) -- 0x040 = 064
      Common Upper -> (0x00,0x01,0x00) -- 0x080 = 128
      Partial Lower One -> (0x00,0x01,0x40) -- 0x0C0 = 192
      Partial Lower Two -> (0x00,0x02,0x00) -- 0x100 = 256
      Common Lower -> (0x00,0x02,0x40) -- 0x140 = 320
      Patch -> (0x00,0x03,0x00) -- 0x180 = 384

-- | Base address (offset) for each parameter type (as 'D50_ADDRESS').
--   These are also the actual addresses of the "temporary area".
--
-- > map parameter_type_base_address parameter_type_seq == [0,64,128,192,256,320,384]
-- > [0,64,128,192,256,320,384] == [0x000,0x040,0x080,0x0C0,0x100,0x140,0x180]
d50_parameter_type_base_address :: D50_Parameter_Type -> D50_ADDRESS
d50_parameter_type_base_address = d50_addr_encode . d50_parameter_type_base_address_t3

-- | Base-address for partials U1, U2, L1 and L2.
d50_partial_base_address_seq :: [D50_ADDRESS]
d50_partial_base_address_seq = [0x000,0x040,0x0C0,0x100]

-- | Base address, initial offset, number of parameters.
--
-- > sum (map (\ty -> let (_,_,n) = parameter_type_extent ty in n) parameter_type_seq) == 312
-- > let (b,o,n) = parameter_type_extent (last parameter_type_seq) in b + o + n == 424
parameter_type_extent :: D50_Parameter_Type -> (D50_ADDRESS,U24,U24)
parameter_type_extent ty =
    case ty of
      Partial _ _ -> (d50_parameter_type_base_address ty,0,54)
      Common _ -> (d50_parameter_type_base_address ty,10,38)
      Patch -> (d50_parameter_type_base_address ty,20,20)

-- | D50_Parameter base address (Top address) (4.1)
parameter_type_address_segments :: [(D50_Parameter_Type,(D50_ADDRESS,U24))]
parameter_type_address_segments =
    let f ty = let (b,o,n) = parameter_type_extent ty
               in (ty,(b,b + o + n - 1))
    in map f d50_parameter_type_seq

-- | Split sequence into groups based on 'D50_Parameter_Type' sequence.
parameter_segment :: [t] -> [(D50_Parameter_Type,[t])]
parameter_segment p =
    let f (ty,(b,x)) = (ty,map (u24_at p) [b .. x])
    in map f parameter_type_address_segments

-- | 4.1 Parameter base address (Top address)
d50_parameter_base_address_tbl :: [(D50_ADDRESS,String,D50_Parameter_Type,String)]
d50_parameter_base_address_tbl =
    let f (ty,(b,x)) = (b,d50_parameter_type_pp ty,ty,range_pp (b,x))
    in map f parameter_type_address_segments

-- | Determine 'D50_Parameter_Type' of value at 'D50_ADDRESS'.
--
-- > mapMaybe (\n -> fmap parameter_type_pp (address_to_parameter_type n)) [0 .. 420]
address_to_parameter_type :: D50_ADDRESS -> Maybe D50_Parameter_Type
address_to_parameter_type a =
    let f ty = let (b,o,n) = parameter_type_extent ty in a >= b && a < b + o + n
    in find f d50_parameter_type_seq

-- | Lookup parameter at address.
--
-- > let ix = [0 .. 420] in zip ix (map address_to_parameter ix)
d50_address_to_parameter :: D50_ADDRESS -> Maybe (D50_Parameter_Type,D50_Parameter)
d50_address_to_parameter a =
    case address_to_parameter_type a of
      Just ty -> let (b,_,_) = parameter_type_extent ty
                     f (i,_,_,_,_) = i == a - b
                 in fmap (\p -> (ty,p)) (find f (d50_parameters_by_type ty))
      Nothing -> Nothing

-- | Given 'D50_Parameter_Type' and name, lookup 'D50_Parameter'.
d50_parameter_lookup :: (D50_Parameter_Type,String) -> Maybe D50_Parameter
d50_parameter_lookup (ty,nm) =
    let f (_,nm',_,_,_) = nm == nm'
    in find f (d50_parameters_by_type ty)

-- | Get 'D50_ADDRESS' of named parameter.
--
-- > d50_named_parameter_to_address (Patch,"Lower Tone Fine Tune") == Just 409
-- > d50_named_parameter_to_address (Patch,"Patch Name 1") == Just 384
-- > d50_named_parameter_to_address (Common Upper,"Tone Name 1") == Just 128
-- > d50_named_parameter_to_address (Common Lower,"Structure No.") == Just 330
-- > d50_named_parameter_to_address (Common Upper,"Partial Mute") == Just 174
d50_named_parameter_to_address :: (D50_Parameter_Type,String) -> Maybe D50_ADDRESS
d50_named_parameter_to_address (ty,nm) =
  let f (n,_,_,_,_) = let (b,_,_) = parameter_type_extent ty in b + n
  in fmap f (d50_parameter_lookup (ty,nm))

{- | The base address for the D50 work area memory.

4.2 Memory Area

[02-00-00] Patch Memory 1-1 = 0x08000
[02-03-40] Patch Memory 1-2 = 0x081C0
...
[03-5C-40] Patch Memory 8-8 = 0x0EE40
[03-60-00] Reverb Data 17   = 0x0F000
[03-62-78] Reverb Data 18   = 0x0F178
...
[04-0C-08] Reverb Data 32   = 0x10608

> map (d50_addr_pp . d50_addr_read) (words "02-00-00 02-03-40 03-5C-40 03-60-00 03-62-78 04-0C-08")

-}
d50_work_area_base_address :: D50_ADDRESS
d50_work_area_base_address = 0x8000

{- | Base address for patch memory /n/ (0,63).

> d50_addr_encode (0x02,0x00,0x00) == 0x8000 -- 32768
> d50_addr_encode (0x02,0x03,0x40) == 0x81C0 -- 33216
> d50_addr_encode (0x03,0x60,0x00) == 0xF000 -- 61440
> 33216 - 32768 == 0x01C0 -- 448
> 32768 + (448 * 64) == 0xF000 -- 61440
> 61440 - 448 == 0xEE40 -- 60992
> M.bits_21_sep_be 60992 == (0x03,0x5C,0x40)

-}
d50_patch_memory_base :: U8 -> D50_ADDRESS
d50_patch_memory_base n = d50_work_area_base_address + (d50_parameter_n * u8_to_u24 n)

-- * SYM

-- | Show partial mute for upper and lower tones as 4-character string.
--   M = Muted, S = Sounding
--
-- > d50_patch_partial_mute_sym p == "SSSS"
d50_patch_partial_mute_sym :: D50_Patch -> String
d50_patch_partial_mute_sym p =
  let u = u24_at p 174
      l = u24_at p 366
  in concatMap (d50_usr_ix (error "?") d50_partial_mute_usr) [u,l]

-- | Show partial structure for upper and lower tones as 4-character string.
--   S = Synthesis, P = PCM
--
-- > d50_patch_structure_sym p == "SSSS"
d50_patch_structure_sym :: D50_Patch -> String
d50_patch_structure_sym p =
  let u = u24_at p 138
      l = u24_at p 330
  in filter (/= 'R') (concatMap (d50_usr_ix (error "?") d50_structure_usr) [u,l])

-- * REVERB

-- | Size of stored (encoded) reverb data segment (376-bytes).
d50_reverb_data_segment_n :: Num n => n
d50_reverb_data_segment_n = 376

-- | Reverb data, 188-element byte-sequence (ie. decoded reverb data segment).
type D50_Reverb = [U8]

-- | Reverb data is 8-bit, but is stored in only the least significant 4-bits of each U8.
d50_reverb_join :: U8 -> U8 -> U8
d50_reverb_join d1 d2 =
  case (M.u8_sep d1,M.u8_sep d2) of
    ((0x0,d3),(0x0,d4)) -> M.u4_join (d3,d4)
    _ -> error "d50_reverb_join?"

-- | Inverse of 'd50_reverb_join'.
d50_reverb_sep :: U8 -> (U8,U8)
d50_reverb_sep = M.u8_sep

-- | Decode 376-element reverb block to 188-element reverb data.
d50_reverb_decode :: [U8] -> [U8]
d50_reverb_decode = map (uncurry d50_reverb_join) . T.adj2 2

-- | Inverse of 'd50_reverb_decode'
d50_reverb_encode :: [U8] -> [U8]
d50_reverb_encode =
  let recur x =
        case x of
          d:x' -> let (d1,d2) = d50_reverb_sep d in d1 : d2 : recur x'
          [] -> []
  in recur

{- | Base address for zero-indexed reverb memory /n/, ie. (0,15).
     The mutable reverbs are those numbered 17-32.
     Each reverb data segment is 376 bytes, the total reverb data segment is 6016 bytes.

> d50_addr_encode (0x03,0x60,0x00) == 61440
> d50_addr_encode (0x03,0x62,0x78) == 61816
> d50_addr_encode (0x04,0x0C,0x08) == 67080
> 61816 - 61440 == 376
> 61440 + (376 * (32 - 17)) == 67080

-}
d50_reverb_memory_base :: U8 -> D50_ADDRESS
d50_reverb_memory_base n = 61440 + (376 * u8_to_u24 n)

-- | (REVERB-INDEX,NAME)
type D50_REVERB_NAME = (U8,String)

-- | Reverbs 1-16 are shared (common) reverb types, 17-32 are user (bank) reverb types.
d50_reverb_type_shared :: [D50_REVERB_NAME]
d50_reverb_type_shared =
  [(01,"Small Hall")
  ,(02,"Medium Hall")
  ,(03,"Large Hall")
  ,(04,"Chapel")
  ,(05,"Box")
  ,(06,"Small Metal Room")
  ,(07,"Small Room")
  ,(08,"Medium Room")
  ,(09,"Medium Large Room")
  ,(10,"Large Room")
  ,(11,"Single Delay (102 ms)")
  ,(12,"Cross Delay (180 ms)")
  ,(13,"Cross Delay (224 ms)")
  ,(14,"Cross Delay (148-296 ms)")
  ,(15,"Short Gate (200 ms)")
  ,(16,"Long Gate (480 ms)")]

-- | USR string variant of 'd50_reverb_type_shared', with indices for 17-32.
d50_reverb_type_usr :: String
d50_reverb_type_usr =
  let rw c =
        case c of
          ' ' -> Just '-'
          '(' -> Nothing
          ')' -> Nothing
          _ -> Just (toUpper c)
  in intercalate ";" (map (mapMaybe rw . snd) d50_reverb_type_shared ++ map show [17::U8 .. 32])

-- * CHAR

-- | Table mapping the (NON-ASCII) D-50 character encoding to it's ASCII character.
--
-- > length d50_char_table == 67
-- > map (\(x,c) -> (x,fromEnum c,c)) d50_char_table
d50_char_table :: [(U8,Char)]
d50_char_table =
    let ch = [[' '],['A'..'Z'],['a'..'z'],['1'..'9'],['0','-','?','?','-']]
    in zip [0..] (concat ch)

-- | Lookup in 'd50_char_table'.
--
-- > mapMaybe d50_byte_to_char [9,40,38,27,40,30] == "Inland"
d50_byte_to_char :: U8 -> Maybe Char
d50_byte_to_char n = lookup n d50_char_table

-- | Erroring variant.
d50_byte_to_char_err :: U8 -> Char
d50_byte_to_char_err = fromMaybe (error "d50_byte_to_char") . d50_byte_to_char

-- | Reverse lookup in 'd50_char_table'.
--
-- > mapMaybe d50_char_to_byte "Inland" == [9,40,38,27,40,30]
d50_char_to_byte :: Char -> Maybe U8
d50_char_to_byte c = T.reverse_lookup c d50_char_table

-- * USR/STR

-- | USR strings naming the 12 pitch-classes.
pitch_class_seq :: [String]
pitch_class_seq = words "C C# D D# E F F# G G# A A# B"

-- | USR strings naming the 84 pitches from C1 to B7.
--
-- > length pitch_seq == 7 * 12
pitch_seq :: [String]
pitch_seq = [p ++ show o | o <- [1::Int .. 7],p <- pitch_class_seq]

-- | "C2 - C7" as USR string.
split_point_usr :: String
split_point_usr = intercalate ";" (take 61 (drop 12 pitch_seq))

-- | "C1 - C7" as USR string.
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

{- * KEY MODE

WHOLE;
DUAL;
SPLIT;
SEP (Separate);
WHOL-S (Whole Solo);
DUAL-S (Dual Solo);
SPL-US (Split Upper Solo);
SPL-LS (Split Lower Solo);
SEP-S (Separate Solo)
-}
d50_key_mode_tbl :: [(U8,String)]
d50_key_mode_tbl = zip [0..] (Split.splitOn ";" d50_key_mode_usr)

d50_key_mode_usr :: String
d50_key_mode_usr = "WHOLE;DUAL;SPLIT;SEP;WHOL-S;DUAL-S;SPL-US;SPL-LS;SEP-S"

-- | Is KEY-MODE 0=WHOLE or 4=WHOL-S?
d50_is_lower_tone_unused :: U8 -> Bool
d50_is_lower_tone_unused x = x == 0 || x == 4

-- * STRUCTURE

-- | Tone structure indicates synthesis type for each partial (S=SYNTHESIS, P=PCM, R=RING-MOD).
d50_structure_usr :: String
d50_structure_usr = "SS;SSR;PS;PSR;SPR;PP;PPR"

-- | Tone structure number diagram in plain text (zero indexed).
--
-- > map d50_structure_pp [0 .. 6]
d50_structure_pp :: U8 -> String
d50_structure_pp n =
    case n + 1 of
      1 -> "S1 + S2"
      2 -> "S1 + RMOD (S1 + S2)"
      3 -> "P1 + S2"
      4 -> "P1 + RMOD (P1 + S2)"
      5 -> "S1 + RMOD (S1 + P2)"
      6 -> "P1 + P2"
      7 -> "P1 + RMOD (P1 + P2)"
      _ -> error "structure_text: ix?"

-- * PARTIAL

-- | Partial parameters (4.3).
--   The indices in this table are zero based.
--
-- > length d50_partial_parameters == 54
d50_partial_parameters :: [D50_Parameter]
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
    ,("WG Mod","LFO;ENV;Bend",[3..5]) -- WG Modulation
    ,("WG Form","Wave;PCM",[6..7]) -- WG Waveform
    ,("WG PW","PW;Velo;Aftr;LFO;LFOD",[8,9,12,10,11]) -- WG Pulse Width
    ,("TVF","Freq;Reso;KF;BP;Blvl",[13..17])
    ,("TVF ENV 1","Dpth;Velo;DKF;TKF",[18..21]) -- TVF ENV
    ,("TVF ENV 2","T1;T2;T3;T4;T5",[22..26]) -- TVF ENV Time
    ,("TVF ENV 3","L1;L2;L3;SusL;EndL",[27..31]) -- TVF ENV Level
    ,("TVF MOD","LFO;LFOD;Aftr",[32..34]) -- TVF Modulation
    ,("TVA","Levl;Velo;BP;Blvl",[35..38])
    ,("TVA ENV 1","T1;T2;T3;T4;T5",[39..43]) -- TVA ENV Time
    ,("TVA ENV 2","L1;L2;L3;SusL;EndL",[44..48]) -- TVA ENV Level
    ,("TVA ENV 3","Velo;TKF",[49..50])
    ,("TVA MOD","LFO;LFOD;Aftr",[51..53]) -- TVA Modulation
    ]

-- * CHORUS

-- | Names of chorus types (1-8).
d50_chorus_type_enum :: [String]
d50_chorus_type_enum =
    ["Chorus 1","Chorus 2"
    ,"Flanger1","Flanger2"
    ,"FBChorus" -- Feedback Chorus
    ,"Tremolo","C Trem" -- Chorus Tremolo
    ,"Dimensn"] -- Dimension

-- | USR string variant of 'd50_chorus_type_enum'.
d50_chorus_type_usr :: String
d50_chorus_type_usr = intercalate ";" (map (map toUpper . filter (/= ' ')) d50_chorus_type_enum)

-- * COMMON (TONE)

-- | USR string for partial-mute value.
d50_partial_mute_usr :: String
d50_partial_mute_usr = "MM;SM;MS;SS"

-- | Common parameters (4.4).
--   This table is zero-indexed but does not include the 10-character tone name.
--
-- > length d50_common_factors == 38
d50_common_factors :: [D50_Parameter]
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
    ,(42,"Chorus Type",8,1,d50_chorus_type_usr)
    ,(43,"Chorus Rate",101,0,"0 - 100")
    ,(44,"Chorus Depth",101,0,"0 - 100")
    ,(45,"Chorus Balance",101,0,"0 - 100")
    ,(46,"Partial Mute",4,0,d50_partial_mute_usr)
    ,(47,"Partial Balance",101,0,"0 - 100")
    ]

-- | 'd50_common_factors' preceded by 10-byte Tone name.
--
-- > length d50_common_parameters == 48
d50_common_parameters :: [D50_Parameter]
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
--   This table is zero-indexed but does not include the 18-character patch name.
--
-- > length d50_patch_factors == 22
d50_patch_factors :: [D50_Parameter]
d50_patch_factors =
    [(18,"Key Mode",9,0,d50_key_mode_usr)
    ,(19,"Split Point",61,0,split_point_usr)
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
    ,(30,"Reverb Type",32,1,d50_reverb_type_usr)
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

-- | 'd50_patch_factors' preceded by 18-character patch name.
--
-- > length d50_patch_parameters == 40
d50_patch_parameters :: [D50_Parameter]
d50_patch_parameters =
    let ch n = (n,"Patch Name " ++ show (n + 1),64,0,d50_char_code_usr)
    in map ch [0 .. 17] ++ d50_patch_factors

-- | Group structure of patch parameters.
d50_patch_groups :: [PARAM_GROUP]
d50_patch_groups =
    [("Patch Name Edit",";;;;;;;;;;;;;;;;;",[0..17])
    ,("MAIN","KEY-MODE;SPLIT-POINT;TONE-BALANCE",[18,19,33])
    ,("Tone Tune","LKey;UKey;LTune;UTune",[23,22,25,24])
    ,("Control Edit","Bend;AfPB;Port;Port;Hold",[26,27,28,20,21])
    ,("Output Mode Edit","Mode;Rev;Rbal;Vol",[29..32])
    ,("Chase Edit","Mode;Levl;Time",[34..36])
    ]

-- * PARAMETER

-- | Complete set of /used/ 'D50_Parameter's.
--   The table is zero-indexed and sparse.
--
-- > length d50_parameters == 352
-- > nub $ sort $ map parameter_user_offset d50_parameters
-- > putStrLn $ unlines $ map show d50_parameters
d50_parameters :: [D50_Parameter]
d50_parameters =
    let f i = map (\(ix,nm,stp,usr_diff,usr_str) -> (ix + i,nm,stp,usr_diff,usr_str))
        a = map d50_parameter_type_base_address d50_parameter_type_seq
        p = [d50_partial_parameters,d50_partial_parameters,d50_common_parameters
            ,d50_partial_parameters,d50_partial_parameters,d50_common_parameters
            ,d50_patch_parameters]
    in concat (zipWith f a p)

-- | The number of D50 patch parameters, including unused.
d50_parameter_n :: Num n => n
d50_parameter_n = 448

-- | A D50 patch is a 448-byte sequence.
type D50_Patch = [U8]

-- | The D50 parameter address space is sparse, ie. has unused addresses.
--   Construct an UNUSED parameter.
d50_unused_parameter :: U24 -> D50_Parameter
d50_unused_parameter ix = (ix,"UNUSED",1,0,"")

-- | Complete 'D50_Parameter' sequence, including all /unused/ parmaters.
--
-- > length d50_parameters_seq == d50_parameter_n
-- > map parameter_ix d50_parameters_seq == [0 .. d50_parameter_n - 1]
d50_parameters_seq :: [D50_Parameter]
d50_parameters_seq =
    let recur n sq =
            case sq of
              [] -> []
              p:sq' -> if n == d50_parameter_ix p
                       then p : recur (n + 1) sq'
                       else d50_unused_parameter n : recur (n + 1) sq
    in recur 0 d50_parameters ++ map d50_unused_parameter [424..447]

-- | Parameters of indicated type.
d50_parameters_by_type :: D50_Parameter_Type -> [D50_Parameter]
d50_parameters_by_type ty =
    case ty of
      Partial _ _ -> d50_partial_parameters
      Common _ -> d50_common_parameters
      Patch -> d50_patch_parameters

-- * PARAMETER

-- | Parameter range as stored.
d50_parameter_range :: D50_Parameter -> (U8,U8)
d50_parameter_range (_,_,stp,_,_) = (0,stp - 1)

-- | Parameter range as displayed (USR).
d50_parameter_range_usr :: D50_Parameter -> (I8,I8)
d50_parameter_range_usr (_,_,stp,diff,_) = (diff,diff + u8_to_i8 stp - 1)

-- | Verify that /x/ is a valid value for parameter, ie. identity or error.
d50_parameter_value_verify :: D50_Parameter -> U8 -> U8
d50_parameter_value_verify (_,_,stp,_,_) x =
    if x < 0 || x > stp
    then error (show ("parameter_value_verify",x,stp))
    else x

d50_usr_ix :: String -> String -> U8 -> String
d50_usr_ix def str x =
  case Split.splitOn ";" str of
    [_] -> def
    e -> u8_at e x

-- | Make USR display value for value /x/ at parameter /p/.
d50_parameter_value_usr :: D50_Parameter -> U8 -> Maybe String
d50_parameter_value_usr p x =
    let (_,_,stp,usr_diff,usr_str) = p
    in if x < 0 || x > stp
       then Nothing
       else Just (d50_usr_ix (show (u8_to_i8 x + usr_diff)) (usr_str) x)

-- | Variant that allows a default string for cases where the value is out of range.
d50_parameter_value_usr_def :: String -> D50_Parameter -> U8 -> String
d50_parameter_value_usr_def def p = fromMaybe def . d50_parameter_value_usr p

-- | Variant that errors if the value is out of range.
d50_parameter_value_usr_err :: D50_Parameter -> U8 -> String
d50_parameter_value_usr_err p x =
  fromMaybe
  (error (show ("parameter_value_usr",p,x)))
  (d50_parameter_value_usr p x)

-- * GROUP

-- | 'PARAM_GROUP' in ADDRESS sequence.
--
-- > maximum (map (\(nm,_,_) -> length nm) (concat d50_group_seq)) == 16
d50_group_seq :: [[PARAM_GROUP]]
d50_group_seq =
    let tn = [d50_partial_groups,d50_partial_groups,d50_common_groups]
    in concat [tn,tn,[d50_patch_groups]]

group_pp :: [(D50_Parameter,U8)] -> PARAM_GROUP -> String
group_pp x_seq (g_nm,p_nm_seq,ix) =
    let f p_nm (p,x) = let x' = d50_parameter_value_usr_def "??" p x
                       in if null p_nm {- ie. CHAR -} then x' else concat [p_nm,"=",x']
        gr_p = zipWith f (Split.splitOn ";" p_nm_seq) (map (u24_at x_seq) ix)
    in T.pad_right ' ' 16 g_nm ++ " -> " ++ unwords gr_p

{- | Pretty printer for D-50 patch following group structure (ie. HW screen layout).

> dir = "/home/rohan/uc/invisible/light/d50/"
> p:_ <- d50_load_hex (dir ++ "d50.hex.text")
> writeFile (dir ++ "d50.group.text") (unlines (d50_patch_group_pp p))
-}
d50_patch_group_pp :: D50_Patch -> [String]
d50_patch_group_pp =
    let f gr pr = "" : d50_parameter_type_pp (fst pr) : map (group_pp (snd pr)) gr
    in concat . zipWith f d50_group_seq . parameter_segment . zip d50_parameters_seq

-- | Patch name, 18-byte ASCII string.
d50_patch_name :: D50_Patch -> String
d50_patch_name = map d50_byte_to_char_err . take 18 . u24_drop (d50_parameter_type_base_address Patch)

-- | Tone name, 10-byte ASCII string.
--
-- > tone_name Upper p
d50_tone_name :: Tone -> D50_Patch -> String
d50_tone_name t =
  map d50_byte_to_char_err .
  take 10 .
  u24_drop (d50_parameter_type_base_address (Common t))

-- | (patch-name,upper-tone-name,lower-tone-name).
d50_patch_name_set :: D50_Patch -> (String,String,String)
d50_patch_name_set p = (d50_patch_name p,d50_tone_name Upper p,d50_tone_name Lower p)

-- | Patch name set pretty printed.
d50_patch_name_set_pp :: D50_Patch -> String
d50_patch_name_set_pp p =
  let (n,u,l) = d50_patch_name_set p
  in concat [n," U: ",u," L: ",l]

-- * SYSEX

-- | SYSEX as sequence of U8.
type D50_Sysex = [U8]

-- | Alias for 'T.byte_seq_hex_pp'
d50_sysex_pp :: D50_Sysex -> String
d50_sysex_pp = T.byte_seq_hex_pp False

-- | Segment byte-sequence into SYSEX messages, no verification,
-- ie. seperate before each @0xF0@.
d50_sysex_segment :: [U8] -> [D50_Sysex]
d50_sysex_segment = tail . T.split_before 0xF0

-- * D-50 SYSEX CMD

-- | D50 SYSEX command ID.
data D50_SYSEX_CMD = RQ1_CMD -- ^ REQUEST DATA - ONE WAY (0x11)
                   | DT1_CMD -- ^ DATA SET - ONE WAY (0x12)
                   | WSD_CMD -- ^ WANT TO SEND DATA (0x40)
                   | RQD_CMD -- ^ REQUEST DATA (0x41)
                   | DAT_CMD -- ^ DATA SET (0x42)
                   | ACK_CMD -- ^ ACKNOWLEDGE (0x43)
                   | EOD_CMD -- ^ END OF DATA (0x45)
                   | ERR_CMD -- ^ COMMUNICATION ERROR (0x4E)
                   | RJC_CMD -- ^ REJECTION (0x4F)
                     deriving (Eq,Show)

-- | Table mapping 'D50_SYSEX_CMD' to it's 'U8' code.
d50_sysex_cmd_tbl :: [(D50_SYSEX_CMD,U8)]
d50_sysex_cmd_tbl =
    [(RQ1_CMD,0x11)
    ,(DT1_CMD,0x12)
    ,(WSD_CMD,0x40)
    ,(RQD_CMD,0x41)
    ,(DAT_CMD,0x42)
    ,(ACK_CMD,0x43)
    ,(EOD_CMD,0x45)
    ,(ERR_CMD,0x4E)
    ,(RJC_CMD,0x4F)]

-- | SYSEX_CMD to 8-bit identifier.
--
-- > let k = d50_sysex_cmd_encode DT1_CMD in (k == 0x12,k == 0b00010010)
-- > let k = d50_sysex_cmd_encode RQ1_CMD in (k == 0x11,k == 0b00010001)
d50_sysex_cmd_encode :: D50_SYSEX_CMD -> U8
d50_sysex_cmd_encode cmd = maybe_err "d50_sysex_cmd_encode" (lookup cmd d50_sysex_cmd_tbl)

-- | Inverse of 'd50_sysex_cmd_encode'.
d50_sysex_cmd_decode :: U8 -> D50_SYSEX_CMD
d50_sysex_cmd_decode i = maybe_err "d50_sysex_cmd_decode" (T.reverse_lookup i d50_sysex_cmd_tbl)

{- | D50 SYSEX header, 5-byte sequence of:

 F0 - Exclusive status
 41 - Roland-ID
 CH - Channel (Device-ID)
 14 - Model-ID
CMD - Command-ID
-}
d50_cmd_hdr :: U8 -> D50_SYSEX_CMD -> [U8]
d50_cmd_hdr ch cmd = [M.k_Sysex_Status,M.k_Roland_ID,ch,d50_id,d50_sysex_cmd_encode cmd]

-- | Data with checksum and F7 appended.
d50_cmd_data_chk :: [U8] -> [U8]
d50_cmd_data_chk dat = dat ++ [roland_checksum dat,M.k_Sysex_End]

-- | Generate ADDR/SIZE category of D-50 SYSEX messages.
d50_addr_sz_cmd_gen :: D50_SYSEX_CMD -> U8 -> D50_ADDRESS -> U24 -> D50_Sysex
d50_addr_sz_cmd_gen cmd ch addr sz =
    let dat = u24_unpack addr ++ u24_unpack sz
    in concat [d50_cmd_hdr ch cmd,d50_cmd_data_chk dat]

-- | Generate WSD command SYSEX.
--
-- > let pp = T.byte_seq_hex_pp True
-- > let syx = T.read_hex_byte_seq_ws "F0 41 00 14 40 02 00 00 02 0F 00 6D F7"
-- > d50_wsd_gen 0 0x8000 0x8780 == syx
-- > d50_cmd_parse syx == Just (0,WSD_CMD,[2,0,0,2,15,0],6)
d50_wsd_gen :: U8 -> D50_ADDRESS -> U24 -> D50_Sysex
d50_wsd_gen = d50_addr_sz_cmd_gen WSD_CMD

-- | Generate RQD command SYSEX.
d50_rqd_gen :: U8 -> D50_ADDRESS -> U24 -> D50_Sysex
d50_rqd_gen = d50_addr_sz_cmd_gen RQD_CMD

{- | Generate RQI command SYSEX.

3.1 Request (One way) RQ1 11H

a 1111 0000 F0 Exclusive status
b 0100 0001 41 Roland-ID
c 0000 nnnn    Device-ID = MIDI CHANNEL
d 0001 0100 14 Model-ID
e 0001 0001 11 Command-ID
f 0aaa aaaa    Address MSB
g 0bbb bbbb    Address
h 0ccc cccc    Address LSB
i 0ddd dddd    Size MSB
j 0eee eeee    Size
k 0fff ffff    Size LSB
l 0ggg gggg    Checksum
m 1111 0111 F7 End of System Exclusive

Address: [00-00-00]
Size: [00-03-25] (421-bytes)

> let r = map T.read_hex_byte_err (words "F0 41 00 14 11 00 00 00 00 03 40 3D F7")
> d50_rq1_gen 0 0 0x1C0 == r
-}
d50_rq1_gen :: U8 -> D50_ADDRESS -> U24 -> D50_Sysex
d50_rq1_gen = d50_addr_sz_cmd_gen RQ1_CMD

-- | Make ACK sysex.
--
-- > pp (d50_ack_gen 0) == "F0 41 00 14 43 F7"
-- > d50_cmd_parse (d50_ack_gen 0) == Just (0,ACK_CMD,[],0)
d50_ack_gen :: U8 -> D50_Sysex
d50_ack_gen ch = d50_cmd_hdr ch ACK_CMD ++ [M.k_Sysex_End]

-- | Make EOD sysex.
--
-- > pp (d50_eod_gen 0) == "F0 41 00 14 45 F7"
-- > d50_cmd_parse (d50_eod_gen 0) == Just (0,EOD_CMD,[],0)
d50_eod_gen :: U8 -> D50_Sysex
d50_eod_gen ch = d50_cmd_hdr ch EOD_CMD ++ [M.k_Sysex_End]

-- | Make RJC sysex.
--
-- > pp (d50_rjc_gen 0) == "F0 41 00 14 4F F7"
-- > d50_cmd_parse (d50_rjc_gen 0) == Just (0,0x4F,[],0)
d50_rjc_gen :: U8 -> D50_Sysex
d50_rjc_gen ch = d50_cmd_hdr ch RJC_CMD ++ [M.k_Sysex_End]

-- | Parse SYSEX to (CH,CMD,DAT,#DAT).
--
-- > d50_cmd_parse (d50_dsc_gen (DT1_CMD,0,1,[50])) == Just (0,DT1_CMD,[0,0,1,50],4)
d50_cmd_parse :: D50_Sysex -> Maybe (U8,D50_SYSEX_CMD,[U8],U24)
d50_cmd_parse b =
    let b0:b1:b2:b3:b4:b' = b
        dat_chk_n = u24_length b' - 1
        (dat_chk,[eox]) = u24_split_at dat_chk_n b'
        cmd = d50_sysex_cmd_decode b4
    in if any not [b0 == M.k_Sysex_Status,b1 == M.k_Roland_ID,b3 == d50_id,eox == M.k_Sysex_End]
       then Nothing
       else if dat_chk_n == 0
            then Just (b2,cmd,[],0)
            else let Just (dat,dat_n) = d50_data_chk (dat_chk,dat_chk_n)
                 in Just (b2,cmd,dat,dat_n)

-- | Check and remove CHECKSUM from DATA.
--
-- > d50_data_chk ([0,0,1,50,77],5) == Just ([0,0,1,50],4)
d50_data_chk :: ([U8],U24) -> Maybe ([U8],U24)
d50_data_chk (dat_chk,dat_chk_n) =
    let (dat,[chk]) = u24_split_at (dat_chk_n - 1) dat_chk
    in if roland_checksum dat == chk
       then Just (dat,dat_chk_n - 1)
       else Nothing

-- | Remove and pack ADDRESS from start of DAT.
--
-- > d50_data_addr ([0,0,1,50],4) == Just (1,[50],1)
d50_data_addr :: ([U8],U24) -> Maybe (U24,[U8],U24)
d50_data_addr (dat,dat_n) =
    case dat of
      a0:a1:a2:dat' -> Just (u24_pack [a0,a1,a2],dat',dat_n - 3)
      _ -> Nothing

-- | Parse addr/size command from SYSEX.
d50_addr_sz_cmd_parse :: D50_Sysex -> Maybe (U8,D50_SYSEX_CMD,D50_ADDRESS,U24)
d50_addr_sz_cmd_parse syx =
  case d50_cmd_parse syx of
    Just (ch,cmd,[a1,a2,a3,s1,s2,s3],6) -> Just (ch,cmd,u24_pack [a1,a2,a3],u24_pack [s1,s2,s3])
    _ -> Nothing

-- * D50 DSC SYSEX (DT1|DAT)

-- | DSC = DATA-SET COMMAND (DT1|DAT), (CMD,DEVICE-ID,ADDRESS,DATA)
type D50_DSC = (D50_SYSEX_CMD,U8,D50_ADDRESS,[U8])

-- | ADDR field of DSC.
dsc_address :: D50_DSC -> D50_ADDRESS
dsc_address (_,_,a,_) = a

-- | DATA field of DSC.
dsc_data :: D50_DSC -> [U8]
dsc_data (_,_,_,d) = d

-- | Set the CMD element of DSC to /cmd/.
dsc_set_cmd :: D50_SYSEX_CMD -> D50_DSC -> D50_DSC
dsc_set_cmd cmd (_,ch,a,d) = (cmd,ch,a,d)

-- | Parse DSC (DT1|DAT) SYSEX message.
--
-- > let b = d50_dsc_gen (DT1_CMD,0,1,[50])
-- > d50_dsc_parse b == Just (DT1_CMD,0,1,[50])
d50_dsc_parse :: D50_Sysex -> Maybe D50_DSC
d50_dsc_parse syx =
    let Just (ch,cmd,addr_dat,addr_dat_n) = d50_cmd_parse syx
        Just (addr,dat,_dat_n) = d50_data_addr (addr_dat,addr_dat_n)
    in if cmd == DT1_CMD || cmd == DAT_CMD
       then Just (cmd,ch,addr,dat)
       else Nothing

-- | Erroring variant.
d50_dsc_parse_err :: D50_Sysex -> D50_DSC
d50_dsc_parse_err = fromMaybe (error "d50_dsc_parse") . d50_dsc_parse

-- | Generate DSC (DT1|DAT) SYSEX message.
--
-- > d50_sysex_pp (d50_dsc_gen (DT1_CMD,0,1,[50])) == "F041001412000001324DF7"
-- > d50_sysex_pp (d50_dsc_gen (DT1_CMD,0,409,[0x10])) == "F0410014120003191054F7"
d50_dsc_gen :: D50_DSC -> D50_Sysex
d50_dsc_gen (cmd,ch,a,d) =
    let dat = u24_unpack a ++ d
    in if length d > 256
       then error "d50_dsc_gen: #DATA > 256"
       else concat [d50_cmd_hdr ch cmd,d50_cmd_data_chk dat]

-- | Generate a sequence of DSC messages segmenting data sets longer than 256 elements.
d50_dsc_gen_seq :: D50_DSC -> [D50_Sysex]
d50_dsc_gen_seq (cmd,ch,a,d) =
    if null d
    then []
    else if length d <= 256
         then [d50_dsc_gen (cmd,ch,a,d)]
         else d50_dsc_gen (cmd,ch,a,take 256 d) : d50_dsc_gen_seq (cmd,ch,a + 256,drop 256 d)

{- | 'd50_dsc_gen' of 'd50_named_parameter_to_address'.

3.2 Data set (One way) DT1 12H

a 1111 0000 F0 Exclusive status
b 0100 0001 41 Roland-ID
c 0000 nnnn    Device-ID = MIDI CHANNEL
d 0001 0100 14 Model-ID
e 0001 0010 12 Command-ID
f 0aaa aaaa    Address MSB
g 0bbb bbbb    Address
h 0ccc cccc    Address LSB
i 0ddd dddd    Data
...
j 0eee eeee    Checksum
k 1111 0111 F7 End of System Exclusive

> let nm = (Patch,"Lower Tone Fine Tune")
> fmap d50_sysex_pp (d50_gen_dt1_nm 0 nm [0x10]) == Just "F0410014120003191054F7"
-}
d50_gen_dt1_nm :: U8 -> (D50_Parameter_Type,String) -> [U8] -> Maybe D50_Sysex
d50_gen_dt1_nm ch nm d =
    let f a = d50_dsc_gen (DT1_CMD,ch,a,d)
    in fmap f (d50_named_parameter_to_address nm)

-- * D50 DSC SEQ

-- | Join a contiguous sequence of 'D50_DSC' into one data segment, ie. (address,size,data).
d50_dsc_seq_join :: [D50_DSC] -> (D50_ADDRESS,U24,[U8])
d50_dsc_seq_join sq =
  let addr_seq = map dsc_address sq
      dat_seq = map dsc_data sq
      size_seq = map u24_length dat_seq
      addr:_ = addr_seq
  in if T.d_dx addr_seq `isPrefixOf` size_seq
     then (addr,sum size_seq,concat dat_seq)
     else error "d50_dsc_seq_join?"

-- | If data segment is a D50 bulk-data transfer partition into 64 patches and 16 reverbs.
d50_bulk_data_transfer_parse :: (D50_ADDRESS,U24,[U8]) -> Maybe ([D50_Patch],[D50_Reverb])
d50_bulk_data_transfer_parse (addr,sz,dat) =
  let patch_n = d50_parameter_n * 64
      rvb_n = d50_reverb_data_segment_n * 16
  in if addr /= 0x8000 || sz /= (patch_n + rvb_n)
     then Nothing
     else let (p_dat,r_dat) = u24_split_at patch_n dat
          in Just (Split.chunksOf d50_parameter_n p_dat
                  ,map d50_reverb_decode (Split.chunksOf d50_reverb_data_segment_n r_dat))

-- * Tuning

-- | Generate DSC/DT1 sequence to set the @WG PITCH KF@ parameters of all
-- four partials to indicated ratio.
--
-- > map d50_dsc_gen (d50_wg_pitch_kf_dt1 (1/4))
d50_wg_pitch_kf_dt1 :: (Eq n,Fractional n) => n -> [D50_DSC]
d50_wg_pitch_kf_dt1 r =
    let e = wg_pitch_kf_to_enum_err r
        a_seq = map (+ 2) d50_partial_base_address_seq
        f a = (DT1_CMD,0,a,[e])
    in map f a_seq

-- * CSV I/O

-- | Given (ADDR,VALUE) for (TYPE,PARAM) make CSV entry.
d50_parameter_csv :: (D50_ADDRESS,U8) -> (D50_Parameter_Type,D50_Parameter) -> String
d50_parameter_csv (a,x) (ty,p) =
    let (ix,nm,_,_,_usr_str) = p
        x' = d50_parameter_value_verify p x
    in intercalate "," [show a,d50_parameter_type_pp ty,show ix,nm
                       ,show x',range_pp (d50_parameter_range p)
                       ,d50_parameter_value_usr_def "??" p x]

-- | Given sequence of parameter values, generate /CSV/ of patch, unused param are Left.
d50_patch_csv_e :: D50_Patch -> [Either D50_ADDRESS String]
d50_patch_csv_e =
    let f (a,v) =
          case d50_address_to_parameter a of
            Just p -> Right (d50_parameter_csv (a,v) p)
            _ -> if v == 0
                 then Left a
                 else error ("d50_patch_csv: VALUE NOT ZERO AT NON-PARAMETER ADDRESS" ++ show (a,v))
    in map f . zip [0 ..]

{- | 'd50_patch_csv_e', if /u/ write unused entries as empty rows, else discard them.

> dir = "/home/rohan/uc/invisible/light/d50/"
> p:_ <- d50_load_hex (dir ++ "d50.hex.text")
> writeFile (dir ++ "d50.csv") (unlines (d50_patch_csv True p))
-}
d50_patch_csv :: Bool -> D50_Patch -> [String]
d50_patch_csv u =
  let hdr = "ADDRESS,PARAMETER-TYPE,INDEX,NAME,VALUE,RANGE,VALUE-USER"
  in (hdr :) .
     (if u then map (either (\a -> show a ++ ",,,,,,") id) else rights) .
     d50_patch_csv_e

-- * HEX I/O

{- | Load text file where each line is a sequence of 448 (0x1C0)
   two-character hexadecimal byte values, ie. a D50 patch.
-}
d50_load_hex :: FilePath -> IO [D50_Patch]
d50_load_hex fn = do
  b <- T.load_hex_byte_seq fn
  case b of
    [] -> error "d50_load_hex?"
    x -> if any (((/=) 448) . length) x then error "d50_load_hex: 448?" else return x

-- | Type specialised 'T.store_hex_byte_seq'
d50_store_hex :: FilePath -> [D50_Patch] -> IO ()
d50_store_hex = T.store_hex_byte_seq

-- * BINARY I/O

-- | Load binary 'U8' sequence from file.
d50_load_binary_u8 :: FilePath -> IO [U8]
d50_load_binary_u8 = fmap B.unpack . B.readFile

-- | Write binary 'U8' sequence to file.
d50_store_binary_u8 :: FilePath -> [U8] -> IO ()
d50_store_binary_u8 fn = B.writeFile fn . B.pack

{-| Load DT1|DAT sequence from 36048-byte D-50 SYSEX file.

> let sysex_fn = "/home/rohan/sw/hsc3-data/data/roland/d50/PN-D50-00.syx"
> b <- d50_load_binary_u8 sysex_fn
> let s = d50_sysex_segment b
> d <- d50_load_sysex_dsc sysex_fn
> let s' = d50_dsc_gen_seq (DT1_CMD,0,d50_patch_memory_base 0,concatMap dsc_data d)
> s == s'

-}
d50_load_sysex_dsc :: FilePath -> IO [D50_DSC]
d50_load_sysex_dsc fn = do
  b <- d50_load_binary_u8 fn
  let b_n = length b
  when (b_n /= 0x8CD0) (putStrLn "d50_load_sysex: sysex != 0x8CD0 (36048)")
  when (b_n < 0x8CD0) (error "d50_load_sysex: sysex < 0x8CD0 (36048)")
  return (map d50_dsc_parse_err (d50_sysex_segment b))

{-| Load patch data (64 patches of 448 bytes) and reverb data (16
  decoded reverbs of 188 bytes) from D-50 SYSEX file.

> let sysex_fn = "/home/rohan/sw/hsc3-data/data/roland/d50/PN-D50-00.syx"
> (p,r) <- d50_load_sysex sysex_fn
> putStrLn$unlines$map d50_patch_name p
> putStrLn$unlines$concatMap d50_patch_group_pp p

-}
d50_load_sysex :: FilePath -> IO ([D50_Patch],[D50_Reverb])
d50_load_sysex fn = do
  dsc <- d50_load_sysex_dsc fn
  case d50_bulk_data_transfer_parse (d50_dsc_seq_join dsc) of
    Just r -> return r
    Nothing -> error "d50_load_sysex?"
