module Sound.SC3.Data.Byte where

import Data.Bits {- base -}
import Data.Int {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import Data.Word {- base -}

import qualified Music.Theory.Math.Convert as T {- hmt -}
import qualified Music.Theory.Tuple as T {- hmt -}

import qualified Sound.Midi.Common as M {- midi-osc -}

-- | 4-bit unsigned integer.
type U4 = Word8

-- | 8-bit unsigned integer.
type U8 = Word8

-- | 12-bit unsigned integer.
type U12 = Word16

-- | 16-bit unsigned integer.
type U16 = Word16

-- | 24-bit unsigned integer.
type U24 = Word32

-- | 32-bit unsigned integer.
type U32 = Word32

-- | 8-bit signed integer.
type I8 = Int8

-- | 12-bit signed integer.
type I12 = Int16

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

u12_to_int :: U12 -> Int
u12_to_int = fromIntegral

i12_to_int :: I12 -> Int
i12_to_int = fromIntegral

-- | Type-specialised 'fromIntegral'.
u8_to_u12 :: U8 -> U12
u8_to_u12 = fromIntegral

-- | Type-specialised 'fromIntegral'.
u4_to_u12 :: U4 -> U12
u4_to_u12 = fromIntegral

-- | (LSB,MSB)
u12_pack :: (U4,U8) -> U12
u12_pack (p,q) = u4_to_u12 p .|. shiftL (u8_to_u12 q) 4

u12_to_i12 :: U12 -> I12
u12_to_i12 = fromIntegral

-- | U12 as I12, ie. two's complement bit-pattern.
--
-- > map u12_to_i12 [0,1,2047,2048,2049,4095] == [0,1,2047,-2048,-2047,-1]
u12_as_i12 :: U12 -> I12
u12_as_i12 x = let y = u12_to_i12 x in if y >= 2048 then y - 2048 * 2 else y

u16_to_u24 :: U16 -> U24
u16_to_u24 = T.word16_to_word32

u16_pack_le :: [U8] -> U16
u16_pack_le l =
  case l of
    [p,q] -> M.u8_to_u16 p .|. shiftL (M.u8_to_u16 q) 8
    _ -> error "u16_pack_le?"

u16_at :: [t] -> U16 -> t
u16_at = genericIndex

u24_length :: [t] -> U24
u24_length = int_to_u24 . length

u24_at :: [t] -> U24 -> t
u24_at = genericIndex

u24_take :: U24 -> [t] -> [t]
u24_take = genericTake

u24_drop :: U24 -> [t] -> [t]
u24_drop = genericDrop

u24_split_at :: U24 -> [t] -> ([t], [t])
u24_split_at = genericSplitAt

-- | Pack 'U24' from three 'U8', MSB-LSB.
u24_pack_be :: [U8] -> U24
u24_pack_be l =
  case l of
    [p,q,r] -> shiftL (M.u8_to_u32 p) 16 .|. shiftL (M.u8_to_u32 q) 8 .|. M.u8_to_u32 r
    _ -> error "u24_pack?"

u24_pack_le :: [U8] -> U24
u24_pack_le = u24_pack_be . reverse

u24_unpack_be :: U24 -> [U8]
u24_unpack_be a =
  [M.u32_to_u8 (shiftR a 16 .&. 0xFF)
  ,M.u32_to_u8 (shiftR a 8 .&. 0xFF)
  ,M.u32_to_u8 (a .&. 0xFF)]

u32_take :: U32 -> [t] -> [t]
u32_take = genericTake

u32_drop :: U32 -> [t] -> [t]
u32_drop = genericDrop

-- | Pack 'U32' from four 'U8', MSB-LSB.
u32_pack_be :: [U8] -> U32
u32_pack_be l =
  case l of
    [p,q,r,s] -> shiftL (M.u8_to_u32 p) 24 .|.
                 shiftL (M.u8_to_u32 q) 16 .|.
                 shiftL (M.u8_to_u32 r)  8 .|.
                 M.u8_to_u32 s
    _ -> error "u32_pack?"

u32_pack_le :: [U8] -> U32
u32_pack_le = u32_pack_be . reverse

-- * U21

-- | Pack 'U21' from three 'U7', MSB-LSB.
--
-- > map u21_pack_be [[0x02,0x00,0x00],[0x02,0x0F,0x00]] == [0x8000,0x8780]
u21_pack_be :: [U8] -> U24
u21_pack_be l =
  case l of
    [p,q,r] -> shiftL (M.u8_to_u32 p) 14 .|. shiftL (M.u8_to_u32 q) 7 .|. M.u8_to_u32 r
    _ -> error "u21_pack?"

u21_pack_le :: [U8] -> U24
u21_pack_le = u21_pack_be . reverse

-- | Unpack 'U24' to 'U8', MSB-LSB.
u21_unpack_be :: U24 -> [U8]
u21_unpack_be = T.t3_to_list . M.bits_21_sep_be
