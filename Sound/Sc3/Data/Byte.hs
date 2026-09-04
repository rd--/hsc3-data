{- | Byte

Lsb = Least significant byte
Msb = Most significant byte
-}
module Sound.Sc3.Data.Byte where

import qualified Data.Bits {- base -}

import qualified Sound.Midi.Common as Midi {- midi-osc -}

import Sound.Sc3.Data.Math.Types {- hsc3-data -}

-- | Bit and (alias for Data.Bits..&.)
bitAnd :: (Data.Bits.Bits b) => b -> b -> b
bitAnd = (Data.Bits..&.)

-- | Bit or (alias for Data.Bits..|.)
bitOr :: (Data.Bits.Bits b) => b -> b -> b
bitOr = (Data.Bits..|.)

-- | Bit shift left (alias for Data.Bits.shiftL)
bitShiftLeft :: (Data.Bits.Bits b) => b -> Int -> b
bitShiftLeft = Data.Bits.shiftL

-- | Bit shift right (alias for Data.Bits.shiftR)
bitShiftRight :: (Data.Bits.Bits b) => b -> Int -> b
bitShiftRight = Data.Bits.shiftR

-- | (Lsb,Msb)
u12_pack_le :: (U4, U8) -> U12
u12_pack_le (p, q) =
  u4_to_u12 p
    `bitOr` bitShiftLeft (u8_to_u12 q) 4

-- | (Lsb,Msb)
u16_pack_le :: (U8, U8) -> U16
u16_pack_le (p, q) =
  u8_to_u16 p
    `bitOr` bitShiftLeft (u8_to_u16 q) 8

{- | Pack 'U24' from three 'U8', Msb-Lsb.

>>> u24_pack_be (0x12, 0x34, 0x56)
1193046

>>> 0x123456
1193046
-}
u24_pack_be :: (U8, U8, U8) -> U24
u24_pack_be (p, q, r) =
  bitShiftLeft (u8_to_u32 p) 16
    `bitOr` bitShiftLeft (u8_to_u32 q) 8
    `bitOr` u8_to_u32 r

u24_pack_le :: (U8, U8, U8) -> U24
u24_pack_le (p, q, r) = u24_pack_be (r, q, p)

{- | Unpack 'U24' to three 'U8', Msb-Lsb.

>>> u24_unpack_be 0x123456
(18,52,86)

>>> (0x12, 0x34, 0x56)
(18,52,86)
-}
u24_unpack_be :: U24 -> (U8, U8, U8)
u24_unpack_be a =
  ( u32_to_u8 (bitShiftRight a 16 `bitAnd` 0xFF)
  , u32_to_u8 (bitShiftRight a 8 `bitAnd` 0xFF)
  , u32_to_u8 (a `bitAnd` 0xFF)
  )

-- | Pack 'U32' from four 'U8', Msb-Lsb.
u32_pack_be :: (U8, U8, U8, U8) -> U32
u32_pack_be (p, q, r, s) =
  bitShiftLeft (u8_to_u32 p) 24
    `bitOr` bitShiftLeft (u8_to_u32 q) 16
    `bitOr` bitShiftLeft (u8_to_u32 r) 8
    `bitOr` u8_to_u32 s

u32_pack_le :: (U8, U8, U8, U8) -> U32
u32_pack_le (p, q, r, s) = u32_pack_be (s, r, q, p)

-- * U21

{- | Pack 'U21' from three 'U7', Msb-Lsb.

>>> map u21_pack_be [(0x02,0x00,0x00),(0x02,0x0F,0x00)]
[32768,34688]

>>> [0x8000,0x8780]
[32768,34688]
-}
u21_pack_be :: (U8, U8, U8) -> U24
u21_pack_be = Midi.bits_21_join_be

u21_pack_le :: (U8, U8, U8) -> U24
u21_pack_le (p, q, r) = u21_pack_be (r, q, p)

-- | Unpack 'U24' to 'U8', Msb-Lsb.
u21_unpack_be :: U24 -> (U8, U8, U8)
u21_unpack_be = Midi.bits_21_sep_be
