module Sound.SC3.Data.Byte where

import Data.Bits {- base -}

import qualified Sound.Midi.Common as M {- midi-osc -}

import Sound.SC3.Data.Math.Types {- hsc3-data -}

-- | (LSB,MSB)
u12_pack_le :: (U4,U8) -> U12
u12_pack_le (p,q) = u4_to_u12 p .|. shiftL (u8_to_u12 q) 4

-- | (LSB,MSB)
u16_pack_le :: (U8,U8) -> U16
u16_pack_le (p,q) = M.u8_to_u16 p .|. shiftL (M.u8_to_u16 q) 8

-- | Pack 'U24' from three 'U8', MSB-LSB.
u24_pack_be :: (U8,U8,U8) -> U24
u24_pack_be (p,q,r) = shiftL (M.u8_to_u32 p) 16 .|. shiftL (M.u8_to_u32 q) 8 .|. M.u8_to_u32 r

u24_pack_le :: (U8,U8,U8) -> U24
u24_pack_le (p,q,r) = u24_pack_be (r,q,p)

u24_unpack_be :: U24 -> (U8,U8,U8)
u24_unpack_be a =
  (M.u32_to_u8 (shiftR a 16 .&. 0xFF)
  ,M.u32_to_u8 (shiftR a 8 .&. 0xFF)
  ,M.u32_to_u8 (a .&. 0xFF))

-- | Pack 'U32' from four 'U8', MSB-LSB.
u32_pack_be :: (U8,U8,U8,U8) -> U32
u32_pack_be (p,q,r,s) =
  shiftL (M.u8_to_u32 p) 24 .|.
  shiftL (M.u8_to_u32 q) 16 .|.
  shiftL (M.u8_to_u32 r)  8 .|.
  M.u8_to_u32 s

u32_pack_le :: (U8,U8,U8,U8) -> U32
u32_pack_le (p,q,r,s) = u32_pack_be (s,r,q,p)

-- * U21

-- | Pack 'U21' from three 'U7', MSB-LSB.
--
-- > map u21_pack_be [(0x02,0x00,0x00),(0x02,0x0F,0x00)] == [0x8000,0x8780]
u21_pack_be :: (U8,U8,U8) -> U24
u21_pack_be = M.bits_21_join_be

u21_pack_le :: (U8,U8,U8) -> U24
u21_pack_le (p,q,r) = u21_pack_be (r,q,p)

-- | Unpack 'U24' to 'U8', MSB-LSB.
u21_unpack_be :: U24 -> (U8,U8,U8)
u21_unpack_be = M.bits_21_sep_be
