module Sound.SC3.Data.Math.Types where

import Data.Int {- base -}
import Data.List {- base -}
import Data.Word {- base -}

-- | 4-bit unsigned integer (0 - 15)
type U4 = Word8

-- | 7-bit unsigned integer (0 - 127)
type U7 = Word8

-- | 8-bit unsigned integer (0 - 255)
type U8 = Word8

-- | 12-bit unsigned integer (0 - 4095)
type U12 = Word16

-- | 16-bit unsigned integer (0 - 65,535)
type U16 = Word16

-- | 24-bit unsigned integer (0 - 16,777,215)
type U24 = Word32

-- | 2^24 - 1
u24_max :: Num n => n
u24_max = 16777215

-- | 32-bit unsigned integer (0 - 4,294,967,295)
type U32 = Word32

-- | 8-bit signed integer (-128 - 127).
type I8 = Int8

-- | 12-bit signed integer (-4096 - 4095).
type I12 = Int16

-- | 16-bit signed integer (-32,768 - 32,767).
type I16 = Int16

type F32 = Float
type F64 = Float

-- | Type-specialised 'fromIntegral'.
u4_to_u12 :: U4 -> U12
u4_to_u12 = fromIntegral

u7_to_u12 :: U7 -> U12
u7_to_u12 = fromIntegral

u7_to_i12 :: U7 -> I12
u7_to_i12 = fromIntegral

-- | Type-specialised 'fromIntegral'.
u8_to_u12 :: U8 -> U12
u8_to_u12 = fromIntegral

u8_to_i8_def :: I8 -> U8 -> I8
u8_to_i8_def d x = if x > 127 then d else fromIntegral x

u8_to_i8_err :: U8 -> I8
u8_to_i8_err x = u8_to_i8_def (error ("u8_to_i8: " ++ show x)) x

u8_to_i16 :: U8 -> I16
u8_to_i16 = fromIntegral

u8_to_u24 :: U8 -> U24
u8_to_u24 = fromIntegral

int_to_u8_def :: U8 -> Int -> U8
int_to_u8_def d x = if x < 0 || x > 255 then d else fromIntegral x

int_to_u8_err :: Int -> U8
int_to_u8_err x = int_to_u8_def (error ("int_to_u8: " ++ show x)) x

u12_to_i12_def :: I12 -> U12 -> I12
u12_to_i12_def d x = if x > 4095 then d else fromIntegral x

u12_to_i12_err :: U12 -> I12
u12_to_i12_err x = u12_to_i12_def (error ("u12_to_i12: " ++ show x)) x

-- | U12 as I12, ie. two's complement bit-pattern.
--
-- > map u12_as_i12 [0,1,2047,2048,2049,4095] == [0,1,2047,-2048,-2047,-1]
u12_as_i12 :: U12 -> I12
u12_as_i12 x = let y = u12_to_i12_err x in if y >= 2048 then y - 2048 * 2 else y

u12_to_int :: U12 -> Int
u12_to_int = fromIntegral

u16_to_u24 :: U16 -> U24
u16_to_u24 = fromIntegral

int_to_u24_def :: U24 -> Int -> U24
int_to_u24_def d x = if x < 0 || x > u24_max then d else fromIntegral x

int_to_u24_err :: Int -> U24
int_to_u24_err x = int_to_u24_def (error ("int_to_u24: " ++ show x)) x

i8_to_u8_def :: U8 -> I8 -> U8
i8_to_u8_def d x = if x < 0 then d else fromIntegral x

i8_to_u8_err :: I8 -> U8
i8_to_u8_err x = i8_to_u8_def (error ("i8_to_u8: " ++ show x)) x

i12_to_int :: I12 -> Int
i12_to_int = fromIntegral

f32_to_u8_def :: U8 -> F32 -> U8
f32_to_u8_def d x = if x < 0 || x > 255 then d else floor x

f32_to_u8_err :: F32 -> U8
f32_to_u8_err = f32_to_u8_def (error "f32_to_u8?")

f32_to_i8_def :: I8 -> F32 -> I8
f32_to_i8_def d x = if x < (-128) || x > 127 then d else floor x

f32_to_i8_err :: F32 -> I8
f32_to_i8_err x = f32_to_i8_def (error ("f32_to_i8: " ++ show x)) x

f32_to_i16_def :: I16 -> F32 -> I16
f32_to_i16_def d x = if x < (-32768) || x > 32767 then d else floor x

f32_to_i16_err :: F32 -> I16
f32_to_i16_err x = f32_to_i16_def (error ("f32_to_i16: " ++ show x)) x

-- * LIST

u8_length :: [t] -> U8
u8_length = int_to_u8_err . length

u8_at :: [t] -> U8 -> t
u8_at = genericIndex

u16_at :: [t] -> U16 -> t
u16_at = genericIndex

u24_length :: [t] -> U24
u24_length = int_to_u24_err . length

u24_at :: [t] -> U24 -> t
u24_at = genericIndex

u24_take :: U24 -> [t] -> [t]
u24_take = genericTake

u24_drop :: U24 -> [t] -> [t]
u24_drop = genericDrop

u24_split_at :: U24 -> [t] -> ([t], [t])
u24_split_at = genericSplitAt

u32_take :: U32 -> [t] -> [t]
u32_take = genericTake

u32_drop :: U32 -> [t] -> [t]
u32_drop = genericDrop
