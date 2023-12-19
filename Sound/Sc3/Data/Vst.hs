-- | Vst Sdk
module Sound.Sc3.Data.Vst where

import Data.Bits {- base -}
import Data.List {- base -}
import Data.Word {- base -}

import qualified Data.ByteString as ByteString {- bytestring -}
import qualified Data.List.Split as Split {- split -}

-- * Byte

word8_to_char :: Word8 -> Char
word8_to_char = toEnum . fromIntegral

word8_to_word32 :: Word8 -> Word32
word8_to_word32 = fromIntegral

word32_to_word8 :: Word32 -> Word8
word32_to_word8 = fromIntegral

int_to_word32 :: Int -> Word32
int_to_word32 = fromIntegral

char_to_word32 :: Char -> Word32
char_to_word32 = int_to_word32 . fromEnum

-- > pack_word32 [0x43,0x63,0x6e,0x4b] == 0x43636e4b
pack_word32 :: [Word8] -> Word32
pack_word32 x =
  let f c = shiftL (word8_to_word32 c)
  in case x of
      [d1, d2, d3, d4] -> sum [f d1 24, f d2 16, f d3 8, f d4 0]
      _ -> error "pack_word32?"

-- > unpack_word32 0x43636e4b == [0x43,0x63,0x6e,0x4b]
unpack_word32 :: Word32 -> [Word8]
unpack_word32 x =
  let f c = word32_to_word8 . shiftR c
  in [f x 24, f x 16, f x 8, f x 0]

-- > map str_to_word32 [fx_c_magic_str,fx_chunk_bank_magic_str] == [0x43636e4b,0x46424368]
-- > str_to_word32 "Obxd" == 0x4f627864
str_to_word32 :: String -> Word32
str_to_word32 str =
  let f c x = shiftL (char_to_word32 c) x
  in case str of
      [d1, d2, d3, d4] -> sum [f d1 24, f d2 16, f d3 8, f d4 0]
      _ -> error "str_to_word32"

word32_to_str :: Word32 -> String
word32_to_str = map word8_to_char . unpack_word32

-- * Fx Store

fx_c_magic :: Word32
fx_c_magic = str_to_word32 "CcnK"

fx_f_magic :: Word32
fx_f_magic = str_to_word32 "FxCk"

fx_bank_magic :: Word32
fx_bank_magic = str_to_word32 "FxBk"

fx_chunk_preset_magic :: Word32
fx_chunk_preset_magic = str_to_word32 "FPCh"

fx_chunk_bank_magic :: Word32
fx_chunk_bank_magic = str_to_word32 "FBCh"

fx_CcnK_FBCh_hdr_structure :: [Word32]
fx_CcnK_FBCh_hdr_structure = [4, 4, 4, 4, 4, 4, 4, 128, 4]

fx_CcnK_FBCh_hdr_sz :: Word32
fx_CcnK_FBCh_hdr_sz = sum fx_CcnK_FBCh_hdr_structure

-- | (FX-ID,FX-VERSION,FX-BANK-SIZE,FX-DATA)
type FX_CcnK_FBCh = (Word32, Word32, Word32, [Word8])

fx_verify_word32_eq :: String -> Word32 -> Word32 -> Bool
fx_verify_word32_eq err p q = p == q || error (show (err, p, q))

fx_verify_word32_elem :: String -> Word32 -> [Word32] -> Bool
fx_verify_word32_elem err p q = not (p `notElem` q) || error (show (err, p, q))

fx_CcnK_FBCh_hdr_verify :: [Word8] -> FX_CcnK_FBCh
fx_CcnK_FBCh_hdr_verify chk =
  let err z = error ("fx_CcnK_FBCh_hdr_verify: " ++ z)
      no_err z = fx_verify_word32_eq ("fx_CcnK_FBCh_hdr_verify: illegal-header: " ++ z)
      (hdr, dat) = genericSplitAt fx_CcnK_FBCh_hdr_sz chk
  in case Split.splitPlaces fx_CcnK_FBCh_hdr_structure hdr of
      [k1, c_size, k2, [0, 0, 0, 1], fx_id, fx_version, bnk_sz, _, opq_sz] ->
        if no_err "c_size" (pack_word32 c_size) (genericLength chk - 8)
          && no_err "c_magic" (pack_word32 k1) fx_c_magic
          && no_err "bank_magic" (pack_word32 k2) fx_chunk_bank_magic
          && no_err "opq_sz" (pack_word32 opq_sz) (genericLength dat)
          then (pack_word32 fx_id, pack_word32 fx_version, pack_word32 bnk_sz, dat)
          else err "?"
      _ -> err "no-split?"

fx_parse_CcnK_FBCh :: [Word8] -> FX_CcnK_FBCh
fx_parse_CcnK_FBCh = fx_CcnK_FBCh_hdr_verify

fx_load_CcnK_FBCh :: FilePath -> IO FX_CcnK_FBCh
fx_load_CcnK_FBCh fn = do
  b <- ByteString.readFile fn
  return (fx_parse_CcnK_FBCh (ByteString.unpack b))

fx_CcnK_FBCh_hdr_pp :: FX_CcnK_FBCh -> IO ()
fx_CcnK_FBCh_hdr_pp (fx_id, fx_v, fx_sz, dat) = do
  print ("fx-id", word32_to_str fx_id)
  print ("fx-version", fx_v)
  print ("fx-program-count", fx_sz)
  print ("fx-dat-size", length dat)
