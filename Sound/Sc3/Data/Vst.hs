-- | Vst Sdk
module Sound.Sc3.Data.Vst where

import qualified Data.Bits {- base -}
import qualified Data.List {- base -}
import qualified Data.Word {- base -}

import qualified Data.ByteString as ByteString {- bytestring -}
import qualified Data.List.Split as Split {- split -}

import qualified Music.Theory.Byte as Byte {- hmt-base -}
import qualified Music.Theory.Math.Convert as Convert {- hmt-base -}

-- * Byte

{- | Pack four bytes into a word

>>> pack_word32 [0x43,0x63,0x6e,0x4b]
1130589771

>>> 0x43636e4b
1130589771
-}
pack_word32 :: [Data.Word.Word8] -> Data.Word.Word32
pack_word32 x =
  let f c = Data.Bits.shiftL (Convert.word8_to_word32 c)
  in case x of
      [d1, d2, d3, d4] -> sum [f d1 24, f d2 16, f d3 8, f d4 0]
      _ -> error "pack_word32?"

{- | Unpack a word into four bytes

>>> unpack_word32 0x43636e4b
[67,99,110,75]

>>> [0x43,0x63,0x6e,0x4b]
[67,99,110,75]
-}
unpack_word32 :: Data.Word.Word32 -> [Data.Word.Word8]
unpack_word32 x =
  let f c = Convert.word32_to_word8 . Data.Bits.shiftR c
  in [f x 24, f x 16, f x 8, f x 0]

{- | Four byte string to word

>>> map str_to_word32 ["CcnK","FxBk"]
[1130589771,1182286443]

>>> str_to_word32 "Obxd"
1331853412

>>> 0x4f627864
1331853412
-}
str_to_word32 :: String -> Data.Word.Word32
str_to_word32 str =
  let f c x = Data.Bits.shiftL (Byte.char_to_word32 c) x
  in case str of
      [d1, d2, d3, d4] -> sum [f d1 24, f d2 16, f d3 8, f d4 0]
      _ -> error "str_to_word32"

word32_to_str :: Data.Word.Word32 -> String
word32_to_str = map Byte.word8_to_char . unpack_word32

-- * Fx Store

fx_c_magic :: Data.Word.Word32
fx_c_magic = str_to_word32 "CcnK"

fx_f_magic :: Data.Word.Word32
fx_f_magic = str_to_word32 "FxCk"

fx_bank_magic :: Data.Word.Word32
fx_bank_magic = str_to_word32 "FxBk"

fx_chunk_preset_magic :: Data.Word.Word32
fx_chunk_preset_magic = str_to_word32 "FPCh"

fx_chunk_bank_magic :: Data.Word.Word32
fx_chunk_bank_magic = str_to_word32 "FBCh"

fx_CcnK_FBCh_hdr_structure :: [Data.Word.Word32]
fx_CcnK_FBCh_hdr_structure = [4, 4, 4, 4, 4, 4, 4, 128, 4]

fx_CcnK_FBCh_hdr_sz :: Data.Word.Word32
fx_CcnK_FBCh_hdr_sz = sum fx_CcnK_FBCh_hdr_structure

-- | (Fx-Id,Fx-Version,Fx-Bank-Size,Fx-Data)
type FX_CcnK_FBCh = (Data.Word.Word32, Data.Word.Word32, Data.Word.Word32, [Data.Word.Word8])

fx_verify_word32_eq :: String -> Data.Word.Word32 -> Data.Word.Word32 -> Bool
fx_verify_word32_eq err p q = p == q || error (show (err, p, q))

fx_verify_word32_elem :: String -> Data.Word.Word32 -> [Data.Word.Word32] -> Bool
fx_verify_word32_elem err p q = not (p `notElem` q) || error (show (err, p, q))

fx_CcnK_FBCh_hdr_verify :: [Data.Word.Word8] -> FX_CcnK_FBCh
fx_CcnK_FBCh_hdr_verify chk =
  let err z = error ("fx_CcnK_FBCh_hdr_verify: " ++ z)
      no_err z = fx_verify_word32_eq ("fx_CcnK_FBCh_hdr_verify: illegal-header: " ++ z)
      (hdr, dat) = Data.List.genericSplitAt fx_CcnK_FBCh_hdr_sz chk
  in case Split.splitPlaces fx_CcnK_FBCh_hdr_structure hdr of
      [k1, c_size, k2, [0, 0, 0, 1], fx_id, fx_version, bnk_sz, _, opq_sz] ->
        if no_err "c_size" (pack_word32 c_size) (Data.List.genericLength chk - 8)
          && no_err "c_magic" (pack_word32 k1) fx_c_magic
          && no_err "bank_magic" (pack_word32 k2) fx_chunk_bank_magic
          && no_err "opq_sz" (pack_word32 opq_sz) (Data.List.genericLength dat)
          then (pack_word32 fx_id, pack_word32 fx_version, pack_word32 bnk_sz, dat)
          else err "?"
      _ -> err "no-split?"

fx_parse_CcnK_FBCh :: [Data.Word.Word8] -> FX_CcnK_FBCh
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
