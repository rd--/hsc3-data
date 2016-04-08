import Data.Word {- base -}
import System.Environment {- base -}

import qualified Music.Theory.Byte as T {- hmt -}

id_w8_seq :: [Word8] -> [Word8]
id_w8_seq = id

usage :: IO ()
usage =
    let h = ["encode text-file binary-file"
            ,"decode binary-file text-file"]
    in putStrLn (unlines h)

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["decode",b_fn,t_fn] -> T.load_byte_seq b_fn >>= T.store_hex_byte_seq t_fn . id_w8_seq
    ["encode",t_fn,b_fn] -> T.load_hex_byte_seq t_fn >>= T.store_byte_seq b_fn . id_w8_seq
    _ -> usage
