-- | A 7-segment Alphabet with a Unique Pattern for Every ASCII Character
--
-- <https://dkeenan.com/7-segment%20ASCII%20characters.txt>
module Sound.Sc3.Data.Bitmap.Font.Segment where

import Data.List {- base -}
import Data.Word {- base -}

import qualified Sound.Sc3.Data.Bitmap.PBM as PBM {- hsc3-data -}
import Sound.Sc3.Data.Bitmap.Type {- hsc3-data -}

{- | Sequence of hex codes for each glyph for each character.
      _                        _    _                   _              _    _
      _   |     _     |  |_    _|  |      |  |_   |      |   _|   _|  | |
       |  | |   _|  | |   _     |    |            |_|  | |    |        _     |

 00   45   34   4C   16   68   47   25   02   60   3C   17   46   42   2B   05
<nul> ^A   ^B   ^C   ^D   ^E   ^F   ^G  <bs> <tab>^J   ^K   <ff> <cr>  ^N   ^O
           _              _                             _         _    _
| |  | |  | |  |     _|       | |        _|  |_|    |  |      |            |
|      |    |   _|   _|  |_|   _|  | |   _   |_   |_        |    |_   |     _

 32   26   27   2C   4E   1D   2E   14   4A   7A   1A   21   12   19   11   28
 ^P   ^Q   ^R   ^S   ^T   ^U   ^V   ^W   ^X   ^Y   ^Z  <esc> ^\   ^]   ^^   ^_
                     _                             _
 _     |  | |  | |   _   |    |_|  |     _|  |_   |_|  |_         _         _|
  |   _        | |   _     |  |_|       |_    _|       |     _|        _   |

 44   0A   22   36   49   24   7E   20   5A   6C   63   70   0C   40   08   52
<sp>  !    "    #    $    %    &    '    (    )    *    +    ,    -    .    /
 _         _    _         _    _    _    _    _    _    _    _         _    _
| |    |   _|   _|  |_|  |_   |_     |  |_|  |_|            |_    _    _|   _|
|_|    |  |_    _|    |   _|  |_|    |  |_|   _|   _    _|        _        |

 3F   06   5B   4F   66   6D   7D   07   7F   6F   09   0D   61   48   43   53
 0    1    2    3    4    5    6    7    8    9    :    ;    <    =    >    ?
 _    _    _    _    _    _    _    _         _         _         _    _    _
 _   |_|  | |  |      |  |_    _   |    |_|    |    |  |    |         | |  | |
|_|  | |  |_   |_   |_|  |_   |    |_|  | |   _|  |_|  | |  |_   | |  | |   _|

 5D   77   3B   39   1F   79   51   3D   76   0F   1E   35   38   15   37   2F
 @    A    B    C    D    E    F    G    H    I    J    K    L    M    N    O
 _    _    _    _    _                   _         _    _         _    _
  |  |_|  | |  |    |    | |  | |  | |   _|  |_|    |  |    |_     |  | |
|     _   |     _|  |    |_|  |_    _   | |  |    |_    _     |   _        |_

 13   6B   33   2D   31   3E   3A   2A   57   72   1B   29   64   0B   23   18
 P    Q    R    S    T    U    V    W    X    Y    Z    [    \    ]    ^    _
 _    _                   _    _    _                   _         _
  |   _|  |_    _    _|  |_|  |_   |_   |_          |  |_   |     _    _    _
     |_|  |_|  |_   |_|  |_   |      |  | |  |     _|  | |  |    | |  | |  |_|

 03   5F   7C   58   5E   7B   71   65   74   10   0E   75   30   55   54   5C
 `    a    b    c    d    e    f    g    h    i    j    k    l    m    n    o
 _    _         _                                  _    _         _    _    _
|_|  |_|   _    _   |_        |_|  |_|   _|  |_|   _   |_         _|        _
|      |  |     _|  |_   |_|        _   | |   _|  |_    _     |   _

 73   67   50   4D   78   1C   62   6A   56   6E   59   69   04   4B   01   41
 p    q    r    s    t    u    v    w    x    y    z    {    |    }    ~   del

> length seq_ix_seq == 128
-}
seq_ix_seq :: Num n => [n]
seq_ix_seq =
  [0x00,0x45,0x34,0x4C,0x16,0x68,0x47,0x25,0x02,0x60,0x3C,0x17,0x46,0x42,0x2B,0x05
  ,0x32,0x26,0x27,0x2C,0x4E,0x1D,0x2E,0x14,0x4A,0x7A,0x1A,0x21,0x12,0x19,0x11,0x28
  ,0x44,0x0A,0x22,0x36,0x49,0x24,0x7E,0x20,0x5A,0x6C,0x63,0x70,0x0C,0x40,0x08,0x52
  ,0x3F,0x06,0x5B,0x4F,0x66,0x6D,0x7D,0x07,0x7F,0x6F,0x09,0x0D,0x61,0x48,0x43,0x53
  ,0x5D,0x77,0x3B,0x39,0x1F,0x79,0x51,0x3D,0x76,0x0F,0x1E,0x35,0x38,0x15,0x37,0x2F
  ,0x13,0x6B,0x33,0x2D,0x31,0x3E,0x3A,0x2A,0x57,0x72,0x1B,0x29,0x64,0x0B,0x23,0x18
  ,0x03,0x5F,0x7C,0x58,0x5E,0x7B,0x71,0x65,0x74,0x10,0x0E,0x75,0x30,0x55,0x54,0x5C
  ,0x73,0x67,0x50,0x4D,0x78,0x1C,0x62,0x6A,0x56,0x6E,0x59,0x69,0x04,0x4B,0x01,0x41]

seq_bitmask_seq :: [Bitseq]
seq_bitmask_seq = map (bitseq_lsb 8) (seq_ix_seq :: [Word8])

seq_bitmask_char :: Char -> Bitseq
seq_bitmask_char = (!!) seq_bitmask_seq . fromEnum

{- | Layout of bits for segment display.

+0000+
5    1
5    1
+6666+
4    2
4    2
+3333+

-}
seg_bit_mask :: Num n => [[[n]]]
seg_bit_mask =
  [[[0,5  ],[0],[0],[0],[0],[0,1  ]]
  ,[[  5  ],[ ],[ ],[ ],[ ],[  1  ]]
  ,[[  5  ],[ ],[ ],[ ],[ ],[  1  ]]
  ,[[4,5,6],[6],[6],[6],[6],[1,2,6]]
  ,[[  4  ],[ ],[ ],[ ],[ ],[  2  ]]
  ,[[  4  ],[ ],[ ],[ ],[ ],[  2  ]]
  ,[[3,4  ],[3],[3],[3],[3],[  2,3]]]

seg_bitarray :: (Num n, Eq n) => [n] -> Bitarray
seg_bitarray bit_set =
  let f = not . null . intersect bit_set
  in ((7,6),map (map f) seg_bit_mask)

seg_char_bitarray :: Char -> Bitarray
seg_char_bitarray = seg_bitarray . (bitseq_elem :: Bitseq -> [Int]) . seq_bitmask_char

seg_char_pbm1 :: Char -> PBM.PBM1
seg_char_pbm1 = PBM.bitarray_pbm1 . seg_char_bitarray

seg_preferred_case :: [Char]
seg_preferred_case = "AbCdEfGhiJkLMnopqrStuvWXyZ"

{-

import Text.Printf {- base -}
wr k = writeFile (printf "/tmp/7-segment-%02X.pbm" k) (seg_char_pbm1 (toEnum k))
mapM_ wr [0 .. 127]

-}
