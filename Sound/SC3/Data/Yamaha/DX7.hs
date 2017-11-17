-- | Yamaha DX7
--
-- <http://homepages.abdn.ac.uk/mth192/pages/dx7/sysex-format.txt>
-- <https://sourceforge.net/u/tedfelix/dx7dump/ci/master/tree/dx7dump.cpp>
module Sound.SC3.Data.Yamaha.DX7 where

import Control.Monad {- base -}
import qualified Data.ByteString as Char8 {- bytestring -}
import Data.List {- base -}
import Data.Maybe {- base -}
import Data.Word {- base -}
import Safe {- safe -}
import qualified System.Process as Process {- process -}
import Text.Printf {- base -}

import qualified Data.List.Split as Split {- split -}
import qualified Music.Theory.Byte as Byte {- hmt -}

-- | Parameter values are at most in 0-99.
type U8 = Int

u8_to_word8 :: U8 -> Word8
u8_to_word8 = fromIntegral

-- | Voice data (# = 155 = 21 * 6 + 29)
type DX7_Voice = [U8]

-- | Yamaha manufacturer ID.
--
-- > import Music.Theory.Bits {- hmt -}
-- > gen_bitseq_pp 8 yamaha_id == "01000011"
--
-- > :set -XBinaryLiterals
-- > (yamaha_id == 67,yamaha_id == 0x43,yamaha_id == 0b01000011)
yamaha_id :: U8
yamaha_id = 0x43

usr_str_tbl :: [(String,String)]
usr_str_tbl =
    [("BOOL","OFF;ON")
    ,("CURVE","-LIN;-EXP;+EXP;+LIN")
    ,("LFO-WAVE","TRIANGLE;SAWTOOTH-DOWN;SAWTOOTH-UP;SQUARE;SINE;SAMPLE-AND-HOLD")
    ,("MODE","RATIO;FIXED")]

-- | (DX7-IX,NAME,STEPS,USR_DIFF,USR_STR)
type DX7_Parameter = (U8,String,U8,Int,String)

dx7_parameter_ix :: DX7_Parameter -> Int
dx7_parameter_ix (n,_,_,_,_) = n

dx7_parameter_nm :: DX7_Parameter -> String
dx7_parameter_nm (_,nm,_,_,_) = nm

dx7_parameter_range :: DX7_Parameter -> (Int,Int)
dx7_parameter_range (_,_,n,_,_) = (0,n - 1)

dx7_parameter_range_usr :: DX7_Parameter -> (Int,Int)
dx7_parameter_range_usr (_,_,n,d,_) = (d,d + n - 1)

-- | Normalise parameter value to be in (0,1).
--
-- > let p = dx7_op_parameter_tbl !! 20
-- > map (dx7_parameter_value_normalise p) [0 .. 14]
dx7_parameter_value_normalise :: DX7_Parameter -> Int -> Float
dx7_parameter_value_normalise (_,_,n,_,_) x = fromIntegral x / fromIntegral (n - 1)

dx7_op_nparam :: Int
dx7_op_nparam = 21

-- | Template for six FM operators.
--
-- > length dx7_op_parameter_tbl == dx7_op_nparam
dx7_op_parameter_tbl :: [DX7_Parameter]
dx7_op_parameter_tbl =
    [(00,"EG RATE 1",100,0,"")
    ,(01,"EG RATE 2",100,0,"")
    ,(02,"EG RATE 3",100,0,"")
    ,(03,"EG RATE 4",100,0,"")
    ,(04,"EG LEVEL 1",100,0,"")
    ,(05,"EG LEVEL 2",100,0,"")
    ,(06,"EG LEVEL 3",100,0,"")
    ,(07,"EG LEVEL 4",100,0,"")
    ,(08,"KBD LEV SCL BRK PT",100,0,"")
    ,(09,"KBD LEV SCL LFT DEPTH",100,0,"")
    ,(10,"KBD LEV SCL RHT DEPTH",100,0,"")
    ,(11,"KBD LEV SCL LFT CURVE",4,0,"-LIN;-EXP;+EXP;+LIN")
    ,(12,"KBD LEV SCL RHT CURVE",4,0,"-LIN;-EXP;+EXP;+LIN")
    ,(13,"KBD RATE SCALING",8,0,"")
    ,(14,"AMP MOD SENSITIVITY",4,0,"")
    ,(15,"KEY VEL SENSITIVITY",8,0,"")
    ,(16,"OPERATOR OUTPUT LEVEL",100,0,"")
    ,(17,"OSC MODE",2,0,"RATIO;FIXED")
    ,(18,"OSC FREQ COARSE",32,0,"")
    ,(19,"OSC FREQ FINE",100,0,"")
    ,(20,"OSC DETUNE",15,-7,"-7 - +7")
    ]

-- | Rewrite 'dx7_op_parameter_tbl' for operator /n/.
dx7_rewrite_op_dx7_parameter_tbl :: Int -> [DX7_Parameter]
dx7_rewrite_op_dx7_parameter_tbl n =
    let n' = 6 - n
        f (ix,nm,stp,usr_diff,usr_str) =
            let ix' = ix + (21 * n')
                nm' = "OP " ++ show n ++ " " ++ nm
            in (ix',nm',stp,usr_diff,usr_str)
    in map f dx7_op_parameter_tbl

operator_group_structure :: [(String,String,[U8])]
operator_group_structure =
    [("EG RATE","1;2;3;4",[0..3])
    ,("EG LEVEL","1;2;3;4",[4..7])
    ,("KBD LEV SCL","BRK-PT;LFT-DEPTH;RHT-DEPTH;RHT-CURVE",[8..12])
    ,("OSC","MODE;FREQ-COARSE;FREQ-FINE;DETUNE",[17..20])
    ]

-- | Six operators, descending order, one-indexed.
--
-- > length dx7_op6_dx7_parameter_tbl == 6 * dx7_op_nparam
dx7_op6_dx7_parameter_tbl :: [DX7_Parameter]
dx7_op6_dx7_parameter_tbl = concatMap dx7_rewrite_op_dx7_parameter_tbl [6,5 .. 1]

-- | Number of shared (non-operator) parameters.
dx7_sh_nparam :: Int
dx7_sh_nparam = 19

-- | Remainder (non-operator) of parameter table.
--
-- > length dx7_sh_parameter_tbl == dx7_sh_nparam
dx7_sh_parameter_tbl :: [DX7_Parameter]
dx7_sh_parameter_tbl =
    [(126,"PITCH EG RATE 1",100,0,"")
    ,(127,"PITCH EG RATE 2",100,0,"")
    ,(128,"PITCH EG RATE 3",100,0,"")
    ,(129,"PITCH EG RATE 4",100,0,"")
    ,(130,"PITCH EG LEVEL 1",100,0,"")
    ,(131,"PITCH EG LEVEL 2",100,0,"")
    ,(132,"PITCH EG LEVEL 3",100,0,"")
    ,(133,"PITCH EG LEVEL 4",100,0,"")
    ,(134,"ALGORITHM #",32,1,"")
    ,(135,"FEEDBACK",8,0,"")
    ,(136,"OSCILLATOR SYNC",2,0,"OFF;ON")
    ,(137,"LFO SPEED",100,0,"")
    ,(138,"LFO DELAY",100,0,"")
    ,(139,"LFO PITCH MOD DEPTH",100,0,"")
    ,(140,"LFO AMP MOD DEPTH",100,0,"")
    ,(141,"LFO SYNC",2,0,"OFF;ON")
    ,(142,"LFO WAVEFORM",6,0,"TR;SD;SU;SQ;SI;SH")
    ,(143,"PITCH MOD SENSITIVITY",8,0,"")
    ,(144,"TRANSPOSE",49,0,"12=C2")]

-- | Number of bytes for voice name.
dx7_name_nchar :: Int
dx7_name_nchar = 10

dx7_name_dx7_parameter_tbl :: [DX7_Parameter]
dx7_name_dx7_parameter_tbl =
    [(145,"VOICE NAME CHAR 01",128,0,"ASCII")
    ,(146,"VOICE NAME CHAR 02",128,0,"ASCII")
    ,(147,"VOICE NAME CHAR 03",128,0,"ASCII")
    ,(148,"VOICE NAME CHAR 04",128,0,"ASCII")
    ,(149,"VOICE NAME CHAR 05",128,0,"ASCII")
    ,(150,"VOICE NAME CHAR 06",128,0,"ASCII")
    ,(151,"VOICE NAME CHAR 07",128,0,"ASCII")
    ,(152,"VOICE NAME CHAR 08",128,0,"ASCII")
    ,(153,"VOICE NAME CHAR 09",128,0,"ASCII")
    ,(154,"VOICE NAME CHAR 10",128,0,"ASCII")]

-- | NOT STORED IN VOICE DATA
dx7_opstatus_param :: DX7_Parameter
dx7_opstatus_param = (155,"OPERATOR ON/OFF",2,0,"BIT5=OP1 - BIT0=OP6")

rem_group_structure :: [(String,String,[U8])]
rem_group_structure =
    [("PITCH EG RATE","1;2;3;4",[126..129])
    ,("PITCH EG LEVEL","1;2;3;4",[130..133])
    ,("LFO","SPEED;DELAY;PITCH-MOD-DEPTH;AMP-MOD-DEPTH;SYNC;WAVEFORM",[137..142])
    ,("VOICE NAME CHAR",";;;;;;;;;",[145..154])]

dx7_nparam :: Int
dx7_nparam = 6 * dx7_op_nparam + dx7_sh_nparam + dx7_name_nchar

-- | All DX7 parameters.
--
-- > length dx7_parameter_tbl == dx7_nparam
dx7_parameter_tbl :: [DX7_Parameter]
dx7_parameter_tbl =
  concat [dx7_op6_dx7_parameter_tbl
         ,dx7_sh_parameter_tbl
         ,dx7_name_dx7_parameter_tbl]

-- | Lookup parameter name given index.
--
-- > dx7_parameter_name 134 ==  "ALGORITHM #"
dx7_parameter_name :: U8 -> String
dx7_parameter_name n =
    fromMaybe (error "dx7_parameter_name") $
    fmap dx7_parameter_nm $
    find ((== n) . dx7_parameter_ix) dx7_parameter_tbl

-- | Lookup parameter index given name.
--
-- > dx7_parameter_index "ALGORITHM #" == 134
dx7_parameter_index :: String -> U8
dx7_parameter_index nm =
    fromMaybe (error "dx7_parameter_ix") $
    fmap dx7_parameter_ix $
    find ((== nm) . dx7_parameter_nm) dx7_parameter_tbl

-- | Pretty print value give 'DX7_Parameter'.
dx7_parameter_value_pp :: DX7_Parameter -> U8 -> String
dx7_parameter_value_pp p x =
  let (_,_,stp,d,u) = p
      (x_clip,err) = if x >= stp
                     then (stp - 1,printf "(ERROR BYTE=0x%02X)" x)
                     else (x,"")
      r = if u == "ASCII"
          then ['\'',toEnum x_clip,'\'']
          else case Split.splitOn ";" u of
                 [_] -> printf "%02d" (x_clip + d)
                 e -> Safe.at e x_clip
  in r ++ err

dx7_parameter_pp :: Bool -> DX7_Parameter -> U8 -> String
dx7_parameter_pp with_ix p x =
  let (ix,nm,_,_,_) = p
  in if with_ix
     then printf "%03d: %s = %s" ix nm (dx7_parameter_value_pp p x)
     else printf "%s = %s" nm (dx7_parameter_value_pp p x)

dx7_parameter_set_pp :: DX7_Parameter -> [U8] -> String
dx7_parameter_set_pp p x =
  let (_,nm,_,_,_) = p
  in printf "%s = %s" nm (intercalate "," (map (dx7_parameter_value_pp p) x))

-- | Print complete parameter sequence.
dx7_parameter_seq_pp :: DX7_Voice -> [String]
dx7_parameter_seq_pp = zipWith (dx7_parameter_pp True) dx7_parameter_tbl

dx7_voice_pp :: DX7_Voice -> [String]
dx7_voice_pp p =
  let p_grp = Split.splitPlaces (replicate 6 (21::Int) ++ [19,10,1]) p
  in concat [zipWith (dx7_parameter_pp False) dx7_sh_parameter_tbl (Safe.at p_grp 6)
            ,zipWith dx7_parameter_set_pp dx7_op_parameter_tbl (transpose (take 6 p_grp))]

dx7_function_parameters_tbl :: [DX7_Parameter]
dx7_function_parameters_tbl =
    [(64,"MODE CHANGE",2,0,"POLY;MONO")
    ,(65,"PITCH BEND RANGE",13,0,"")
    ,(66,"PITCH BEND STEP",13,0,"")
    ,(67,"PORTAMENTO MODE ",2,0,"RETAIN;FOLLOW")
    ,(68,"PORTAMENTO GLISS",2,0,"")
    ,(69,"PORTAMENTO TIME ",100,0,"")
    ,(70,"MOD WHEEL RANGE ",100,0,"")
    ,(71,"MOD WHEEL ASSIGN",8,0,"PITCH;AMP;EG-BIAS")
    ,(72,"FOOT CONTROL RANGE",100,0,"")
    ,(73,"FOOT CONTROL ASSIGN",8,0,"")
    ,(74,"BREATH CONT RANGE",100,0,"")
    ,(75,"BREATH CONT ASSIGN",8,0,"")
    ,(76,"AFTERTOUCH RANGE",100,0,"")
    ,(77,"AFTERTOUCH ASSIGN",8,0,"")]

-- * Voice

dx7_voice_name :: DX7_Voice -> String
dx7_voice_name v = map (toEnum . Safe.at v) [145 .. 154]

-- * DX7 VOICE DATA LIST

-- | Arrangement of parameters on printed DX7 VOICE DATA LIST.
--
-- > sort (concatMap (\(_,_,ix) -> ix) dx7_voice_data_list) == [0 .. 20] ++ [126 .. 144]
dx7_voice_data_list :: [(String, [String], [U8])]
dx7_voice_data_list =
  [(""
   ,["ALGORITHM"],[134])
  ,(""
   ,["FEEDBACK"],[135])
  ,("LFO"
   ,["WAVE","SPEED","DELAY","PMD","AMD","SYNC"],[142,137,138,139,140,141])
  ,("MOD SENSITIVITY"
   ,["PITCH","AMPLITUDE"],[143,14])
  ,("OSCILLATOR"
   ,["MOD","SYNC","FREQ. COARSE","FREQ. FINE","DETUNE"],17:136:[18 .. 20])
  ,("EG"
   ,["R1","R2","R3","R4","L1","L2","L3","L4"],[0 .. 7])
  ,("KEYBOARD LEVEL SCALING"
   ,["BREAK POINT","CURVE L","CURVE R","DEPTH L","DEPTH R"],[8,11,12,9,10])
  ,("",["K.BOARD RATE SCALING"],[13])
  ,("OPERATOR"
   ,["OUTPUT LEVEL","VELO SENS"],[16,15])
  ,("PITCH EG"
   ,["R1","R2","R3","R4","L1","L2","L3","L4"],[126 .. 133])
  ,(""
   ,["KEY TRANSPOSE"],[144])]

-- | Plain text voice data list.
--
-- > putStrLn$unlines$ dx7_voice_data_list_pp (replicate 155 0)
dx7_voice_data_list_pp :: DX7_Voice -> [String]
dx7_voice_data_list_pp d =
  let op_ix_set n = [n, n + 21 .. n + 21 * 5]
      op_ix_pp n = map
                   (dx7_parameter_value_pp (Safe.at dx7_op_parameter_tbl n))
                   (map (Safe.at d) (op_ix_set n))
      is_op_ix n = n < 126
      ix_val n =
        if is_op_ix n
        then intercalate "," (op_ix_pp n)
        else dx7_parameter_value_pp (Safe.at dx7_sh_parameter_tbl (n - 126)) (Safe.at d n)
      pp z nm ix = concat [z,nm,"=",ix_val ix]
      f (grp,nm,ix) =
        if null grp
        then zipWith (pp "") nm ix
        else grp : zipWith (pp "  ") nm ix
      vc_nm = "NAME=" ++ dx7_voice_name d
  in vc_nm : concatMap f dx7_voice_data_list

-- * DX7 ALGORITHM

-- | Algorithm given as a list of (dst,src) operator edges and a list of output operators.
--   Operators are zero-indexed.
type DX7_Algorithm = ([(Int,Int)],[Int])

-- | The 32 DX7 algorithms in sequence.
dx7_algorithms :: [DX7_Algorithm]
dx7_algorithms =
  [([(0,1),(2,3),(3,4),(4,5),(5,5)],[0,2]) -- 1
  ,([(0,1),(1,1),(2,3),(3,4),(4,5)],[0,2])
  ,([(0,1),(1,2),(3,4),(4,5),(5,5)],[0,3])
  ,([(0,1),(1,2),(3,4),(4,5),(5,3)],[0,3])
  ,([(0,1),(2,3),(4,5),(5,5)],[0,2,4]) -- 5
  ,([(0,1),(2,3),(4,5),(5,4)],[0,2,4])
  ,([(0,1),(2,3),(2,4),(4,5),(5,5)],[0,2])
  ,([(0,1),(2,3),(2,4),(4,5),(3,3)],[0,2])
  ,([(0,1),(2,3),(2,4),(4,5),(1,1)],[0,2])
  ,([(0,1),(1,2),(3,4),(3,5),(2,2)],[0,3]) -- 10
  ,([(0,1),(1,2),(3,4),(3,5),(5,5)],[0,3])
  ,([(0,1),(2,3),(2,4),(2,5),(1,1)],[0,2]) -- 12
  ,([(0,1),(2,3),(2,4),(2,5),(5,5)],[0,2])
  ,([(0,1),(2,3),(3,4),(3,5),(5,5)],[0,2]) -- 14
  ,([(0,1),(2,3),(3,4),(3,5),(1,1)],[0,2])
  ,([(0,1),(0,2),(0,4),(2,3),(4,5),(5,5)],[0])
  ,([(0,1),(0,2),(0,4),(2,3),(4,5),(1,1)],[0])
  ,([(0,1),(0,2),(0,3),(3,4),(4,5),(2,2)],[0])
  ,([(0,1),(1,2),(3,5),(4,5),(5,5)],[0,3,4])
  ,([(0,2),(1,2),(3,4),(3,5),(2,2)],[0,1,3])
  ,([(0,2),(1,2),(3,5),(4,5),(2,2)],[0,1,3,4])
  ,([(0,1),(2,5),(3,5),(4,5),(5,5)],[0,2,3,4])
  ,([(1,2),(3,5),(4,5),(5,5)],[0,1,3,4])
  ,([(2,5),(3,5),(4,5),(5,5)],[0,1,2,3,4])
  ,([(3,5),(4,5),(5,5)],[0,1,2,3,4])
  ,([(1,2),(3,4),(3,5),(5,5)],[0,1,3])
  ,([(1,2),(3,4),(3,5),(2,2)],[0,1,3])
  ,([(0,1),(2,3),(3,4),(4,4)],[0,2,5])
  ,([(2,3),(4,5),(5,5)],[0,1,2,4])
  ,([(2,3),(3,4),(4,4)],[0,1,2,5])
  ,([(4,5),(5,5)],[0,1,2,3,4])
  ,([(5,5)],[0,1,2,3,4,5])]

{- | Simple dot graph of algorithm.

> let ad = unlines . dx7_algorithm_dot . (!!) dx7_algorithms
> let wr k = writeFile (printf "/tmp/dx7.%02d.dot" k) (ad k)
> mapM_ wr [0 .. 31]

-}
dx7_algorithm_dot :: DX7_Algorithm -> [String]
dx7_algorithm_dot (e,o) =
  let n_f k = printf "%d [shape=square,label=%d];" k (k + 1)
      e_f (dst,src) = printf
                      "%d -> %d [color=%s,constraint=%s];"
                      src
                      dst
                      (if src > dst then "black" else "slategray")
                      (if src > dst then "true" else "false") -- orangered
      o_f k = printf "%d -> o;" k
  in concat
     [["digraph g {"
      ,"graph [layout=dot,splines=ortho];" -- polyline ortho line
      ,"node [style=solid,color=black];"
      ,"edge [arrowhead=dot,arrowsize=0.35];"]
     ,map n_f [0::Int .. 5]
     ,["o [shape=\"circle\",label=\"·\"];"]
     ,map e_f e
     ,map o_f o
     ,["}"]]

-- * SYSEX IO

-- | Read unpacked 4960 parameter sequence from text sysex file (see cmd/dx7-unpack.c).
dx7_load_sysex_hex :: FilePath -> IO [DX7_Voice]
dx7_load_sysex_hex fn = do
  d <- Byte.load_hex_byte_seq fn
  when (length d /= 4960) (error "dx7_load_sysex_hex")
  return (Split.chunksOf 155 d)

{- | Read unpacked 4960 parameter sequence from binary sysex file (see cmd/dx7-unpack.c).

> d <- dx7_load_sysex "/home/rohan/sw/hsc3-data/data/yamaha/dx7/ROM1A.syx"
> dx7_store_sysex_hex "/home/rohan/sw/hsc3-data/data/yamaha/dx7/ROM1A.syx.text" d

-}
dx7_store_sysex_hex :: FilePath -> [DX7_Voice] -> IO ()
dx7_store_sysex_hex fn b = do
  let d = concat b
  when (length d /= 4960) (error "dx7_store_sysex_hex")
  Byte.store_hex_byte_seq fn d

{- | Read unpacked 4960 parameter sequence from binary sysex file (see cmd/dx7-unpack.c).

> d <- dx7_load_sysex "/home/rohan/sw/hsc3-data/data/yamaha/dx7/ROM1A.syx"
> mapM_ (putStrLn . dx7_voice_name) d
> mapM_ (putStrLn . unlines . dx7_parameter_seq_pp) d
> mapM_ (putStrLn . unlines . dx7_voice_pp) d
> mapM_ (putStrLn . unlines . dx7_voice_data_list_pp) d

> t <- dx7_load_sysex_hex "/home/rohan/sw/hsc3-data/data/yamaha/dx7/ROM1A.syx.text"
> (length d,length t,d == t)

-}
dx7_load_sysex :: String -> IO [DX7_Voice]
dx7_load_sysex fn = do
  s <- Process.readProcess "hsc3-dx7-unpack" ["unpack","binary","text",fn] ""
  return (Split.chunksOf 155 (Byte.read_hex_byte_seq s))

{- | Write DX7 sysex file.

> let fn = "/home/rohan/sw/hsc3-data/data/yamaha/dx7/ROM1A.syx"
> d <- dx7_load_sysex fn
> dx7_store_sysex "/tmp/ROM1A.syx" d
> Process.rawSystem "cmp" ["-l",fn,"/tmp/ROM1A.syx"]

-}
dx7_store_sysex :: FilePath -> [DX7_Voice] -> IO ()
dx7_store_sysex fn b = do
  let d = concat b
      d_str = Byte.byte_seq_hex_pp d ++ "\n"
  when (length d /= 4960) (error "dx7_store_sysex")
  syx <- Process.readProcess "hsc3-dx7-unpack" ["repack","text","text"] d_str
  Char8.writeFile fn (Char8.pack (map u8_to_word8 (Byte.read_hex_byte_seq syx)))
