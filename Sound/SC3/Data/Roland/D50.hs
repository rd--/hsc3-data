-- | Roland Corporation. Roland MIDI Linear Synthesiser Programmer PG-1000 Owner's Manual. Hamamatsu, JP, 1987.
-- <http://cdn.roland.com/assets/media/pdf/PG-1000_OM.pdf>
module Sound.SC3.Data.Roland.D50 where

import Data.Bits {- base -}

bits_pp :: [Bool] -> String
bits_pp = map (\b -> if b then '1' else '0')

gen_bitseq :: FiniteBits b => Int -> b -> [Bool]
gen_bitseq n x = if finiteBitSize x < n then error "gen_bitseq" else map (testBit x) (reverse [0 .. n - 1])

gen_bitseq_pp :: FiniteBits b => Int -> b -> String
gen_bitseq_pp n = bits_pp . gen_bitseq n

-- > gen_bitseq_pp 8 sysex_status == "11110000"
sysex_status :: Int
sysex_status = 0xF0

-- > gen_bitseq_pp 8 sysex_end == "11110111"
sysex_end :: Int
sysex_end = 0xF7

-- > gen_bitseq_pp 8 roland_id == "01000001"
roland_id :: Int
roland_id = 0x41

-- > gen_bitseq_pp 8 d50_id == "00010100"
d50_id :: Int
d50_id = 0x14

-- > gen_bitseq_pp 8 dti_cmd_id
dti_cmd_id :: Int
dti_cmd_id = 0x12

-- | The checksum is a derived from the address (three bytes) and the data bytes (for the D-50 always one byte).
-- <ftp://ftp.monash.edu.au/pub/midi/DOC/Roland-checksum>
--
-- > roland_checksum [0x40,0x00,0x04,0x64] == 0x58
roland_checksum :: (Ord n,Num n) => [n] -> n
roland_checksum =
    let f n w =
            case w of
              [] -> 0x80 - n
              x:w' -> let n' = n + x in f (if n' > 0x80 then n' - 0x80 else n') w'
    in f 0

-- | MSB -> LSB
--
-- > address_to_bytes 128 == (0x00,0x01,0x00)
-- > address_to_bytes 192 == (0x00,0x01,0x40)
-- > address_to_bytes 320 == (0x00,0x02,0x40)
address_to_bytes :: (Num t, Bits t) => t -> (t,t,t)
address_to_bytes a = (shiftR a 14 .&. 0x7F,shiftR a 7 .&. 0x7F,a .&. 0x7F)

-- > address_from_bytes (0x00,0x01,0x00) == 128
-- > address_from_bytes (0x00,0x01,0x40) == 192
-- > address_from_bytes (0x00,0x02,0x40) == 320
address_from_bytes :: (Num t, Bits t) => (t,t,t) -> t
address_from_bytes (p,q,r) = shiftL p 14 .|. shiftL q 7 .|. r

-- > gen_d50_data_set_sysex 0 1 50 == [240,65,0,20,18,0,0,1,50,77,247]
gen_d50_data_set_sysex :: Int -> Int -> Int -> [Int]
gen_d50_data_set_sysex ch a d =
    let (p,q,r) = address_to_bytes a
        chk = roland_checksum [p,q,r,d]
    in [sysex_status,roland_id,ch,d50_id,dti_cmd_id,p,q,r,d,chk,sysex_end]

{-

3.2 Data set (One way) DT1 12H *3—2
( Transmitted and Recognized )

Byte        Description
a 1111 0000 Exclusive status
b 0100 0001 Roland—ID #
c 0000 nnnn Device—ID # = MIDI basic channel. (0 - 15)
d 0001 0100 Model-ID # (D-50)
e 0001 0010 Command-ID # ( DTI )
f 0aaa aaaa Address MSB                 *3—3,5
g 0bbb bbbb Address
h 0ccc cccc Address LSB
i 0ddd dddd Data                        *3-4,5
...
j 0eee eeee Checksum
k 1111 0111 End of System Exclusive

Notes :

*3—3 If aaaaaaa - ccccccc doesn't indicate the address of the
tone parameter or the patch factor, the message will be
ignored.

*3—4 The received value that exceeds the valid range of the parameter
will be ignored.

When the Manual button is pushed, all the parameter values (knob's
positions on the panel) of the Partial, Common and Patch will be
transmitted.

*3-5 See section 4 ( ADDRESS MAPPING OF PARAMETERS AND REMOTE FUNCTION)

4. ADDRESS MAPPING OF PARAMETERS AND REMOTE FUNCTION

4.1 Parameter base address (Top address)

[ 00-00—00 ] Upper Partial 1 (   0 -  53 )
[ 00—00-40 ] Upper Partial 2 (  64 — 117 )
[ 00-01-00 ] Upper Common    ( 128 - 175 )
[ 00—01—40 ] Lower Partial 1 ( 192 - 245 )
[ 00—02—00 ] Lower Partial 2 ( 256 — 309 )
[ 00—02-40 ] Lower Common    ( 320 - 367 )
[ 00-03—00 ] Patch           ( 384 - 420 )

-}

-- | 4.1 Parameter base address (Top address)
d50_parameter_base_address :: Num i => [((i,i,i),String,String)]
d50_parameter_base_address =
    [((0x00,0x00,0x00),"Upper Partial 1","0 - 53")
    ,((0x00,0x00,0x40),"Upper Partial 2","64 - 117")
    ,((0x00,0x01,0x00),"Upper Common","128 - 175")
    ,((0x00,0x01,0x40),"Lower Partial 1","192 - 245")
    ,((0x00,0x02,0x00),"Lower Partial 2","256 - 309")
    ,((0x00,0x02,0x40),"Lower Common","320 - 367")
    ,((0x00,0x03,0x00),"Patch","384 - 420")
    ]

{-

4.2 Patch write address
    (Transmitted only)

Transmitted when the Manual Button is pushed twice while holding the Partial Mute button down.

Address Description

[ 00-20-00 ] Patch write function *4-1

*4-l Transmitted a Data byte consisting of two 0011 (2 bytes).

-}

-- | 4.3 Partial parameters
d50_partial_parameters :: Num i => [(i,String,(i,i),String)]
d50_partial_parameters =
    [(0,"WG Pitch Coarse",(0,72),"C1 - C7")
    ,(1,"WG Pitch Fine",(0,100),"-50 - +50")
    ,(2,"WG Pitch Keyfollow",(0,16),"-1, -1/2, -4/1, 0, 1/8, 1/4, 3/8, 1/2, 5/6, 3/4, 7/8, 1, 5/4, 3/2, 2, s1, s2")
    ,(3,"WG Mod LFO Mode",(0,3),"OFF, (+), (~), A&L")
    ,(4,"WG Mod P-ENV Mode",(0,2),"OFF, (+), (-)")
    ,(5,"WG Mod Bender Mode",(0,2),"OFF, KF, NORMAL")
    ,(6,"WG Waveform",(0,1),"SQU, SAW")
    ,(7,"WG PCM Wave No.",(0,99),"1 - 100")
    ,(8,"WG Pulse Width",(0,100),"0 - 100")
    ,(9,"WG PW Velocity Range",(0,14),"-7 - +7")
    ,(10,"WG PW LFO Select",(0,5),"+1, -1, +2, -2, +3, -3")
    ,(11,"WG PW LFO Depth",(0,100),"0 - 100")
    ,(12,"WG PW Aftertouch Range",(0,14),"-7 - +7")
    ,(13,"TVF Cutoff Frequency",(0,100),"0 - 100")
    ,(14,"TVF Resonance",(0,30),"0 - 30")
    ,(15,"TVF Keyfollow",(0,14),"-1, -1/2, -4/1, 0, 1/8, 1/4, 3/8 ,1/2, 5/8, 3/4, 7/8, 1, 5/4, 3/2, 2")
    ,(16,"TVF Bias Point/Direction",(0,127),"<A1 - <C7, >A1 - >C7")
    ,(17,"TVF Bias Level",(0,14),"-7 - +7")
    ,(18,"TVF ENV Depth",(0,100),"0 - 100")
    ,(19,"TVF ENV Velocity Range",(0,100),"0 - 100")
    ,(20,"TVF ENV Depth Keyfollow",(0,4),"0 - 4")
    ,(21,"TVF ENV Time Keyfollow",(0,10),"0 - 10")
    ,(22,"TVF ENV Time 1",(0,100),"0 - 100")
    ,(23,"TVF ENV Time 2",(0,100),"0 - 100")
    ,(24,"TVF ENV Time 3",(0,100),"0 - 100")
    ,(25,"TVF ENV Time 4",(0,100),"0 - 100")
    ,(26,"TVF ENV Time 5",(0,100),"0 - 100")
    ,(27,"TVF ENV Level 1",(0,100),"0 - 100")
    ,(28,"TVF ENV Level 2",(0,100),"0 - 100")
    ,(29,"TVF ENV Level 3",(0,100),"0 - 100")
    ,(30,"TVF ENV Sustain Level",(0,100),"0 - 100")
    ,(31,"TVF ENV End Level",(0,1),"0,100")
    ,(32,"TVF Mod LFO Select",(0,5),"+1, -1, +2, -2, +3, -3")
    ,(33,"TVF Mod LFO Depth",(0,100),"0 - 100")
    ,(34,"TVF Mod Aftertouch Range",(0,14),"-7 - +7")
    ,(35,"TVA Level",(0,100),"0 - 100")
    ,(36,"TVA Velocity Range",(0,100),"-50 - +50")
    ,(37,"TVA Bias Point Direction",(0,127),"<A1 - <C7, >A1 - >C7")
    ,(38,"TVA Bias Level",(0,12),"-12 - 0")
    ,(39,"TVA ENV Time 1",(0,100),"0 - 100")
    ,(40,"TVA ENV Time 2",(0,100),"0 - 100")
    ,(41,"TVA ENV Time 3",(0,100),"0 - 100")
    ,(42,"TVA ENV Time 4",(0,100),"0 - 100")
    ,(43,"TVA ENV Time 5",(0,100),"0 - 100")
    ,(44,"TVA ENV Level 1",(0,100),"0 - 100")
    ,(45,"TVA ENV Level 2",(0,100),"0 - 100")
    ,(46,"TVA ENV Level 3",(0,100),"0 - 100")
    ,(47,"TVA ENV Sustain Level",(0,100),"0 - 100")
    ,(48,"TVA ENV End Level",(0,1),"0,100")
    ,(49,"TVA ENV T1 Velo Follow",(0,4),"0 - 4")
    ,(50,"TVA ENV Time Keyfollow",(0,4),"0 - 4")
    ,(51,"TVA Mod LFO Select",(0,5),"+1, -1, +2, -2, +3, -3")
    ,(52,"TVA Mod LFO Depth",(0,100),"0 - 100")
    ,(53,"TVA Mod Aftertouch Range",(0,14),"-7 - +7")
    ]

-- | 4.4 Common parameters
d50_common_parameters :: Num i => [(i,String,(i,i),String)]
d50_common_parameters =
    [(10,"Structure No.",(0,6),"1 - 7")
    ,(11,"P-ENV Velocity Range",(0,2),"0 - 2")
    ,(12,"P-ENV Time Keyfollow",(0,4),"0 - 4")
    ,(13,"P-ENV Time 1",(0,50),"0 - 50")
    ,(14,"P-ENV Time 2",(0,50),"0 - 50")
    ,(15,"P-ENV Time 3",(0,50),"0 - 50")
    ,(16,"P-ENV Time 4",(0,50),"0 - 50")
    ,(17,"P-ENV Level 0",(0,100),"-50 - +50")
    ,(18,"P-ENV Level ]",(0,100),"-50 - +50")
    ,(19,"P-ENV Level 2",(0,100),"-50 - +50")
    ,(20,"P-ENV Sustain Level",(0,100),"-50 - +50")
    ,(21,"P-ENV End Level",(0,100),"-50 - +50")
    ,(22,"Pitch Mod LFO Depth",(0,100),"0 - 100")
    ,(23,"Pitch Mod Lever",(0,100),"0 - 100")
    ,(24,"Pitch Mod Aftertouch",(0,100),"0 - 100")
    ,(25,"LFO-1 Waveform",(0,3),"TRI, SAW, SQU, RND")
    ,(26,"LFO-1 Rate",(0,100),"0 - 100")
    ,(27,"LFO-1 Delay Time",(0,100),"0 - 100")
    ,(28,"LFO-1 Sync",(0,2),"OFF, ON, KEY")
    ,(29,"LFO-2 Waveform",(0,3),"TRI, SAW, SQU, RND")
    ,(30,"LFO-2 Rate",(0,100),"0 - 100")
    ,(31,"LFO-2 Delay Time",(0,100),"0 - 100")
    ,(32,"LFO-2 Sync",(0,1),"OFF, ON")
    ,(33,"LFO-3 Waveform",(0,3),"TRI, SAW, SQU, RND")
    ,(34,"LFO-3 Rate",(0,100),"0 - 100")
    ,(35,"LFO-3 Delay Time",(0,100),"0 - 100")
    ,(36,"LFO-S Sync",(0,1),"OFF, ON")
    ,(37,"Low EQ Frequency",(0,15),"63,75,88,105,125,150,175,210,250,300,350,420,500,600,700,840")
    ,(38,"Low EQ Gain",(0,24),"-12 - +12")
    ,(39,"High EQ Frequency",(0,21),"250,300,350,420,500,600,700,840,1.0,1.2,1.4,1.7,2.0,2.4,2.8,3.4,4.0,4.8,5.7,6.7,8.0,9.5")
    ,(40,"High EQ Q",(0,8),"0.3,0.5,0.7,1.0,1.4,2.0,3.0,4.2,6.0")
    ,(41,"High EQ Gain",(0,24),"-12 - +12")
    ,(42,"Chorus Type",(0,7),"1 - 8")
    ,(43,"Chorus Rate",(0,100),"0 - 100")
    ,(44,"Chorus Depth",(0,100),"0 - 100")
    ,(45,"Chorus Balance",(0,100),"0 - 100")
    ,(46,"Partial Mute",(0,3),"MM,SM,MS,SS")
    ,(47,"Partial Balance",(0,100),"0 - 100")
     ]

-- | 4.5 Patch Factors
d50_patch_factors :: Num i => [(i,String,(i,i),String)]
d50_patch_factors =
    [(20,"Portamento Mode",(0,2),"U, L, UL")
    ,(21,"Hold Mode",(0,2),"U, L, UL")
    ,(22,"Upper Tone Key Shift",(0,48),"-24 - +24")
    ,(23,"Lower Tone Key Shift",(0,48),"-24 - +24")
    ,(24,"Upper Tone Fine Tune",(0,100),"-50 - +50")
    ,(25,"Lower Tone Fine Tune",(0,100),"-50 - +50")
    ,(26,"Bender Range",(0,12),"0 - 12")
    ,(27,"After Bend Range",(0,24),"-12 - +12")
    ,(28,"Portamento Time",(0,100),"0 - 100")
    ,(29,"Output Mode",(0,3),"1 - 4")
    ,(30,"Reverb Type",(0,31),"1 - 32")
    ,(31,"Reverb Balance",(0,100),"0 - 100")
    ,(32,"Total Volume",(0,100),"0 - 100")
    ,(33,"Tone Balance",(0,100),"0 - 100")
    ,(34,"Chase Mode",(0,2),"UL, ULL, ULU")
    ,(35,"Chase Level",(0,100),"0 - 100")
    ,(36,"Chase Time",(0,100),"0 - 100")
    ]
