module Sound.SC3.Data.Roland.D50 where

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
g 0bb  bbbb Address
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

Address      Description

[ 00-00-00 ] Upper Partial 1 ( 0 - 53 )
[ 00-00-40 ] Upper Partial 2 ( 64 - 117 )
[ 00-01-00 ] Upper Common    ( 128 - 175)
[ 00-01-40 ] Lower Partial I ( 192 - 245 )
[ 00-02-00 ] Lower Partial 2 ( 256 - 309 )
[ 00-02-40 ] Lower Common    ( 320 - 367 )
[ 00-03-00 ] Patch           ( 384 - 420 )

4.2 Patch write address
    (Transmitted only)

Transmitted when the Manual Button is pushed twice while holding the Partial Mute button down.

Address Description

[ 00-20-00 ] Patch write function *4-1

*4-l Transmitted a Data byte consisting of two 0011 (2 bytes).

4.3 Partial parameters
( Parameter address = Base address + Offset )

Offset Function Value

-}

d50_partial_parameters :: [(Int,String,(Int,Int),String)]
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
    ]

{-

TVA Level
TVA Velocity Range
TVA Bias Point Direction
TVA Bias Level
TVA ENV Time 1
TVA ENV Time 2
TVA ENV Time 3
TVA ENV Time 4
TVA ENV Time 5
TVA ENV Level 1
TVA ENV Level 2
TVA ENV Level 3
TVA ENV Sustain Level
TVA ENV End Level
TVA ENV T1 Velo Follow
TVA ENV Time Keyfollow
TVA Mod LFO Select
TVA Mod LFO Depth
TVA Mod Aftertouch Range

-}
