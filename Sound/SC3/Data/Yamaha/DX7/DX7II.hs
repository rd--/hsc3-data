-- | Yamaha DX7II
module Sound.SC3.Data.Yamaha.DX7.DX7II where

-- * 5-1. Voice Parameter (VCED FORMAT) - Add-8

{-

  0  R1     EG RATE1                         99
  1  R2     EG RATE2                         99
  2  R3     EG RATE3                         99
  3  R4     EG RATE4                         99
  4  L1     EG LEVEL1                        99
  5  L2     EG LEVEL2                        99
  6  L3     EG LEVEL3                        99
  7  L4     EG LEVEL4                        00
  8  BP     BREAK POINT                      39
  9  LD     LEFT DEPTH                        0
 10  RD     RIGHT DEPTH                       0
 11  LC     LEFT CURVE                        0
 12  RC     RIGHT CURVE                       0
 13  RS     RATE SCALING                      0
 14  AMS    MODULATION SENSITIVITY            0
 15  TS     TOUCH SENSITIVITY                 0
 16  TL     TOTAL LEVEL                       0 OP1:99
 17  PM     FREQUENCY MODE                    0
 18  PC     FREQUENCY COURSE                  1
 19  PF     FREQUENCY FINE                    0
 20  PD     DETUNE                            7

126  PR1    PEG RATE1                        99
127  PR2    PEG RATE2                        99
128  PR3    PEG RATE3                        99
129  PR4    PEG RATE4                        99
130  PL1    PEG LEVEL1                       50
131  PL2    PEG LEVEL2                       50
132  PL3    PEG LEVEL3                       50
133  PL4    PEG LEVEL4                       50
134  ALS    ALGORITHM SELECTOR                0
135  FBL    FEED BACK LEVEL                   0
136  OPI    OSC.PHASE INIT                    1
137  LFS    LFO SPEED                        35
138  LFD    LFO DELAY TIME                    0
139  LPMD   PITCH MODULATION DEPTH            0
140  LAMD   AMPLITUDE MODULATION DEPTH        0
141  LFKS   LFO KEY SYNC                      1
142  LFW    LFO WAVE                          0
143  LPMS   LFO PITCH MODULATION SENSITIVITY  3
144  TRNP   TRANSPOSE                        24

155  OPE    OPERATOR ENABLE B5:0P1,--,B0:OP6  -
156  OPSEL  OPERATOR SELECT 0:OP1,--,5:OP6    -

-}

-- * 5-2. Additional Parameters (ACED format)

{-

  0        OP6 scaling mode normal/fraction
  1        OP5 scaling mode normal/fraction
  2        OP4 scaling mode normal/fraction
  3        OP3 scaling mode normal/fraction
  4        OP2 scaling mode normal/fraction
  5        OP1 scaling mode norma!/fraction
  6        OP6 amplitude modulation sensitivity
  7        OP5 amplitude modulation sensitivity
  8        OP4 amplitude modulation sensitivity
  9        OP3 amplitude modulation sensitivity
 10        OP2 amplitude modulation sensitivity
 11        OP1 amplitude modulation sensitivity
 12        pitch EG range 8va/4va/lva/1/2va
 13        LFO key trigger (delay) single/multi
 14        pitch EG by velocity switch off/on:0/1
 15  PMOD
 16        pitch bend range
 17                   step
 18                   mode low/high/k.on
 19  RNDP
 20        portamento mode rtn/fllw fngrd/flltm
 21                   step
 22                   time
 23        modulation wheel pitch mod range
 24                         amplitude mod range
 25                         EG bias range
 26        foot controler 1 pitch mod range
 27                       amplitude mod range
 28                       EG bias range
 29                       volume range
 30        breath controler pitch mod range
 31                         amplitude mod range
 32                         EG bias range
 33                         pitch bias range
 34        after touch pitch mod range
 35                    amplitude mod range
 36                    EG bias range
 37                    pitch bias range
 38  PGRS

 64        pitch mod. range
 65        amp   mod. range
 66        EG    bias range
 67        volume     range
 68        pitch mod. range
 69        amp   mod. range
 70        EG    bias range
 71        volume     range
 72  UDTN
 73        foot cntl.1 use as CS1 switch off/on:0/1

-}

-- * 5-3. PERFORMANCE Parameters (PCED, PMEM format)

{-

 0  PLMD    0-2       SINGLE/DUAL/SPLIT
 1  VNMA    0-127     A-CH VOICE NUMBER
 2  VNMB    0-127     B-CH VOICE NUMBER
 3  MCTB    0-74      MICRO TUNING TABLE SELECT
 4  MCKY    0-11      MICRO TUNING KEY
 5  MCSW    0-3       MICRO TUNING SWITCH BIT0:A,BIT1:B 0/1:0FF/ON
 6  DDTN    0-7       DUAL DETUNE
 7  SPPT    0-127     SPLIT POINT
 8  FDMP    0-1       EG FORCED DAMPING SWITCH 0/1:0FF/ON
 9  SFSW    0-3       SUSTAIN FOOT SWITCH BIT0:A,BITLB 0/1:0FF/ON
10  FSAS    0-3       FOOT SWITCH ASSIGN 0:SUS,1:POR,2:KHLD,3:SFT
11  FSW     0-3       FOOT SWITCH BIT0:A,BIT1:B 0/1:0FF/ON
12  SPRNG   0-7       SOFT PEDAL RANGE
13  NSFTA   0-48      NOTE SHIFT RANGE FOR SINGLE,DUAL,SPLIT(A)
14  NSFTB   0-48      NOTE SHIFT RANGE FOR SPLIT(B)
15  BLNC    0-100     VOLUME BALANCE (-50 - +50)
16  TVLM    0-99      TOTAL VOLUME
17  CSLD1   0-105     CONTINUOUS SLIDER 1
18  CSLD2   0-109     CONTINUOUS SLIDER 2
19  CSSW    0-3       CONTINUOUS SLIDER ASSIGN SWITCH bI1,3:B,b0,2:A
20  PNMD    0-3       PAN MODE 0:MIX,1:0N-ON,2:ON-OFF,3:OFF-ON
21  PANRNG  0-99      PAN CONTROLL RANGE
22  PANASN  0-2       PAN CONTROLL ASSIGN 0/1/2:LFO/VELOCITY/KEY#
23  PNEGR1  0-99      PAN EG RATE 1
24  PNEGR2  0-99      PAN EG RATE 2
25  PNEGR3  0-99      PAN EG RATE 3
26  PNEGR4  0-99      PAN EG RATE 4
27  PNEGL1  0-99      PAN EG LEVEL 1
28  PNEGL2  0-99      PAN EG LEVEL 2
29  PNEGL3  0-99      PAN EG LEVEL 3
30  PNEGL4  0-99      PAN EG LEVEL 4

-}

-- * 5-6. System Set-up Parameters

{-

64   0  TXCH        MIDI TX channel
65   1  CVMSW       MIDI channel voice message TRANS switch
66   2  RXCHA       MIDI RX channel 16:off
67   3  RXCHB       MIDI RX channel 16:off
68   4  OMNI        MIDI OMNI MODE SWITCH 0/1:0FF/ON
69   5  MCONTA      MIDI CONTROLER NUMBER
70   6  MCONTB      MIDI CONTROLER NUMBER
71   7  MCSNUM1     CONTINUOUS SLIDER 1 CONTROLL MUMBER
72   8  MCSNUM2     CONTINUOUS SLIDER 2 CONTROLL NUMBER
73   9  MKOEFG      MIDI key on/off normal/odd/even:0/1/2 flag
74  10  PPCMOD      PROGRAM CHANGE TRANS MODE FLAG 0/1/2:0f/nor/prg
75  11  LOCAL       LOCAL SWITCH 0/1:0FF/ON
76  12  MTBFLG      MIDI transmit block flag
77  13  MRBFLG      MIDI recieve block flag
78  14  SCMCH       MIDI system common message RX channel (device No.)
79  15  SCMSW       MIDI system common message switch
80  16  APTBNK1     cartridge appoint bank number
81  17  APTBNK2     cartridge appoint bank number
82  18  APTBNK3     cartridge appoint bank number
83  19  PROTECT     memory protect --- bit0=INT. bit1=CRT.

        MSTUNE      master tune
        PPCBUF      PROGRAMMABLE PROGRAM CHANGE TRANS SET BUFFER

-}
