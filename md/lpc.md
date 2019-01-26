# lpc

~~~~
$ fn=/home/rohan/sw/hsc3-data/data/lpc/fate.lpc
$ hsc3-lpc be print header $fn
LPCHeader {
  lpcHeaderSize = 28,
  lpcMagic = 999,
  lpcNPoles = 34,
  lpcFrameSize = 38,
  lpcFrameRate = 220.5,
  lpcSampleRate = 44100.0,
  lpcAnalysisDuration = 1.58839,
  lpcNFrames = 350
}
$ hsc3-lpc be print frame csv 4 $fn 0 | cut -c -68
14.6889,44.3598,0.1096,94.1241,-0.1584,0.3528,-0.2999,0.1266,0.1123,
$ hsc3-lpc be print column csv 4 $fn 3 | cut -c -70
94.1241,70.0000,101.5844,115.7910,105.8535,112.0934,119.6707,126.3819,
$ hsc3-lpc be print column csv 4 $fn 3 | hsc3-plot csv row 0 /dev/stdin
$ fn=/home/rohan/uc/invisible/clarity/lpc/z.01.lpc
$ hsc3-lpc txt print column csv 4 $fn 3 | hsc3-plot csv row 0 /dev/stdin
$
~~~~
