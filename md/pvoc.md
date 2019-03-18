# pvoc

## header

~~~~
$ hsc3-pvoc header /home/rohan/uc/invisible/clarity/pvx/z.01.pvx
WAVE_FMT_16 
{ audioFormat = WAVE_FORMAT_EXTENSIBLE
, numChannels = 1
, sampleRate = 24000
, byteRate = 48000
, blockAlign = 2
, bitsPerSample = 16
}
WAVE_FMT_PVOC_80 
{ cbSize = 62
, wValidBitsPerSample = 16
, dwChannelMask = 0
, subFormat = "8312B9C2-2E6E-11D4-A824-DE5B96C3AB21"
, dwVersion = 1
, dwDataSize = 32
, wWordFormat = PVOC_IEEE_FLOAT
, wAnalFormat = PVOC_AMP_FREQ
, wSourceFormat = WAVE_FORMAT_PCM
, wWindowType = PVOC_UNUSED
, nAnalysisBins = 257
, dwWinlen = 1024
, dwOverlap = 128
, dwFrameAlign = 2056
, fAnalysisRate = 187.5
, fWindowParam = 0.0
}
NFRAMES = 7876
$
~~~~

## plot

~~~~
$ hsc3-pvoc plot /home/rohan/uc/invisible/clarity/pvx/z.01.pvx 12 24
$
~~~~

