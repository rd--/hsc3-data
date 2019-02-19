# hsc3-midi

MIDI utilities.

## midi-header

Print MIDI header data to `stdout`.

~~~~
$ hsc3-midi midi-header ~/uc/sp-id/midi/ngv/s-airport.midi
("file-type",0)
("time-div",1024)
("number-of-tracks",1)
$
~~~~

## midi-to-csv-mnd

Convert a standard `MIDI` file to a `CSV MND` file.

~~~~
$ hsc3-midi midi-to-csv-mnd ~/uc/sp-id/midi/ngv/s-airport.midi /dev/stdout | head
time,on/off,note,velocity,channel,param
0.0000,on,64,127,0,
0.0000,on,0,127,0,
0.0000,on,111,127,0,
0.1045,off,111,0,0,
0.1465,off,64,0,0,
0.2383,off,0,0,0,
0.8193,on,80,127,0,
0.8193,on,16,127,0,
0.9102,on,65,127,0,
$
~~~~

## midi-to-csv-text

Convert standard `MIDI` file to a plain `TEXT` notation.

~~~~
$ hsc3-midi midi-to-csv-text ~/uc/sp-id/midi/ngv/s-airport.midi | head
0,0     ,T,   ,   ,   ,tempo-change,1000000
0,0     ,M,144,64 ,127,            ,
0,0     ,M,144,0  ,127,            ,
0,0     ,M,144,111,127,            ,
0,107   ,M,128,111,0  ,            ,
0,150   ,M,128,64 ,0  ,            ,
0,244   ,M,128,0  ,0  ,            ,
0,839   ,M,144,80 ,127,            ,
0,839   ,M,144,16 ,127,            ,
0,932   ,M,144,65 ,127,            ,
$
~~~~
