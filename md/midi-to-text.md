# midi-to-text

Convert standard `MIDI` file to a plain `TEXT` notation.

~~~~
$ hsc3-midi-to-text header ~/uc/sp-id/midi/ngv/s-airport.midi
("file-type",0)
("time-div",1024)
("number-of-tracks",1)
$ hsc3-midi-to-text csv ~/uc/sp-id/midi/ngv/s-airport.midi | head
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
