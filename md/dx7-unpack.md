# dx7-unpack

DX7 SYSEX files have some fields bit-packed.

dx7-unpack translates between the 4096-element packed data sequence
and the 4960-element unpacked data sequence.

Data is read from `stdin` as an hexedecimal ASCII text sequence and
written in the same format to `stdout`.

This program is used by `Sound.SC3.Data.Yamaha.DX7` to read and write
DX7 sysex files.

~~~~
$ cd ~/sw/hsc3-data/data/yamaha/dx7/
$ hsc3-dx7 sysex print hex ROM1A.syx > ROM1A.syx.text
$
~~~~
