# hsc3-data

## ats header

~~~~
$ hsc3-data ats header ~/sw/hsc3-data/data/ats/pf-c5.4.ats
ats_sample_rate = 44100.0
ats_frame_size = 2205
ats_window_size = 8821
ats_n_partials = 36
ats_n_frames = 234
ats_max_amplitude = 8.300120462729725e-2
ats_max_frequency = 2112.43387414816
ats_analysis_duration = 11.591451644897461
ats_file_type = 4
ats_frame_length = 134
$
~~~~

## ats write-au

Write data `Ats` data as a 32-bit floating point `Next/Au` file.

## au to-pbm

Convert 32-bit floating point `Next/Au` file to `Pbm` file.
Threshold is `0.5`.

## au to-pgm

Convert 32-bit floating point `Next/Au` file to `Pgm` file of indicated bit depth.

## csv mnd-to-midi

Read `Csv/Mnd` file and write `Midi` format zero file.

## csv mnd-to-pgm

Convert a midi note data `Csv` file to a `Pgm` image of specified dimensions.

## csv to-image

Convert a `Csv` file having bit-indices of the form (row,column) to `Pbm` file.

## hex decode

Read binary data and write each byte as a two-character hexadecimal number.

## hex encode

Read hex-encoded data and write each byte in binary format.

## image query uniq

List of uniqe colours with location of initial occurence, in row-order.

## image to-pbm

Convert a `Greyscale` or `Rgb` image file to `Pbm`, either requiring that the
image contains only black & white pixels, or using the `Rec.709`
luminence model & a threshold value.

## image to-pgm

Convert an `Rgb` image file to `Pgm`, either requiring that the image
contains only grey pixels, or using the `Rec.709` luminence model.

## pbm indices

Write (row,column) indices for `Pbm` in text form, as `Csv` or `Json`.

~~~~
$ hsc3-data pbm indices csv ~/sw/hsc3-data/data/pbm/fh.pbm /dev/stdout | wc -lc
   8378   58673
$
~~~~

## pbm to-csv-mnd

Translate `Pbm` file into `Midi` data, written in `Csv/Mnd` format.

In the parameter arguments list the following abbreviations and shorthands are used:

- + = increment
- ~ = table (multiplier)
- mnn = midi note number"
- inv | id = frequency inverse, id = identity"
- le | id = leading edge
- tm = time
- du = duration
- gn = gain

## pbm to-table

Convert `Pbm` file to a 32-bit floating point `Next/Au` sound file
using the indicated method to reduce each column in the image to a
single real value.

## pgm to-au

Convert a `Pgm` image to a 32-bit floating point `Next/Au` sound file.

The sound file will have as many channels as there are rows in the
image, and as many frames as there are columns.
