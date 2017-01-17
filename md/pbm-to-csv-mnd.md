# pbm-to-csv-mnd

Translate `PBM` file into `MIDI` data, written in `CSV/MND` format.

~~~~
$ hsc3-pbm-to-csv-mnd -h
pbm-to-csv-mnd mnn mnn+ mnn~ inv le tm+ tm~ du du~ gn gn~ pbm-file csv-file
 + = increment, ~ = table (multiplier) | nil
 mnn = midi note number
 inv | id = frequency inverse, id = identity
 le | id = leading edge
 tm = time, du = duration, gn = gain
$
~~~~
