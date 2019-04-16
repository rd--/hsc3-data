# text-osc

Read incoming OSC messages and maintain curses window.

`/set_ln n txt` clears line `n` and replaces it with `txt`.

`/set_str x y txt` writes `txt` at line `x` and column `y`.

~~~~
$ hsc3-text-osc --help
text-osc [opt]

  port:int -- UDP port number; default=57350
$
~~~~
