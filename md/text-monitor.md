# text-monitor

A line based console text monitor.

Incoming lines are integer prefixed, and the monitor draws each line
that arrives at the indicated line on the console.

~~~~
$ midi-osc-sc3 1,1000,freq,220,440,exp,hz 2,1000,ampl,0,1,amp,* | \
  hsc3-text-monitor stdin space
1 1000  freq 355.640 hz
2 1000  ampl   0.304 *
$
~~~~