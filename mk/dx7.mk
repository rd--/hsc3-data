%.syx.text : %.syx
	hsc3-dx7 sysex print hex $< > $@

%.syx.names.text : %.syx
	hsc3-dx7 sysex print voice-names $< > $@

%.syx.param.text : %.syx
	hsc3-dx7 sysex print parameters $< > $@
