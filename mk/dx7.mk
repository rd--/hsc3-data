%.syx.text : %.syx
	hsc3-dx7-unpack unpack text $< > $@

%.names.text : %.syx.text
	hsc3-dx7-hex print-voice-names $< > $@

%.param.text : %.syx.text
	hsc3-dx7-hex print-parameters $< > $@
