
.PHONY: default

BEAMS := $(patsubst %.erl,%.beam,$(wildcard *.erl))

#default: $(BEAMS)
default: flow0.erl

flow0.erl: flow0.tab

%.erl: %.tab
	./generator $<

%.beam: %.erl
	erlc $<

