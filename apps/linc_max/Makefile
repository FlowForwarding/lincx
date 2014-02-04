
.PHONY: default

BEAMS := $(patsubst %.erl,%.beam,$(wildcard src/*.erl))

default: $(BEAMS)
#default: flow0.erl

flow0.erl: flow0.tab

%.erl: %.tab
	scripts/generator $<

%.beam: %.erl
	erlc -o ebin $<

