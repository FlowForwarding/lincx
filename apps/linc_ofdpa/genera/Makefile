
.PHONY: default

BEAMS := $(patsubst %.erl,%.beam,$(wildcard *.erl))

default: grammar $(BEAMS) $(OBJS)

grammar: api_lexer.beam api_parser.beam \
		data_lexer.beam data_parser.beam

%.beam: %.erl
	erlc $<

%.erl: %.xrl
	erlc $<

%.erl: %.yrl
	erlc $<

.DELETE_ON_ERROR:
