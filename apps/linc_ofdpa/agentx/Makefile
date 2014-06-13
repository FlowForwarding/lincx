
.PHONY: default

OBJS := $(patsubst %.c,%.o,$(wildcard *.c))

CPPFLAGS :=
CFLAGS :=
LDFLAGS := -lapr-1

default: agentx

agentx: $(OBJS)
	gcc -o $@ $(LDFLAGS) $^

%.o: %.c
	gcc -c $(CPPFLAGS) $(CFLAGS) $<

