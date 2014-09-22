
.PHONY: default

OBJS := $(patsubst %.c,%.o,$(wildcard *.c))

CPPFLAGS :=
CFLAGS :=

LDFLAGS := -L /broadcom/of-dpa/ofdpa-1.0.0.1-im-cdp/bin/im-x86-trident-ub12

LDFLAGS_FINAL := -lapr-1
LDFLAGS_FINAL += -lrpc_client

default: agentx

agentx: $(OBJS)
	gcc -o $@ $(LDFLAGS) $^ $(LDFLAGS_FINAL)

%.o: %.c
	gcc -c $(CPPFLAGS) $(CFLAGS) $<

