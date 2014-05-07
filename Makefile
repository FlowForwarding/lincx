.PHONY: default lincx compile get-deps update-deps test clean deep-clean

default: lincx

compile: get-deps update-deps
	@./rebar compile 

get-deps:
	@./rebar get-deps

update-deps:
	@./rebar update-deps

test: compile
	@./rebar skip_deps=true apps="linc,linc_max" eunit

clean:
	@./rebar clean

deep-clean: clean
	@./rebar delete-deps

#-------------------------------------------------------------------------------
# LING-related targets
#

SHELL := bash

include LINGConfig.mk

lincx:
	@./rebar co skip_deps=true
	@./rebar ling-build-image

DOMCONF := domain_config

boot: $(DOMCONF)
	xl create -c $(DOMCONF)

APPS_EBIN_DIRS := $(addprefix /lincx/,$(wildcard apps/*/ebin))
DEPS_EBIN_DIRS := $(addprefix /lincx/,$(wildcard deps/*/ebin))
PATHZ := $(APPS_EBIN_DIRS) $(DEPS_EBIN_DIRS)
SYSCONF := /lincx/priv/sys.config

EXTRA := $(LING_NETSPEC)
EXTRA += -home /lincx
EXTRA += -pz $(PATHZ)
EXTRA += -config $(SYSCONF)
EXTRA += -eval \"lists:map(fun application:start/1, [crypto,asn1,public_key,ssh,compiler,syntax_tools,xmerl,mnesia,lager,linc])\"
EXTRA += $(REMOTE_MOUNTS)
EXTRA += $(patsubst %,-of_controller %,$(OF_CONTROLLERS))

$(DOMCONF): LINGConfig.mk
	@echo "name = \"lincx\"" >$(DOMCONF)
	@echo "kernel = \"vmling\"" >>$(DOMCONF)
	@echo 'extra = "$(EXTRA)"' >> $(DOMCONF)
	@echo "memory = \"$(MEMORY)\"" >>$(DOMCONF)
	@echo "vif = [ 'bridge=$(DEFAULT_BRIDGE)'," >>$(DOMCONF)
	@for ((i = 1; i < $(NUM_PORTS); i++)); do echo "    'bridge=$(BRIDGE_PREFIX)$$i'," >>$(DOMCONF); done
	@echo "    'bridge=$(BRIDGE_PREFIX)$(NUM_PORTS)' ]" >>$(DOMCONF)

