.PHONY: rel compile get-deps update-deps test clean deep-clean

rel: compile
	@./rebar generate -f
	@./scripts/post_generate_hook

offline:
	@./rebar compile
	@./rebar generate -f
	@./scripts/post_generate_hook

compile: get-deps update-deps
	@./rebar compile 
get-deps:
	@./rebar get-deps

update-deps:
	@./rebar update-deps

test: compile
	@./rebar skip_deps=true apps="linc,linc_us4" eunit

test_us3: compile
	@./rebar skip_deps=true apps="linc,linc_us3" eunit

clean:
	@./rebar clean

deep-clean: clean
	@./rebar delete-deps

setup_dialyzer:
	dialyzer --build_plt --apps erts kernel stdlib mnesia compiler syntax_tools runtime_tools crypto tools inets ssl webtool public_key observer
	dialyzer --add_to_plt deps/*/ebin

dialyzer: compile
	dialyzer apps/*/ebin

dev_prepare: compile
	./scripts/pre_develop_hook

dev:
	erl -env ERL_MAX_ETS_TABLES 3000 -pa apps/*/ebin apps/*/test deps/*/ebin -config rel/files/sys.config -args_file rel/files/vm.args -eval "lists:map(fun application:start/1, [kernel, stdlib, public_key, crypto, ssl, compiler, syntax_tools, runtime_tools, xmerl, mnesia, lager, linc, sync])"

#-------------------------------------------------------------------------------
# LING-related targets
#

APPS_EBIN_DIRS := $(addprefix /lincx/,$(wildcard apps/*/ebin))
DEPS_EBIN_DIRS := $(addprefix /lincx/,$(wildcard deps/*/ebin))
PATHZ := $(APPS_EBIN_DIRS) $(DEPS_EBIN_DIRS)
SYSCONF := /lincx/priv/sys.config
DOMCONF := domain_config

$(DOMCONF):
	@echo "name = \"lincx\"" >$(DOMCONF)
	@echo "kernel = \"vmling\"" >>$(DOMCONF)
	@echo "extra = \"-dhcp -goofs /lincx/log -home /lincx -pz $(PATHZ) -config $(SYSCONF)\"" >>$(DOMCONF)
	@echo "memory = \"1024\"" >>$(DOMCONF)
	@echo "disk = [ \"tap:aio:/home/mk/lincx/lincxdisk1.img,xvda,w\" ]" >>$(DOMCONF)
	@echo "vif = [ '', '', '' ]" >>$(DOMCONF)

x:	$(DOMCONF)
	sudo xl create -c $(DOMCONF)

