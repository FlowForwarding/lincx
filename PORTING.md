
# Notes on porting

----[12/12/13 12:54]------------------------------------------------------------

LINC-Switch builds cleanly

The following external dependencies are referenced:

* lager
* of_protocol
* enetconf
* of_config
* pkt
* meck
* procket
* epcap
* tunctl
* sync

The following two referenced libraries have c_src -- procket and epcap -- they
will definitely require a replacement/rewrite.

Code that starts external programs (using os:cmd/1):

	deps/tunctl/src/tunctl.erl:    case os:cmd(Cmd) of
	deps/sync/src/sync_scanner.erl:            os:cmd(lists:flatten(Cmd))
	deps/procket/src/procket.erl:    case os:cmd(Cmd) of
	deps/meck/test/meck_tests.erl:    [] = os:cmd("epmd -daemon"),

meck and sync are development-related thus we still end up with two major
incomaptibilities: epcap and procket.

The procket dependency is very slim. There are only a few lines that use it:

	linc_us3/src/linc_us3_port_native.erl:    procket:write(Socket, Frame);
	linc_us3/src/linc_us3_port_native.erl:    procket:write(Socket, Frame);
	linc_us3/src/linc_us3_port_native.erl:    procket:close(Socket).
	linc_us4/src/linc_us4_port_native.erl:    procket:write(Socket, Frame);
	linc_us4/src/linc_us4_port_native.erl:    procket:write(Socket, Frame);
	linc_us4/src/linc_us4_port_native.erl:    procket:close(Socket).

And procket:write() calls happen on darwin or netbsd only.

Note: when adding bridge that connect to primary NIC (eth0) provide static
configuration that matches that of eth0.

----[13/12/13 17:36]------------------------------------------------------------

Note: there are two configuration files rel/files/sys.config and
rel/linc/releases/1.0/sys.config. rel/linc/bin/linc console uses the latter.

The mock controller and linc switch tested using Ping demo.

----[16/12/13 23:55]------------------------------------------------------------

LINC uses reltool for release packaging. This does not play well with the
current build service interface that expects the 'standard' rebar layout with
deps and src directories, etc. The code can be repackaged or the reltool layout
should be added to the build service and the ling_builder plugin.

Currently linc gets started using bash scripts. This will not work for LING.
The scripts that start linc in certain configuration must reside in Dom0 and use
command line ('extra' parameter of the domain config) to pass options to linc.

'make test' reports that all tests passed (except for linc_tests that says that
asn1 not started). The unit tests is the minimum proof of correctness of the
ported version.

----[17/12/13 17:19]------------------------------------------------------------

The Erlang on Xen image can be seen as alternative to the output of the reltool.

A preliminary build using the Erlang on Xen build service attempted. A few
stanzas added to rebar.config to enable ling_builder plugin. The build was
successful.

The output (unstripped) image is less than 8M. The plugin successfully imported
all relevant sources including dependencies but not compiled tests. The compiled
tests are needed to run eunit.

Contents of apps/linc/test directory copied to app/linc/src to ensure that compiled
tests are available at runtime. A new LING image produced that includes the tests.

The first try unsuccessful:

	Eshell V5.10.2  (abort with ^G)
	1> 
	1> eunit:test(linc_buffer_tests).
	undefined
	*unexpected termination of test process*
	::{badarg,[{erlang,atom_to_list,[[]],[]},
		   {eunit_lib,fun_parent,1,[{file,"eunit_lib.erl"},{line,383}]},
		   {eunit_data,parse_function,1,[{file,"eunit_data.erl"},{line,434}]},
		   {eunit_data,next,1,[{file,"eunit_data.erl"},{line,170}]},
		   {eunit_data,lookahead,1,[{file,[...]},{line,...}]},
		   {eunit_data,group,1,[{file,...},{...}]},
		   {eunit_data,next,1,[{...}|...]},
		   {eunit_data,lookahead,1,[...]}]}

	=======================================================
	  Failed: 0.  Skipped: 0.  Passed: 0.
	One or more tests were cancelled.
	error

erlang:fun_info(F, name) returns {name,[]} on LING. This needs a fix. A
relevant bit from term_util.c:

    case A_NAME:
        return nil; //TODO

Fixed. fun_info(F, name) returns {name,[]} only for unloaded modules.

The first ever unit test passes now:

	1> eunit:test(linc_buffer_tests).
  	All 3 tests passed.
	ok
	2> 


