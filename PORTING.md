
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

rebar eunit reports that all 107 tests passed. The unit tests is the minimum
proof of correctness of the ported version.

