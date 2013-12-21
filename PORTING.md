
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

----[18/12/13 11:38]------------------------------------------------------------

Another case of differing behaviour:

	Eshell V5.10.2  (abort with ^G)
	1> 
	1> eunit:test(linc_ofconfig_tests).
	undefined
	*unexpected termination of test process*
	::{not_implemented,[{erlang,is_builtin,3,
				    [{file,"preload/erlang.erl"},{line,736}]},
			    {meck_proc,expect_type,3,
				       [{file,"src/meck_proc.erl"},{line,324}]},
			    {meck_proc,'-normal_exports/1-lc$^0/1-0-',2,
				       [{file,"src/meck_proc.erl"},{line,314}]},
			    {meck_proc,normal_exports,1,
				       [{file,"src/meck_proc.erl"},{line,313}]},
			    {meck_proc,init,1,[{file,[...]},{line,...}]},
			    {gen_server,init_it,6,[{file,...},{...}]},
			    {proc_lib,init_p_do_apply,3,[{...}|...]}]}

	=======================================================
	  Failed: 0.  Skipped: 0.  Passed: 0.
	One or more tests were cancelled.
	error

A relevant bit from erlang.erl:

	is_builtin(_M, _F, _A) ->
		erlang:error(not_implemented). %%TODO

LING has the same function in a different module - bifs:is_builtin/1. These
should be merged - done.

A reference to cover application added to rebar.config as {import_lib,tools}.

Another error from the meck:

	Eshell V5.10.2  (abort with ^G)
	1> eunit:test(linc_ofconfig_tests).
	undefined
	*unexpected termination of test process*
	::{{case_clause,{error,beam_lib,{not_a_beam_file,<<70,79,82,49,0,0,...>>}}},
	   [{meck_code,abstract_code,1,[{file,"src/meck_code.erl"},{line,44}]},
		{meck_proc,backup_original,2,[{file,"src/meck_proc.erl"},{line,338}]},
		{meck_proc,init,1,[{file,"src/meck_proc.erl"},{line,191}]},
		{gen_server,init_it,6,[{file,"gen_server.erl"},{line,304}]},
		{proc_lib,init_p_do_apply,3,[{file,[...]},{line,...}]}]}

	=======================================================
	  Failed: 0.  Skipped: 0.  Passed: 0.
	One or more tests were cancelled.
	error

meck_proc.erl fixed to accept more general errors including the above. A
reference to compiler application added.

meck is using dynamic generation of mock modules. In general, this is possible
with LING but requires changes to meck. The obstacle is the transformation of
.beam files to .ling files. BEAM does a transformation at runtime. LING
transforms the code statically. The problems manifest itself now as:

	Eshell V5.10.2  (abort with ^G)
	1> eunit:test(linc_ofconfig_tests).
	undefined
	*unexpected termination of test process*
	::{compile_forms,{error,[{[],[{none,compile,{crash,beam_asm,{...}}}]}],[]}}

	=======================================================
	  Failed: 0.  Skipped: 0.  Passed: 0.
	One or more tests were cancelled.
	error
	2> 

The build service does the .beam-to-.ling transformation of individual modules.
Calls to compile_forms() and the like inside meck should call the build service
to obtain loadable modules.

Note: the compiler uses filename:absname(). This function fails if the current
directory is not defined as is the case for LING by default. The current
directory can be set using '-home Dir' command-line option. If current directory
is set then the error above changes to (a more sensible):

	3> eunit:test(linc_ofconfig_tests).
	No LING tag found
	undefined

	=ERROR REPORT==== 18-Dec-2013::12:51:08 ===
	Loading of  failed: not_loaded
	*unexpected termination of test process*
	::{error_loading_module,inet,not_loaded}

	=======================================================
	  Failed: 0.  Skipped: 0.  Passed: 0.
	One or more tests were cancelled.
	error

The error means that the compiler produced a beam file but the file cannot be
loaded on LING. A transformation via the build service is needed. There is a
project called lingkit that does this. It is added as a dependency,
meck_code.erl changed to use it.

----[19/12/13 12:29]------------------------------------------------------------

lingkit uses inets/httpc to use the build service. Unfortunately this does not
work for LINC. linc_ofconfig_tests (and other tests?) mock inet module and
cripple lingkit along the way. The solution is to add .beam -> .ling
transformation to the image and stop using the build service (and lingkit).

ling_ofconfig_tests now passes with all the mocking:

	1> 
	1> eunit:test(linc_ofconfig_tests).
  	All 3 tests passed.
	ok
	2> 

The testing harness itself is a hard test for the virtual machine. I had to
enable the complete dynamic compilation support to the detriment of the startup
time. There is now a '-nobeam' option to turn this off if your code does not
expect to load dynamically compiled code.

----[18/12/13 18:23]------------------------------------------------------------

A new issue:

	1> eunit:test(linc_tests).
	undefined
	*unexpected termination of test process*
	::{badarg,[{erlang,round,[30000],[]},
			   {eunit_data,parse,1,[{file,"eunit_data.erl"},{line,253}]},
			   {eunit_data,next,1,[{file,"eunit_data.erl"},{line,170}]},
			   {eunit_data,lookahead,1,[{file,"eunit_data.erl"},{line,530}]},
			   {eunit_data,group,1,[{file,[...]},{line,...}]},
			   {eunit_data,next,1,[{file,...},{...}]},
			   {eunit_data,iter_next,1,[{...}|...]},
			   {eunit_proc,get_next_item,1,[...]}]}

	=======================================================
	  Failed: 0.  Skipped: 0.  Passed: 0.
	One or more tests were cancelled.
	error
	2> 

LING does not accept integer as an argument to erlang:round(). Fixed.
linc_tests wants public_key application - added as a dependency.

As expected linc_tests returns the same error as on Erlang/OTP:

	1> 
	1> eunit:test(linc_tests).
	module 'linc_tests'
	*** context setup failed ***
	**in function linc_tests:setup/0 (src/linc_tests.erl, line 70)
	**error:{badmatch,{error,{not_started,asn1}}}


	=======================================================
	  Failed: 0.  Skipped: 0.  Passed: 0.
	One or more tests were cancelled.
	error
	2> 

Let us fix this. asn1 and crypto added as dependencies. application:start(asn1)
and application:start(crypto) run before the test. public_key, ssh, xmerl,
mnesia, syntax_tools are added as dependencies in rebar.config too. 

----[21/12/13 00:52]------------------------------------------------------------

For meck to work I had to preserve the abstract code embedded inside .beam
files. This increased the image size dramatically. This should be made optional
because production systems do not need the source code. meck library patched to
use ling_lib:abstract_code() if beam_lib:chunks() fails.

linc_tests still does not pass:

	1> application:start(asn1).
	ok
	2> application:start(crypto).
	ok
	3> eunit:test(linc_tests).
	22:50:20.595 [error] Failed to open log file log/error.log with error not owner
	22:50:20.597 [error] Failed to open log file log/console.log with error not
	owner
	22:50:21.092 [error] Failed to open crash log file log/crash.log with error: not
	owner
	linc_tests: switch_setup_test_ (Start/stop LINC common logic)...*timed out*
	undefined
	=======================================================
	  Failed: 0.  Skipped: 0.  Passed: 0.
	One or more tests were cancelled.
	error

The "Failed to open" errors means that we need to add a writable filesystem
under log/.

A writable filesystem mounted at /lincx/log. A sparse 4G file added as a block
device to the virtual instance. A '-goofs /lincx/log' option tells the boot
sequence that we want to use the first block device as a GooFS volume and mount
it at /lincx/log. The volume gets formatted automatically on the first use.

The writable filesystem allows us to collect the logs. error.log and crash.log
are empty. console.log contains:

	2013-12-20 23:12:45.020 [info] <0.6.0> Application lager started on node nonode@nohost
	2013-12-20 23:12:49.308 [info] <0.6.0> Application enetconf started on node nonode@nohost
	2013-12-20 23:12:50.027 [info] <0.6.0> Application compiler exited with reason: stopped

It took me half a day to figure out the timeout set in the eunit specification
is "per group of tests" and the default timeout of 5s still applies to
individual tests. Changing the specification in the beginning of linc_tests.erl
to:
	switch_setup_test_() ->
		{timeout, 30,
		 {setup,
		  fun setup/0,
		  fun teardown/1,
		  [{timeout, 30, {"Start/stop LINC common logic", fun logic/0}}]}}.

brings us a step further:

	2> eunit:test(linc_tests).
	linc_tests: switch_setup_test_ (Start/stop LINC common logic)...*failed*
	in function linc_tests:'-logic/0-fun-2-'/1 (src/linc_tests.erl, line 64)
	in call from linc_tests:logic/0 (src/linc_tests.erl, line 64)
	**error:{assertEqual_failed,
		[{module,linc_tests},
		 {line,64},
		 {expression,"application : start ( linc )"},
		 {expected,ok},
		 {value,
			 {error,
				 {bad_return,
					 {{linc,start,[normal,[]]},{'EXIT',{aborted,{...}}}}}}}]}


	13:48:31.794 [error] CRASH REPORT Process <0.350.0> with 0 neighbours exited
	with reason: {{aborted,{no_exists,[linc_ofconfig_startup,startup]}},[]}
	=======================================================
	  Failed: 1.  Skipped: 0.  Passed: 0.
	error

no_exists is a mnesia error code. Records in error.log and crash.log indicate
that exception happens in linc_ofconfig:init() where mnesia gets set up. mnesia
is configured to use disc_copies. It is possible to make mnesia to use writable
disk, the same way as lager, but for now disc_copies removed from the init()
function. linc_tests now passes:

	1> 
	1> eunit:test(linc_tests).
	linc_started
	  Test passed.
	ok
	2> 

----[21/12/13 17:35]------------------------------------------------------------

Proceeding to linc_us3 application. All test modules are copied to src directory
to ensure they get baked into the image. The first module in alphabetical order
is linc_us3_actions_tests:

	1>
	1> eunit:test(linc_us3_actions_tests, [verbose]).
	======================== EUnit ========================
	module 'linc_us3_actions_tests'
	  linc_us3_actions_tests: actions_complex_test_ (Change dest IP and output to egress port)...[0.001 s] ok
	  linc_us3_actions_tests: actions_set_test_ (Action Set: precedence of Group action)...ok
	  linc_us3_actions_tests: actions_set_test_ (Action Set: drop if no Group or Output action)...ok
	  linc_us3_actions_tests: actions_set_test_ (Action Set: Output action)...ok
	  linc_us3_actions_tests: actions_test_ (Action Output)...ok
	  linc_us3_actions_tests: actions_test_ (Action Group)...ok
	  linc_us3_actions_tests: actions_test_ (Action Experimenter)...ok
	  linc_us3_actions_tests: actions_test_ (Action Set-Field)...*failed*
	in function linc_us3_actions_tests:'-check_action/3-fun-0-'/2
	(src/linc_us3_actions_tests.erl, line 348)
	in call from linc_us3_actions_tests:'-action_set_field/0-lc$^0/1-0-'/1
	(src/linc_us3_actions_tests.erl, line 131)
	**error:{assertEqual_failed,
		[{module,linc_us3_actions_tests},
		 {line,348},
		 {expression,"Pkt2"},
		 {expected,
			 {linc_pkt,undefined,
				 {ofp_match,[]},
				 [],
				 [{ether,<<0,0,0,...>>,<<0,0,...>>,200,0}],
				 0,default,undefined,false,undefined,no_buffer,...}},
		 {value,
			 {linc_pkt,undefined,
				 {ofp_match,[]},
				 [],
				 [{ether,<<0,0,...>>,<<0,...>>,100,...}],
				 0,default,undefined,false,undefined,...}}]}


	  linc_us3_actions_tests: actions_test_ (Action Set-Queue)...ok
	  linc_us3_actions_tests: actions_test_ (Action Push-Tag: VLAN)...ok
	  linc_us3_actions_tests: actions_test_ (Action Pop-Tag: VLAN)...ok
	  linc_us3_actions_tests: actions_test_ (Action Push-Tag: MPLS)...ok
	  linc_us3_actions_tests: actions_test_ (Action Pop-Tag: MPLS)...ok
	  linc_us3_actions_tests: actions_test_ (Action Change-TTL: set MPLS TTL)...ok
	  linc_us3_actions_tests: actions_test_ (Action Change-TTL: decrement MPLS TTL)...ok
	  linc_us3_actions_tests: actions_test_ (Action Change-TTL: invalid MPLS TTL)...ok
	  linc_us3_actions_tests: actions_test_ (Action Change-TTL: set IP TTL)...ok
	  linc_us3_actions_tests: actions_test_ (Action Change-TTL: decrement IP TTL)...ok
	  linc_us3_actions_tests: actions_test_ (Action Change-TTL: invalid IP TTL)...ok
	  linc_us3_actions_tests: actions_test_ (Action Change-TTL: copy TTL outwards)...[0.001 s] ok
	  linc_us3_actions_tests: actions_test_ (Action Change-TTL: copy TTL inwards)...ok
	  [done in 11.038 s]
	=======================================================
	  Failed: 1.  Skipped: 0.  Passed: 20.
	error
	2>

This is a bug in the test suite. linc_us3_packet_edit:set_field want
<<Value:16>> as an argument when field is eth_type. The test case fixed.
linc_us3_actions_tests passes:

	1> 
	1> eunit:test(linc_us3_actions_tests).
	  All 21 tests passed.
	ok
	2> 

linc_us3_convert_tests passes:

	3> eunit:test(linc_us3_convert_tests, [verbose]).
	======================== EUnit ========================
	module 'linc_us3_convert_tests'
	  linc_us3_convert_tests: convert_test_ (OXM Ethertype field generated for
	packets shouldn't ignore VLAN tags)...[0.001 s] ok
	  linc_us3_convert_tests: convert_test_ (OXM VLAN fields should match on outer
	VLAN tag only)...ok
	  [done in 0.006 s]
	=======================================================
	  All 2 tests passed.
	ok

linc_us3_flow_tests passes on the first try:

	4> eunit:test(linc_us3_flow_tests, [verbose]).
	======================== EUnit ========================
	module 'linc_us3_flow_tests'
	  linc_us3_flow_tests: table_mod_test_ (Get default value)...ok
	  linc_us3_flow_tests: table_mod_test_ (Set config)...ok
	  linc_us3_flow_tests: table_mod_test_ (Set all config)...[0.001 s] ok
	  linc_us3_flow_tests: timer_test_ (Idle timeout)...[2.701 s] ok
	  linc_us3_flow_tests: timer_test_ (Hard timeout)...[2.500 s] ok
	  linc_us3_flow_tests: statistics_test_ (Update lookup counter)...ok
	  linc_us3_flow_tests: statistics_test_ (Update match counter)...ok
	  linc_us3_flow_tests: statistics_test_ (Update match counter, bad flow_id)...ok
	  linc_us3_flow_tests: statistics_test_ (Empty flow stats)...ok
	  linc_us3_flow_tests: statistics_test_ (Flow stats 1 table)...ok
	  linc_us3_flow_tests: statistics_test_ (Flow stats all tables)...[0.002 s] ok
	  linc_us3_flow_tests: statistics_test_ (Empty aggregate stats)...ok
	  linc_us3_flow_tests: statistics_test_ (Aggregate stats 1 table)...ok
	  linc_us3_flow_tests: statistics_test_ (Aggregate stats all tables)...[0.001 s] ok
	  linc_us3_flow_tests: statistics_test_ (Empty table stats)...[0.005 s] ok
	  linc_us3_flow_tests: flow_mod_test_ (Bad table_id)...ok
	  linc_us3_flow_tests: flow_mod_test_ (Duplicate fields)...ok
	  linc_us3_flow_tests: flow_mod_test_ (Prerequisite field present)...ok
	  linc_us3_flow_tests: flow_mod_test_ (Prerequisite field present bad val)...ok
	  linc_us3_flow_tests: flow_mod_test_ (Prerequisite field missing)...ok
	  linc_us3_flow_tests: flow_mod_test_ (Goto table with smaller table_id)...ok
	  linc_us3_flow_tests: flow_mod_test_ (Valid out port)...ok
	  linc_us3_flow_tests: flow_mod_test_ (Invalid out port)...ok
	  linc_us3_flow_tests: flow_mod_test_ (Valid out group)...ok
	  linc_us3_flow_tests: flow_mod_test_ (Invalid out group)...ok
	  linc_us3_flow_tests: flow_mod_test_ (Duplicate instruction type)...ok
	  linc_us3_flow_tests: flow_mod_test_ (Set field incompatible with match)...ok
	  linc_us3_flow_tests: flow_mod_test_ (Add 1 flow, no check_overlap)...ok
	  linc_us3_flow_tests: flow_mod_test_ (Add 1 flow, check_overlap)...ok
	  linc_us3_flow_tests: flow_mod_test_ (Add 2 non overlapping flows, no check_overlap)...ok
	  linc_us3_flow_tests: flow_mod_test_ (Add 2 non overlapping flows, check_overlap)...ok
	  linc_us3_flow_tests: flow_mod_test_ (Add 2 overlapping flows, no check_overlap)...ok
	  linc_us3_flow_tests: flow_mod_test_ (Add 2 with overlapping flow, check_overlap)...ok
	  linc_us3_flow_tests: flow_mod_test_ (Add 2 with exact match, reset_counters)...ok
	  linc_us3_flow_tests: flow_mod_test_ (Add 2 with exact match, no reset_counters)...[0.001 s] ok
	  linc_us3_flow_tests: flow_mod_test_ (Flow entry priority order)...ok
	  linc_us3_flow_tests: flow_mod_test_ (Modify flow, strict, no reset_counts)...ok
	  linc_us3_flow_tests: flow_mod_test_ (Modify flow, strict, reset_counts)...ok
	  linc_us3_flow_tests: flow_mod_test_ (Modify flow, non-strict, cookie no match)...ok
	  linc_us3_flow_tests: flow_mod_test_ (Modify flow, non-strict, cookie match)...ok
	  linc_us3_flow_tests: flow_mod_test_ (Delete flow, strict)...ok
	  linc_us3_flow_tests: flow_mod_test_ (Delete flow, non-strict, cookie no match)...ok
	  linc_us3_flow_tests: flow_mod_test_ (Delete flow, non-strict, cookie match)...ok
	  linc_us3_flow_tests: flow_mod_test_ (Delete flow, non-strict, send flow rem)...ok
	  linc_us3_flow_tests: flow_mod_test_ (Delete flow, outport no match)...ok
	  linc_us3_flow_tests: flow_mod_test_ (Delete flow, outport match)...ok
	  linc_us3_flow_tests: flow_mod_test_ (Delete flow, outgroup no match)...ok
	  linc_us3_flow_tests: flow_mod_test_ (Delete flow, outgroup match)...ok
	  linc_us3_flow_tests: flow_mod_test_ (Delete flow, all tables)...[0.002 s] ok
	  linc_us3_flow_tests: flow_mod_test_ (Delete where group)...[0.002 s] ok
	  [done in 23.481 s]
	=======================================================
	  All 50 tests passed.
	ok

linc_us3_groups_tests - same:

	5> eunit:test(linc_us3_groups_tests, [verbose]).
	======================== EUnit ========================
	module 'linc_us3_groups_tests'
	  linc_us3_groups_tests: group_test_ (Add group)...ok
	  linc_us3_groups_tests: group_test_ (Modify group)...ok
	  linc_us3_groups_tests: group_test_ (Delete group)...ok
	  linc_us3_groups_tests: group_test_ (Chain deletion)...ok
	  linc_us3_groups_tests: group_test_ (Apply to packet)...[0.001 s] ok
	  linc_us3_groups_tests: group_test_ (Stats & features)...ok
	  linc_us3_groups_tests: group_test_ (is_valid)...ok
	  [done in 0.017 s]
	=======================================================
	  All 7 tests passed.
	ok

linc_us3_instructions_tests - same:

	6> eunit:test(linc_us3_instructions_tests, [verbose]).
	======================== EUnit ========================
	module 'linc_us3_instructions_tests'
	  linc_us3_instructions_tests: instruction_test_ (Apply-Actions)...[0.001 s] ok
	  linc_us3_instructions_tests: instruction_test_ (Clear-Actions)...ok
	  linc_us3_instructions_tests: instruction_test_ (Write-Actions)...ok
	  linc_us3_instructions_tests: instruction_test_ (Write-Metadata)...ok
	  linc_us3_instructions_tests: instruction_test_ (Goto-Table)...ok
	  linc_us3_instructions_tests: instruction_test_ (Empty instruction list)...ok
	  [done in 0.015 s]
	=======================================================
	  All 6 tests passed.
	ok

12 of 17 case of linc_us3_packet_edit_tests fail (no listing). This is because
the tests are not synchronised with changes to linc_us3_packet_edit.erl.
linc_us3_packet_edit fails 12 cases on Erlang/OTP too. The module skipped.

linc_us3_port_tests - no problems:

	2> eunit:test(linc_us3_port_tests).
	  All 20 tests passed.
	ok
	3> 

linc_us3_queue_tests - same:

	4> eunit:test(linc_us3_queue_tests, [verbose]).
	======================== EUnit ========================
	module 'linc_us3_queue_tests'
	  linc_us3_queue_tests: queue_test_ (Port multipart: queue_stats_request)...ok
	  linc_us3_queue_tests: queue_test_ (Sending through queue)...ok
	  linc_us3_queue_tests: queue_test_ (Set queue property: min-rate)...ok
	  linc_us3_queue_tests: queue_test_ (Set queue property: max-rate)...ok
	  [done in 0.008 s]
	=======================================================
	  All 4 tests passed.
	ok

linc_us3_routing_tests - 1 failing case:

	5> eunit:test(linc_us3_routing_tests, [verbose]).
	======================== EUnit ========================
	module 'linc_us3_routing_tests'
	  linc_us3_routing_tests: routing_test_ (Routing: match on Flow Table entry)...[0.001 s] ok
	  linc_us3_routing_tests: routing_test_ (Routing: match on Flow Table entry with highest priority)...ok
	  linc_us3_routing_tests: routing_test_ (Routing: match on Flow Table entry with empty match list)...ok
	  linc_us3_routing_tests: routing_test_ (Routing: match on next Flow Table because of Goto instruction)...ok
	  linc_us3_routing_tests: routing_test_ (Routing: table miss - continue to next table)...ok
	  linc_us3_routing_tests: routing_test_ (Routing: table miss - send to controller)...[3.564 s] ok
	  linc_us3_routing_tests: routing_test_ (Routing: table miss - drop packet)...ok
	  linc_us3_routing_tests: routing_test_ (Routing: match fields with masks)...*failed*
	in function linc_us3_routing:two_fields_match/2
	  called as
	two_fields_match({ofp_field,openflow_basic,undefined,false,<<>>,undefined},{ofp_field,openflow_basic,undefined,true,<<>>,<<>>})
	in call from linc_us3_routing_tests:'-mask_match/0-fun-0-'/3
	(src/linc_us3_routing_tests.erl, line 150)
	in call from linc_us3_routing_tests:'-mask_match/0-lc$^0/1-0-'/1
	(src/linc_us3_routing_tests.erl, line 150)
	**error:undef

	  linc_us3_routing_tests: routing_test_ (Routing: spawn new route process)...ok
	  [done in 3.617 s]
	=======================================================
	  Failed: 1.  Skipped: 0.  Passed: 8.
	error

Erlang/OTP fails the same case with the same error. I guess somebody else should
get the tests in sync.

The only case of linc_us3_tests fails:

	5> eunit:test(linc_us3_tests).
	linc_us3_tests: switch_setup_test_ (Start/stop LINC v3 switch backend w/o OF-Config subsystem)...*failed*
	in function linc_us3_tests:'-no_ofconfig/0-fun-0-'/1 (src/linc_us3_tests.erl, line 50)
	in call from linc_us3_tests:'-no_ofconfig/0-lc$^0/1-0-'/1 (src/linc_us3_tests.erl, line 50)
	**error:{assertEqual_failed,
		[{module,linc_us3_tests},
		 {line,50},
		 {expression,"application : start ( linc )"},
		 {expected,ok},
		 {value,
			 {error,
				 {bad_return,
					 {{linc,start,[normal,[]]},
					  {'EXIT',{{badmatch,...},[...]}}}}}}]}

A few applications (asn1, crypto, public_key) are added to setup code for the
test.

