latex input:            mmd-article-header
Title:			LINC on LING: compatibility, performance, and the pixie dust
Author:			Maxim Kharchenko, Cloudozer LLP
Date:			04/01/2014
latex mode:				memoir
base header level:      2
use xelatex:            true
latex input:            mmd-article-begin-doc

# Overview

The present document is an interim report on the status of the project that aims
to port the LINC switch to Erlang on Xen. Erlang on Xen is an alternative
implementation of the language runtime that does not require an operating
system. The codename of the Erlang on Xen<!--\rq -->s virtual machine is _LING_.
Thus the port is usually referred to as LINC/LING as opposed to the standard
LINC/BEAM combination that uses BEAM, the virtual machine from Erlang/OTP.

Besides the discussion of the results of the project and its current status, the
document contains suggestions regarding possible future directions for the
project.

# Status

1. No pending compatibility issues. Tests confirm that LING is compatible enough
to run the LINC switch.

1. The first round of performance tuning showed that LINC/LING outperforms
LINC/BEAM by 10-15% in the configuration tested.

# Compatibility checks

The unit tests of the LINC switch were chosen for the first round of compatibility
testing. The following [table][unit_tests] summarizes the results. All failing
test cases in the table fail on LINC/BEAM too. These test cases must be 'out of
sync' with changes to the code. The linc\_us3\_tests case is skipped as it
depends on the configuration file in a non-trivial way and the similar
linc\_us4\_tests case passes.

Application | Test module | Total | Ok | Failed
------------|-------------|:-----:|:--:|:-----:
linc | linc\_buffer\_tests | 3 | 3 | -
 | linc\_ofconfig\_tests | 3 | 3 | -
 | linc\_tests | 1 | 1 | -
linc\_us3 | linc\_us3\_actions\_tests | 21 | 21 | -
 | linc\_us3\_convert\_tests | 2 | 2 | -
 | linc\_us3\_flow\_tests | 50 | 50 | -
 | linc\_us3\_groups\_tests | 7 | 7 | -
 | linc\_us3\_instructions\_tests | 6 | 6 | -
 | linc\_us3\_packet\_edit\_tests | 17 | 5 | 12
 | linc\_us3\_queue\_tests | 4 | 4 | -
 | linc\_us3\_routing\_tests | 9 | 8 | 1
 | linc\_us3\_tests | 1 | skipped ||
linc\_us4 | linc\_us4\_actions\_tests | 25 | 25 | -
 | linc\_us4\_convert\_tests | 15 | 15 | -
 | linc\_us4\_groups\_tests | 13 | 13 | -
 | linc\_us4\_instructions\_tests | 7 | 7 | -
 | linc\_us4\_meter\_tests | 28 | 28 | -
 | linc\_us4\_packet\_tests | 44 | 44 | -
 | linc\_us4\_port\_tests | 21 | 21 | -
 | linc\_us4\_queue\_tests | 4 | 4 | -
 | linc\_us4\_routing\_tests | 16 | 10 | 6
 | linc\_us4\_table\_features\_tests | 5 | 5 | -
 | linc\_us4\_tests | 5 | 5 | -
[Unit testing of LINC/LING][unit\_tests]

The [appendix][incomp] contains the list of detected (and fixed)
incompatibilities.

# Performance tuning

## LINC/BEAM performance baseline

The performance of LINC/BEAM was tested first to establish the baseline case for
comparison. The testing of LINC/BEAM used the network configuration shown below.
The testbed mostly follows the setup described in the LINC on Xen document[^lincxen].
A special new scenario (simple\_iperf\_test) was added to the mock controller
(of\_controller\_v4.erl).

[^lincxen]: https://github.com/FlowForwarding/LINC-Switch/blob/master/docs/testbed-setup.md

![The LINC/BEAM performance testbed][testbed1]

[testbed1]: testbed1.png

The [table][baseline_perf] summarizes the observed LINC/BEAM performance.

Parameter | Value 
----------|------:
Throughput (TCP) | 140Mbits/s
Latency | 1.40ms
RAM footprint | 450M
CPU utilization | 200%
[Observed LINC/BEAM performance][baseline_perf]

The [appendix][reprod1] lists steps needed to reproduce the baseline performance
test.

## LINC/LING performance testing

The LINC/LING tests used the network configuration shown below. The testbed is
sufficiently similar to the baseline case except the LINC switch runs in a
separate Xen domain, not as a Unix process.

![The LINC/LING performance testbed][testbed2]

[testbed2]: testbed2.png

## The first tuning round

The profiler shows that the LINC switch spends 70% of time executing a single
function, the calculation of the IP checksum. The implementation of the function
in the pkt package is not optimal. It can be improved as shown in the comment on
the line 955 of pkt.erl file. Even faster option is to implement the IP checksum
calculation as a BIF. A new BIF (binary:ip\_checksum/1) is added to LING.

The performance of LINC/LING after the first round of testing is summarized in
the [table][ling_perf] below.

Parameter | Value 
----------|------:
Throughput (TCP) | 168Mbits/s
Latency | 1.36ms
RAM footprint | 128M
CPU utilization | 100%
[LINC/LING performance results][ling_perf]

The steps to reproduce the LINC/LING performance results can be found in the
[appendix][reprod2].

# Observations and conclusions

## LING is compatible

The LINC switch is a large application, which touches every corner of the
virtual machine through its main sources or one of its dependencies. The
incompatibilities found are of relatively minor nature.

## LING is performant

The demonstrated latency and throughput of LINC/LING is generally on par with
LINC/BEAM. The memory and CPU consumption seem significantly lower.

## LINC/LING image is handy

The Xen image that contains the LINC switch code and all bits of Erlang runtime
needed is about 12M in size. It does not have any operating system dependencies.

## Erlang is the right tool

Any highly-configurable network switching fabric uses pattern matching. The
pattern matching compilation, a standard trait of functional languages, can
increase performance of this function considerably. The dynamic changes to the
rules require that the compilation takes place at runtime and the rule matching
code gets reloaded. This makes Erlang an ideal tool for such fabric as the
language has both advanced pattern matching (with bit syntax) and the dynamic
code loading.

## Design Principles do not cut it

The analysis of profiler output shows that a lot of time is spent inside
gen\_server code. Wrapping ports with a gen\_server is a bad idea when performance
is important.

## ETS is a database

The LINC code uses ETS tables extensively. The fast path includes ETS tables
updates. This is the opposite of a fast code.

## GC needs custom heuristics

The memory consumption is an important obstacle when implementing fast network
switching using a garbage-collected language, such as Erlang. The heuristics
Erlang uses when it decides when to run an incremental or a full-sweep garbage
collection do not play well here. The switching code should either run garbage
collection explicitly or new heuristics/options should be added to the runtime.

# Fighting latency

The amount of time a frame spends inside the LINC switch is key for faster
throughput. Let us take a look at the latencies of various parts of a frame
path.

![The latency breakdown][latency1]

[latency1]: latency1.png

The latencies were measured (or deduced) using the ping command and the direct
observation via the virtual machine instrumentation. The difference between
145<!--$\mu$-->s and 102<!--$\mu$-->s is probably the effect of not having a
complete operating system between br-linc1 and vif-linc/eth1 (and br-linc2 and
vif-linc2/eth2).

The 220<!--$\mu$-->s the frame spends in LING applying a couple of trivial rules
seems high. The nullx experiment establishes the low bound of this number.

For the experiment the whole code of the switch was replaced with this module:

```
-module(nullx);
-export([start/0]).

start() ->
        spawn(fun() ->
                {ok,P1} = net_vif:open(eth1),
                {ok,P2} = net_vif:open(eth2),

                io:format("plug: ~w|~w\n", [P1,P2]),
                plug(P1, P2)
        end).

plug(P1, P2) ->
        plug(P1, P2, 1000).

plug(P1, P2, 0) ->
        erlang:garbage_collect(),
        plug(P1, P2);

plug(P1, P2, N) ->
        receive
        {P1,{data,Frame}} ->
                erlang:port_command(P2, Frame),
                plug(P1, P2, N -1);
        {P2,{data,Frame}} ->
                erlang:port_command(P1, Frame),
                plug(P1, P2, N -1)
        end.
```

Unsurprisingly, nullx forwards packets faster.

Parameter | Value 
----------|------:
Throughput (TCP) | 1.05Gbits/s
Latency | 850<!--$\mu$-->s
[nullx performance results][nullx_perf]

The time a frame spends inside the Erlang VM is only 5-10<!--$\mu$-->s. This is
the lower bound for the LINC switch.

# Future directions

The LINC on LING project may progress in the following directions:

## Faster LINC backend

The fast path of the switch should be implemented as a set of functions that use
the Erlang pattern matching directly to make decisions about packets. The code
of this functions should be reloaded when the configuration of the switch
changes.

The implementation of the faster backend should start with the skeleton similar
to the nullx module discussed above. The latency and thus the throughput of the
skeleton module should be measured and any change to the code should be
associated with the additional latency it introduces. Thus, the performance of
the switch will be ensured from the start.

## LINC as a driver domain

The latencies associated with bridging virtual interfaces in Dom0 can be removed
by using 'PCI passthrough'. LING currently does not support this feature. PCI
passthrough capability can be added to LING and then LINC/LING can be configured
as a 'driver domain'. The LINC/LING driver domain will have an exclusive (and
fast) control over physical network interfaces and will switch traffic between
other domains and the world outside.

## Distributed LINC

The LINC switch supports multiple logical switches. In the case of LINC/LING, it
is probably not the best architecture. A better alternative is to have each
logical switch running on a separate virtual machine inside a dedicated Xen domain.
This way, the switching task may span multiple cores.

# Appendix

## Fixed incompatibilities [incomp] ##

1. erlang:fun\_info(F, name) returned `{name,[]}`. Fixed to duplicate the
behaviour of BEAM. Now the call returns the compiler-generated function name,
such as `{name,'-expr/5-fun-1-'}`.

1. erlang:is\_builtin/3 function not implemented. LING has this function in a
different module. An implementation added as:

```
	%% erlang.erl
	is_builtin(M, F, A) ->
		ling_bifs:is_builtin(M, F, A).
```

1. erlang:load\_module/3 unable to load .beam code. LING and BEAM have
incompatible (yet similar) bytecode. The .beam-.ling conversion used to happen
at the build service only. The ability to do the .beam-.ling transformation
locally added.

1. The compiled code for standard libraries did not contain any debugging
information. This prevented meck framework from working. The standard libraries
were regenerated with the `debug\_info` option. After running the tests the
change was rolled back as it affected the image size.

1. erlang:round/1 throws `badarg` if the argument is already an integer. Fixed.

## Porting changes

The following changes were introduced to the LINC switch sources:

1. linc\_ofconfig:init() edited to stop mnesia from using `disc\_copies`.
Persistent copies require a writable filesystem. While such filesystems are
possible with LING it complicates configuration and does not help performance
testing.

2. linc\_us3\_actions\_tests test case related to linc\_us3\_packet\_edit:set\_field()
fixed as the function expects a binary as its arguments. There are other 'out of
sync' tests that were not fixed.

3. A new 'port type' <!-- --- --> vif <!-- --- --> was introduced. The
corresponding code snippets were added to linc\_us4\_port.erl and
linc\_us4\_port\_native.erl.

## Steps to reproduce the baseline case [reprod1] ##

1. Create two virtual machines, setup bridges as shown in the picture.
2. Run scripts/setup-network.sh perf
3. Start LINC/BEAM with 'make devx'
4. Run scripts/setup-network.sh taps
5. Run cd scripts; of\_controller\_v4.sh -s simple\_iperf\_test
6. Exit the controller shell
7. Run 'iperf -s' in one vm and 'iperf -c <ip-addr>' in the other

## Steps to reproduce LINC/LING performance tests [reprod2] ##

1. Create two virtual machines, setup bridges as shown in the picture
1. Crate LINC/LING domain with 'make x'
1. Execute two command from the paste1 file in the LINC/LING shell
1. Run scripts/setup-network.sh ling
1. Run 'iperf -s' in one vm and 'iperf -c <ip-addr>' in the other

