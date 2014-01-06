
This is a journal of performance testing on LINC switch first on Erlang/OTP
platform, then on LING.

----[25/12/13 18:40]------------------------------------------------------------

We took the performance testing setup described in "Cost, Performance &
Flexibility in OpenFlow: Pick Three" paper. The setup is shown on a picture:

![testbed](testbed1.png)

Note: the ping.pcap packet has a checksum field set to 0. This makes it
unsuitable for testing as it gets discarded soon. A new ping.pcap recorded.

The testbed network should function in two modes: 1. vms has Internet
connectivity, ip forwarding is on in Dom0; 2. no ip forwarding, the only way for
vms to talk to each other is through the LINC switch. The mode 1. mostly needed
for installing new packages to Linuxes inside vms. Performance testing uses mode
2.

----[26/12/13 04:33]------------------------------------------------------------

A script called setup-network.sh has been added to switch between modes 1 and 2.

Two flows were added to LINC switch using controllers messages similar to
FlowMod described in the Ping demo document. Each message must have unique xid
and cookie or it is ignored by the switch.

It was double verified that it is the switch that provides the connectivity
between vms. The first run of iperf gives the first performance figure for
LINC/BEAM for the described setup - 140Mbits/s.

----[26/12/13 17:05]------------------------------------------------------------

Repeating the runs gives gradually decreasing throughtput. The controller process
consumes a lot of memory. It looks like the switch send all packets to the
controller for inspection. Flows must be configured differently?

----[27/12/13 03:17]------------------------------------------------------------

The iperf test setup added as a scenario to of_controller_v4.erl. The scenario
first removes all data flow then adds two flows from port 1 to port 2 and back.
New tests confirm the poor throughput observed earlier - 140Mbits/s. The
likely cause is the latency of 1.5ms that affects TCP throughput. When iperf
runs in UDP mode the throughput is much better - 810Mbits/s.

I have to start with the following (not spectacular) baseline.

## LINC/BEAM performance baseline

The following table summarize the performance of LINC switch running on Erlang/OTP.

Parameter | Value
----------|------
Throughput (TCP) | 140Mbits/s
Throughput (UDP) | 810Mbits/s
Latency | 1.4ms
RAM footprint | 450M (TCP), 5G (UDP)
CPU utilization | 220%

### Steps to reproduce

1. Create two virtual machines, setup bridges as shown on the picture.
2. Run scripts/setup-network.sh perf
3. Start LINC with 'make devx'
4. Run scripts/setup-network.sh taps
5. Run cd scripts; of_controller_v4.sh -s simple_iperf_test
6. Exit the controller shell
7. Run 'iperf -s' in one vm and 'iperf -c <ip-addr>' in the other

----[27/12/13 16:34]------------------------------------------------------------

Most probably, UDP figures are bogus as a part of the traffic gets lost. More
checks needed. The thoughput should be read on the receiving end.

----[31/12/13 01:14]------------------------------------------------------------

LINC/LING setup in the configuration similar to the one used for the baseline.
LINC runs in a separate DomU its vifs are bridged to corrsponding vifs of vm1
and vm2 -- see scripts/setup-network.sh (ling option).

The performance of LINC/LING is similar to that of LINC/BEAM in terms of
throughput and latency. The memory and CPU consumption is way lower.

The performance of LINC/LING improved greatly after reimplementing IP checksum
calculation as a BIF (binary:ip_checksum()). The performance of LINC switch can
be improved by getting rid of gen_server for ports. If it is necessary to wrap
LINC ports into a process a barebone process should be used. It is (much) faster 
than gen_server.

----[02/01/14]------------------------------------------------------------------

## nullx experiment

For this experiment the entire code of LINC replaced with the following stub:

	-module(nullx).
	-export([start/0]).

	start() ->
		spawn(fun() ->
			{ok,P1} = net_vif:open(eth1),
			{ok,P2} = net_vif:open(eth2),

			io:format("plug: ~w|~w\n", [P1,P2]),
			plug(P1, P2)
		end).

	plug(P1, P2) ->
		plug(P1, P2, 2000).

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

The server shovels data between two interfaces and no more. The manual garbage
collection is needed as the standard strategy does not prevent the overflow.

The nullx/LING show the latency of 850-950us and the TCP thoughput of
1.05Gbits/s. This means that a frame spends only 5-10us inside Erlang.

The experiment suggests that LINC implementation may not be performance-oriented
enough as the simplest rules introduce the latency of 220us.

The garbage collection behaviour is of much importance for the experiment and
for LINC performance. LINC may need non-standard options or entire new garbage
collection algorithm.

--------------------------------------------------------------------------------

The dependency between latency (ping) and TCP throughput (iperf) is tricky:

Latency | TCP throughput | Note
--------|----------------|-----
1.43ms | 140Mbits/s | LINC/BEAM
1.36ms | 170Mbits/s | LINC/LING
0.850ms | 1.05Gbits/s | nullx/LING
0.640ms | 20Gbits/s | same bridge - no switching

There seems to be a step-wise dependency when throughput grows abruptly when the
latency falls below 1ms.

----[03/01/14]------------------------------------------------------------------

LINC may benefit from the following approach to the Erlang implementation of its
performance critical part (fastpath):

1. Keep matching rules in a programmer friendly format. For example:

	[
		{src_mac,<<0,1,2,3,4,5>>},{forward,{port,1}},
		{dst_ip,{1,2,3,4}},drop}
		...
	]

2. Translates this representation to an Erlang module that does direct pattern
matching on the incoming frames, e.g.

	action(<<0,1,2,3,4,5,_/binary>> =Frame, P1, P2) -> forward(P1);
	action(<<_:12/binary,4,3,2,1,_/binary>> =Frame, P1, P2) -> drop;
	...

3. Compile and reload the module to apply the changes to the matching rules.

It will be difficult to beat this performance-wise even with hand-crafted C
code. The crucial step is the pattern matching compilation which is diffcult to
reproduce manually in most cases.


