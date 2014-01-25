
This is the journal of the activities to produce a faster backend for LINC
switch.

The general idea is to keep the complexity on the 'fast path' as little as
possible and use dynamical compilation/code loading when matching rules change.

----[01/15/14 15:06]------------------------------------------------------------

trace1 file shows the trace of a single ping between vms. The trace only shows
linc_us4_port function calls.

trace2 file contains a complete trace (exceptions/BIFs/calls) for a single ping
between vms.

A quick look shows that the following things happen on the fast path:

1. A new process spawned (erlang:spawn_link)
1. 7 objects looked up from ETS (ets:lookup)
1. A scanning ETS query run (ets:match_object)
1. An ETS table converted to a list (ets:tab2list)

Any of these should be avoided.

# LINC fast path

1. A frame retrieved from the mailbox using `{Port,{data,Frame}}` pattern.

1. Enter handle\_frame(Frame, SwitchId, PortNo, PortConfig).

1. Check if `no_recv` flag is present. This requires 2 calls to lists:member().

1. Decode the frame using linc_us4_packet:binary_to_record(). The Ethernet type,
the source and destination MAC addresses get extracted using pkt:decapsulate().

1. rx counters updated by calling ets:update_counter(). This is relatively slow
and unnecessary.

1. `no_packet_in` option checked. This requires upto two calls to
lists:member().

1. The call to linc_us4_routing:spawn_route(LincPkt) spawns a new process that
executes proc_lib:spawn_link(). A process per packet is very bad idea. The
processing of the frame by the initial thread is complete. Everything below
happens on the new spawned thread.

1. proc_lib:spawn_link() performs a few unwieldy preparatory steps in addition to
calling erlang:spawn_link(). All these are unnecessary.

1. Enter linc_us4_rounting:route().

1. A flow table is retrieved using tab2list(). This is totally unsatisfactory.
All rules get copied from ETS space to Erlang space. This is like making a
database dump for each query. BTW, tab2list() is a wrapper over
ets:match\_object() function.

1. The packet gets matched against all entries of the flow table. The
match\_flow\_entry() is simple.

1. More checks on the contents of the packet performed by
pkt\_fields\_match\_flow\_fields() function. The function applies a matching fun
to all flow fields.

1. The match fun applies yet another matching to all packet fields. Application
of these funs in N^2 operation of the number of fields. Even in the most trivial
case the matching takes about two dozen function invocations per flow table
entry.

1. The second flow table entry matches and ETS table flow\_table\_counters gets
updated. Yet another in-memory database update on the fast path.

1. linc_us4_instructions:apply() convert instructions from the flow table into
actions. The actions are applied by linc\_us4\_actions:apply\_list().

1. In our case the (only) action is to forward the frame to another port.
linc\_us4\_actions:apply\_list() interprets the action and calls
linc\_us4\_port:send().

1. The matching clause of linc\_us4\_port:send() retrieves pid of the process
associated with the port. This does two lookups to ETS tables. Bad.

1. The packets is sent to the process using gen\_server:cast(). The use of
gen\_server is not justified here. The trace shows four layers of function calls
before the message is actually sent and four more until the message reaches
handle\_cast(). All this can be avoided by using a bare process.

1. The frame is repackaged using pkt:encapsulate(). pkt:encapsulate()
recalculates the ip checksum. The function calls erlang:setelement() 6 times.
Each time the entire packet is copied.

1. Yet another update of the counters in ETS tables. This time it is tx
counters.

1. The frame is swallowed by the port. Done.

----[01/16/2014]----------------------------------------------------------------

The LING contains simple instrumentation that lets us measure the processing
latency for ping. The instrumentation assumes that the ping packet first appears
on eth1 then sent to eth2 then appears again on eth2 and sent to eth1. The time
the packet spends in processing is recorded and averaged.

# LINC switch baseline latency

The following are observed processing latencies (PL) for ping packets. LING is
configured for the full speed except that assertions are not suppressed. Each
run is an average for 10 ping roundtrips.

Run # | To/From | PL, us
------|------------
1 | --> | 189.1
 | <-- | 218.5
2 | --> | 192.1
 | <-- | 215.7
3 | --> | 178.5
 | <-- | 223.7
4 | --> | 197.5
 | <-- | 210.7
5 | --> | 196.8
 | <-- | 209.4
6 | --> | 184.7
 | <-- | 209.1
7 | --> | 185.7
 | <-- | 224.3
8 | --> | 208.8
 | <-- | 219.7
9 | --> | 205.7
 | <-- | 227.8
10 | --> | 165.7
 | <-- | 220.3

The summary of the above:

PL (all) = 204.2 +/- 7.5 us (95%)
PL (to) = 190.5 +/- 7.9 us (95%)
PL (from) = 217.9 +/- 4.1 us (95%)

The processing latency on the way back seems slightly higher. It looks like the
port 1 get a better treatment.

# nullx baseline latency (data as lists)

NB: the port did not have `binary` option set. All data were represented as
lists slowing things down

The following are processing latencies similar to above observed for the nullx
forwarding application.

Run | To/From | PL, us
----|---------|-------
1 | --> | 5.92
 | <-- | 6.85
2 | --> | 4.49
 | <-- | 10.21
3 | --> | 5.89
 | <-- | 10.14
4 | --> | 5.72
 | <-- | 9.59
5 | --> | 5.77
 | <-- | 10.02
6 | --> | 5.55
 | <-- | 9.71
7 | --> | 5.83
 | <-- | 9.53
8 | --> | 5.63
 | <-- | 10.05
9 | --> | 7.07
 | <-- | 8.61
10 | --> | 6.06
 | <-- | 7.80

The summary of the above:

PL (all) = 7.52 +/- 0.87 us (95%)
PL (to) = 5.79 +/- 0.39 us (95%)
PL (from) = 9.25 +/- 0.70 us (95%)

Again the traffic from port1 to port2 is preffered. This must be the effect of
the order of matching rules in the receive statement.

# nullx baseline latency (data as binary)

Similar to above but ports open with `binary` option.

Run | To/From | PL, us
----|---------|-------
1 | --> | 2.92 
 | <-- | 6.90
2 | --> | 5.05
 | <-- | 9.36
3 | --> | 3.22
 | <-- | 4.34
4 | --> | 3.68
 | <-- | 5.71
5 | --> | 4.35
 | <-- | 4.81
6 | --> | 3.95
 | <-- | 5.49
7 | --> | 3.38
 | <-- | 5.18
8 | --> | 3.98
 | <-- | 5.45
9 | --> | 3.73 
 | <-- | 5.29
10 | --> | 2.91
 | <-- | 4.40

And the summary:

PL (all) = 4.71 +/- 0.66 us (95%)
PL (to) = 3.72 +/- 0.41 us (95%)
PL (from) = 5.69 +/- 0.92 us (95%)

The nullx figures give us the low bound of the latency we can ever achieve. The
comparison with the similar test with the list representation shows a difference
of about 3us.

# Packet decapsulation/encapsulation

The experiment assesses the latency introduced by
pkt:encapsulate()/pkt:decapsulate() functions. The table shows the number of
encapsulate() operations and the number of decapsulation done for each packet
and the observed latency.

# decap | # encap | PL, us
10 | 10 | 332.5
5 | 15 | 441.4
15 | 5 | 210.0
5 | 5 | 170.9
5 | 10 | 307.0
10 | 5 | 198.4

The regression analysis shows that pkt:encapsulate() is much costlier than
pkt:decapsulate():

Function | Added latency, us
---------|------------------
pkt:decapsulate() | 4.1
pkt:encapsulate() | 27.0

If we continue to use the pkt:\* functions we need to look into
pkt:encapsulate() implementation.

# Message passing (barebone process)

nullx spawns a process per port and sends the frame to the peer process which in
its turn forwards it to the port it controls. This add a single instance of
'message passing' per frame.

PL (all) = 8.11 +/- 1.09 us (95%)
PL (to) = 6.93 +/- 1.57 us (95%)
PL (from) = 9.29 +/- 1.11 us (95%)

The observed latency says that the latency cost of message passing of a frame
is 3.4us.

# Message passing (gen_server)

The experiment is the same a previous except the processes are gen_servers.

PL (all) = 12.74 +/- 1.99 us (95%)
PL (to) = 8.78 +/- 0.96 us (95%)
PL (from) = 16.69 +/- 1.69 us (95%)

The latency cost of message passing using gen_servers is 8.0us.

# ETS lookup

10 lookups to a table that contains 100 records added to the fast path.

PL (all) = 33.81 +/- 2.68 us (95%)
PL (to) = 30.38 +/- 1.95 us (95%)
PL (from) = 37.24 +/- 3.98 us (95%)

Thus the latency cost of a single small ETS table lookup is 2.9us.

Let us increase the size of the test table to 10000 records.

PL (all) = 51.64 +/- 3.05 us (95%)
PL (to) = 45.66 +/- 2.50 us (95%)
PL (from) = 57.62 +/- 1.90 us (95%)

The bigger ETS table lookup has a latency cost of 4.7us.

# ETS update_counter

10 update_counter() calls to a table that contains 100 recored added to the
forwarding path.

PL (all) = 40.98 +/- 2.27 us (95%)
PL (to) = 41.10 +/- 3.38 us (95%)
PL (from) = 40.86 +/- 3.04 us (95%)

The latency cost of the update_counter() on a small ETS table is 3.6us.
ETS is generally fast.

# ETS tab2list

A tab2list() call for a table with 100 elements added to the fast path.

PL (all) = 37.03 +/- 1.68 us (95%)
PL (to) = 38.03 +/- 2.84 us (95%)
PL (from) = 36.02 +/- 1.58 us (95%)

The latency cost a single tab2list() operation on the table with 100 records is
32.2us. This cost grows nearly linearly from the table size.

# Pattern matching (routing table)

A function constructed that does matching similar to Iternet routing. The
function has 10 clauses similar to this:


route(<<_Eth:14/binary,_Ip:12/binary,_Src:4/binary,128,91,_/binary>>) -> r1;
route(<<_Eth:14/binary,_Ip:12/binary,_Src:4/binary,130,91,_/binary>>) -> r2;
route(<<_Eth:14/binary,_Ip:12/binary,_Src:4/binary,158,130,19,_/binary>>) -> r3;
route(<<_Eth:14/binary,_Ip:12/binary,_Src:4/binary,158,130,18,_/binary>>) -> r4;
route(<<_Eth:14/binary,_Ip:12/binary,_Src:4/binary,158,130,17,_/binary>>) -> r5;
route(<<_Eth:14/binary,_Ip:12/binary,_Src:4/binary,158,130,_/binary>>) -> r6;
route(<<_Eth:14/binary,_Ip:12/binary,_Src:4/binary,165,123,243,_/binary>>) -> r7;
route(<<_Eth:14/binary,_Ip:12/binary,_Src:4/binary,165,123,_/binary>>) -> r8;
route(<<_Eth:14/binary,_Ip:12/binary,_Src:4/binary,192,5,44,_/binary>>) -> r9;
route(<<_Eth:14/binary,_Ip:12/binary,_Src:4/binary,192,84,2,_/binary>>) -> r10;
route(_) -> drop.

The date has been taken from the real CIDR records published by University of
Pennsylvania. 10 calls to the function added to the fast path.

PL (all) = 7.45 +/- 1.39 us (95%)
PL (to) = 4.95 +/- 0.21 us (95%)
PL (from) = 9.95 +/- 1.68 us (95%)

The calculation of the added latency reveals that the call is cheap - 0.3us.

Let us construct a much longer 'routing table'.

A new routing function generated from the real CIDR data of (a portion?) of
CHINANET backbone. The function now contains 912 clauses.

The results are encouraging:

PL (all) = 8.74 +/- 0.92 us (95%)
PL (to) = 7.15 +/- 1.11 us (95%)
PL (from) = 10.33 +/- 0.46 us (95%)

The cost of running the matching function for 912 'rules' is 4.0us.

# Is OpenFlow spec implementable?

Below are the notes taken while reading the latest OpenFlow specification
looking for parts applicable to the fast path of the switch. The notes discuss
how to implement the applicable functions using the suggested approach and how
much latency the implementation may add.

* TBD: a flow table may get updated reactively (in response to the packet). This
may add a lengthy recompilation to the fast path.

* Multiple flow tables can be implemented as multiple functions. A flow table is
a matching function.

* Counters should be implemented as the arguments of the matching function. The
same goes to the meatadata passed from one flow table to the other.

* The switch should decrement TTL field and the checksum must be recalculated
because of this. We cannot forward unmodified packets.

* A meter can be mapped to a process that runs a dynamically-generated function.

* "The ingress port can be used when matching packets"

* The CONTROLLER port traffic should not be considered the fast path. Otherwise
we have to reimplement the whole switch.

* Forwarding to TABLE port is the same as calling the function of the topmost
flow table.

* What is the LOCAL port in the context of LINC/LING?

* A table-miss entry of a flow table nicely maps to a function clause with
underscore patterns for all arguments.

* What are IEEE requirements with respect to packet reordering?

* A priority of a flow entry influences the order of clauses of the matching
function and not used explicitly later.

* TBD: per-entry counters? These can not be mapped to arguments.

* We have to detect the overlapping entries with same priority and not include
these into the matching function. We may be able to use erl\_list for this.

* The flow entry expiry can be implemented by amount of time left until the
shortest-living entry expires and use this number as a timeout for the
receive statement that handles incoming packets. Upon timeout matching functions
should be recompiled to remove expired entries.

* TBD: the idle timeouts are related to per-entry counters.

* Group tables again maps nicely to functions.

* TBD: group counters?

* Meters should be implemented as processes that recalculate the current rate
and make decisions depending on its bands. Most probably, there is no need to
recompile the band table dynamically.


