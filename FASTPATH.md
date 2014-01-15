
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
associated with the port. This does to lookups in ETS tables. Bad.

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

