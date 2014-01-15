
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

