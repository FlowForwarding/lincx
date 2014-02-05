latex input:            mmd-article-header
Title:			The latency cost of Erlang primitives
Author:			Maxim Kharchenko, Cloudozer LLP
Date:			18/01/2014
latex mode:				memoir
base header level:      2
use xelatex:            true
latex input:            mmd-article-begin-doc

# Summary

1. Analysis of latency costs of Erlang primitives confirms the approach for the
faster LINC backend: use dynamically-recompiled rule matching function, avoid
ETS tables and message passing.

1. The new LINC backend should have an average processing delay of 25<!--$\mu s$-->.
The maximum delay is 150<!--$\mu s$-->.

# Overview

Each piece of network equipment introduces a delay on the path of a network
frame. The delay related to processing of the frame inside the box is referred
to as 'processing delay' in the literature[^globecom2004.pdf].

This document shows how adding Erlang constructs to the fast path of the
software switch affects the processing delay. The latency cost of an Erlang
construct is the increase in the processing delay associated with the use of the
construct.

# Baseline delay

The latency cost is the increase of the processing delay. If no processing
takes place in the switch there is still a delay associated with Erlang ports,
scheduling, etc. This baseline delay is established by nullx experiment. All
intervals are constructed for 95% confidence level.

\\[ Baseline\ delay = 4.71 \pm 0.66 \mu s \\]

The possibility to lower the baseline delay is small. The baseline delay is the
low bound of the latency we can ever achieve in the software switch based on
LING VM.

# Latency cost

To measure the latency cost of an Erlang primitive it was added to the fast path
of the nullx forwarder. The processing delay was measured by the instrumentation
of the LING VM. The baseline delay was subtracted to calculate the latency cost.
See [][latcost1] below.

Construct | LC, us | Note
----------|-------:|-----
pkt:decapsulate() | 4.1 |
pkt:encapsulate() | 27.0 | 
Message passing | 3.4 | barebone
Message passing | 8.0 | gen\_server
ETS lookup() | 2.9 | 100 records
ETS lookup() | 4.7 | 10,000 records
ETS update\_counter | 3.6 |
ETS tab2list() | 32.2 | 100 records
Pattern matching | 0.3 | 10 rules (UPENN CIDRs)
Pattern matching | 4.0 | 911 rules (CHINANET CIDRs)
[Latency cost][latcost1]

# Observations

1. pkt:encapsulate() is very slow. The trace shows that the function uses
setelement() many (six) times. Each time the record gets copied.

1. ets:tab2list() is slow. This is expected. The function is more suitable for a
database backup, not for the fast path of the software switch.

1. The pattern matching is quite efficient. A realistic routing over almost a
thousand rules adds only 4<!--$\mu s$-->.

# Setting targets

The following line of reasoning is not strictly rooted in evidence. Yet it is
better to have targets set using the gut feeling then to proceed without.

The nullx forwarder shows the throughput of 2.75Gbit/s. Most probably the
thoughtput in this case is capped by latencies of other elements on the path of
packets, such as Linux bridges. These latencies are out of scope of the current
effort.

The hardware routers have processing latencies starting at 10<!--$\mu s$-->.
The faster hardware may use ASICs or FPGAs. The boxes that do deep
packet inspection may have a processing delay of 1000<!--$\mu s$-->.

The low bound of the processing delay we can achieve is established
[earlier][Baseline delay]. We cannot go lower than 4.71<!--$\mu s$-->. There is
also a data point for unsatisfactorily long delay --- 204.2<!--$\mu s$-->.

Given the above the following target processing delays suggest themselves:

\\[ Average\ target\ delay = 25 \mu s \\]
\\[ Max\ target\ delay = 150 \mu s \\]

The faster backend we are going to build should never process a network frame longer
then the max target delay. The idea of configurations that can be used to verify
the target delays are as follows:

1. Average target delay: 4 ports, 20 rules
2. Max target delay: 32 ports, 200 rules

# Appendix

## The fast path as implemented

The processing delay of the LINC switch in the simplest confguration is as
follows:

\\[ LINC\ delay = 204.2 \pm 7.5 \mu s \\]

In the basic configuration of the LINC switch the following sequence of events
happens for each network frame:

1. A frame retrieved from the mailbox using `{Port,{data,Frame}}` pattern.

1. Enter handle\_frame().

1. Check if `no_recv` flag is present. This requires 2 calls to lists:member().

1. Decode the frame using linc\_us4\_packet:binary\_to\_record(). The Ethernet type,
the source and destination MAC addresses get extracted using pkt:decapsulate().

1. rx counters updated by calling ets:update\_counter().

1. `no_packet_in` option checked. This requires upto two calls to
lists:member().

1. The call to linc\_us4\_routing:spawn\_route(LincPkt) spawns a new process that
executes proc\_lib:spawn\_link(). Everything below happens on the new spawned
thread.

1. proc\_lib:spawn\_link() performs a few unwieldy preparatory steps in addition to
calling erlang:spawn\_link().

1. Enter linc\_us4\_rounting:route().

1. A flow table retrieved using tab2list().  All rules get copied from ETS
space to Erlang space. This is like making a database dump for each query. BTW,
tab2list() is a wrapper over ets:match\_object() function.

1. The packet gets matched against all entries of the flow table. The
match\_flow\_entry() is simple.

1. More checks on the contents of the packet performed by
pkt\_fields\_match\_flow\_fields() function. The function applies a matching fun
to all flow fields.

1. The match fun applies yet another matching to all packet fields.
Even in the most trivial case the matching takes about two dozen function
invocations per flow table entry.

1. The second flow table entry matches and ETS table flow\_table\_counters gets
updated.

1. linc\_us4\_instructions:apply() convert instructions from the flow table into
actions. The actions are applied by linc\_us4\_actions:apply\_list().

1. In our case the (only) action is to forward the frame to another port.
linc\_us4\_actions:apply\_list() interprets the action and calls
linc\_us4\_port:send().

1. The matching clause of linc\_us4\_port:send() retrieves pid of the process
associated with the port. This does two lookups to ETS tables.

1. The packets sent to the process using gen\_server:cast(). The trace shows
four layers of function calls before the message is actually sent and four more
until the message reaches handle\_cast().

1. The frame repackaged using pkt:encapsulate(). pkt:encapsulate()
recalculates the ip checksum. The function calls erlang:setelement() 6 times.

1. Yet another update of the counters in ETS tables. This time it is tx
counters.

1. The frame swallowed by the port. Done.

