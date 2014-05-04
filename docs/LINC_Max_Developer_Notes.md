
# Introduction

The present document is a collection of assorted notes that should help you
understand the inner workings of the new faster LINC switch backend ---
linc\_max.

The linc\_max backend is work in progress and certain its features are
unimplemented/incomplete. The documents mentions such features and outline the
path to completion.

# Fast path

linc\_max clearly distinguishes between the fast path and the rest of the
switch. All fast path functions are touched thousands or millions times per
seconds and thus implemented differently from their slow cousins. The emphasis
is on small memory footprint, GC avoidance, and generation of efficient
bytecode. This sometimes makes the code of the fast path repeatative or obscure.

# Blaze process

The fast path is encapsulated in a single process referred to as the 'blaze'
process below. There must be a blaze process per logical switch. Currently this
is not supported due to naming of modules implementing flow tables.

The blaze process starts with the call to
`linc_max_fast_path:start(SwitchConfig, FlowTab0)`. SwitchConfig contains the
portion of the configuration pertinent for a particular logical switch. FlowTab0
is the name of the module for the first flow table in the pipeline.

Upon startup, the blaze process opens all ports mentioned in the configuration.
Note that ports are not wrapped with Erlang processes. Erlang ports that
correspond to OpenFlow ports are accessed directly by the blaze process.

The blaze() function is the central loop of the blaze process. It receives
messages from Erlang ports, translates them into OpenFlow ports, and initiate
the flow processing.

The processing of the incoming packets starts with pre-parsing. Preparsing
converts the packet into a series of subbinaries that represent various
protocol headers of the packet. The following headers/fields are recognized by
the preparser:

* Ethernet header
* VLAN tag
* PBB tag
* MPLS tag
* IPv4 header
* IPv6 header
* IPv6 extension header bits
* IP traffic class
* IP protocol
* ARP message
* ICMP message
* ICMPv6 message
* ICMPv6 ND SLL
* ICMPv6 ND TLL
* TCP header
* UDP header
* SCTP header

All these headers/fields are either small integers or subbinaries and thus
consume relatively little memory.

All these headers/fields as well as other information, such as input port number
or TunnelId are passed to the matching function. If certain headers/fields are
not present in the packet, then its value is 'undefined'. The matching function
is invoked directly by preparser code and the `linc_max_preparser:inject()`
returns the outcome of the matching. The possible outcomes are either the action
set or the `miss` atom.

The blaze process gets 'reignited' after processing certain number of packets
(16384). After processing this many packets the blaze process copies itself and
reconnect all the ports to the copy. Afterwards it drains remaining packets and
reinserts them into the copy&#8217;s mailbox. Then the blaze process exits and
the copy continues in its stead.

Reingiting the blaze process avoids much of the overhead related to garbage
collection. The garbage collection strategy of the Erlang VM, including LING, is
optimized for applications, radically different from wire speed network
switching.

INCOMPLETE: packet processing on table miss.

# Matching function

The dynamically-generated matching function is the cornerstone of the speed-up
achieved by the linc\_max backend. It allows the switch to use the pattern
matching facility of Erlang directly.

For each flow table of the switch there is a module named as flow_table_<N>,
where N is the ordinal number of the flow table. The module contains a single
function with `match/24` signature.

The slow portion of the backend keeps a high-level representation of a flow
table in a ETS table. The fast path never touches this (or any other) ETS table.
The `linx_max_flow:modify()` function now intercepts all changes to flow tables
and regenerates corresponding matching functions.

The matching function arguments are described in the table below.

N | Argument | Type | Description
-:|---------|------|------------
1 | Packet | binary | The whole packet being matched
2 | VlanTag | binary | VLAN tag
3 | EthType | integer | Ethernet type
4 | PbbTag | binary | PBB I-Tag
5 | MplsTag | binary | MPLS stack entry
6 | Ip4Hdr | binary | IPv4 header
7 | Ip6Hdr | binary | IPv6 header
8 | Ip6Ext | binary | IPv6 extesion headers bitmap (16-bit wide)
9 | IpTclass | binary | IP traffic class
10 | IpProto | integer | IP protocol number
11 | ArpMsg | binary | ARP header and data
12 | IcmpMsg | binary | ICMP header and data
13 | Icmp6Hdr | binary | ICMPv6 header
14 | Icmp6Sll | binary | ICMPv6 ND soure link-layer address
15 | Icmp6Tll | binary | ICMPv6 ND target link-layer address
16 | TcpHdr | binary | TCP header
17 | UdpHdr | binary | UDP header
18 | SctpHdr | binary | SCTP header
19 | Metadata | binary | Metadata
20 | InPort | integer | Input port
21 | InPhyPort | integer | Input physical port
22 | TunnelId | binary | Tunnel ID
23 | Actions | tuple | Action set
24 | Blaze | tuple | Auxilliary information (read-only)

Example:

match (a) ipv4_src = 192.168.1.0/27
	  (b) ip_ecn = 2
	  (c) tcp_dst = 80

The matching patterns are generated by `linc_max_generator:build_patters()`.
Three arguments of the matching function: Ip4Hdr, IpTclass, TcpHdr --- come into
play. (a) line matches on Ip4Hdr, (b) --- on IpTclass, (c) --- on TcpHdr. The
matching value and the mask are converted into a bit range (or multiple bit
ranges). Then the bit ranges are combined to optimize the pattern. The
`linc_max_generator:build_patterns()` returns the list of patterns for all
arguments of the matching function. For example, the following pattern is
generated for IPv4 header (Ip4Hdr):

```
{bin,0,
   [{bin_element,0,{var,0,'_'},{integer,0,96},[bits]},
	{bin_element,0,{integer,0,192},default,default},
	{bin_element,0,{integer,0,168},default,default},
	{bin_element,0,{integer,0,1},default,default},
	{bin_element,0,{integer,0,0},{integer,0,3},default},
	{bin_element,0,{var,0,'_'},default,[bits]}]},
```

Each flow entry of a flow table is mapped to a clause of the matching function.
The body of a clause contains a snippet to execute the intrsuctions.  It may
contain calls to functions in linc\_max\_fast\_actions module. If processing of
the message should continue to the next flow table, then the snippet ends with
the call to the match/24 function in the respective module.

The following functions can be used to explore the generation facility further:

linc\_max\_generator:flow\_table\_forms(FlowTab, FlowEnts) -> {ok,Forms}
:	generates 'forms' that constitute the matching function given flow entries
	in the of\_protocol format.

erl\_pp:forms(Forms) -> ok
:	prints the source code of the Erlang module given its abstarct forms.

linc\_max\_demo:generate\_flows(Spec) -> Flows
:	generates (random) flow entries for testing purposes. See the source code of
	linc\_max\_demo module.

# Packet modifying actions

The Apply-Actions instruction may request Set-Field and other actions that
modify the packet. Such packet-modifying actions are (should be) implemented
using the new module: linc\_max\_splicer. The splicer modifies the packet
without the complete decapsulation. Its operation is similar to
linc\_max\_preparser. The central function of the splicer module is
`edit(Packet, Field, Value)` that scans the packet to find the first occurence
of the Field and replaces (splices) it with the new Value.

Currently, only Set-Field action uses the splicer. Other packet-modifying
actions are implemented with a complete decapculation/encapsulation cycle. This
is slow.

INCOMPLETE: The splicing versions of TTL modification, Push/Pop tag actions.

# Overview of new/modified modules

linc_max_convert.erl
:	unmodified, phase out when all packet-modifying actions use splicer.

linc_max_demo.erl
:	new, testing only.

linc_max.erl
:	slighly modified, mostly start/1.

linc_max_fast_actions.erl
:	new, fast path, called by matching functions.

linc_max_fast_path.erl
:	new, blaze process.

linc_max_flow.erl
:	slightly modified, modified to sync matching functions.

linc_max_generator.erl
:	new, matching function generation.

linc_max_meter_sup.erl
:	unmodified, started but not used.

linc_max_packet.erl
:	unmodified, phase out from fast path --- use splicer for all actions.

linc_max_port.erl
:	modified, fills in port descriptions only, detached from fast path.

linc_max_port_native.erl
:	rewritten, opens native vif ports on LING VM.

linc_max_preparser.erl
:	new, preparses packets and calls matching functions.

linc_max_splicer.erl
:	new, 'in-place' packet modification.

linc_max_sup.erl
:	modified not to start port supervisor.

# Meters

Meters are not implemented yet. They should be represented as processes and
written targeting zero memory footprint.

INCOMPLETE: meters.

# Queueus

See Meters.

INCOMPLETE: queues.

# Counters

Counter represent a particular difficly for Erlang when the maximum performance
is needed. The portable approach is to use two (three) variables per counter to
avoid bignums and wrap the counting into a process (processes).

The maximum performance for counters can be achieved using the non-standard fast
counters of LING VM:

```
erlang:new_counter(Bits) -> Ref
erlang:update_counter(Ref, Incr) -> true
erlang:read_counter(Ref) -> N
erlang:release_counter(Ref)
```

INCOMPLETE: counters.

