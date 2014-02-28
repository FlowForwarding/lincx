
# Introduction

The present document is a collection of assorted notes that should help you
understand the inner workings of the new faster LINC switch backend ---
linc\_max.

The linc\_max backend is work in progress and certain its features are
unimplemented/incomplete. The documents mentions such features and outline the
path to completion.

# By module

TODO

# Fast path

linc\_max clearly distinguishes between the fast path and the rest of the
switch. All fast path functions are touched thousands or millions times per
seconds and thus implemented differently from their slow cousins. The emphasis
is on small memory footprint, GC avoidance, and generation of efficient
bytecode. This sometimes makes the code of the fast path repeatative or obscure.

# Blaze process

The fast path is encapsulated in a single process referred to as the 'blaze'
process below. There must be a blaze process per logical switch. Currently this
is not supported due to naming of modules implementing flow tables. See below.

The blaze process starts with the call to
`linc_max_fast_path:start(SwitchConfig, FlowTab0)`. SwitchConfig contains the
portion of the configuration pertinent for a particular logical switch. FlowTab0
is the name of the module for the first flow table in the pipeline.

Upon startup, the blaze process opens all ports mentioned in the configuration.
Note that there are no separate processes that wrap ports. Erlang ports that
correspond to OpenFlow ports are accessed directly by the blaze process.

The blaze() function is the central loop of the blaze process. It receives
messages from Erlang ports, translates them into OpenFlow ports, and initiate
the flow pipeline.

The processing of the incoming packets starts with pre-parsing. Preparsing
converts the packet into a series of subbinaries that correspond to various
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

All these headers/fields are either small integers or subbinaries that consume
relatively little memory.

All these headers/fields as well as other information, such as input port number
or TunnelId are passed to the matching function. If certain headers/fields are
not present in the packet, then the corresponding value is 'undefined'. The
matching function is invoked directly by preparser code and the
`linc_max_preparser:inject()` returns the outcome of the matching. The possible
outcomes are either the action set or the `miss` atom.

The blaze process gets 'reignited' after processing certain number of packets
(16384). After processing this many packets the blaze process copies itself and
reconnect all the ports to the copy. Afterwards it drain remaining packets and
reinsert them into the copy&#8217;s mailbox. The the blaze process exits and its
copy continues in its stead.

Reingiting the blaze process avoids much of the overhead related to garbage
collection. The garbage collection strategy of the Erlang VM, including LING, is
optimized for applications, radically different from wire speed network
switching.

INCOMPLETE: packet processing on table miss.

# Matching function

TODO

# Packet modifying actions

Packet modifying actions are (mostly) rewritten not to use pkt:*. 

_INCOMPLETE_


linc_max_convert.erl
linc_max_demo.erl
linc_max.erl
linc_max_fast_actions.erl
linc_max_fast_actions_tests.erl
linc_max_fast_path.erl
linc_max_flow.erl
linc_max_generator.erl
linc_max_generator_tests.erl
linc_max_meter_sup.erl
linc_max_packet.erl
linc_max_port.erl
linc_max_port_native.erl
linc_max_preparser.erl
linc_max_preparser_tests.erl
linc_max_splicer.erl
linc_max_splicer_tests.erl
linc_max_sup.erl
