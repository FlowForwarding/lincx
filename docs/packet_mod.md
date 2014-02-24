latex input:            mmd-article-header
Title:			Faster packet modification	
Author:			Maxim Kharchenko, Cloudozer LLP
Date:			24/02/2014
latex mode:				memoir
base header level:      2
use xelatex:            true
latex input:            mmd-article-begin-doc

# Summary

* Packet modification as reimplemented in linc\_max backend is 10x faster;
* Packet modification adds 1<!--$\mu s$--> to the processing delay.

# Status quo

The standard LINC switch backend --- linc\_us4 --- does not distringuish between
packet-modifying and simple forwarding flows. In linc\_us4, each incoming packet
undergoes a complete decapsulation. The matching machinery and actions operate
on the `record` representation of the packet. The packet gets encapsulated just
before exiting the switch. This makes a packet-modifying flow as fast (as slow)
as a flow that does simple forwarding.

The decapsulation/encapsulation is a costly operation. Its latency cost was
estimated at 4.1/27.0<!--$\mu s$-->.

# Preparser and splicer

The linc\_max backend matches packet without decapsulation. It does just enough
parsing of an incoming packet to be able to apply the matching rules. A new
module --- linc\_max\_preparser --- does the initial packet parsing.

The representation of the packet produced by the preparser is not suitable for
Set-Field actions. The packet modification is based on a new module ---
linc\_max\_splicer. The splicer is optimized to keep amount of copying to the
minimum.

# Splicer capabilities and speed

The [][caps] summarizes capabilities and latency costs of the splicer. Some
fields cannot be modified using the splicer because doing so will corrupt the
packet. The example is the `ip_proto` field. Such fields are marked 
`protected`.

As mentioned earlier, the latency depends on necessity to recalculate checksums
when the packet changes. The Ethernet header does not have a checksum, and its
modification costs about 400ns. The IPv4 header modification is costlier,
1.6<!--$\mu s$-->, because it has a checksum.

The modification of TCP/UDP headers may require even more time because of the
length of the data that require checksumming. SCTP is especially costly because
it uses a non-trivial algorithm (CRC32).

Field | Support | Delay |
------|:-------:|-------|
eth\_dst | yes | 0.41<!--$\mu s$--> |
eth\_src | yes | 0.35<!--$\mu s$--> |
vlan\_vid | yes | - |
vlan\_pcp  | yes | - |
ip\_dscp | yes | 1.65<!--$\mu s$--> |
ip\_ecn | yes | 1.56<!--$\mu s$--> |
ip\_proto | protected | - |
ipv4\_src | yes | 1.59<!--$\mu s$--> |
ipv4\_dst | yes | 1.62<!--$\mu s$--> |
tcp\_src | yes | - |
tcp\_dst | yes | - |
udp\_src | yes | - |
udp\_dst | yes | - |
sctp\_src | yes | - |
sctp\_dst | yes | - |
icmpv4\_type | protected | - |
icmpv4\_code | protected | - |
arp\_op | yes | - |
arp\_spa | yes | - |
arp\_tpa | yes | - |
arp\_sha | yes | - |
arp\_tha | yes | - |
ipv6\_src | yes | - |
ipv6\_dst | yes | - |
ipv6\_flabel | yes | - |
icmpv6\_type | protected | - |
icmpv6\_code | protected | - |
ipv6\_nd\_target | yes | - |
ipv6\_nd\_sll | unimplemented | - |
ipv6\_nd\_tll | unimplemented | - |
mpls\_label | yes | - |
mpls\_tc | yes | - |
mpls\_bos | protected | - |
pbb\_isid | yes | - |
pbb\_uca | yes | - |
[Splicer capabilities][caps]






 
