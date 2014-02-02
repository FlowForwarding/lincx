latex input:            mmd-article-header
Title:			The anatomy of a flow table
Author:			Maxim Kharchenko, Cloudozer LLP
Date:			31/01/2014
latex mode:				memoir
base header level:      2
use xelatex:            true
latex input:            mmd-article-begin-doc


Match field | Description
------------|------------
OFPXMT\_OFB\_IN\_PORT | Switch input port.
OFPXMT\_OFB\_IN\_PHY\_PORT | Switch physical input port.
OFPXMT\_OFB\_METADATA | Metadata passed between tables.
OFPXMT\_OFB\_ETH\_DST | Ethernet destination address.        
OFPXMT\_OFB\_ETH\_SRC | Ethernet source address. 
OFPXMT\_OFB\_ETH\_TYPE | Ethernet frame type. 
OFPXMT\_OFB\_VLAN\_VID | VLAN id. 
OFPXMT\_OFB\_VLAN\_PCP | VLAN priority. 
OFPXMT\_OFB\_IP\_DSCP | IP DSCP (6 bits in ToS field). 
OFPXMT\_OFB\_IP\_ECN | IP ECN (2 bits in ToS field). 
OFPXMT\_OFB\_IP\_PROTO | IP protocol. 
OFPXMT\_OFB\_IPV4\_SRC | IPv4 source address. 
OFPXMT\_OFB\_IPV4\_DST | IPv4 destination address. 
OFPXMT\_OFB\_TCP\_SRC | TCP source port. 
OFPXMT\_OFB\_TCP\_DST | TCP destination port. 
OFPXMT\_OFB\_UDP\_SRC | UDP source port. 
OFPXMT\_OFB\_UDP\_DST | UDP destination port. 
OFPXMT\_OFB\_SCTP\_SRC | SCTP source port. 
OFPXMT\_OFB\_SCTP\_DST | SCTP destination port. 
OFPXMT\_OFB\_ICMPV4\_TYPE | ICMP type. 
OFPXMT\_OFB\_ICMPV4\_CODE | ICMP code. 
OFPXMT\_OFB\_ARP\_OP | ARP opcode. 
OFPXMT\_OFB\_ARP\_SPA | ARP source IPv4 address. 
OFPXMT\_OFB\_ARP\_TPA | ARP target IPv4 address. 
OFPXMT\_OFB\_ARP\_SHA | ARP source hardware address. 
OFPXMT\_OFB\_ARP\_THA | ARP target hardware address. 
OFPXMT\_OFB\_IPV6\_SRC | IPv6 source address. 
OFPXMT\_OFB\_IPV6\_DST | IPv6 destination address. 
OFPXMT\_OFB\_IPV6\_FLABEL | IPv6 Flow Label 
OFPXMT\_OFB\_ICMPV6\_TYPE | ICMPv6 type. 
OFPXMT\_OFB\_ICMPV6\_CODE | ICMPv6 code. 
OFPXMT\_OFB\_IPV6\_ND\_TARGET | Target address for ND. 
OFPXMT\_OFB\_IPV6\_ND\_SLL | Source link-layer for ND. 
OFPXMT\_OFB\_IPV6\_ND\_TLL | Target link-layer for ND. 
OFPXMT\_OFB\_MPLS\_LABEL | MPLS label. 
OFPXMT\_OFB\_MPLS\_TC | MPLS TC. 
OFPXMT\_OFP\_MPLS\_BOS | MPLS BoS bit. 
OFPXMT\_OFB\_PBB\_ISID | PBB I-SID. 
OFPXMT\_OFB\_TUNNEL\_ID | Logical Port Metadata. 
OFPXMT\_OFB\_IPV6\_EXTHDR | IPv6 Extension Header pseudo-field 
OFPXMT\_OFB\_PBB\_UCA | PBB UCA header field. 


# OFPXMT\_OFB\_IN\_PORT, OFPXMT\_OFB\_IN\_PHY\_PORT

```
flow0(1 = _InPort, 2 = _InPhyPort,...) ->
	...
```

# OFPXMT\_OFB\_METADATA

```
flow0(..., 100 = _Metadata,...) ->
	...
```

# OFPXMT\_OFB\_ETH\_DST, OFPXMT\_OFB\_ETH\_SRC, OFPXMT\_OFB\_ETH\_TYPE

```
flow0(..., <<0,1,2,3,4,5,_/binary>> = _EthHdr,...) ->
	...
flow0(..., <<_:6/binary,0,1,2,3,4,5,_/binary>> = _EthHdr,...) ->
	...
flow0(..., <<_:12/binary,16#80,0,_/binary>> = _EthType,...) ->
	...
```

# OFPXMT\_OFB\_VLAN\_VID, OFPXMT\_OFB\_VLAN\_PCP

```
flow0(..., <<_:12/binary,16#81,0,_:20,42:12,_/binary>> = _EthType,...) ->
	...
flow0(..., <<_:12/binary,16#81,0,_:16,0:3,_/bits>> = _EthType,...) ->
	...
```

# OFPXMT\_OFB\_IP\_DSCP, OFPXMT\_OFB\_IP\_ECN, OFPXMT\_OFB\_IP\_PROTO

```
flow0(..., <<_,0:6,_/bits>> = _Ip4Hdr, none = _Ip6Hdr,...) ->
	...
flow0(..., <<_:14,0:2,_/binary>> = _Ip4Hdr, none = _Ip6Hdr,...) ->
	...
flow0(..., <<_:10/binary,42:16,_/binary>> = _Ip4Hdr, none = _Ip6Hdr,...) ->
	...
flow0(..., none = _Ip4Hdr, <<_:4,0:6,_/bits>> = _Ip6Hdr,...) ->
	...
flow0(..., none = _Ip4Hdr, <<_:10,0:2,_/bits>> = _Ip6Hdr,...) ->
	...
flow0(..., none = _Ip4Hdr, <<_:6/binary,42:16,_/binary>> = _Ip6Hdr,...) ->
	...
```

# OFPXMT\_OFB\_IPV4\_SRC, OFPXMT\_OFB\_IPV4\_DST

```
flow0(..., <<_:12/binary,1,2,3,4,_/binary>> = _Ip4Hdr,...) ->
	...
flow0(..., <<_:16/binary,1,2,3,4,_/binary>> = _Ip4Hdr,...) ->
	...
```

# OFPXMT\_OFB\_TCP\_SRC, OFPXMT\_OFB\_TCP\_DST

```
flow0(..., <<1,2,_/binary>> = _TcpHdr,...) ->
	...
flow0(..., <<_,_,1,2,_/binary>> = _TcpHdr,...) ->
	...
```

# OFPXMT\_OFB\_UDP\_SRC, OFPXMT\_OFB\_UDP\_DST

```
flow0(..., <<1,2,_/binary>> = _UdpHdr,...) ->
	...
flow0(..., <<_,_,1,2,_/binary>> = _UdpHdr,...) ->
	...
```

# OFPXMT\_OFB\_SCTP\_SRC, OFPXMT\_OFB\_SCTP\_DST

```
flow0(..., <<1,2,_/binary>> = _SctpHdr,...) ->
	...
flow0(..., <<_,_,1,2,_/binary>> = _SctpHdr,...) ->
	...
```

# OFPXMT\_OFB\_ICMPV4\_TYPE, OFPXMT\_OFB\_ICMPV4\_CODE

```
flow0(..., <<42,_/binary>> = _IcmpHdr,...) ->
	...
flow0(..., <<_,42,_/binary>> = _IcmpHdr,...) ->
	...
```

# OFPXMT\_OFB\_ARP\_OP, OFPXMT\_OFB\_ARP\_SPA, OFPXMT\_OFB\_ARP\_TPA

```
flow0(..., <<_:6/binary,42,_/binary>> = _ArpHdr,...) ->
	...
flow0(..., <<_:14/binary,1,2,3,4,_/binary>> = _ArpHdr,...) ->
	...
flow0(..., <<_:24/binary,1,2,3,4,_/binary>> = _ArpHdr,...) ->
	...
```

# OFPXMT\_OFB\_ARP\_SHA, OFPXMT\_OFB\_ARP\_THA

```
flow0(..., <<_:8/binary,0,1,2,3,4,5,_/binary>> = _ArpHdr,...) ->
	...
flow0(..., <<_:18/binary,0,1,2,3,4,5,_/binary>> = _ArpHdr,...) ->
	...
```

# OFPXMT\_OFB\_IPV6\_SRC, OFPXMT\_OFB\_IPV6\_DST, OFPXMT\_OFB\_IPV6\_FLABEL

```
flow0(..., <<_:8/binary,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,_/binary>> = _Ip6Hdr,...) ->
	...
flow0(..., <<_:24/binary,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,_/bianry>> = _Ip6Hdr,...) ->
	...
flow0(..., <<_:12,42:20,_/binary>> = _Ip6Hdr,...) -> ...
```

# OFPXMT\_OFB\_ICMPV6\_TYPE, OFPXMT\_OFB\_ICMPV6\_CODE

```
flow0(..., <<42,_/binary>> = _Icmp6Hdr) ->
	...
flow0(..., <<_,42,_/binary>> = _Icmp6Hdr) ->
	...
```

# OFPXMT\_OFB\_IPV6\_ND\_TARGET, OFPXMT\_OFB\_IPV6\_ND\_SLL, OFPXMT\_OFB\_IPV6\_ND\_TLL

```
flow0(..., <<135,_:7/binary,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,_/binary>> = _Icmp6Hdr) ->
	...
flow0(..., <<136,_:7/binary,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,_/binary>> = _Icmp6Hdr) ->
	...
flow0(..., <<135,_/binary>> = _Icmp6Hdr, 42 = _Icmp6OptSll) ->
	...
flow0(..., <<136,_/binary>> = _Icmp6Hdr, 42 = _Icmp6OptTll) ->
	...
```

# OFPXMT\_OFB\_MPLS\_LABEL, OFPXMT\_OFB\_MPLS\_TC, OFPXMT\_OFP\_MPLS\_BOS

```
flow0(..., <<42:20,_/bits>> = _MplsHdr,...) ->
	...
flow0(..., <<_:20,0:3,_/bits>> = _MplsHdr,...) ->
	...
flow0(..., <<_:23,0:1,_/binary>> = _MplsHdr,...) ->
	...
```

TODO: Set pre-requisites for EthType

# OFPXMT\_OFB\_PBB\_ISID

```
flow0(..., <<_:19/binary,0,1,2,_/binary>> = _EthHdr,...) ->
	...
```

TODO: Set pre-requisites for EthType

# OFPXMT\_OFB\_TUNNEL\_ID

```
flow0(..., 42 = _TunId,...) ->
	...
```

# OFPXMT\_OFB\_IPV6\_EXTHDR

```
flow0(..., <<1:1,_:1,_:1,_:1,_:1,0:1,_:1,_:1,_:1>> = _Ip6ExtHdr,...) ->
	...
```

# OFPXMT\_OFB\_PBB\_UCA

TODO: it must be a one-bit field close to I-SID

# Matching function arguments

Argument | Type | Description
---------|------|------------
InPort | N | Input port
InPhyPort | B | Input physical port
Metadata | N | Metadata
EthHdr | B | Ethernet header
Ip4Hdr | B | IP v4 header
Ip6Hdr | B | IP v6 header
TcpHdr | B | TCP header
UdpHdr | B | UDP header
SctpHdr | B | SCTP header
IcmpHdr | B | ICMP header
ArpHdr | B | ARP header
Icmp6Hdr | B | ICMP v6 header
Icmp6OptSll | N | ICMP v6 source link layer option
Icmp6OptTll | N | ICMP v6 target link layer option
MplsHdr | B | MPLS header
TunId | N | Tunnel Id
Ip6ExtHdr | B | IP v6 extension header

