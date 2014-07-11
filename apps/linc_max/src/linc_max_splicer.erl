%%
%%
%%

%% @author Cloudozer LLP. <info@cloudozer.com>
%% @copyright 2012 FlowForwarding.org
-module(linc_max_splicer).
-export([edit/3]).

-include_lib("pkt/include/pkt.hrl").
-include("pkt_max.hrl").
-include_lib("linc/include/linc_logger.hrl").

%%
%% Value is an integer for field narrower than 29 bits. For wider fields Value
%% is a binary.
%%

edit(Packet, Field, Value) ->
	ether(Packet, Field, Value).

ether(Packet, eth_dst, ValueBin) ->
	splice_binary(Packet, 0, 6, ValueBin);
ether(Packet, eth_src, ValueBin) ->
	splice_binary(Packet, 48, 6, ValueBin);
ether(Packet, eth_type, Value) ->
	splice_bits(Packet, 96, 16, Value);
ether(<<_:12/binary,Rest/binary>> =Packet, Field, Value) ->
	ether(Packet, Field, Value, 96, Rest).

ether(Packet, vlan_pcp, Value, Pos, <<?ETH_P_802_1Q:16,_/binary>>) ->
	splice_bits(Packet, Pos +16, 3, Value);
ether(Packet, vlan_vid, Value, Pos, <<?ETH_P_802_1Q:16,_/binary>>) ->
	splice_bits(Packet, Pos +16 +3, 13, Value);
ether(Packet, Field, Value, Pos, <<?ETH_P_802_1Q:16,_:16,Rest/binary>>) ->
	ether(Packet, Field, Value, Pos +16 +16, Rest);

ether(Packet, vlan_pcp, Value, Pos, <<?ETH_P_PBB_B:16,_/binary>>) ->
	splice_bits(Packet, Pos +16, 3, Value);
ether(Packet, vlan_vid, Value, Pos, <<?ETH_P_PBB_B:16,_/binary>>) ->
	splice_bits(Packet, Pos +16 +3, 13, Value);
ether(Packet, Field, Value, Pos, <<?ETH_P_PBB_B:16,_:16,Rest/binary>>) ->
	ether(Packet, Field, Value, Pos +16 +16, Rest);

ether(Packet, pbb_uca, Value, Pos, <<?ETH_P_PBB_I:16,_/binary>>) ->
	splice_bits(Packet, Pos +16 +4, 1, Value);
ether(Packet, pbb_isid, Value, Pos, <<?ETH_P_PBB_I:16,_/binary>>) ->
	splice_bits(Packet, Pos +16 +8, 24, Value);
ether(Packet, vlan_pcp, _Value, _Pos, <<?ETH_P_PBB_I:16,_/binary>>) ->
	?DEBUG("missing vlan_pcp"),
	Packet;
ether(Packet, vlan_vid, _Value, _Pos, <<?ETH_P_PBB_I:16,_/binary>>) ->
	?DEBUG("missing vlan_vid"),
	Packet;
ether(Packet, Field, Value, Pos, <<?ETH_P_PBB_I:16,_:4/binary,_:(6+6)/binary,Rest/binary>>) ->
	ether(Packet, Field, Value, Pos +16 +32 +48 +48, Rest);

ether(Packet, mpls_label, Value, Pos, <<?ETH_P_MPLS_UNI:16,_/binary>>) ->
	splice_bits(Packet, Pos +16, 20, Value);
ether(Packet, mpls_tc, Value, Pos, <<?ETH_P_MPLS_UNI:16,_/binary>>) ->
	splice_bits(Packet, Pos +16 +20, 3, Value);
ether(Packet, mpls_bos, Value, Pos, <<?ETH_P_MPLS_UNI:16,_/binary>>) ->
	splice_bits(Packet, Pos +16 +20 +3, 1, Value);
ether(Packet, mpls_label, Value, Pos, <<?ETH_P_MPLS_MULTI:16,_/binary>>) ->
	splice_bits(Packet, Pos +16, 20, Value);
ether(Packet, mpls_tc, Value, Pos, <<?ETH_P_MPLS_MULTI:16,_/binary>>) ->
	splice_bits(Packet, Pos +16 +20, 3, Value);
ether(Packet, mpls_bos, Value, Pos, <<?ETH_P_MPLS_MULTI:16,_/binary>>) ->
	splice_bits(Packet, Pos +16 +20 +3, 1, Value);
%% MPLS tag is always after VLAN/PBB
ether(Packet, vlan_pcp, _Value, _Pos, <<?ETH_P_MPLS_UNI:16,_Rest/binary>>) ->
	?DEBUG("missing vlan_pcp"),
	Packet;
ether(Packet, vlan_vid, _Value, _Pos, <<?ETH_P_MPLS_UNI:16,_Rest/binary>>) ->
	?DEBUG("missing vlan_vid"),
	Packet;
ether(Packet, pbb_uca, _Value, _Pos, <<?ETH_P_MPLS_UNI:16,_Rest/binary>>) ->
	?DEBUG("missing pbb_uca"),
	Packet;
ether(Packet, pbb_isid, _Value, _Pos, <<?ETH_P_MPLS_UNI:16,_Rest/binary>>) ->
	?DEBUG("missing pbb_isid"),
	Packet;
ether(Packet, vlan_pcp, _Value, _Pos, <<?ETH_P_MPLS_MULTI:16,_Rest/binary>>) ->
	?DEBUG("missing vlan_pcp"),
	Packet;
ether(Packet, vlan_vid, _Value, _Pos, <<?ETH_P_MPLS_MULTI:16,_Rest/binary>>) ->
	?DEBUG("missing vlan_vid"),
	Packet;
ether(Packet, pbb_uca, _Value, _Pos, <<?ETH_P_MPLS_MULTI:16,_Rest/binary>>) ->
	?DEBUG("missing pbb_uca"),
	Packet;
ether(Packet, pbb_isid, _Value, _Pos, <<?ETH_P_MPLS_MULTI:16,_Rest/binary>>) ->
	?DEBUG("missing pbb_isid"),
	Packet;
ether(Packet, Field, Value, Pos, <<?ETH_P_MPLS_UNI:16,Rest/binary>>) ->
	mpls(Packet, Field, Value, Pos +16, Rest);	%% skip mpls stack
ether(Packet, Field, Value, Pos, <<?ETH_P_MPLS_MULTI:16,Rest/binary>>) ->
	mpls(Packet, Field, Value, Pos +16, Rest);	%% skip mpls stack
	
ether(Packet, Field, Value, Pos, <<?ETH_P_IP:16,Rest/binary>>) ->
	ipv4(Packet, Field, Value, Pos +16, Rest);
ether(Packet, Field, Value, Pos, <<?ETH_P_IPV6:16,Rest/binary>>) ->
	ipv6(Packet, Field, Value, Pos +16, Rest);

ether(Packet, arp_op, Value, Pos, <<?ETH_P_ARP:16,_/binary>>) ->
	splice_bits(Packet, Pos +16 +48, 16, Value);
ether(Packet, arp_spa, ValueBin, Pos, <<?ETH_P_ARP:16,_/binary>>) ->
	splice_binary(Packet, Pos +16 +112, 4, ValueBin);
ether(Packet, arp_tpa, ValueBin, Pos, <<?ETH_P_ARP:16,_/binary>>) ->
	splice_binary(Packet, Pos +16 +192, 4, ValueBin);
ether(Packet, arp_sha, ValueBin, Pos, <<?ETH_P_ARP:16,_/binary>>) ->
	splice_binary(Packet, Pos +16 +64, 6, ValueBin);
ether(Packet, arp_tha, ValueBin, Pos, <<?ETH_P_ARP:16,_/binary>>) ->
	splice_binary(Packet, Pos +16 +144, 6, ValueBin);

ether(Packet, _Field, _Value, _Pos, _Rest) ->
	?DEBUG("missing ether"),
	Packet.

mpls(Packet, Field, Value, Pos, <<_:23,1:1,_,Rest/binary>>) ->
	%% at the bottom
	case Rest of
	<<4:4,_/bits>> ->
		ipv4(Packet, Field, Value, Pos +32, Rest);
	<<6:4,_/bits>> ->
		ipv6(Packet, Field, Value, Pos +32, Rest)
	end;
mpls(Packet, Field, Value, Pos, <<_:4/binary,Rest/binary>>) ->
	%% skip mpls tag
	mpls(Packet, Field, Value, Pos +32, Rest).

ipv4(Packet, ip_dscp, Value, Pos, _Rest) ->
	splice_ipv4_header_bits(Packet, Pos, 8, 6, Value);
ipv4(Packet, ip_ecn, Value, Pos, _Rest) ->
	splice_ipv4_header_bits(Packet, Pos, 14, 2, Value);
ipv4(Packet, ip_proto, Value, Pos, _Rest) ->
	splice_ipv4_header_bits(Packet, Pos, 72, 8, Value);
ipv4(Packet, ipv4_src, ValueBin, Pos, _Rest) ->
	splice_ipv4_header_binary(Packet, Pos, 96, 4, ValueBin);
ipv4(Packet, ipv4_dst, ValueBin, Pos, _Rest) ->
	splice_ipv4_header_binary(Packet, Pos, 128, 4, ValueBin);

%% Cannot happen: vlan, pbb, mpls, arp
ipv4(Packet, vlan_pcp, _Value, _Pos, _Rest) ->
	?DEBUG("missing ipv4 vlan_pcp"),
	Packet;
ipv4(Packet, vlan_vid, _Value, _Pos, _Rest) ->
	?DEBUG("missing ipv4 vlan_vid"),
	Packet;
ipv4(Packet, pbb_uca, _Value, _Pos, _Rest) ->
	?DEBUG("missing ipv4 pbb_uca"),
	Packet;
ipv4(Packet, pbb_isid, _Value, _Pos, _Rest) ->
	?DEBUG("missing ipv4 pbb_isid"),
	Packet;
ipv4(Packet, mpls_label, _Value, _Pos, _Rest) ->
	?DEBUG("missing ipv4 mpls_label"),
	Packet;
ipv4(Packet, mpls_tc, _Value, _Pos, _Rest) ->
	?DEBUG("missing ipv4 mpls_tc"),
	Packet;
ipv4(Packet, mpls_bos, _Value, _Pos, _Rest) ->
	?DEBUG("missing ipv4 mpls_bos"),
	Packet;
ipv4(Packet, arp_op, _Value, _Pos, _Rest) ->
	?DEBUG("missing ipv4 arp_op"),
	Packet;
ipv4(Packet, arp_spa, _Value, _Pos, _Rest) ->
	?DEBUG("missing ipv4 arp_spa"),
	Packet;
ipv4(Packet, arp_tpa, _Value, _Pos, _Rest) ->
	?DEBUG("missing ipv4 arp_tpa"),
	Packet;
ipv4(Packet, arp_sha, _Value, _Pos, _Rest) ->
	?DEBUG("missing ipv4 arp_sha"),
	Packet;
ipv4(Packet, arp_tha, _Value, _Pos, _Rest) ->
	?DEBUG("missing ipv4 arp_tha"),
	Packet;

ipv4(Packet, Field, Value, Pos,
		<<_:4,			%% version (4)
		  IHL:4,
		  _:5/binary,	%% class, length, id
		  _:2,			%% flags
		  0:1,			%% no more fragments
		  0:13,			%% the first fragment
		  _,			%% ttl, 
		  Proto,
		  _:16,			%% checksum
		  SrcAddr:4/binary,
		  DstAddr:4/binary,
		  _/binary>> =HR) ->
	HLen = IHL *4,
	<<_Ip4Hdr:(HLen)/binary,Rest/binary>> = HR,
	%% SrcAddr and DstAddr are needed for possible checksum recalculation
	proto(Packet, Field, Value, Pos +HLen *8, Rest, Proto, SrcAddr, DstAddr);

ipv4(Packet, _Field, _Value, _Pos, _Rest) ->
	?DEBUG("fragmented ipv4"),
	Packet.

ipv6(Packet, ipv6_src, ValueBin, Pos, _Rest) ->
	Packet1 = splice_binary(Packet, Pos +64, 16, ValueBin),
	ether(Packet1, tcp_csum, undefined);
ipv6(Packet, ipv6_dst, ValueBin, Pos, _Rest) ->
	Packet1 = splice_binary(Packet, Pos +192, 16, ValueBin),
	ether(Packet1, tcp_csum, undefined);
ipv6(Packet, ipv6_flabel, Value, Pos, _Rest) ->
	splice_bits(Packet, Pos +12, 20, Value);

ipv6(Packet, ip_dscp, Value, Pos, _Rest) ->
	splice_bits(Packet, Pos +4, 6, Value);
ipv6(Packet, ip_ecn, Value, Pos, _Rest) ->
	splice_bits(Packet, Pos +10, 2, Value);
ipv6(Packet, ip_proto, _Value, _Pos, _Rest) ->
	?DEBUG("protected ipv6 ip_proto"),
	Packet;

%% Cannot happen: vlan, pbb, mpls, arp
ipv6(Packet, vlan_pcp, _Value, _Pos, _Rest) ->
	?DEBUG("missing ipv6 vlan_pcp"),
	Packet;
ipv6(Packet, vlan_vid, _Value, _Pos, _Rest) ->
	?DEBUG("missing ipv6 vlan_vid"),
	Packet;
ipv6(Packet, pbb_uca, _Value, _Pos, _Rest) ->
	?DEBUG("missing ipv6 pbb_uca"),
	Packet;
ipv6(Packet, pbb_isid, _Value, _Pos, _Rest) ->
	?DEBUG("missing ipv6 pbb_isid"),
	Packet;
ipv6(Packet, mpls_label, _Value, _Pos, _Rest) ->
	?DEBUG("missing ipv6 mpls_label"),
	Packet;
ipv6(Packet, mpls_tc, _Value, _Pos, _Rest) ->
	?DEBUG("missing ipv6 mpls_tc"),
	Packet;
ipv6(Packet, mpls_bos, _Value, _Pos, _Rest) ->
	?DEBUG("missing ipv6 mpls_bos"),
	Packet;
ipv6(Packet, arp_op, _Value, _Pos, _Rest) ->
	?DEBUG("missing ipv6 arp_op"),
	Packet;
ipv6(Packet, arp_spa, _Value, _Pos, _Rest) ->
	?DEBUG("missing ipv6 arp_spa"),
	Packet;
ipv6(Packet, arp_tpa, _Value, _Pos, _Rest) ->
	?DEBUG("missing ipv6 arp_tpa"),
	Packet;
ipv6(Packet, arp_sha, _Value, _Pos, _Rest) ->
	?DEBUG("missing ipv6 arp_sha"),
	Packet;
ipv6(Packet, arp_tha, _Value, _Pos, _Rest) ->
	?DEBUG("missing ipv6 arp_tha"),
	Packet;

%% skip IPv6 extension headers
ipv6(Packet, Field, Value, Pos,
		<<_:6/binary,Next,_,SrcAddr:16/binary,DstAddr:16/binary,Rest/binary>>) ->
	%% save src and dst addrs for pseudoheaders
	ipv6_skip(Packet, Field, Value, Pos +320, Rest, Next, SrcAddr, DstAddr).

ipv6_skip(Packet, Field, Value, Pos,
		<<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>,
				?IPV6_HDR_HOP_BY_HOP, SrcAddr, DstAddr) ->
	ipv6_skip(Packet, Field, Value, Pos +Len*64 +64, Rest, Next, SrcAddr, DstAddr);
ipv6_skip(Packet, Field, Value, Pos,
		<<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>,
				?IPV6_HDR_DEST_OPTS, SrcAddr, DstAddr) ->
	ipv6_skip(Packet, Field, Value, Pos +Len*64 +64, Rest, Next, SrcAddr, DstAddr);
ipv6_skip(Packet, Field, Value, Pos,
		<<Next,0,0,0,_:4/binary,Rest/binary>>,
				?IPV6_HDR_ROUTING, SrcAddr, DstAddr) ->
	%% empty routing list, see below
	ipv6_skip(Packet, Field, Value, Pos +64, Rest, Next, SrcAddr, DstAddr);
ipv6_skip(Packet, Field, Value, Pos,
		<<Next,Len,0,SegLeft,_:4/binary,AddrList:(Len)/binary-unit:64,Rest/binary>>,
				?IPV6_HDR_ROUTING, SrcAddr, _DstAddr) ->
	
	%% RFC2460:
	%%   If the IPv6 packet contains a Routing header, the Destination
    %%   Address used in the pseudo-header is that of the final
    %%   destination.  At the originating node, that address will be in
    %%   the last element of the Routing header; at the recipient(s),
    %%   that address will be in the Destination Address field of the
    %%   IPv6 header.

	DstAddr = binary:part(AddrList, (SegLeft -1) *16, 16),
	ipv6_skip(Packet, Field, Value, Pos +Len*64 +64, Rest, Next, SrcAddr, DstAddr);
ipv6_skip(Packet, Field, Value, Pos,
		<<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>,
				?IPV6_HDR_FRAGMENT, SrcAddr, DstAddr) ->
	ipv6_skip(Packet, Field, Value, Pos +Len*64 +64, Rest, Next, SrcAddr, DstAddr);
ipv6_skip(Packet, Field, Value, Pos,
		<<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>,
				?IPV6_HDR_AUTH, SrcAddr, DstAddr) ->
	ipv6_skip(Packet, Field, Value, Pos +Len*64 +64, Rest, Next, SrcAddr, DstAddr);
ipv6_skip(Packet, Field, Value, Pos,
		<<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>,
				?IPV6_HDR_ESP, SrcAddr, DstAddr) ->
	ipv6_skip(Packet, Field, Value, Pos +Len*64 +64, Rest, Next, SrcAddr, DstAddr);
ipv6_skip(Packet, _Field, _Value, _Pos, _Rest,
				?IPV6_HDR_NO_NEXT_HEADER, _SrcAddr, _DstAddr) ->
	?DEBUG("missing ipv6 hdr_no_next"),
	Packet;
ipv6_skip(Packet, Field, Value, Pos, Rest, Proto, SrcAddr, DstAddr) ->
	proto(Packet, Field, Value, Pos, Rest, Proto, SrcAddr, DstAddr).
	
proto(Packet, Field, Value, Pos, Rest, ?IPPROTO_IP, _SrcAddr, _DstAddr) ->
	ipv4(Packet, Field, Value, Pos, Rest);
proto(Packet, Field, Value, Pos, Rest, ?IPPROTO_IPV6, _SrcAddr, _DstAddr) ->
	ipv6(Packet, Field, Value, Pos, Rest);
proto(Packet, tcp_csum, _Value, Pos,
		<<SrcPort:16,DstPort:16,_/binary>>, ?IPPROTO_TCP, SrcAddr, DstAddr) ->
	splice_tcp_header(Packet, Pos, SrcPort, DstPort, SrcAddr, DstAddr);
proto(Packet, tcp_csum, _Value, _Pos, _Rest, _Proto, _SrcAddr, _DstAddr) ->
	Packet;
proto(Packet, Field, Value, Pos,
		<<SrcPort:16,DstPort:16,_/binary>>, ?IPPROTO_TCP, SrcAddr, DstAddr) ->
	%% src and dst ports extracted to avoid copy during checksum recalculation
	tcp(Packet, Field, Value, Pos, SrcPort, DstPort, SrcAddr, DstAddr);
proto(Packet, Field, Value, Pos,
		<<SrcPort:16,DstPort:16,_/binary>>, ?IPPROTO_UDP, SrcAddr, DstAddr) ->
	%% src and dst ports extracted to avoid copy during checksum recalculation
	udp(Packet, Field, Value, Pos, SrcPort, DstPort, SrcAddr, DstAddr);
proto(Packet, Field, Value, Pos, _Rest, ?IPPROTO_ICMP, _SrcAddr, _DstAddr) ->
	icmp(Packet, Field, Value, Pos);
proto(Packet, Field, Value, Pos, <<Type,_/binary>>, ?IPPROTO_ICMPV6, SrcAddr, DstAddr) ->
	icmpv6(Packet, Field, Value, Pos, Type, SrcAddr, DstAddr);

proto(Packet, Field, Value, Pos,
		<<SrcPort:16,DstPort:16,_/binary>>, ?IPPROTO_SCTP, _SrcAddr, _DstAddr) ->
	sctp(Packet, Field, Value, Pos, SrcPort, DstPort).

tcp(Packet, tcp_src, Value, Pos, _SrcPort, DstPort, SrcAddr, DstAddr) ->
	splice_tcp_header(Packet, Pos, Value, DstPort, SrcAddr, DstAddr);
tcp(Packet, tcp_dst, Value, Pos, SrcPort, _DstPort, SrcAddr, DstAddr) ->
	splice_tcp_header(Packet, Pos, SrcPort, Value, SrcAddr, DstAddr);
tcp(Packet, _Field, _Value, _Pos, _SrcPort, _DstPort, _SrcAddr, _DstAddr) ->
	?DEBUG("missing tcp"),
	Packet.

udp(Packet, udp_src, Value, Pos, _SrcPort, DstPort, SrcAddr, DstAddr) ->
	splice_udp_header(Packet, Pos, Value, DstPort, SrcAddr, DstAddr);
udp(Packet, udp_dst, Value, Pos, SrcPort, _DstPort, SrcAddr, DstAddr) ->
	splice_udp_header(Packet, Pos, SrcPort, Value, SrcAddr, DstAddr);
udp(Packet, _Field, _Value, _Pos, _SrcPort, _DstPort, _SrcAddr, _DstAddr) ->
	?DEBUG("missing udp"),
	Packet.

sctp(Packet, sctp_src, Value, Pos, _SrcPort, DstPort) ->
	splice_sctp_header(Packet, Pos, Value, DstPort);
sctp(Packet, sctp_dst, Value, Pos, SrcPort, _DstPort) ->
	splice_sctp_header(Packet, Pos, SrcPort, Value);
sctp(Packet, _Field, _Value, _Pos, _SrcPort, _DstPort) ->
	?DEBUG("missing sctp"),
	Packet.

icmp(Packet, icmpv4_type, Type, Pos) ->
	<<Prefix:Pos/bits, _OldType, Code, _OldSum:16, Rest/binary>> = Packet,
	TypeCode = Type * 256 + Code,
	NewSum = checksum(Rest, all, TypeCode),
	<<Prefix/binary, Type, Code, NewSum:16, Rest/binary>>;

icmp(Packet, icmpv4_code, Code, Pos) ->
	<<Prefix:Pos/bits, Type, _OldCode, _OldSum:16, Rest/binary>> = Packet,
	TypeCode = Type * 256 + Code,
	NewSum = checksum(Rest, all, TypeCode),
	<<Prefix/binary, Type, Code, NewSum:16, Rest/binary>>;

icmp(Packet, _Field, _Value, _Pos) ->
	?DEBUG("missing icmp"),
	Packet.

icmpv6(Packet, icmpv6_type, Type, Pos, _Type, SrcAddr, DstAddr) ->
	<<Prefix:Pos/bits, _OldType, Code, _OldSum:16, Rest/binary>> = Packet,
	Icmpv6Len = byte_size(Rest) + 4,
	TypeCode = Type * 256 + Code,
	InitSum = pseudoheader(SrcAddr, DstAddr, ?IPPROTO_ICMPV6, Icmpv6Len) + TypeCode,
	NewSum = checksum(Rest, all, InitSum),
	<<Prefix/binary,TypeCode:16,NewSum:16,Rest/binary>>;

icmpv6(Packet, icmpv6_code, Code, Pos, _Type, SrcAddr, DstAddr) ->
	<<Prefix:Pos/bits, Type, _OldCode, _OldSum:16, Rest/binary>> = Packet,
	Icmpv6Len = byte_size(Rest) + 4,
	TypeCode = Type * 256 + Code,
	InitSum = pseudoheader(SrcAddr, DstAddr, ?IPPROTO_ICMPV6, Icmpv6Len) + TypeCode,
	NewSum = checksum(Rest, all, InitSum),
	<<Prefix/binary,TypeCode:16,NewSum:16,Rest/binary>>;

icmpv6(Packet, ipv6_nd_target, ValueBin, Pos, Type, SrcAddr, DstAddr)
	when Type =:= ?ICMPV6_NDP_NS orelse Type =:= ?ICMPV6_NDP_NA ->
	<<Prefix:Pos/bits,W1:16,_OldSum:16,W2:16,W3:16,_OldAddr:16/binary,Rest/binary>> =Packet,
	<<T1:16,T2:16,T3:16,T4:16,T5:16,T6:16,T7:16,T8:16>> =ValueBin,
	NdpLen = byte_size(Rest) +2 +2 +2 +2 +16,
	InitSum = pseudoheader(SrcAddr, DstAddr, ?IPPROTO_ICMPV6, NdpLen)
				+W1 +W2 +W3
				+T1 +T2 +T3 +T4 +T5 +T6 +T7 +T8,
	NewSum = checksum(Rest, all, InitSum),
	<<Prefix/binary,W1:16,NewSum:16,W2:16,W3:16,ValueBin/binary,Rest/binary>>;

icmpv6(Packet, ipv6_nd_sll, ValueBin, Pos, Type, SrcAddr, DstAddr)
	when Type =:= ?ICMPV6_NDP_NS orelse Type =:= ?ICMPV6_NDP_NA ->
	ipv6_nd(Packet, Pos, ?NDP_OPT_SLL, ValueBin, SrcAddr, DstAddr);
icmpv6(Packet, ipv6_nd_tll, ValueBin, Pos, Type, SrcAddr, DstAddr)
	when Type =:= ?ICMPV6_NDP_NS orelse Type =:= ?ICMPV6_NDP_NA ->
	ipv6_nd(Packet, Pos, ?NDP_OPT_TLL, ValueBin, SrcAddr, DstAddr);
icmpv6(Packet, _Field, _ValueBin, _Pos, _Type, _SrcAddr, _DstAddr) ->
	?DEBUG("missing icmpv6"),
	Packet.

ipv6_nd(Packet, Pos, Type, Value, SrcAddr, DstAddr) ->
	<<Prefix:Pos/bits,W1:16,_OldSum:16,W10:20/binary,Opts/binary>> = Packet,
	NdpLen = byte_size(Opts) +2 +2 +20,
	InitSum = pseudoheader(SrcAddr, DstAddr, ?IPPROTO_ICMPV6, NdpLen) + W1,
	NewOpts = ipv6_nd_opt(Opts, Type, Value, <<>>),
	Suffix = <<W10/binary,NewOpts/binary>>,
	NewSum = checksum(Suffix, all, InitSum),
	<<Prefix/binary,W1:16,NewSum:16,Suffix/binary>>.

ipv6_nd_opt(<<>>, _, _, Acc) ->
	Acc;
ipv6_nd_opt(<<T,L,V/binary>>, Type, Value, Acc) ->
	%% Neighbor Discovery options format:
	%% http://tools.ietf.org/html/rfc4861#section-4.6
	Size = L * 8 - 2,
	case V of
		<<OldValue:Size/binary, Rest/binary>> ->
			if
				T =:= Type ->
					ipv6_nd_opt(Rest, Type, Value, <<Acc/binary,T,L,Value/binary>>);
				true ->
					ipv6_nd_opt(Rest, Type, Value, <<Acc/binary,T,L,OldValue/binary>>)
			end;
		_ ->
			?DEBUG("missing ipv6_nd_opt"),
			<<Acc/binary,T,L,V/binary>>
	end;
ipv6_nd_opt(Rest, _, _, Acc) ->
	?DEBUG("missing ipv6_nd_opt"),
	<<Acc/binary,Rest/binary>>.

%%------------------------------------------------------------------------------

splice_binary(Packet, Pos, ByteLen, ValueBin) ->
	<<Prefix:(Pos)/bits,_:(ByteLen)/binary,Suffix/bits>> =Packet,
	<<Prefix/bits,ValueBin/binary,Suffix/bits>>.

splice_bits(Packet, Pos, BitLen, Value) ->
	<<Prefix:(Pos)/bits,_:(BitLen)/bits,Suffix/bits>> =Packet,
	<<Prefix/bits,Value:(BitLen),Suffix/bits>>.

%% splice and recalculate the checksum
splice_tcp_header(Packet, Pos, SrcPort, DstPort, SrcAddr, DstAddr) ->
	%% TCP header starts at Pos bit offset
	<<Prefix:(Pos)/bits,_OldSrcPort:16,_OldDstPort:16,Rest/binary>> =Packet,
	%% Rest contains partial TCP header and TCP data

	TcpLen = byte_size(Rest) +2 +2,	%% add port fields sizes
	InitSum = pseudoheader(SrcAddr, DstAddr, ?IPPROTO_TCP, TcpLen),

	%% Checksum is at byte offset 16 of the TCP header or its 8th word. The
	%% partial header does not have src and dst port fields. Thus we need to
	%% skip the 6th word when calculating the checksum.

	NewSum = checksum(Rest, 6, InitSum +SrcPort +DstPort),

	<<Shred:12/binary,_StaleSum:16,Rest1/binary>> =Rest,
	<<Prefix/binary,SrcPort:16,DstPort:16,Shred/binary,NewSum:16,Rest1/binary>>.

splice_udp_header(Packet, Pos, SrcPort, DstPort, SrcAddr, DstAddr) ->
	%% UDP header starts at Pos bit offset
	<<Prefix:(Pos)/bits,_Src:16,_Dst:16,UdpLen:16,_Sum:16,Data/binary>> =Packet,
	InitSum = pseudoheader(SrcAddr, DstAddr, ?IPPROTO_UDP, UdpLen),
	NewSum = checksum(Data, all, InitSum +SrcPort +DstPort +UdpLen), %% no skipping
	<<Prefix/binary,SrcPort:16,DstPort:16,UdpLen:16,NewSum:16,Data/binary>>.

splice_sctp_header(Packet, Pos, SrcPort, DstPort) ->
	<<Prefix:(Pos)/bits,_Src:16,_Dst:16,VerTag:32,_OldSum:32,Data/binary>> =Packet,
	Shred = <<SrcPort:16,DstPort:16,VerTag:32>>,
	%% TODO: this is VERY slow, needs nif
	Sum1 = crc32c(Shred),
	Sum2 = crc32c(Sum1, <<0,0,0,0>>),	%% literal
	Sum3 = crc32c(Sum2, Data),
	<<Prefix/binary,Shred/binary,(bnot Sum3):32/integer-little,Data/binary>>.

splice_ipv4_header_binary(Packet, Pos, Off, ByteLen, ValueBin) ->
	<<Prefix:(Pos)/bits,HR/binary>> =Packet,
	HLen = binary:at(HR, 0) band 15,	%% IHL (in 32-bit words)

	%%
	%% It is not obvious what is faster. A non-standard unit uses a general case
	%% (slower) VM instruction.
	%%

	<<IpHdr:(HLen)/binary-unit:32,Suffix/binary>> =HR,
	IpHdr1 = splice_binary(IpHdr, Off, ByteLen, ValueBin),
	IpHdr2 = update_ipv4_checksum(IpHdr1),

	%%
	%% We must update the TCP/UDP/SCTP checksum if the packet wraps such a packet. This
	%% function updates ipv4_src and ipv4_dst. Both are present in TCP/UDP/SCTP
	%% pseudoheaders.
	%%

	%% Extract address from the update IPv4 header
	SrcAddr = binary:part(IpHdr2, 12, 4),
	DstAddr = binary:part(IpHdr2, 16, 4),

	Proto = binary:at(HR, 9),
	if Proto =:= ?IPPROTO_TCP ->
		%% Suffix contains TCP header and TCP data
		TcpLen = byte_size(Suffix),

		InitSum = pseudoheader(SrcAddr, DstAddr, ?IPPROTO_TCP, TcpLen),
		NewSum = checksum(Suffix, 8, InitSum),

		<<Shred:16/binary,_StaleSum:16,Rest/binary>> =Suffix,
		<<Prefix/binary,IpHdr2/binary,Shred/binary,NewSum:16,Rest/binary>>;

	Proto =:= ?IPPROTO_UDP ->
		%% Suffix contains UDP header and data
		<<SrcPort:16,DstPort:16,UdpLen:16,_Sum:16,Data/binary>> =Suffix,
		InitSum = pseudoheader(SrcAddr, DstAddr, ?IPPROTO_UDP, UdpLen),
		NewSum = checksum(Data, all, InitSum +SrcPort +DstPort +UdpLen), %% no skipping
		<<Prefix/binary,SrcPort:16,DstPort:16,UdpLen:16,NewSum:16,Data/binary>>;

	true ->
		%% SCTP does not use pseudoheader
		<<Prefix/binary,IpHdr2/binary,Suffix/binary>>
	end.

splice_ipv4_header_bits(Packet, Pos, Off, BitLen, Value) ->
	<<Prefix:(Pos)/bits,HR/binary>> =Packet,
	HLen = binary:at(HR, 0) band 15,	%% IHL (in 32-bit words)
	<<IpHdr:(HLen)/binary-unit:32,Suffix/binary>> =HR,
	IpHdr1 = splice_bits(IpHdr, Off, BitLen, Value),
	IpHdr2 = update_ipv4_checksum(IpHdr1),
	<<Prefix/binary,IpHdr2/binary,Suffix/binary>>.

update_ipv4_checksum(<<Pre:10/binary,_StaleSum:16,Aft/binary>> =IpHdr) ->
	NewSum = checksum(IpHdr, 5),	%% skip the 5th word (old checksum)
	<<Pre/binary,NewSum:16,Aft/binary>>.

pseudoheader(<<S1:16,S2:16>>, <<D1:16,D2:16>>, Proto, Len) ->

	%% TCP/UDP pseudoheader for IPv4:
	%%
	%%	SrcAddr [4]
	%%	DstAddr [4]
	%%  Reserved (0) [1]
	%%	Proto [1]
	%%	Len [2]

	S1 +S2 +D1 +D2 +Proto +Len;

pseudoheader(<<S1:16,S2:16,S3:16,S4:16,S5:16,S6:16,S7:16,S8:16>>,
			 <<D1:16,D2:16,D3:16,D4:16,D5:16,D6:16,D7:16,D8:16>>, Proto, Len) ->

	%% TCP/UDP pseudoheader for IPv6:
	%%
	%%	SrcAddr [16]
	%%	DstAddr [16]
	%%	Len [4]
	%%	Zeros [3]
	%%	Proto [1]

	Len1 = Len bsr 16,
	Len2 = Len band 16#ffff,

	 S1 +S2 +S3 +S4 +S5 +S6 +S7 +S8
	+D1 +D2 +D3 +D4 +D5 +D6 +D7 +D8
				+Len1 +Len2 +Proto.

checksum(Data, Skip) ->
	checksum(Data, Skip, 0, 0).

checksum(Data, Skip, InitSum) ->
	checksum(Data, Skip, InitSum, 0).

checksum(<<>>, _Skip, Acc, _Index) ->
	bnot prune(Acc);
checksum(<<B>>, _Skip, Acc, _Index) ->
	%% odd size, pad with zero
	bnot prune(Acc + (B bsl 8));
checksum(<<_:16,Data/binary>>, Index =Skip, Acc, Index) ->
	%% skip the word
	checksum(Data, Skip, Acc, Index +1);
checksum(<<W:16,Data/binary>>, Skip, Acc, Index) ->
	checksum(Data, Skip, Acc +W, Index +1).

prune(Sum) when Sum > 16#ffff ->
	prune((Sum band 16#ffff) + (Sum bsr 16));
prune(Sum) ->
	Sum.

crc32c(Buf) ->
	crc32c(16#FFFFFFFF, Buf).
crc32c(Sum, <<>>) ->
	Sum;
crc32c(Sum, <<D, Rest/binary>>) ->
	crc32c(((Sum bsr 8) bxor crc32c_table((Sum bxor D) band 16#FF)), Rest).

crc32c_table(I) -> element(I+1, {
	16#00000000, 16#F26B8303, 16#E13B70F7, 16#1350F3F4,
	16#C79A971F, 16#35F1141C, 16#26A1E7E8, 16#D4CA64EB,
	16#8AD958CF, 16#78B2DBCC, 16#6BE22838, 16#9989AB3B,
	16#4D43CFD0, 16#BF284CD3, 16#AC78BF27, 16#5E133C24,
	16#105EC76F, 16#E235446C, 16#F165B798, 16#030E349B,
	16#D7C45070, 16#25AFD373, 16#36FF2087, 16#C494A384,
	16#9A879FA0, 16#68EC1CA3, 16#7BBCEF57, 16#89D76C54,
	16#5D1D08BF, 16#AF768BBC, 16#BC267848, 16#4E4DFB4B,
	16#20BD8EDE, 16#D2D60DDD, 16#C186FE29, 16#33ED7D2A,
	16#E72719C1, 16#154C9AC2, 16#061C6936, 16#F477EA35,
	16#AA64D611, 16#580F5512, 16#4B5FA6E6, 16#B93425E5,
	16#6DFE410E, 16#9F95C20D, 16#8CC531F9, 16#7EAEB2FA,
	16#30E349B1, 16#C288CAB2, 16#D1D83946, 16#23B3BA45,
	16#F779DEAE, 16#05125DAD, 16#1642AE59, 16#E4292D5A,
	16#BA3A117E, 16#4851927D, 16#5B016189, 16#A96AE28A,
	16#7DA08661, 16#8FCB0562, 16#9C9BF696, 16#6EF07595,
	16#417B1DBC, 16#B3109EBF, 16#A0406D4B, 16#522BEE48,
	16#86E18AA3, 16#748A09A0, 16#67DAFA54, 16#95B17957,
	16#CBA24573, 16#39C9C670, 16#2A993584, 16#D8F2B687,
	16#0C38D26C, 16#FE53516F, 16#ED03A29B, 16#1F682198,
	16#5125DAD3, 16#A34E59D0, 16#B01EAA24, 16#42752927,
	16#96BF4DCC, 16#64D4CECF, 16#77843D3B, 16#85EFBE38,
	16#DBFC821C, 16#2997011F, 16#3AC7F2EB, 16#C8AC71E8,
	16#1C661503, 16#EE0D9600, 16#FD5D65F4, 16#0F36E6F7,
	16#61C69362, 16#93AD1061, 16#80FDE395, 16#72966096,
	16#A65C047D, 16#5437877E, 16#4767748A, 16#B50CF789,
	16#EB1FCBAD, 16#197448AE, 16#0A24BB5A, 16#F84F3859,
	16#2C855CB2, 16#DEEEDFB1, 16#CDBE2C45, 16#3FD5AF46,
	16#7198540D, 16#83F3D70E, 16#90A324FA, 16#62C8A7F9,
	16#B602C312, 16#44694011, 16#5739B3E5, 16#A55230E6,
	16#FB410CC2, 16#092A8FC1, 16#1A7A7C35, 16#E811FF36,
	16#3CDB9BDD, 16#CEB018DE, 16#DDE0EB2A, 16#2F8B6829,
	16#82F63B78, 16#709DB87B, 16#63CD4B8F, 16#91A6C88C,
	16#456CAC67, 16#B7072F64, 16#A457DC90, 16#563C5F93,
	16#082F63B7, 16#FA44E0B4, 16#E9141340, 16#1B7F9043,
	16#CFB5F4A8, 16#3DDE77AB, 16#2E8E845F, 16#DCE5075C,
	16#92A8FC17, 16#60C37F14, 16#73938CE0, 16#81F80FE3,
	16#55326B08, 16#A759E80B, 16#B4091BFF, 16#466298FC,
	16#1871A4D8, 16#EA1A27DB, 16#F94AD42F, 16#0B21572C,
	16#DFEB33C7, 16#2D80B0C4, 16#3ED04330, 16#CCBBC033,
	16#A24BB5A6, 16#502036A5, 16#4370C551, 16#B11B4652,
	16#65D122B9, 16#97BAA1BA, 16#84EA524E, 16#7681D14D,
	16#2892ED69, 16#DAF96E6A, 16#C9A99D9E, 16#3BC21E9D,
	16#EF087A76, 16#1D63F975, 16#0E330A81, 16#FC588982,
	16#B21572C9, 16#407EF1CA, 16#532E023E, 16#A145813D,
	16#758FE5D6, 16#87E466D5, 16#94B49521, 16#66DF1622,
	16#38CC2A06, 16#CAA7A905, 16#D9F75AF1, 16#2B9CD9F2,
	16#FF56BD19, 16#0D3D3E1A, 16#1E6DCDEE, 16#EC064EED,
	16#C38D26C4, 16#31E6A5C7, 16#22B65633, 16#D0DDD530,
	16#0417B1DB, 16#F67C32D8, 16#E52CC12C, 16#1747422F,
	16#49547E0B, 16#BB3FFD08, 16#A86F0EFC, 16#5A048DFF,
	16#8ECEE914, 16#7CA56A17, 16#6FF599E3, 16#9D9E1AE0,
	16#D3D3E1AB, 16#21B862A8, 16#32E8915C, 16#C083125F,
	16#144976B4, 16#E622F5B7, 16#F5720643, 16#07198540,
	16#590AB964, 16#AB613A67, 16#B831C993, 16#4A5A4A90,
	16#9E902E7B, 16#6CFBAD78, 16#7FAB5E8C, 16#8DC0DD8F,
	16#E330A81A, 16#115B2B19, 16#020BD8ED, 16#F0605BEE,
	16#24AA3F05, 16#D6C1BC06, 16#C5914FF2, 16#37FACCF1,
	16#69E9F0D5, 16#9B8273D6, 16#88D28022, 16#7AB90321,
	16#AE7367CA, 16#5C18E4C9, 16#4F48173D, 16#BD23943E,
	16#F36E6F75, 16#0105EC76, 16#12551F82, 16#E03E9C81,
	16#34F4F86A, 16#C69F7B69, 16#D5CF889D, 16#27A40B9E,
	16#79B737BA, 16#8BDCB4B9, 16#988C474D, 16#6AE7C44E,
	16#BE2DA0A5, 16#4C4623A6, 16#5F16D052, 16#AD7D5351
}).

%%EOF
