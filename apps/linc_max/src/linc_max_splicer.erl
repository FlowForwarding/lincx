%%
%%
%%

%% @author Cloudozer LLP. <info@cloudozer.com>
%% @copyright 2012 FlowForwarding.org
-module(linc_max_splicer).
-export([edit/3]).

-include_lib("pkt/include/pkt.hrl").
-include("pkt_max.hrl").

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
ether(_Packet, vlan_pcp, _Value, _Pos, <<?ETH_P_PBB_I:16,_/binary>>) ->
	missing;
ether(_Packet, vlan_vid, _Value, _Pos, <<?ETH_P_PBB_I:16,_/binary>>) ->
	missing;
ether(Packet, Field, Value, Pos, <<?ETH_P_PBB_I:16,_:4/binary,_:(6+6)/binary,Rest/binary>>) ->
	ether(Packet, Field, Value, Pos +16 +32 +48 +48, Rest);

ether(Packet, mpls_label, Value, Pos, <<?ETH_P_MPLS_UNI:16,_/binary>>) ->
	splice_bits(Packet, Pos +16, 20, Value);
ether(Packet, mpls_tc, Value, Pos, <<?ETH_P_MPLS_UNI:16,_/binary>>) ->
	splice_bits(Packet, Pos +16 +20, 3, Value);
ether(_Packet, mpls_bos, _Value, _Pos, <<?ETH_P_MPLS_UNI:16,_/binary>>) ->
	protected;
ether(Packet, mpls_label, Value, Pos, <<?ETH_P_MPLS_MULTI:16,_/binary>>) ->
	splice_bits(Packet, Pos +16, 20, Value);
ether(Packet, mpls_tc, Value, Pos, <<?ETH_P_MPLS_MULTI:16,_/binary>>) ->
	splice_bits(Packet, Pos +16 +20, 3, Value);
ether(_Packet, mpls_bos, _Value, _Pos, <<?ETH_P_MPLS_MULTI:16,_/binary>>) ->
	protected;
%% MPLS tag is always after VLAN/PBB
ether(_Packet, vlan_pcp, _Value, _Pos, <<?ETH_P_MPLS_UNI:16,_Rest/binary>>) ->
	missing;
ether(_Packet, vlan_vid, _Value, _Pos, <<?ETH_P_MPLS_UNI:16,_Rest/binary>>) ->
	missing;
ether(_Packet, pbb_uca, _Value, _Pos, <<?ETH_P_MPLS_UNI:16,_Rest/binary>>) ->
	missing;
ether(_Packet, pbb_isid, _Value, _Pos, <<?ETH_P_MPLS_UNI:16,_Rest/binary>>) ->
	missing;
ether(_Packet, vlan_pcp, _Value, _Pos, <<?ETH_P_MPLS_MULTI:16,_Rest/binary>>) ->
	missing;
ether(_Packet, vlan_vid, _Value, _Pos, <<?ETH_P_MPLS_MULTI:16,_Rest/binary>>) ->
	missing;
ether(_Packet, pbb_uca, _Value, _Pos, <<?ETH_P_MPLS_MULTI:16,_Rest/binary>>) ->
	missing;
ether(_Packet, pbb_isid, _Value, _Pos, <<?ETH_P_MPLS_MULTI:16,_Rest/binary>>) ->
	missing;
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

ether(_Packet, _Field, _Value, _Pos, _Rest) ->
	missing.

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
ipv4(_Packet, ip_proto, _Value, _Pos, _Rest) ->
	splice_ipv4_header_bits(Packet, Pos, 72, 8, Value);
ipv4(Packet, ipv4_src, ValueBin, Pos, _Rest) ->
	splice_ipv4_header_binary(Packet, Pos, 96, 4, ValueBin);
ipv4(Packet, ipv4_dst, ValueBin, Pos, _Rest) ->
	splice_ipv4_header_binary(Packet, Pos, 128, 4, ValueBin);

%% Cannot happen: vlan, pbb, mpls, arp
ipv4(_Packet, vlan_pcp, _Value, _Pos, _Rest) ->
	missing;
ipv4(_Packet, vlan_vid, _Value, _Pos, _Rest) ->
	missing;
ipv4(_Packet, pbb_uca, _Value, _Pos, _Rest) ->
	missing;
ipv4(_Packet, pbb_isid, _Value, _Pos, _Rest) ->
	missing;
ipv4(_Packet, mpls_label, _Value, _Pos, _Rest) ->
	missing;
ipv4(_Packet, mpls_tc, _Value, _Pos, _Rest) ->
	missing;
ipv4(_Packet, mpls_bos, _Value, _Pos, _Rest) ->
	missing;
ipv4(_Packet, arp_op, _Value, _Pos, _Rest) ->
	missing;
ipv4(_Packet, arp_spa, _Value, _Pos, _Rest) ->
	missing;
ipv4(_Packet, arp_tpa, _Value, _Pos, _Rest) ->
	missing;
ipv4(_Packet, arp_sha, _Value, _Pos, _Rest) ->
	missing;
ipv4(_Packet, arp_tha, _Value, _Pos, _Rest) ->
	missing;

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

ipv4(_Packet, _Field, _Value, _Pos, _Rest) ->
	%% the fragmented datagram
	fragmented.

ipv6(Packet, ipv6_src, ValueBin, Pos, _Rest) ->
	splice_binary(Packet, Pos +64, 16, ValueBin);
ipv6(Packet, ipv6_dst, ValueBin, Pos, _Rest) ->
	splice_binary(Packet, Pos +192, 16, ValueBin);
ipv6(Packet, ipv6_flabel, Value, Pos, _Rest) ->
	splice_bits(Packet, Pos +12, 20, Value);

ipv6(Packet, ip_dscp, Value, Pos, _Rest) ->
	splice_bits(Packet, Pos +4, 6, Value);
ipv6(Packet, ip_ecn, Value, Pos, _Rest) ->
	splice_bits(Packet, Pos +10, 2, Value);
ipv6(_Packet, ip_proto, _Value, _Pos, _Rest) ->
	protected;

%% Cannot happen: vlan, pbb, mpls, arp
ipv6(_Packet, vlan_pcp, _Value, _Pos, _Rest) ->
	missing;
ipv6(_Packet, vlan_vid, _Value, _Pos, _Rest) ->
	missing;
ipv6(_Packet, pbb_uca, _Value, _Pos, _Rest) ->
	missing;
ipv6(_Packet, pbb_isid, _Value, _Pos, _Rest) ->
	missing;
ipv6(_Packet, mpls_label, _Value, _Pos, _Rest) ->
	missing;
ipv6(_Packet, mpls_tc, _Value, _Pos, _Rest) ->
	missing;
ipv6(_Packet, mpls_bos, _Value, _Pos, _Rest) ->
	missing;
ipv6(_Packet, arp_op, _Value, _Pos, _Rest) ->
	missing;
ipv6(_Packet, arp_spa, _Value, _Pos, _Rest) ->
	missing;
ipv6(_Packet, arp_tpa, _Value, _Pos, _Rest) ->
	missing;
ipv6(_Packet, arp_sha, _Value, _Pos, _Rest) ->
	missing;
ipv6(_Packet, arp_tha, _Value, _Pos, _Rest) ->
	missing;

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
ipv6_skip(_Packet, _Field, _Value, _Pos, _Rest,
				?IPV6_HDR_NO_NEXT_HEADER, _SrcAddr, _DstAddr) ->
	missing;
ipv6_skip(Packet, Field, Value, Pos, Rest, Proto, SrcAddr, DstAddr) ->
	proto(Packet, Field, Value, Pos, Rest, Proto, SrcAddr, DstAddr).
	
proto(Packet, Field, Value, Pos, Rest, ?IPPROTO_IP, _SrcAddr, _DstAddr) ->
	ipv4(Packet, Field, Value, Pos, Rest);
proto(Packet, Field, Value, Pos, Rest, ?IPPROTO_IPV6, _SrcAddr, _DstAddr) ->
	ipv6(Packet, Field, Value, Pos, Rest);
proto(Packet, Field, Value, Pos,
		<<SrcPort:16,DstPort:16,_/binary>>, ?IPPROTO_TCP, SrcAddr, DstAddr) ->
	%% src and dst ports extracted to avoid copy during checksum recalculation
	tcp(Packet, Field, Value, Pos, SrcPort, DstPort, SrcAddr, DstAddr);
proto(Packet, Field, Value, Pos,
		<<SrcPort:16,DstPort:16,_/binary>>, ?IPPROTO_UDP, SrcAddr, DstAddr) ->
	%% src and dst ports extracted to avoid copy during checksum recalculation
	udp(Packet, Field, Value, Pos, SrcPort, DstPort, SrcAddr, DstAddr);
proto(_Packet, Field, _Value, _Pos, _Rest, ?IPPROTO_ICMP, _SrcAddr, _DstAddr) ->
	icmp(Field);
proto(Packet, Field, Value, Pos,
		<<?ICMPV6_NDP_NS,_/binary>>, ?IPPROTO_ICMPV6, SrcAddr, DstAddr) ->
	ndp(Packet, Field, Value, Pos, SrcAddr, DstAddr);
proto(Packet, Field, Value, Pos,
		<<?ICMPV6_NDP_NA,_/binary>>, ?IPPROTO_ICMPV6, SrcAddr, DstAddr) ->
	ndp(Packet, Field, Value, Pos, SrcAddr, DstAddr);
proto(_Packet, Field, _Value, _Pos, _Rest, ?IPPROTO_ICMPV6, _SrcAddr, _DstAddr) ->
	icmpv6(Field);
proto(Packet, Field, Value, Pos,
		<<SrcPort:16,DstPort:16,_/binary>>, ?IPPROTO_SCTP, _SrcAddr, _DstAddr) ->
	sctp(Packet, Field, Value, Pos, SrcPort, DstPort).

tcp(Packet, tcp_src, Value, Pos, _SrcPort, DstPort, SrcAddr, DstAddr) ->
	splice_tcp_header(Packet, Pos, Value, DstPort, SrcAddr, DstAddr);
tcp(Packet, tcp_dst, Value, Pos, SrcPort, _DstPort, SrcAddr, DstAddr) ->
	splice_tcp_header(Packet, Pos, SrcPort, Value, SrcAddr, DstAddr);
tcp(_Packet, _Field, _Value, _Pos, _SrcPort, _DstPort, _SrcAddr, _DstAddr) ->
	missing.

udp(Packet, udp_src, Value, Pos, _SrcPort, DstPort, SrcAddr, DstAddr) ->
	splice_udp_header(Packet, Pos, Value, DstPort, SrcAddr, DstAddr);
udp(Packet, udp_dst, Value, Pos, SrcPort, _DstPort, SrcAddr, DstAddr) ->
	splice_udp_header(Packet, Pos, SrcPort, Value, SrcAddr, DstAddr);
udp(_Packet, _Field, _Value, _Pos, _SrcPort, _DstPort, _SrcAddr, _DstAddr) ->
	missing.

sctp(Packet, sctp_src, Value, Pos, _SrcPort, DstPort) ->
	splice_sctp_header(Packet, Pos, Value, DstPort);
sctp(Packet, sctp_dst, Value, Pos, SrcPort, _DstPort) ->
	splice_sctp_header(Packet, Pos, SrcPort, Value);
sctp(_Packet, _Field, _Value, _Pos, _SrcPort, _DstPort) ->
	missing.

icmp(icmpv4_type) ->
	protected;
icmp(icmpv4_code) ->
	protected;
icmp(_) ->
	missing.

icmpv6(icmpv6_type) ->
	protected;
icmpv6(icmpv6_code) ->
	protected;
icmpv6(_) ->
	missing.

ndp(Packet, ipv6_nd_target, ValueBin, Pos, SrcAddr, DstAddr) ->
	<<Prefix:(Pos)/bits,W1:16,_OldSum:16,W2:16,W3:16,_OldAddr:16/binary,Rest/binary>> =Packet,
	<<T1:16,T2:16,T3:16,T4:16,T5:16,T6:16,T7:16,T8:16>> =ValueBin,
	NdpLen = byte_size(Rest) +2 +2 +2 +2 +16,
	InitSum = pseudoheader(SrcAddr, DstAddr, ?IPPROTO_ICMPV6, NdpLen)
				+W1 +W2 +W3
				+T1 +T2 +T3 +T4 +T5 +T6 +T7 +T8,
	NewSum = checksum(Rest, all, InitSum),
	<<Prefix/binary,W1:16,NewSum:16,W2:16,W3:16,ValueBin/binary,Rest/binary>>;

ndp(_Packet, ipv6_nd_sll, _ValueBin, _Pos, _SrcAddr, _DstAddr) ->
	unimplemented;
ndp(_Packet, ipv6_nd_tll, _ValueBin, _Pos, _SrcAddr, _DstAddr) ->
	unimplemented;
ndp(_Packet, _Field, _ValueBin, _Pos, _SrcAddr, _DstAddr) ->
	missing.

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
	Sum1 = erlang:crc32(Shred),
	Sum2 = erlang:crc32(Sum1, <<0,0,0,0>>),	%% literal
	NewSum = erlang:crc32(Sum2, Data),
	<<Prefix/binary,Shred/binary,NewSum:32,Data/binary>>.

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
	<<Prefix/binary,IpHdr2/binary,Suffix/binary>>.

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

%%EOF
