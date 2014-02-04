-module(linc_max_headers).
-export([split/2]).

-define(ETH_P_IP,			16#0800).
-define(ETH_P_ARP,			16#0806).
-define(ETH_P_IPV6,			16#86dd).
-define(ETH_P_802_1Q,		16#8100).
-define(ETH_P_MPLS_UNI,		16#8847).
-define(ETH_P_MPLS_MULTI,	16#8848).
-define(ETH_P_PBB_B,		16#88a8).
-define(ETH_P_PBB_I,		16#88e7).

-define(IPV6_HDR_NONEXT,	59).
-define(IPV6_HDR_ESP,		50).
-define(IPV6_HDR_AUTH,		51).
-define(IPV6_HDR_DEST,		60).
-define(IPV6_HDR_FRAG,		44).
-define(IPV6_HDR_ROUTER,	43).
-define(IPV6_HDR_HOP,		0).

-define(IPV6_EXT_NONEXT,	1).
-define(IPV6_EXT_ESP,		2).
-define(IPV6_EXT_AUTH,		4).
-define(IPV6_EXT_DEST,		8).
-define(IPV6_EXT_FRAG,		16).
-define(IPV6_EXT_ROUTER,	32).
-define(IPV6_EXT_HOP,		64).
-define(IPV6_EXT_UNREP,		128).
-define(IPV6_EXT_UNSEQ,		256).

-define(IPPROTO_IP,		0).
-define(IPPROTO_ICMP,	1).
-define(IPPROTO_TCP,	6).
-define(IPPROTO_UDP,	17).
-define(IPPROTO_IPV6,	41).
-define(IPPROTO_GRE,	47).
-define(IPPROTO_ICMPV6,	58).
-define(IPPROTO_SCTP,	132).
-define(IPPROTO_RAW,	255).

-define(ICMPV6_NDP_NS,	135).
-define(ICMPV6_NDP_NA,	136).

-define(NDP_OPT_SLL,	1).
-define(NDP_OPT_TLL,	2).

%% A note to a maintainer:
%%
%% The code in this module starting with the function split() prepares the
%% incoming packet for matching by flow tables. It does nothing else. In that
%% sense it does less than pkt:decapsulate() as the output of the splitting
%% process is not enough to reconstruct the packet, or modify its fields.
%%
%% The code may look too repetitive and old school, yet this is needed to get
%% fastest bytecode and the lowest heap footprint. Notice that the code does not
%% create tuples. Heaps consumed by matching contexts and subbinaries only. The
%% compiler will usually convert a wall of code to a handful of instructions.
%% Use BEAM disassembler to confirm.
%%
%% The dark side of this approach is that it may be difficult to modify the code
%% to support new packet types, etc. This is the price you pay for performance.
%%

-record(state, {
	in_port =0,
	in_phy_port =0,
	metadata = <<0:64>>,
	tunnel_id = <<0:64>>
}).

%% rewrite as macros
in_port(#state{in_port =InPort}) -> InPort.
in_phy_port(#state{in_phy_port =InPhyPort}) -> InPhyPort.
metadata(#state{metadata =Metadata}) -> Metadata.
tunnel_id(#state{tunnel_id =TunnelId}) -> TunnelId.

split(<<_:12/binary,Rest/binary>> =Packet, St) ->
	%% set headers/tags that may appear multiple times to none
	split_eth(Packet, none, none, none, none, none, none, none, Rest, St).

%% "Ethernet type of the OpenFlow packet payload, after VLAN tags."

split_eth(Packet, none, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		<<?ETH_P_802_1Q:16,VlanTag:2/binary,Rest/binary>>, St) ->
	split_eth(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, Rest, St);
split_eth(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		<<?ETH_P_802_1Q:16,_SkipTag:2/binary,Rest/binary>>, St) ->
	split_eth(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, Rest, St);

split_eth(Packet, none, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		<<?ETH_P_PBB_B:16,VlanTag:2/binary,Rest/binary>>, St) ->
	split_eth(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, Rest, St);
split_eth(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		<<?ETH_P_PBB_B:16,_SkipTag:2/binary,Rest/binary>>, St) ->
	split_eth(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, Rest, St);
split_eth(Packet, VlanTag, EthType, none, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		<<?ETH_P_PBB_I:16,PbbTag:2/binary,_Skip:(6+6+4+4)/binary,Rest/binary>>, St) ->
	split_eth(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, Rest, St);
split_eth(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		<<?ETH_P_PBB_I:16,_Skip:(2+6+6+4+4)/binary,Rest/binary>>, St) ->
	split_eth(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, Rest, St);

split_eth(Packet, VlanTag, EthType, PbbTag, none, Ip4Hdr, Ip6Hdr, Ip6Ext,
		<<?ETH_P_MPLS_UNI:16,MplsTag:4/binary,Rest/binary>>, St) ->
	split_eth(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, Rest, St);
split_eth(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		<<?ETH_P_MPLS_UNI:16,_SkipTag:4/binary,Rest/binary>>, St) ->
	split_eth(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, Rest, St);
split_eth(Packet, VlanTag, EthType, PbbTag, none, Ip4Hdr, Ip6Hdr, Ip6Ext,
		<<?ETH_P_MPLS_MULTI:16,MplsTag:4/binary,Rest/binary>>, St) ->
	split_eth(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, Rest, St);
split_eth(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		<<?ETH_P_MPLS_MULTI:16,_SkipTag:4/binary,Rest/binary>>, St) ->
	split_eth(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, Rest, St);

split_eth(Packet, VlanTag, none, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		<<?ETH_P_IP:16,Rest/binary>>, St) ->
	split_ipv4(Packet, VlanTag, ?ETH_P_IP, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, Rest, St);
split_eth(Packet, VlanTag, none, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		<<?ETH_P_IPV6:16,Rest/binary>>, St) ->
	split_ipv6(Packet, VlanTag, ?ETH_P_IPV6, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, Rest, St);
split_eth(Packet, VlanTag, none, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		<<?ETH_P_ARP:16,ArpMsg/binary>>, St) ->
	flow0:arp(Packet, VlanTag, ?ETH_P_ARP, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, <<Ip6Ext:16>>, ArpMsg,
			in_port(St),
			in_phy_port(St),
			metadata(St),
			tunnel_id(St),
			St).

split_ipv4(Packet, VlanTag, EthType, PbbTag, MplsTag, none, Ip6Hdr, Ip6Ext,
		<<(4 *16 +5),_:8/binary,Proto,_/binary>> =IpHdrRest, St) ->
	%% IHL is 5, no options
	<<Ip4Hdr:20/binary,Rest/binary>> = IpHdrRest,
	split_proto(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, Proto, Rest, St);
split_ipv4(Packet, VlanTag, EthType, PbbTag, MplsTag, none, Ip6Hdr, Ip6Ext,
		<<4:4,IHL:4,_:8/binary,Proto,_/binary>> =IpHdrRest, St) ->
	HdrLen = IHL *4,
	<<Ip4Hdr:(HdrLen)/binary,Rest/binary>> = IpHdrRest,
	split_proto(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, Proto, Rest, St);
split_ipv4(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		<<(4 *16 +5),_:8/binary,Proto,_/binary>> =IpHdrRest, St) ->
	%% IHL is 5, no options
	<<_Skip:20/binary,Rest/binary>> = IpHdrRest,
	split_proto(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, Proto, Rest, St);
split_ipv4(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		<<4:4,IHL:4,_:8/binary,Proto,_/binary>> =IpHdrRest, St) ->
	HdrLen = IHL *4,
	<<_Skip:(HdrLen)/binary,Rest/binary>> = IpHdrRest,
	split_proto(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, Proto, Rest, St).

split_ipv6(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, none, none,
		<<Ip6Hdr:40/binary,Rest/binary>>, St) ->
	Next = binary:at(Ip6Hdr, 6),
	split_ip6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr,
			?IPV6_EXT_NONEXT, Next, Rest, St);
split_ipv6(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		<<SkipHdr:40/binary,Rest/binary>>, St) ->
	Next = binary:at(SkipHdr, 6),
	split_ip6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
			Next, Rest, St).

split_ip6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPV6_HDR_HOP, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>, St) ->
	split_ip6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr,
			ext_flags(Ip6Ext, ?IPV6_EXT_HOP), Next, Rest, St);
split_ip6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPV6_HDR_DEST, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>, St) ->
	split_ip6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr,
			ext_flags(Ip6Ext, ?IPV6_EXT_DEST), Next, Rest, St);
split_ip6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPV6_HDR_ROUTER, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>, St) ->
	split_ip6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr,
			ext_flags(Ip6Ext, ?IPV6_EXT_ROUTER), Next, Rest, St);
split_ip6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPV6_HDR_FRAG, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>, St) ->
	split_ip6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr,
			ext_flags(Ip6Ext, ?IPV6_EXT_FRAG), Next, Rest, St);
split_ip6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPV6_HDR_AUTH, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>, St) ->
	split_ip6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr,
			ext_flags(Ip6Ext, ?IPV6_EXT_AUTH), Next, Rest, St);
split_ip6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPV6_HDR_ESP, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>, St) ->
	split_ip6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr,
			ext_flags(Ip6Ext, ?IPV6_EXT_ESP), Next, Rest, St);
split_ip6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPV6_HDR_NONEXT, _Rest, St) ->
	flow0:nonext(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, <<Ip6Ext:16>>,
			in_port(St),
			in_phy_port(St),
			metadata(St),
			tunnel_id(St),
			St);
split_ip6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		Proto, Rest, St) ->
	split_proto(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, Proto, Rest, St).

%%
%% The preferred extension header order according to RFC 2460:
%%
%% 	 Hop-by-Hop Options header
%%   Destination Options header
%%   Routing header
%%   Fragment header
%%   Authentication header
%%   Encapsulating Security Payload header
%%   Destination Options header
%%

%% reset IPV6_EXT_NONEXT flag
%% set IPV6_EXT_UNREP flag if repeated
%% set IPV6_EXT_UNSEQ flag if out of order
ext_flags(Ip6Ext, ?IPV6_EXT_HOP) ->
	Ip6Ext band (bnot ?IPV6_EXT_NONEXT)
		bor ?IPV6_EXT_HOP
		bor repeated(Ip6Ext, ?IPV6_EXT_HOP)
		bor out_of_order(Ip6Ext, 0);
ext_flags(Ip6Ext, ?IPV6_EXT_DEST) ->
	%% Destination Options header can be repeated
	%% NB: order not checked strictly
	Ip6Ext band (bnot ?IPV6_EXT_NONEXT)
		bor ?IPV6_EXT_HOP;
ext_flags(Ip6Ext, ?IPV6_EXT_ROUTER) ->
	Ip6Ext band (bnot ?IPV6_EXT_NONEXT)
		bor ?IPV6_EXT_ROUTER
		bor repeated(Ip6Ext, ?IPV6_EXT_ROUTER)
		bor out_of_order(Ip6Ext, ?IPV6_EXT_HOP bor
								 ?IPV6_EXT_DEST);
ext_flags(Ip6Ext, ?IPV6_EXT_FRAG) ->
	Ip6Ext band (bnot ?IPV6_EXT_NONEXT)
		bor ?IPV6_EXT_FRAG
		bor repeated(Ip6Ext, ?IPV6_EXT_FRAG)
		bor out_of_order(Ip6Ext, ?IPV6_EXT_HOP bor
								 ?IPV6_EXT_DEST bor
								 ?IPV6_EXT_ROUTER);
ext_flags(Ip6Ext, ?IPV6_EXT_AUTH) ->
	Ip6Ext band (bnot ?IPV6_EXT_NONEXT)
		bor ?IPV6_EXT_AUTH
		bor repeated(Ip6Ext, ?IPV6_EXT_AUTH)
		bor out_of_order(Ip6Ext, ?IPV6_EXT_HOP bor
								 ?IPV6_EXT_DEST bor
								 ?IPV6_EXT_ROUTER bor
								 ?IPV6_EXT_FRAG);
ext_flags(Ip6Ext, ?IPV6_EXT_ESP) ->
	Ip6Ext band (bnot ?IPV6_EXT_NONEXT)
		bor ?IPV6_EXT_ESP
		bor repeated(Ip6Ext, ?IPV6_EXT_ESP).

repeated(Ip6Ext, Mask) when Ip6Ext band Mask =/= 0 -> ?IPV6_EXT_UNREP;
repeated(_Ip6Ext, _Mask) -> 0.

out_of_order(Ip6Ext, Mask)
	when Ip6Ext band (bnot Mask) band (bnot ?IPV6_EXT_NONEXT) =/= 0 -> ?IPV6_EXT_UNSEQ;
out_of_order(_Ip6Ext, _Mask) -> 0.

split_ip6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPV6_HDR_HOP, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>, St) ->
	split_ip6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, Next, Rest, St);
split_ip6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPV6_HDR_DEST, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>, St) ->
	split_ip6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, Next, Rest, St);
split_ip6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPV6_HDR_ROUTER, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>, St) ->
	split_ip6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, Next, Rest, St);
split_ip6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPV6_HDR_FRAG, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>, St) ->
	split_ip6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, Next, Rest, St);
split_ip6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPV6_HDR_AUTH, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>, St) ->
	split_ip6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, Next, Rest, St);
split_ip6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPV6_HDR_ESP, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>, St) ->
	split_ip6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, Next, Rest, St);
split_ip6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPV6_HDR_NONEXT, _Rest, St) ->
	flow0:nonext(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, <<Ip6Ext:16>>,
			in_port(St),
			in_phy_port(St),
			metadata(St),
			tunnel_id(St),
			St);
split_ip6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		Proto, Rest, St) ->
	split_proto(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, Proto, Rest, St).

split_proto(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPPROTO_IP, Rest, St) ->
	split_ipv4(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext, Rest, St);
split_proto(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPPROTO_IPV6, Rest, St) ->
	split_ipv6(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext, Rest, St);
split_proto(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPPROTO_ICMP, IcmpMsg, St) ->
	flow0:icmp(Packet, VlanTag, EthType, PbbTag, MplsTag,
	  		Ip4Hdr, Ip6Hdr, <<Ip6Ext:16>>, IcmpMsg,
			in_port(St),
			in_phy_port(St),
			metadata(St),
			tunnel_id(St),
			St);
split_proto(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPPROTO_ICMPV6, Icmp6Packet, St) ->
	split_icmpv6(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext, Icmp6Packet, St);
split_proto(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPPROTO_TCP, TcpPacket, St) ->
	flow0:tcp(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, <<Ip6Ext:16>>, TcpPacket,
			in_port(St),
			in_phy_port(St),
			metadata(St),
			tunnel_id(St),
			St);
split_proto(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPPROTO_UDP, UdpPacket, St) ->
	flow0:udp(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, <<Ip6Ext:16>>, UdpPacket,
			in_port(St),
			in_phy_port(St),
			metadata(St),
			tunnel_id(St),
			St);
split_proto(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPPROTO_SCTP, SctpPacket, St) ->
	flow0:sctp(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, <<Ip6Ext:16>>, SctpPacket,
			in_port(St),
			in_phy_port(St),
			metadata(St),
			tunnel_id(St),
			St).

%% IPPROTO_GRE not expected

%% needed to prepare options for matching
split_icmpv6(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		<<?ICMPV6_NDP_NS,_:23/binary,Opts/binary>> =Icmp6Hdr, St) ->
	Icmp6OptSll = icmpv6_opt(?NDP_OPT_SLL, Opts),
	flow0:icmpv6(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, <<Ip6Ext:16>>, Icmp6Hdr, Icmp6OptSll, none,
			in_port(St),
			in_phy_port(St),
			metadata(St),
			tunnel_id(St),
			St);
split_icmpv6(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		<<?ICMPV6_NDP_NA,_:23/binary,Opts/binary>> =Icmp6Hdr, St) ->
	Icmp6OptTll = icmpv6_opt(?NDP_OPT_TLL, Opts),
	flow0:icmpv6(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, <<Ip6Ext:16>>, Icmp6Hdr, none, Icmp6OptTll,
			in_port(St),
			in_phy_port(St),
			metadata(St),
			tunnel_id(St),
			St);
split_icmpv6(Packet, VlanTag, EthType, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext, Icmp6Hdr, St) ->
	flow0:icmpv6(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, <<Ip6Ext:16>>, Icmp6Hdr, none, none,
			in_port(St),
			in_phy_port(St),
			metadata(St),
			tunnel_id(St),
			St).

icmpv6_opt(_OptType, <<>>) ->
	none;
icmpv6_opt(OptType, <<OptType,Len,Rest/binary>>) ->
	OptLen = 8 *Len -2,
	<<Data:OptLen/binary,_/binary>> = Rest,
	Data;
icmpv6_opt(OptType, <<_,Len,Rest/binary>>) ->
	OptLen = 8 *Len -2,
	<<_:OptLen/binary,Opts/binary>> = Rest,
	icmpv6_opt(OptType, Opts).

%%EOF
