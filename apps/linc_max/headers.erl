-module(headers).
-compile(export_all).

-define(ETH_P_IP,         16#0800).
-define(ETH_P_ARP,        16#0806).
-define(ETH_P_IPV6,       16#86dd).
-define(ETH_P_802_1Q,     16#8100).
-define(ETH_P_MPLS_UNI,   16#8847).
-define(ETH_P_MPLS_MULTI, 16#8848).
-define(ETH_P_ALL,        16#0300).
-define(ETH_P_PBB_B,      16#88a8).
-define(ETH_P_PBB_I,      16#88e7).

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

-define(IPPROTO_IP, 0).
-define(IPPROTO_ICMP, 1).
-define(IPPROTO_TCP, 6).
-define(IPPROTO_UDP, 17).
-define(IPPROTO_IPV6, 41).
-define(IPPROTO_GRE, 47).
-define(IPPROTO_ICMPV6, 58).
-define(IPPROTO_SCTP, 132).
-define(IPPROTO_RAW, 255).

-define(ICMPV6_NDP_NS, 135).
-define(ICMPV6_NDP_NA, 136).

-define(NDP_OPT_SLL, 1).
-define(NDP_OPT_TLL, 2).

split(<<_:12/binary,Rest/binary>> =Packet) ->
	%% set headers/tags that may appear multiple times to none
	split_eth(Packet, none, none, none, none, none, none, Rest).

split_eth(Packet, none = _VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		<<?ETH_P_802_1Q:16,VlanTag:2/binary,Rest/binary>>) ->
	split_eth(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext, Rest);
split_eth(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		<<?ETH_P_802_1Q:16,_Skip:2/binary,Rest/binary>>) ->
	split_eth(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext, Rest);

split_eth(Packet, none, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		<<?ETH_P_PBB_B:16,VlanTag:2/binary,Rest/binary>>) ->
	split_eth(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext, Rest);
split_eth(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		<<?ETH_P_PBB_B:16,_Skip:2/binary,Rest/binary>>) ->
	split_eth(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext, Rest);
split_eth(Packet, VlanTag, none, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		<<?ETH_P_PBB_I:16,PbbTag:2/binary,_Skip:(6+6+4+4)/binary,Rest/binary>>) ->
	split_eth(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext, Rest);
split_eth(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		<<?ETH_P_PBB_I:16,_Skip:(2+6+6+4+4)/binary,Rest/binary>>) ->
	split_eth(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext, Rest);

split_eth(Packet, VlanTag, PbbTag, none, Ip4Hdr, Ip6Hdr, Ip6Ext,
		<<?ETH_P_MPLS_UNI:16,MplsTag:4/binary,Rest/binary>>) ->
	split_eth(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext, Rest);
split_eth(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		<<?ETH_P_MPLS_UNI:16,_Skip:4/binary,Rest/binary>>) ->
	split_eth(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext, Rest);
split_eth(Packet, VlanTag, PbbTag, none, Ip4Hdr, Ip6Hdr, Ip6Ext,
		<<?ETH_P_MPLS_MULTI:16,MplsTag:4/binary,Rest/binary>>) ->
	split_eth(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext, Rest);
split_eth(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		<<?ETH_P_MPLS_MULTI:16,_Skip:4/binary,Rest/binary>>) ->
	split_eth(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext, Rest);

split_eth(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		<<?ETH_P_IP:16,Rest/binary>>) ->
	split_ipv4(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext, Rest);
split_eth(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		<<?ETH_P_IPV6:16,Rest/binary>>) ->
	split_ipv6(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext, Rest);
split_eth(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		<<?ETH_P_ARP:16,Rest/binary>>) ->
	flow_arp(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext, Rest).

split_ipv4(Packet, VlanTag, PbbTag, MplsTag, none, Ip6Hdr, Ip6Ext,
		<<(4 *16 +5),_:8/binary,Proto,_/binary>> =IpHdrRest) ->
	%% IHL is 5, no options
	<<Ip4Hdr:20/binary,Rest/binary>> = IpHdrRest,
	split_proto(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext, Proto, Rest);
split_ipv4(Packet, VlanTag, PbbTag, MplsTag, none, Ip6Hdr, Ip6Ext,
		<<4:4,IHL:4,_:8/binary,Proto,_/binary>> =IpHdrRest) ->
	HdrLen = IHL *4,
	<<Ip4Hdr:(HdrLen)/binary,Rest/binary>> = IpHdrRest,
	split_proto(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext, Proto, Rest);
split_ipv4(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		<<(4 *16 +5),_:8/binary,Proto,_/binary>> =IpHdrRest) ->
	%% IHL is 5, no options
	<<_Skip:20/binary,Rest/binary>> = IpHdrRest,
	split_proto(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext, Proto, Rest);
split_ipv4(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		<<4:4,IHL:4,_:8/binary,Proto,_/binary>> =IpHdrRest) ->
	HdrLen = IHL *4,
	<<_Skip:(HdrLen)/binary,Rest/binary>> = IpHdrRest,
	split_proto(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext, Proto, Rest).

split_ipv6(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, none, none,
		<<Ip6Hdr:40/binary,Rest/binary>> =Rest) ->
	Next = binary:at(Ip6Hdr, 6),
	split_ip6_chain(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, ?IPV6_EXT_NONEXT, Next, Rest);
split_ipv6(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		<<SkipHdr:40/binary,Rest/binary>> =Rest) ->
	Next = binary:at(SkipHdr, 6),
	split_ip6_skip(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext, Next, Rest).

split_ip6_chain(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPV6_HDR_HOP, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>) ->
	split_ip6_chain(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr,
			ext_flags(Ip6Ext, ?IPV6_EXT_HOP), Next, Rest);
split_ip6_chain(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPV6_HDR_DEST, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>) ->
	split_ip6_chain(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr,
			ext_flags(Ip6Ext, ?IPV6_EXT_DEST), Next, Rest);
split_ip6_chain(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPV6_HDR_ROUTER, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>) ->
	split_ip6_chain(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr,
			ext_flags(Ip6Ext, ?IPV6_EXT_ROUTER), Next, Rest);
split_ip6_chain(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPV6_HDR_FRAG, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>) ->
	split_ip6_chain(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr,
			ext_flags(Ip6Ext, ?IPV6_EXT_FRAG), Next, Rest);
split_ip6_chain(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPV6_HDR_AUTH, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>) ->
	split_ip6_chain(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr,
			ext_flags(Ip6Ext, ?IPV6_EXT_AUTH), Next, Rest);
split_ip6_chain(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPV6_HDR_ESP, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>) ->
	split_ip6_chain(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr,
			ext_flags(Ip6Ext, ?IPV6_EXT_ESP), Next, Rest);
split_ip6_chain(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPV6_HDR_NONEXT, _Rest) ->
	flow_empty(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext);
split_ip6_chain(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		Proto, Rest) ->
	split_proto(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext, Proto, Rest).

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
	Ip6Ext band (not ?IPV6_EXT_NONEXT)
		bor ?IPV6_EXT_HOP
		bor repeated(Ip6Ext, ?IPV6_EXT_HOP)
		bor out_of_order(Ip6Ext, 0);
ext_flags(Ip6Ext, ?IPV6_EXT_DEST) ->
	%% Destination Options header can be repeated
	%% NB: order not checked strictly
	Ip6Ext band (not ?IPV6_EXT_NONEXT)
		bor ?IPV6_EXT_HOP;
ext_flags(Ip6Ext, ?IPV6_EXT_ROUTER) ->
	Ip6Ext band (not ?IPV6_EXT_NONEXT)
		bor ?IPV6_EXT_ROUTER
		bor repeated(Ip6Ext, ?IPV6_EXT_ROUTER)
		bor out_of_order(Ip6Ext, ?IPV6_EXT_HOP bor
								 ?IPV6_EXT_DEST);
ext_flags(Ip6Ext, ?IPV6_EXT_FRAG) ->
	Ip6Ext band (not ?IPV6_EXT_NONEXT)
		bor ?IPV6_EXT_FRAG
		bor repeated(Ip6Ext, ?IPV6_EXT_FRAG)
		bor out_of_order(Ip6Ext, ?IPV6_EXT_HOP bor
								 ?IPV6_EXT_DEST bor
								 ?IPV6_EXT_ROUTER);
ext_flags(Ip6Ext, ?IPV6_EXT_AUTH) ->
	Ip6Ext band (not ?IPV6_EXT_NONEXT)
		bor ?IPV6_EXT_AUTH
		bor repeated(Ip6Ext, ?IPV6_EXT_AUTH)
		bor out_of_order(Ip6Ext, ?IPV6_EXT_HOP bor
								 ?IPV6_EXT_DEST bor
								 ?IPV6_EXT_ROUTER bor
								 ?IPV6_EXT_FRAG);
ext_flags(Ip6Ext, ?IPV6_EXT_ESP) ->
	Ip6Ext band (not ?IPV6_EXT_NONEXT)
		bor ?IPV6_EXT_ESP
		bor repeated(Ip6Ext, ?IPV6_EXT_ESP).

repeated(Ip6Ext, Mask) when Ip6Ext band Mask =/= 0 -> ?IPV6_EXT_UNREP;
repeated(_Ip6Ext, _Mask) -> 0.

out_of_order(Ip6Ext, Mask)
	when Ip6Ext band (not Mask) band (not ?IPV6_EXT_NONEXT) =/= 0 -> ?IPV6_EXT_UNSEQ;
out_of_order(_Ip6Ext, _Mask) -> 0.

split_ip6_skip(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPV6_HDR_HOP, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>) ->
	split_ip6_skip(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext, Next, Rest);
split_ip6_skip(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPV6_HDR_DEST, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>) ->
	split_ip6_skip(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext, Next, Rest);
split_ip6_skip(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPV6_HDR_ROUTER, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>) ->
	split_ip6_skip(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext, Next, Rest);
split_ip6_skip(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPV6_HDR_FRAG, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>) ->
	split_ip6_skip(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext, Next, Rest);
split_ip6_skip(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPV6_HDR_AUTH, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>) ->
	split_ip6_skip(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext, Next, Rest);
split_ip6_skip(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPV6_HDR_ESP, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>) ->
	split_ip6_skip(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext, Next, Rest);
split_ip6_skip(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPV6_HDR_NONEXT, _Rest) ->
	flow_empty(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext);
split_ip6_skip(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		Proto, Rest) ->
	split_proto(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext, Proto, Rest).

split_proto(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPPROTO_IP, Rest) ->
	split_ipv4(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext, Rest);
split_proto(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPPROTO_IPV6, Rest) ->
	split_ipv6(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext, Rest);
split_proto(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPPROTO_ICMP, IcmpPacket) ->
	flow_icmp(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext, IcmpPacket);
split_proto(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPPROTO_ICMPV6, Icmp6Packet) ->
	split_icmpv6(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext, Icmp6Packet);
split_proto(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPPROTO_TCP, TcpPacket) ->
	flow_tcp(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext, TcpPacket);
split_proto(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPPROTO_UDP, UdpPacket) ->
	flow_tcp(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext, UdpPacket);
split_proto(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		?IPPROTO_SCTP, SctpPacket) ->
	flow_sctp(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext, SctpPacket).

%% IPPROTO_GRE not expected

%% needed to prepare options for matching
split_icmpv6(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		<<?ICMPV6_NDP_NS,_:23/binary,Opts/binary>> =Icmp6Hdr) ->
	Icmp6OptSll = icmpv6_opt(?NDP_OPT_SLL, Opts),
	flow_icmpv6(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext, Icmp6Hdr, Icmp6OptSll, 0);
split_icmpv6(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
		<<?ICMPV6_NDP_NA,_:23/binary,Opts/binary>> =Icmp6Hdr) ->
	Icmp6OptTll = icmpv6_opt(?NDP_OPT_TLL, Opts),
	flow_icmpv6(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
			Icmp6Hdr, none, Icmp6OptTll);
split_icmpv6(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext, Icmp6Hdr) ->
	flow_icmpv6(Packet, VlanTag, PbbTag, MplsTag, Ip4Hdr, Ip6Hdr, Ip6Ext,
			Icmp6Hdr, none, none).

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
