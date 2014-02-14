%%
%%
%%

%% @author Cloudozer, LLP. <info@cloudozer.com>
%% @copyright 2014 FlowForwarding.org
%% @doc Packet preparser
-module(linc_max_preparser).

%% public
-export([inject/5]).

%% A note to a maintainer:
%%
%% The code in this module starting with the function inject() prepares the
%% incoming packet for matching by flow tables. It does nothing else. In that
%% sense it does less than pkt:decapsulate() as the output of the splitting
%% process is not enough to reconstruct the packet, or modify its fields.
%%
%% The code may look too repetitive and old school, yet this is needed to get
%% fastest bytecode and the lowest heap footprint. Notice that the code does not
%% create tuples. Heaps consumed by matching contexts and subbinaries only.
%% Empty action sets live in a literal pool. The compiler will usually convert a
%% wall of code to just a handful of instructions. Use BEAM disassembler to confirm.
%%
%% The dark side of this approach is that it may be difficult to modify the code
%% to support new packet types, etc. This is the price you pay for performance.
%%

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

%% @doc Inject/reinject a packet into a flow pipeline.
inject(<<_:12/binary,Rest/binary>> =Packet, Metadata, PortInfo, Actions, FlowTab) ->
	eth(Packet,
		undefined,	%% VlanTag
		undefined,	%% EthType
		undefined,	%% PbbTag
		undefined,	%% MplsTag
		undefined,	%% Ip4Hdr
		undefined,	%% Ip6Hdr
		undefined,	%% Ip6Ext
		undefined,	%% IpTclass,
		undefined,	%% IpProto,
		Metadata,
		PortInfo,
		Actions,
		FlowTab,
		%% standard arguments end
		Rest).

%% "Ethernet type of the OpenFlow packet payload, after VLAN tags."

%% @doc Consume VLAN/PBB/MPLS tags
eth(Packet, undefined = _VlanTag, EthType, PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, PortInfo, Actions, FlowTab,
	<<?ETH_P_802_1Q:16,VlanTag:2/binary,Rest/binary>>) ->
	%% The first VLAN tag, keep
	eth(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, FlowTab,
		Rest);
eth(Packet, VlanTag, EthType, PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, PortInfo, Actions, FlowTab,
	<<?ETH_P_802_1Q:16,_SkipVlanTag:2/binary,Rest/binary>>) ->
	%% The second/third VLAN Tag, ignore
	eth(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, FlowTab,
		Rest);

%% See https://sites.google.com/site/amitsciscozone/home/pbb/understanding-pbb 

eth(Packet, undefined = _VlanTag, EthType, PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, PortInfo, Actions, FlowTab,
	<<?ETH_P_PBB_B:16,VlanTag:2/binary,Rest/binary>>) ->
	%% 802.1ad (or 802.1ah) frame, keep S-VLAN tag
	%%NB: EthType not set
	eth(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, FlowTab,
		Rest);
eth(Packet, VlanTag, EthType, PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, PortInfo, Actions, FlowTab,
	<<?ETH_P_PBB_B:16,_Skip:2/binary,Rest/binary>>) ->
	%% 802.1ad (or 802.1ah) frame, skip
	%%NB: EthType not set
	eth(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, FlowTab,
		Rest);
eth(Packet, VlanTag, EthType, undefined = _PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, PortInfo, Actions, FlowTab,
	<<?ETH_P_PBB_I:16,PbbTag:4/binary,_Skip:(6+6+4+4)/binary,Rest/binary>>) ->
	%% 802.1ah frame, keep I-TAG
	eth(Packet, VlanTag, up(EthType, ?ETH_P_PBB_I), PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, FlowTab,
		Rest);
eth(Packet, VlanTag, EthType, PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, PortInfo, Actions, FlowTab,
	<<?ETH_P_PBB_I:16,_Skip:(4+6+6+4+4)/binary,Rest/binary>>) ->
	%% 802.1ah frame, skip
	eth(Packet, VlanTag, up(EthType, ?ETH_P_PBB_I), PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, FlowTab,
		Rest);

%% http://en.wikipedia.org/wiki/Multiprotocol_Label_Switching

eth(Packet, VlanTag, EthType, PbbTag, undefined = _MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, PortInfo, Actions, FlowTab,
	<<?ETH_P_MPLS_UNI:16,MplsTag:4/binary,Rest/binary>>) ->
	%% MPLS unicast frame, keep
	eth(Packet, VlanTag, up(EthType, ?ETH_P_MPLS_UNI), PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, FlowTab,
		Rest);
eth(Packet, VlanTag, EthType, PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, PortInfo, Actions, FlowTab,
	<<?ETH_P_MPLS_UNI:16,_SkipTag:4/binary,Rest/binary>>) ->
	%% MPLS unicast frame, skip
	eth(Packet, VlanTag, up(EthType, ?ETH_P_MPLS_UNI), PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, FlowTab,
		Rest);
eth(Packet, VlanTag, EthType, PbbTag, undefined = _MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, PortInfo, Actions, FlowTab,
	<<?ETH_P_MPLS_MULTI:16,MplsTag:4/binary,Rest/binary>>) ->
	%% MPLS multicast frame, keep
	eth(Packet, VlanTag, up(EthType, ?ETH_P_MPLS_MULTI), PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, FlowTab,
		Rest);
eth(Packet, VlanTag, EthType, PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, PortInfo, Actions, FlowTab,
	<<?ETH_P_MPLS_MULTI:16,_SkipTag:4/binary,Rest/binary>>) ->
	%% MPLS multicast frame, skip
	eth(Packet, VlanTag, up(EthType, ?ETH_P_MPLS_MULTI), PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, FlowTab,
		Rest);

%% More conventional Ethernet types

eth(Packet, VlanTag, EthType, PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, PortInfo, Actions, FlowTab,
	<<?ETH_P_IP:16,Rest/binary>>) ->
	ipv4(Packet, VlanTag, up(EthType, ?ETH_P_IP), PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, FlowTab,
		Rest);
eth(Packet, VlanTag, EthType, PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, PortInfo, Actions, FlowTab,
	<<?ETH_P_IPV6:16,Rest/binary>>) ->
	ipv6(Packet, VlanTag, up(EthType, ?ETH_P_IPV6), PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, FlowTab,
		Rest);
eth(Packet, VlanTag, EthType, PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, {InPort,InPhyPort,TunnelId} = _PortInfo, Actions, FlowTab,
	<<?ETH_P_ARP:16,Rest/binary>>) ->
	%% ARP frame
	FlowTab:match(Packet,
		VlanTag,
		up(EthType, ?ETH_P_ARP),
		PbbTag,
		MplsTag,
		Ip4Hdr,
		Ip6Hdr,
		bin16(Ip6Ext),
		IpTclass,
		IpProto,
		Rest,		%% ArpMsg
		undefined,	%% IcmpMsg
		undefined,	%% Icmp6Hdr
		undefined,	%% Icmp6Sll
		undefined,	%% Icmp6Tll
		undefined,	%% TcpHdr
		undefined,	%% UdpHdr
		undefined,	%% SctpHdr
		Metadata,
		InPort,
		InPhyPort,
		TunnelId,
		Actions).

ipv4(Packet, VlanTag, EthType, PbbTag, MplsTag,
		undefined = _Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, FlowTab,
		<<(4 *16 +5),IpTclass1:1/binary,_Skip:7/binary,IpProto1,_/binary>> =IpHdrRest) ->
		%% IHL is 5, no options
		<<Ip4Hdr:20/binary,Rest/binary>> = IpHdrRest,
		proto(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, up(IpTclass, IpTclass1), up(IpProto, IpProto1),
			Metadata, PortInfo, Actions, FlowTab,
			IpProto1, Rest);
ipv4(Packet, VlanTag, EthType, PbbTag, MplsTag,
		undefined = _Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, FlowTab,
		<<4:4,IHL:4,IpTclass1:1/binary,_Skip:7/binary,IpProto1,_/binary>> =IpHdrRest) ->
		HdrLen = IHL *4,
		<<Ip4Hdr:(HdrLen)/binary,Rest/binary>> = IpHdrRest,
		proto(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, up(IpTclass, IpTclass1), up(IpProto, IpProto1),
			Metadata, PortInfo, Actions, FlowTab,
			IpProto1, Rest);
ipv4(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, FlowTab,
		<<4:4,IHL:4,_:8/binary,LastProto,_/binary>> =IpHdrRest) ->
		HdrLen = IHL *4,
		<<_Skip:(HdrLen)/binary,Rest/binary>> = IpHdrRest,
		proto(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
			Metadata, PortInfo, Actions, FlowTab,
			LastProto, Rest).

ipv6(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, undefined = _Ip6Hdr, _Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, FlowTab,
		<<_:4,IpTclass1:1/binary,_/bits>> =HdrRest) ->
		%% IPv6 header first seen
		<<Ip6Hdr:40/binary,Rest/binary>> =HdrRest,
		Next = binary:at(Ip6Hdr, 6),
		ipv6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, ?IPV6_EXT_NONEXT, up(IpTclass, IpTclass1), IpProto,
			Metadata, PortInfo, Actions, FlowTab,
			Next, Rest);
ipv6(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, FlowTab,
		<<SkipHdr:40/binary,Rest/binary>>) ->
		Next = binary:at(SkipHdr, 6),
		ipv6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
			Metadata, PortInfo, Actions, FlowTab,
			Next, Rest).

ipv6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, FlowTab,
		?IPV6_HDR_HOP =Q, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>) ->
		ipv6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, ext_flags(Ip6Ext, Q), IpTclass, IpProto,
			Metadata, PortInfo, Actions, FlowTab,
			Next, Rest);
ipv6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, FlowTab,
		?IPV6_HDR_DEST =Q, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>) ->
		ipv6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, ext_flags(Ip6Ext, Q), IpTclass, IpProto,
			Metadata, PortInfo, Actions, FlowTab,
			Next, Rest);
ipv6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, FlowTab,
		?IPV6_HDR_ROUTER =Q, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>) ->
		ipv6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, ext_flags(Ip6Ext, Q), IpTclass, IpProto,
			Metadata, PortInfo, Actions, FlowTab,
			Next, Rest);
ipv6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, FlowTab,
		?IPV6_HDR_FRAG =Q, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>) ->
		ipv6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, ext_flags(Ip6Ext, Q), IpTclass, IpProto,
			Metadata, PortInfo, Actions, FlowTab,
			Next, Rest);
ipv6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, FlowTab,
		?IPV6_HDR_AUTH =Q, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>) ->
		ipv6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, ext_flags(Ip6Ext, Q), IpTclass, IpProto,
			Metadata, PortInfo, Actions, FlowTab,
			Next, Rest);
ipv6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, FlowTab,
		?IPV6_HDR_ESP =Q, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>) ->
		ipv6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, ext_flags(Ip6Ext, Q), IpTclass, IpProto,
			Metadata, PortInfo, Actions, FlowTab,
			Next, Rest);
ipv6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, {InPort,InPhyPort,TunnelId} = _PortInfo, Actions, FlowTab,
		?IPV6_HDR_NONEXT =Q, _Rest) ->
	%% Empty IPv6 frame
	FlowTab:match(Packet,
		VlanTag,
		EthType,
		PbbTag,
		MplsTag,
		Ip4Hdr,
		Ip6Hdr,
		bin16(Ip6Ext),
		IpTclass,
		up(IpProto, Q),
		undefined,	%% ArpMsg
		undefined,	%% IcmpMsg
		undefined,	%% Icmp6Hdr
		undefined,	%% Icmp6Sll
		undefined,	%% Icmp6Tll
		undefined,	%% TcpHdr
		undefined,	%% UdpHdr
		undefined,	%% SctpHdr
		Metadata,
		InPort,
		InPhyPort,
		TunnelId,
		Actions);
ipv6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
			Metadata, PortInfo, Actions, FlowTab,
			LastProto, Rest) ->
	proto(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, up(IpProto, LastProto),
		Metadata, PortInfo, Actions, FlowTab,
		LastProto, Rest).

ipv6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, FlowTab,
		?IPV6_HDR_HOP, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>) ->
		ipv6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
			Metadata, PortInfo, Actions, FlowTab,
			Next, Rest);
ipv6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, FlowTab,
		?IPV6_HDR_DEST, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>) ->
		ipv6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
			Metadata, PortInfo, Actions, FlowTab,
			Next, Rest);
ipv6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, FlowTab,
		?IPV6_HDR_ROUTER, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>) ->
		ipv6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
			Metadata, PortInfo, Actions, FlowTab,
			Next, Rest);
ipv6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, FlowTab,
		?IPV6_HDR_FRAG, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>) ->
		ipv6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
			Metadata, PortInfo, Actions, FlowTab,
			Next, Rest);
ipv6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, FlowTab,
		?IPV6_HDR_AUTH, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>) ->
		ipv6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
			Metadata, PortInfo, Actions, FlowTab,
			Next, Rest);
ipv6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, FlowTab,
		?IPV6_HDR_ESP, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>) ->
		ipv6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
			Metadata, PortInfo, Actions, FlowTab,
			Next, Rest);
ipv6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, {InPort,InPhyPort,TunnelId} = _PortInfo, Actions, FlowTab,
		?IPV6_HDR_NONEXT, _Rest) -> 
	%% Empty IPv6 frame
	FlowTab:match(Packet,
		VlanTag,
		EthType,
		PbbTag,
		MplsTag,
		Ip4Hdr,
		Ip6Hdr,
		bin16(Ip6Ext),
		IpTclass,
		IpProto,
		undefined,	%% ArpMsg
		undefined,	%% IcmpMsg
		undefined,	%% Icmp6Hdr
		undefined,	%% Icmp6Sll
		undefined,	%% Icmp6Tll
		undefined,	%% TcpHdr
		undefined,	%% UdpHdr
		undefined,	%% SctpHdr
		Metadata,
		InPort,
		InPhyPort,
		TunnelId,
		Actions);
ipv6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, FlowTab,
		LastProto, Rest) -> 
		proto(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
			Metadata, PortInfo, Actions, FlowTab,
			LastProto, Rest).

proto(Packet, VlanTag, EthType, PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, PortInfo, Actions, FlowTab,
	?IPPROTO_IP, Rest) ->
	ipv4(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, FlowTab,
		Rest);
proto(Packet, VlanTag, EthType, PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, PortInfo, Actions, FlowTab,
	?IPPROTO_IPV6, Rest) ->
	ipv6(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, FlowTab,
		Rest);
proto(Packet, VlanTag, EthType, PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, {InPort,InPhyPort,TunnelId} = _PortInfo, Actions, FlowTab,
	?IPPROTO_ICMP, IcmpMsg) ->
	%% ICMP frame
	FlowTab:match(Packet,
		VlanTag,
		EthType,
		PbbTag,
		MplsTag,
		Ip4Hdr,
		Ip6Hdr,
		bin16(Ip6Ext),
		IpTclass,
		IpProto,
		undefined,	%% ArpMsg
		IcmpMsg,	%% IcmpMsg
		undefined,	%% Icmp6Hdr
		undefined,	%% Icmp6Sll
		undefined,	%% Icmp6Tll
		undefined,	%% TcpHdr
		undefined,	%% UdpHdr
		undefined,	%% SctpHdr
		Metadata,
		InPort,
		InPhyPort,
		TunnelId,
		Actions);
proto(Packet, VlanTag, EthType, PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, PortInfo, Actions, FlowTab,
	?IPPROTO_ICMPV6, Icmp6Packet) ->
	icmpv6(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, FlowTab,
		Icmp6Packet);
proto(Packet, VlanTag, EthType, PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, {InPort,InPhyPort,TunnelId} = _PortInfo, Actions, FlowTab,
	?IPPROTO_TCP, TcpHdrLoad) ->
	%% ICMP frame
	FlowTab:match(Packet,
		VlanTag,
		EthType,
		PbbTag,
		MplsTag,
		Ip4Hdr,
		Ip6Hdr,
		bin16(Ip6Ext),
		IpTclass,
		IpProto,
		undefined,	%% ArpMsg
		undefined,	%% IcmpMsg
		undefined,	%% Icmp6Hdr
		undefined,	%% Icmp6Sll
		undefined,	%% Icmp6Tll
		TcpHdrLoad,	%% TcpHdr
		undefined,	%% UdpHdr
		undefined,	%% SctpHdr
		Metadata,
		InPort,
		InPhyPort,
		TunnelId,
		Actions);
proto(Packet, VlanTag, EthType, PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, {InPort,InPhyPort,TunnelId} = _PortInfo, Actions, FlowTab,
	?IPPROTO_UDP, UdpHdrLoad) ->
	%% ICMP frame
	FlowTab:match(Packet,
		VlanTag,
		EthType,
		PbbTag,
		MplsTag,
		Ip4Hdr,
		Ip6Hdr,
		bin16(Ip6Ext),
		IpTclass,
		IpProto,
		undefined,	%% ArpMsg
		undefined,	%% IcmpMsg
		undefined,	%% Icmp6Hdr
		undefined,	%% Icmp6Sll
		undefined,	%% Icmp6Tll
		undefined,	%% TcpHdr
		UdpHdrLoad,	%% UdpHdr
		undefined,	%% SctpHdr
		Metadata,
		InPort,
		InPhyPort,
		TunnelId,
		Actions);
proto(Packet, VlanTag, EthType, PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, {InPort,InPhyPort,TunnelId} = _PortInfo, Actions, FlowTab,
	?IPPROTO_SCTP, SctpHdrLoad) ->
	%% ICMP frame
	FlowTab:match(Packet,
		VlanTag,
		EthType,
		PbbTag,
		MplsTag,
		Ip4Hdr,
		Ip6Hdr,
		bin16(Ip6Ext),
		IpTclass,
		IpProto,
		undefined,	%% ArpMsg
		undefined,	%% IcmpMsg
		undefined,	%% Icmp6Hdr
		undefined,	%% Icmp6Sll
		undefined,	%% Icmp6Tll
		undefined,	%% TcpHdr
		undefined,	%% UdpHdr
		SctpHdrLoad,%% SctpHdr
		Metadata,
		InPort,
		InPhyPort,
		TunnelId,
		Actions).

%% IPPROTO_GRE not expected

icmpv6(Packet, VlanTag, EthType, PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, {InPort,InPhyPort,TunnelId} = _PortInfo, Actions, FlowTab,
	<<?ICMPV6_NDP_NS,_:23/binary,Opts/binary>> =Icmp6Hdr) ->
	%% ICMPv6 NDP frame
	Icmp6Sll = icmpv6_opt(?NDP_OPT_SLL, Opts),
	FlowTab:match(Packet,
		VlanTag,
		EthType,
		PbbTag,
		MplsTag,
		Ip4Hdr,
		Ip6Hdr,
		bin16(Ip6Ext),
		IpTclass,
		IpProto,
		undefined,	%% ArpMsg
		undefined,	%% IcmpMsg
		Icmp6Hdr,	%% Icmp6Hdr
		Icmp6Sll,	%% Icmp6Sll
		undefined,	%% Icmp6Tll
		undefined,	%% TcpHdr
		undefined,	%% UdpHdr
		undefined,	%% SctpHdr
		Metadata,
		InPort,
		InPhyPort,
		TunnelId,
		Actions);
icmpv6(Packet, VlanTag, EthType, PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, {InPort,InPhyPort,TunnelId} = _PortInfo, Actions, FlowTab,
	<<?ICMPV6_NDP_NA,_:23/binary,Opts/binary>> =Icmp6Hdr) ->
	%% ICMPv6 NDP frame
	Icmp6Tll = icmpv6_opt(?NDP_OPT_TLL, Opts),
	FlowTab:match(Packet,
		VlanTag,
		EthType,
		PbbTag,
		MplsTag,
		Ip4Hdr,
		Ip6Hdr,
		bin16(Ip6Ext),
		IpTclass,
		IpProto,
		undefined,	%% ArpMsg
		undefined,	%% IcmpMsg
		Icmp6Hdr,	%% Icmp6Hdr
		undefined,	%% Icmp6Sll
		Icmp6Tll,	%% Icmp6Tll
		undefined,	%% TcpHdr
		undefined,	%% UdpHdr
		undefined,	%% SctpHdr
		Metadata,
		InPort,
		InPhyPort,
		TunnelId,
		Actions);
icmpv6(Packet, VlanTag, EthType, PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, {InPort,InPhyPort,TunnelId} = _PortInfo, Actions, FlowTab,
	Icmp6Hdr) ->
	%% ICMPv6 frame
	FlowTab:match(Packet,
		VlanTag,
		EthType,
		PbbTag,
		MplsTag,
		Ip4Hdr,
		Ip6Hdr,
		bin16(Ip6Ext),
		IpTclass,
		IpProto,
		undefined,	%% ArpMsg
		undefined,	%% IcmpMsg
		Icmp6Hdr,	%% Icmp6Hdr
		undefined,	%% Icmp6Sll
		undefined,	%% Icmp6Tll
		undefined,	%% TcpHdr
		undefined,	%% UdpHdr
		undefined,	%% SctpHdr
		Metadata,
		InPort,
		InPhyPort,
		TunnelId,
		Actions).

icmpv6_opt(_OptType, <<>>) ->
	undefined;
icmpv6_opt(OptType, <<OptType,Len,Rest/binary>>) ->
	OptLen = 8 *Len -2,
	<<Data:OptLen/binary,_/binary>> = Rest,
	Data;
icmpv6_opt(OptType, <<_,Len,Rest/binary>>) ->
	OptLen = 8 *Len -2,
	<<_:OptLen/binary,Opts/binary>> = Rest,
	icmpv6_opt(OptType, Opts).

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

up(undefined, New) -> New;
up(Old, _New) -> Old.

bin16(undefined) -> undefined;
bin16(Int) -> <<Int:16>>.

%%EOF
