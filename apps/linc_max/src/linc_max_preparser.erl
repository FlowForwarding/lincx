%%
%%
%%

%% @author Cloudozer, LLP. <info@cloudozer.com>
%% @copyright 2014 FlowForwarding.org
%% @doc Packet preparser
-module(linc_max_preparser).

-include_lib("pkt/include/pkt.hrl").
-include("pkt_max.hrl").

-include("fast_path.hrl").

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

%% @doc Inject/reinject a packet into a flow pipeline.
inject(<<_:12/binary,Rest/binary>> =Packet, Metadata, PortInfo, Actions, Blaze) ->
	ether(Packet,
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
		Blaze,
		%% standard arguments end
		Rest).

%% "Ethernet type of the OpenFlow packet payload, after VLAN tags."

%% @doc Consume VLAN/PBB/MPLS tags
ether(Packet, undefined = _VlanTag, EthType, PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, PortInfo, Actions, Blaze,
	<<?ETH_P_802_1Q:16,VlanTag:2/binary,Rest/binary>>) ->
	%% The first VLAN tag, keep
	ether(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, Blaze,
		Rest);
ether(Packet, VlanTag, EthType, PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, PortInfo, Actions, Blaze,
	<<?ETH_P_802_1Q:16,_SkipVlanTag:2/binary,Rest/binary>>) ->
	%% The second/third VLAN Tag, ignore
	ether(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, Blaze,
		Rest);

%% See https://sites.google.com/site/amitsciscozone/home/pbb/understanding-pbb 

ether(Packet, undefined = _VlanTag, EthType, PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, PortInfo, Actions, Blaze,
	<<?ETH_P_PBB_B:16,VlanTag:2/binary,Rest/binary>>) ->
	%% 802.1ad (or 802.1ah) frame, keep S-VLAN tag
	%%NB: EthType not set
	ether(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, Blaze,
		Rest);
ether(Packet, VlanTag, EthType, PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, PortInfo, Actions, Blaze,
	<<?ETH_P_PBB_B:16,_Skip:2/binary,Rest/binary>>) ->
	%% 802.1ad (or 802.1ah) frame, skip
	%%NB: EthType not set
	ether(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, Blaze,
		Rest);
ether(Packet, VlanTag, EthType, undefined = _PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, PortInfo, Actions, Blaze,
	<<?ETH_P_PBB_I:16,PbbTag:4/binary,_Skip:(6+6)/binary,Rest/binary>>) ->
	%% 802.1ah frame, keep I-TAG
	ether(Packet, VlanTag, up(EthType, ?ETH_P_PBB_I), PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, Blaze,
		Rest);
ether(Packet, VlanTag, EthType, PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, PortInfo, Actions, Blaze,
	<<?ETH_P_PBB_I:16,_Skip:(4+6+6)/binary,Rest/binary>>) ->
	%% 802.1ah frame, skip
	ether(Packet, VlanTag, up(EthType, ?ETH_P_PBB_I), PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, Blaze,
		Rest);

%% http://en.wikipedia.org/wiki/Multiprotocol_Label_Switching

%%
%% MPLS encapsulates IP datagrams only (no eth_type)?
%% 
ether(Packet, VlanTag, EthType, PbbTag, _MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, PortInfo, Actions, Blaze,
	<<?ETH_P_MPLS_UNI:16,Rest/binary>>) ->
	%% MPLS unicast frame
	<<MplsTag:4/binary,_/binary>> =Rest,
	mpls(Packet, VlanTag, up(EthType, ?ETH_P_MPLS_UNI), PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, Blaze,
		Rest);
ether(Packet, VlanTag, EthType, PbbTag, _MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, PortInfo, Actions, Blaze,
	<<?ETH_P_MPLS_MULTI:16,Rest/binary>>) ->
	%% MPLS multicast frame
	<<MplsTag:4/binary,_/binary>> =Rest,
	mpls(Packet, VlanTag, up(EthType, ?ETH_P_MPLS_MULTI), PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, Blaze,
		Rest);

%% More conventional Ethernet types

ether(Packet, VlanTag, EthType, PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, PortInfo, Actions, Blaze,
	<<?ETH_P_IP:16,Rest/binary>>) ->
	ipv4(Packet, VlanTag, up(EthType, ?ETH_P_IP), PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, Blaze,
		Rest);
ether(Packet, VlanTag, EthType, PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, PortInfo, Actions, Blaze,
	<<?ETH_P_IPV6:16,Rest/binary>>) ->
	ipv6(Packet, VlanTag, up(EthType, ?ETH_P_IPV6), PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, Blaze,
		Rest);
ether(Packet, VlanTag, EthType, PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, {InPort,InPhyPort,TunnelId} = _PortInfo, Actions, #blaze{start_at =FlowTab} =Blaze,
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
		Actions,
		Blaze);

%% Non-common Ethernet type - never a match
ether(_Packet, _VlanTag, _EthType, _PbbTag, _MplsTag,
	_Ip4Hdr, _Ip6Hdr, _Ip6Ext, _IpTclass, _IpProto,
	_Metadata, _PortInfo, _Actions, _Blaze, _Rest) ->
	miss.
	
mpls(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, Blaze,
		<<_:23,1:1,_,Rest/binary>>) ->
		%% bottom reached
		case Rest of
		<<4:4,_/bits>> ->
			ipv4(Packet, VlanTag, EthType, PbbTag, MplsTag,
				Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
				Metadata, PortInfo, Actions, Blaze,
				Rest);
		<<6:4,_/bits>> ->
			ipv6(Packet, VlanTag, EthType, PbbTag, MplsTag,
				Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
				Metadata, PortInfo, Actions, Blaze,
				Rest)
		end;
mpls(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, Blaze,
		<<_SkipTag:4/binary,Rest/binary>>) ->
		mpls(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
			Metadata, PortInfo, Actions, Blaze,
			Rest).

ipv4(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, {InPort,InPhyPort,TunnelId}, Actions, #blaze{start_at =FlowTab} =Blaze,
		<<_:51,FragOff:13,_/binary>>) when FragOff > 0 ->
		%% not the first fragment - match as is
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
		Actions,
		Blaze);

ipv4(Packet, VlanTag, EthType, PbbTag, MplsTag,
		undefined = _Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, Blaze,
		<<(4 *16 +5),IpTclass1:1/binary,_Skip:7/binary,IpProto1,_/binary>> =IpHdrRest) ->
		%% IHL is 5, no options
		<<Ip4Hdr:20/binary,Rest/binary>> = IpHdrRest,
		proto(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, up(IpTclass, IpTclass1), up(IpProto, IpProto1),
			Metadata, PortInfo, Actions, Blaze,
			IpProto1, Rest);
ipv4(Packet, VlanTag, EthType, PbbTag, MplsTag,
		undefined = _Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, Blaze,
		<<4:4,IHL:4,IpTclass1:1/binary,_Skip:7/binary,IpProto1,_/binary>> =IpHdrRest) ->
		HdrLen = IHL *4,
		<<Ip4Hdr:(HdrLen)/binary,Rest/binary>> = IpHdrRest,
		proto(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, up(IpTclass, IpTclass1), up(IpProto, IpProto1),
			Metadata, PortInfo, Actions, Blaze,
			IpProto1, Rest);
ipv4(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, Blaze,
		<<4:4,IHL:4,_:8/binary,LastProto,_/binary>> =IpHdrRest) ->
		HdrLen = IHL *4,
		<<_Skip:(HdrLen)/binary,Rest/binary>> = IpHdrRest,
		proto(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
			Metadata, PortInfo, Actions, Blaze,
			LastProto, Rest).

ipv6(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, undefined = _Ip6Hdr, _Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, Blaze,
		<<_:4,IpTclass1:1/binary,_/bits>> =HdrRest) ->
		%% IPv6 header first seen
		<<Ip6Hdr:40/binary,Rest/binary>> =HdrRest,
		Next = binary:at(Ip6Hdr, 6),
		ipv6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, 0, up(IpTclass, IpTclass1), IpProto,
			Metadata, PortInfo, Actions, Blaze,
			Next, Rest);
ipv6(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, Blaze,
		<<SkipHdr:40/binary,Rest/binary>>) ->
		Next = binary:at(SkipHdr, 6),
		ipv6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
			Metadata, PortInfo, Actions, Blaze,
			Next, Rest).

ipv6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, Blaze,
		?IPV6_HDR_HOP_BY_HOP =Q, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>) ->
		ipv6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, ext_flags(Ip6Ext, Q), IpTclass, IpProto,
			Metadata, PortInfo, Actions, Blaze,
			Next, Rest);
ipv6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, Blaze,
		?IPV6_HDR_DEST_OPTS =Q, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>) ->
		ipv6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, ext_flags(Ip6Ext, Q), IpTclass, IpProto,
			Metadata, PortInfo, Actions, Blaze,
			Next, Rest);
ipv6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, Blaze,
		?IPV6_HDR_ROUTING =Q, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>) ->
		ipv6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, ext_flags(Ip6Ext, Q), IpTclass, IpProto,
			Metadata, PortInfo, Actions, Blaze,
			Next, Rest);
ipv6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, Blaze,
		?IPV6_HDR_FRAGMENT =Q, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>) ->
		ipv6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, ext_flags(Ip6Ext, Q), IpTclass, IpProto,
			Metadata, PortInfo, Actions, Blaze,
			Next, Rest);
ipv6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, Blaze,
		?IPV6_HDR_AUTH =Q, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>) ->
		ipv6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, ext_flags(Ip6Ext, Q), IpTclass, IpProto,
			Metadata, PortInfo, Actions, Blaze,
			Next, Rest);
ipv6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, Blaze,
		?IPV6_HDR_ESP =Q, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>) ->
		ipv6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, ext_flags(Ip6Ext, Q), IpTclass, IpProto,
			Metadata, PortInfo, Actions, Blaze,
			Next, Rest);
ipv6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, {InPort,InPhyPort,TunnelId} = _PortInfo, Actions, #blaze{start_at =FlowTab} =Blaze,
		?IPV6_HDR_NO_NEXT_HEADER =Q, _Rest) ->
	%% Empty IPv6 frame
	FlowTab:match(Packet,
		VlanTag,
		EthType,
		PbbTag,
		MplsTag,
		Ip4Hdr,
		Ip6Hdr,
		bin16(ext_flags(Ip6Ext, Q)),
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
		Actions,
		Blaze);
ipv6_chain(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
			Metadata, PortInfo, Actions, Blaze,
			LastProto, Rest) ->
	proto(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, up(IpProto, LastProto),
		Metadata, PortInfo, Actions, Blaze,
		LastProto, Rest).

ipv6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, Blaze,
		?IPV6_HDR_HOP_BY_HOP, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>) ->
		ipv6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
			Metadata, PortInfo, Actions, Blaze,
			Next, Rest);
ipv6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, Blaze,
		?IPV6_HDR_DEST_OPTS, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>) ->
		ipv6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
			Metadata, PortInfo, Actions, Blaze,
			Next, Rest);
ipv6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, Blaze,
		?IPV6_HDR_ROUTING, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>) ->
		ipv6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
			Metadata, PortInfo, Actions, Blaze,
			Next, Rest);
ipv6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, Blaze,
		?IPV6_HDR_FRAGMENT, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>) ->
		ipv6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
			Metadata, PortInfo, Actions, Blaze,
			Next, Rest);
ipv6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, Blaze,
		?IPV6_HDR_AUTH, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>) ->
		ipv6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
			Metadata, PortInfo, Actions, Blaze,
			Next, Rest);
ipv6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, Blaze,
		?IPV6_HDR_ESP, <<Next,Len,_:6/binary,_:(Len)/binary-unit:64,Rest/binary>>) ->
		ipv6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
			Metadata, PortInfo, Actions, Blaze,
			Next, Rest);
ipv6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, {InPort,InPhyPort,TunnelId} = _PortInfo, Actions, #blaze{start_at =FlowTab} =Blaze,
		?IPV6_HDR_NO_NEXT_HEADER, _Rest) -> 
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
		Actions,
		Blaze);
ipv6_skip(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, Blaze,
		LastProto, Rest) -> 
		proto(Packet, VlanTag, EthType, PbbTag, MplsTag,
			Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
			Metadata, PortInfo, Actions, Blaze,
			LastProto, Rest).

proto(Packet, VlanTag, EthType, PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, PortInfo, Actions, Blaze,
	?IPPROTO_IP, Rest) ->
	ipv4(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, Blaze,
		Rest);
proto(Packet, VlanTag, EthType, PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, PortInfo, Actions, Blaze,
	?IPPROTO_IPV6, Rest) ->
	ipv6(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, Blaze,
		Rest);
proto(Packet, VlanTag, EthType, PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, {InPort,InPhyPort,TunnelId} = _PortInfo, Actions, #blaze{start_at =FlowTab} =Blaze,
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
		Actions,
		Blaze);
proto(Packet, VlanTag, EthType, PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, PortInfo, Actions, Blaze,
	?IPPROTO_ICMPV6, Icmp6Packet) ->
	icmpv6(Packet, VlanTag, EthType, PbbTag, MplsTag,
		Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
		Metadata, PortInfo, Actions, Blaze,
		Icmp6Packet);
proto(Packet, VlanTag, EthType, PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, {InPort,InPhyPort,TunnelId} = _PortInfo, Actions, #blaze{start_at =FlowTab} =Blaze,
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
		Actions,
		Blaze);
proto(Packet, VlanTag, EthType, PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, {InPort,InPhyPort,TunnelId} = _PortInfo, Actions, #blaze{start_at =FlowTab} =Blaze,
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
		Actions,
		Blaze);
proto(Packet, VlanTag, EthType, PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, {InPort,InPhyPort,TunnelId} = _PortInfo, Actions, #blaze{start_at =FlowTab} =Blaze,
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
		Actions,
		Blaze).

%% IPPROTO_GRE not expected

icmpv6(Packet, VlanTag, EthType, PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, {InPort,InPhyPort,TunnelId} = _PortInfo, Actions, #blaze{start_at =FlowTab} =Blaze,
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
		Actions,
		Blaze);
icmpv6(Packet, VlanTag, EthType, PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, {InPort,InPhyPort,TunnelId} = _PortInfo, Actions, #blaze{start_at =FlowTab} =Blaze,
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
		Actions,
		Blaze);
icmpv6(Packet, VlanTag, EthType, PbbTag, MplsTag,
	Ip4Hdr, Ip6Hdr, Ip6Ext, IpTclass, IpProto,
	Metadata, {InPort,InPhyPort,TunnelId} = _PortInfo, Actions, #blaze{start_at =FlowTab} =Blaze,
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
		Actions,
		Blaze).

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

%% set IPV6_EXT_UNREP flag if repeated
%% set IPV6_EXT_UNSEQ flag if out of order
ext_flags(Ip6Ext, ?IPV6_HDR_NO_NEXT_HEADER) ->
	%% NB: order or repeatition not checked
	Ip6Ext bor ?IPV6_EXT_NONEXT;
ext_flags(Ip6Ext, ?IPV6_HDR_HOP_BY_HOP) ->
	Ip6Ext bor ?IPV6_EXT_HOP
		bor repeated(Ip6Ext, ?IPV6_EXT_HOP)
		bor out_of_order(Ip6Ext, ?IPV6_EXT_HOP);
ext_flags(Ip6Ext, ?IPV6_HDR_DEST_OPTS) ->
	%% Destination Options header can be repeated
	%% NB: order not checked strictly
	Ip6Ext bor ?IPV6_EXT_DEST;
ext_flags(Ip6Ext, ?IPV6_HDR_ROUTING) ->
	Ip6Ext bor ?IPV6_EXT_ROUTER
		bor repeated(Ip6Ext, ?IPV6_EXT_ROUTER)
		bor out_of_order(Ip6Ext, ?IPV6_EXT_HOP bor
								 ?IPV6_EXT_DEST bor
								 ?IPV6_EXT_ROUTER);
ext_flags(Ip6Ext, ?IPV6_HDR_FRAGMENT) ->
	Ip6Ext bor ?IPV6_EXT_FRAG
		bor repeated(Ip6Ext, ?IPV6_EXT_FRAG)
		bor out_of_order(Ip6Ext, ?IPV6_EXT_HOP bor
								 ?IPV6_EXT_DEST bor
								 ?IPV6_EXT_ROUTER bor
								 ?IPV6_EXT_FRAG);
ext_flags(Ip6Ext, ?IPV6_HDR_AUTH) ->
	Ip6Ext bor ?IPV6_EXT_AUTH
		bor repeated(Ip6Ext, ?IPV6_EXT_AUTH)
		bor out_of_order(Ip6Ext, ?IPV6_EXT_HOP bor
								 ?IPV6_EXT_DEST bor
								 ?IPV6_EXT_ROUTER bor
								 ?IPV6_EXT_FRAG bor
								 ?IPV6_EXT_AUTH);
ext_flags(Ip6Ext, ?IPV6_HDR_ESP) ->
	Ip6Ext bor ?IPV6_EXT_ESP
		bor repeated(Ip6Ext, ?IPV6_EXT_ESP).

repeated(Ip6Ext, Mask) when Ip6Ext band Mask =/= 0 -> ?IPV6_EXT_UNREP;
repeated(_Ip6Ext, _Mask) -> 0.

out_of_order(Ip6Ext, Mask)
	when Ip6Ext band (bnot Mask)
				band (bnot ?IPV6_EXT_UNREP)
				band (bnot ?IPV6_EXT_UNSEQ) =/= 0 -> ?IPV6_EXT_UNSEQ;
out_of_order(_Ip6Ext, _Mask) -> 0.

up(undefined, New) -> New;
up(Old, _New) -> Old.

bin16(undefined) -> undefined;
bin16(Int) -> <<Int:16>>.

%%EOF
