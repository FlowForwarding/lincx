%%
%%
%%

%% @author Cloudozer, LLP. <info@cloudozer.com>
%% @copyright 2014 FlowForwarding.org
-module(linc_max_tests).

%% mock flow table interface
-export([match/23]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("pkt/include/pkt.hrl").
-include("linc_max.hrl").

port_info_test_() ->
	AnyFrame = pkt:encapsulate([#ether{},#ipv4{},#udp{}]),
	AnyMeta = <<0:64>>,

	[fun() ->
		PortInfo = {InPort,InPhyPort,TunnelId},
		Ms = linc_max_preparser:inject(AnyFrame, AnyMeta, PortInfo,
									  #actions{}, ?MODULE),
		present(Present, Ms),
		absent(Absent, Ms)
	 end
		|| {InPort,InPhyPort,TunnelId,Present,Absent} <-

		[{42,undefined,undefined,[{in_port,42}],[in_phy_port,tunnel_id]},
		 {1,42,undefined,[{in_phy_port,42}],[tunnel_id]},
		 {undefined,undefined,undefined,[],[in_port,in_phy_port,tunnel_id]},
		 {1,1,<<42:64>>,[{tunnel_id,<<42:64>>}],[]}]].

metadata_test() ->
	AnyFrame = pkt:encapsulate([#ether{},#ipv4{},#udp{}]),
	AnyPortInfo = {1,1,undefined},
	Ms = linc_max_preparser:inject(AnyFrame, <<42:64>>, AnyPortInfo,
								  #actions{}, ?MODULE),
	present([{metadata,<<42:64>>}], Ms).

fields_test_() ->
	[].

%%TODO

%% Utils -----------------------------------------------------------------------

present(Present, Ms) ->
	lists:foreach(fun(S) when is_atom(S) ->
		true = lists:keymember(S, Ms);
	(S) ->
		true = lists:member(S, Ms)
	end, Present).

absent(Absent, Ms) ->
	lists:foreach(fun(S) when is_atom(S) ->
		false = lists:keymember(S, 1, Ms);
	(S) ->
		false = lists:member(S, Ms)
	end, Absent).

%% Mock flow table -------------------------------------------------------------

match(_ = _Packet,
      _ = VlanTag,
      _ = EthType,
      _ = PbbTag,
      _ = MplsTag,
      _ = Ip4Hdr,
      _ = Ip6Hdr,
      _ = Ip6Ext,
      _ = IpTclass,
      _ = IpProto,
      _ = ArpMsg,
      _ = IcmpMsg,
      _ = Icmp6Hdr,
      _ = Icmp6OptSll,
      _ = Icmp6OptTll,
      _ = TcpHdr,
      _ = UdpHdr,
      _ = SctpHdr,
      _ = Metadata,
      _ = InPort,
      _ = InPhyPort,
      _ = TunnelId,
      _ = _Actions) ->
	Hdrs = 
		[{vlan_tag,VlanTag},
		 {eth_type,EthType},
		 {pbb_tag,PbbTag},
		 {mpls_tag,MplsTag},
		 {ip4_hdr,Ip4Hdr},
		 {ip6_hdr,Ip6Hdr},
		 {ip6_ext,Ip6Ext},
		 {ip_tclass,IpTclass},
		 {ip_proto,IpProto},
		 {arp_msg,ArpMsg},
		 {icmp_msg,IcmpMsg},
		 {icmp6_hdr,Icmp6Hdr},
		 {icmp6_sll,Icmp6OptSll},
		 {icmp6_tll,Icmp6OptTll},
		 {tcp_hdr,TcpHdr},
		 {udp_hdr,UdpHdr},
		 {sctp_hdr,SctpHdr},
		 {metadata,Metadata},
		 {in_port,InPort},
		 {in_phy_port,InPhyPort},
		 {tunnel_id,TunnelId}],

	lists:filter(fun({_,V}) -> V =/= undefined end, Hdrs).

%%EOF
