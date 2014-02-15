%%
%%
%%

%% @author Cloudozer, LLP. <info@cloudozer.com>
%% @copyright 2014 FlowForwarding.org
-module(linc_max_preparser_tests).

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

	[{42,undefined,undefined,
			[{in_port,42}],[in_phy_port,tunnel_id]},
	 {1,42,undefined,
			[{in_phy_port,42}],[tunnel_id]},
	 {undefined,undefined,undefined,
			[],[in_port,in_phy_port,tunnel_id]},
	 {1,1,<<42:64>>,
			[{tunnel_id,<<42:64>>}],[]}]].

metadata_test() ->
	AnyFrame = pkt:encapsulate([#ether{},#ipv4{},#udp{}]),
	AnyPortInfo = {1,1,undefined},
	Ms = linc_max_preparser:inject(AnyFrame, <<42:64>>, AnyPortInfo,
								  #actions{}, ?MODULE),
	present([{metadata,<<42:64>>}], Ms).

fields_test_() ->
	AnyMeta = <<0:64>>,
	AnyPortInfo = {1,1,undefined},
	Ipv6 = #ipv6{saddr = <<0:128>>,daddr = <<0:128>>},
	Mac = <<1,2,3,4,5,6>>,

	[fun() ->
		Frame = pkt:encapsulate(Pkt),
		io:format("~p: ~p\n", [Present,Frame]),
		Ms = linc_max_preparser:inject(Frame, AnyMeta, AnyPortInfo,
									  #actions{}, ?MODULE),
		present(Present, Ms),
		absent(Absent, Ms)
	 end
		|| {Pkt,Present,Absent} <-

	[{[#ether{},#ipv4{},#udp{}],
			[{eth_type,16#800}],[]},
	 {[#ether{},Ipv6,#udp{}],
			[{eth_type,16#86dd}],[]},

	 %% Ethernet type of the OpenFlow packet payload, after VLAN tags.
	 {[#ether{},#ieee802_1q_tag{},#ipv4{},#udp{}],
			[{eth_type,16#800}],[]},

	 %% pcp:3 cfi:1 vid:12
	 {[#ether{},#ieee802_1q_tag{pcp =5,cfi =0,vid = <<42:12>>},#ipv4{},#udp{}],
			[{vlan_tag,<<5:3,0:1,42:12>>}],[]},
	 {[#ether{},#ipv4{},#udp{}],
			[],[ieee802_1q_tag]},

	 %% dscp:6 ecn:2
	 {[#ether{},#ipv4{dscp =42,ecn =2},#udp{}],
			[{ip_tclass,<<42:6,2:2>>}],[]},
	 {[#ether{},Ipv6#ipv6{class =126},#udp{}],
			[{ip_tclass,<<126>>}],[]},
	 {[#ether{},#arp{}],
			[],[ip_tclass]},

	 %% IP protocol numbers
	 {[#ether{},#ipv4{},#tcp{}],
			[{ip_proto,6}],[]},
	 {[#ether{},#ipv4{},#udp{}],
			[{ip_proto,17}],[]},
	 {[#ether{},Ipv6,#tcp{}],
			[{ip_proto,6}],[]},
	 {[#ether{},Ipv6,#udp{}],
			[{ip_proto,17}],[]},
	 {[#ether{},#arp{}],
			[],[ip_proto]},

	 %% IPv4/IPv6 headers
	 {[#ether{},#ipv4{},#udp{}],
			[ip4_hdr],[ip6_hdr]},
	 {[#ether{},Ipv6,#udp{}],
			[ip6_hdr],[ip4_hdr]},
	 {[#ether{},#arp{}],
			[],[ip4_hdr,ip6_hdr]},

	 %% TCP/UDP/SCTP headers
	 {[#ether{},#ipv4{},#tcp{}],
			[tcp_hdr],[udp_hdr,sctp_hdr]},
	 {[#ether{},#ipv4{},#udp{}],
			[udp_hdr],[tcp_hdr,sctp_hdr]},
	 {[#ether{},#ipv4{},#sctp{}],
			[sctp_hdr],[tcp_hdr,udp_hdr]},

	 %% ICMPv4/ICMPv6/ARP
	 {[#ether{},#ipv4{},#icmp{}],
			[icmp_msg],[icmp6_hdr,arp_msg]},
	 {[#ether{},#ipv4{},#icmpv6{}],
			[icmp6_hdr],[icmp_msg,arp_msg]},
	 {[#ether{},#arp{}],
			[arp_msg],[icmp_msg,icmp6_hdr]},

	 %% ICMPv6 NDP
	 {[#ether{},#ipv4{},#icmpv6{type=135},#ndp_ns{sll =Mac}],
			[{icmp6_sll,Mac}],[icmp6_tll]},
	 {[#ether{},#ipv4{},#icmpv6{type=136},#ndp_na{tll =Mac}],
			[{icmp6_tll,Mac}],[icmp6_sll]},
	 {[#ether{},#ipv4{},#icmpv6{}],
			[],[icmp6_sll,icmp6_tll]},

	 %% MPLS
	 {[#ether{},#mpls_tag{},#ipv4{},#udp{}],
			[mpls_tag],[]},
	 {[#ether{},#ipv4{},#udp{}],
			[],[mpls_tag]},

	 %% PBB
	 {[#pbb{},#ether{},#ipv4{},#udp{}],
			[pbb_tag],[]},
	 {[#ether{},#ipv4{},#udp{}],
			[],[pbb_tag]} ]].

exthdr_test_() ->
	AnyMeta = <<0:64>>,
	Ipv6 = #ipv6{saddr = <<0:128>>,daddr = <<0:128>>},
	AnyPortInfo = {1,1,undefined},

	AnyOpt = <<1,2,3,4,5,6>>,	%% size matters
	%%H0 = #ipv6_header{type=ipv6_hdr_no_next,content= AnyOpt},
	H3 = #ipv6_header{type= ipv6_hdr_dest_opts,content= AnyOpt},
	H4 = #ipv6_header{type= ipv6_hdr_fragments,content= AnyOpt},
	H5 = #ipv6_header{type= ipv6_hdr_routing,content= AnyOpt},
	H6 = #ipv6_header{type= ipv6_hdr_hop_by_hop,content= AnyOpt},

	[fun() ->
		Frame = pkt:encapsulate(Pkt),
		io:format("Frame = ~p\n", [Frame]),
		Ms = linc_max_preparser:inject(Frame, AnyMeta, AnyPortInfo,
									  #actions{}, ?MODULE),
		io:format("Ms = ~p\n", [Ms]),
		io:format("Present/Absent = ~p/~p\n", [Present,Absent]),
		present(Present, Ms),
		absent(Absent, Ms)
	 end
		|| {Pkt,Present,Absent} <-

	[{[#ether{},Ipv6#ipv6{next =59}],
			[{ip6_ext,<<0:15,1:1>>}],[]},
	 {[#ether{},Ipv6,H3,#udp{}],
			[{ip6_ext,<<0:12,1:1,0:3>>}],[]},
	 {[#ether{},Ipv6,H4,#udp{}],
			[{ip6_ext,<<0:11,1:1,0:4>>}],[]},
	 {[#ether{},Ipv6,H5,#udp{}],
			[{ip6_ext,<<0:10,1:1,0:5>>}],[]},
	 {[#ether{},Ipv6,H6,#udp{}],
			[{ip6_ext,<<0:9,1:1,0:6>>}],[]},

	 %% multiple IPv6 headers
	 {[#ether{},Ipv6,H6,H5,#udp{}],
			[{ip6_ext,<<0:9,3:2,0:5>>}],[]},
	 {[#ether{},Ipv6,H3,H5,#udp{}],
			[{ip6_ext,<<0:10,1:1,0:1,1:1,0:3>>}],[]},

	 %% repeated opts
	 {[#ether{},Ipv6,H4,H4,#udp{}],
			[{ip6_ext,<<0:8,1:1,0:2,1:1,0:4>>}],[]},
	 {[#ether{},Ipv6,H5,H5,#udp{}],
			[{ip6_ext,<<0:8,1:1,0:1,1:1,0:5>>}],[]},
	 {[#ether{},Ipv6,H6,H6,#udp{}],
			[{ip6_ext,<<0:8,1:1,1:1,0:6>>}],[]},

	 %% out-of-order opts (H6 H5 H4)
	 {[#ether{},Ipv6,H5,H6,#udp{}],
			[{ip6_ext,<<0:7,1:1,0:1,3:2,0:5>>}],[]},
	 {[#ether{},Ipv6,H4,H5,#udp{}],
			[{ip6_ext,<<0:7,1:1,0:2,3:2,0:4>>}],[]},
	 {[#ether{},Ipv6,H4,H6,#udp{}],
			[{ip6_ext,<<0:7,1:1,0:1,1:1,0:1,1:1,0:4>>}],[]} ]].

%% Utils -----------------------------------------------------------------------

present(Present, Ms) ->
	lists:foreach(fun(S) when is_atom(S) ->
		true = lists:keymember(S, 1, Ms);
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
