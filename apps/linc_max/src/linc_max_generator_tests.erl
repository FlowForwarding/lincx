%%
%%
%%

%% @author Cloudozer, LLP. <info@cloudozer.com>
%% @copyright 2014 FlowForwarding.org
-module(linc_max_generator_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("pkt/include/pkt.hrl").
-include("linc_max.hrl").

ports_test() ->
	port_info([{in_port,126}], {126,2,undefined}, match),
	port_info([{in_port,126}], {1,2,undefined}, miss),
	port_info([{in_phy_port,126}], {1,126,undefined}, match),
	port_info([{in_phy_port,126}], {1,2,undefined}, miss).

tunnel_id_test_() ->
	[fun() ->
		port_info([{tunnel_id,MVal,Mask}], {1,2,TVal}, Expected)
	 end
		|| {MVal,Mask,TVal,Expected} <- masked(64)].

metadata_test_() ->
	[fun() ->
		metadata([{metadata,MVal,Mask}], TVal, Expected)
	 end
		|| {MVal,Mask,TVal,Expected} <- masked(64) ].

ether_test_() ->
	Ipv6 = #ipv6{saddr = <<0:128>>,daddr = <<0:128>>},

	[[?_test(packet([#ether{dhost =TVal},#ipv4{},#udp{}],
			[{eth_dst,MVal,Mask}], Expected))
		|| {MVal,Mask,TVal,Expected} <- masked(48)],
	 [?_test(packet([#ether{shost =TVal},#ipv4{},#udp{}],
			[{eth_src,MVal,Mask}], Expected))
		|| {MVal,Mask,TVal,Expected} <- masked(48)],

	 %% Ethernet type of the OpenFlow packet payload, after VLAN tags.
	 ?_test(packet([#ether{},#ieee802_1q_tag{},#ipv4{},#udp{}],
			[{eth_type,16#8100}], miss)),
	 ?_test(packet([#ether{},#ieee802_1q_tag{},#ipv4{},#udp{}],
			[{eth_type,16#800}], match)),
	 ?_test(packet([#ether{},Ipv6,#udp{}],
			[{eth_type,16#86dd}], match)),
	 ?_test(packet([#ether{},#arp{}],
			[{eth_type,16#806}], match)) ].

vlan_test_() ->
	 [?_test(packet([#ether{},#ieee802_1q_tag{vid = <<42:12>>},#ipv4{},#udp{}],
			[{vlan_vid,16#1000,16#1000}], match)),	%% any VLAN
	  ?_test(packet([#ether{},#ieee802_1q_tag{vid = <<126:12>>},#ipv4{},#udp{}],
			[{vlan_vid,16#1000,16#1000}], match)),	%% any VLAN
	  ?_test(packet([#ether{},#ipv4{},#udp{}],
			[{vlan_vid,16#1000,16#1000}], miss)),	%% any VLAN
	  ?_test(packet([#ether{},#ipv4{},#udp{}],
			[{vlan_vid,16#0000,nomask}], match)),	%% no VLAN
	  ?_test(packet([#ether{},#ieee802_1q_tag{},#ipv4{},#udp{}],
			[{vlan_vid,16#0000,nomask}], miss)),	%% no VLAN
	  ?_test(packet([#ether{},#ieee802_1q_tag{vid = <<42:12>>},#ipv4{},#udp{}],
			[{vlan_vid,42,nomask}], match)),
	  ?_test(packet([#ether{},#ieee802_1q_tag{vid = <<42:12>>},#ipv4{},#udp{}],
			[{vlan_vid,126,nomask}], miss)),
	  ?_test(packet([#ether{},#ieee802_1q_tag{pcp =5},#ipv4{},#udp{}],
			[{vlan_pcp,5}], match)),
	  ?_test(packet([#ether{},#ieee802_1q_tag{pcp =5},#ipv4{},#udp{}],
			[{vlan_pcp,3}], miss)) ].

traffic_class_test_() ->
	Ipv6 = #ipv6{saddr = <<0:128>>,daddr = <<0:128>>},

	[?_test(packet([#ether{},#ipv4{dscp =63},#udp{}],
			[{ip_dscp,63}], match)),
	 ?_test(packet([#ether{},#ipv4{dscp =63},#udp{}],
			[{ip_dscp,42}], miss)),
	 ?_test(packet([#ether{},Ipv6#ipv6{class =63*4},#udp{}],
			[{ip_dscp,63}], match)),
	 ?_test(packet([#ether{},Ipv6#ipv6{class =63*4},#udp{}],
			[{ip_dscp,42}], miss)),
	 ?_test(packet([#ether{},#arp{}],
			[{ip_dscp,42}], miss)),
	 ?_test(packet([#ether{},#ipv4{ecn =3},#udp{}],
			[{ip_ecn,3}], match)),
	 ?_test(packet([#ether{},#ipv4{ecn =3},#udp{}],
			[{ip_ecn,2}], miss)),
	 ?_test(packet([#ether{},Ipv6#ipv6{class =3},#udp{}],
			[{ip_ecn,3}], match)),
	 ?_test(packet([#ether{},Ipv6#ipv6{class =3},#udp{}],
			[{ip_ecn,2}], miss)),
	 ?_test(packet([#ether{},#arp{}],
			[{ip_ecn,2}], miss)) ].

ip_proto_test_() ->
	Ipv6 = #ipv6{saddr = <<0:128>>,daddr = <<0:128>>},

	[?_test(packet([#ether{},#ipv4{},#udp{}],
			[{ip_proto,17}], match)),
	 ?_test(packet([#ether{},#ipv4{},#tcp{}],
			[{ip_proto,6}], match)),
	 ?_test(packet([#ether{},#ipv4{},#sctp{}],
			[{ip_proto,6}], miss)),
	 ?_test(packet([#ether{},#arp{}],
			[{ip_proto,6}], miss)),
	 ?_test(packet([#ether{},Ipv6,#udp{}],
			[{ip_proto,17}], match)),
	 ?_test(packet([#ether{},Ipv6,#tcp{}],
			[{ip_proto,6}], match)) ].

ipv4_test_() ->
	[[?_test(packet([#ether{},#ipv4{saddr =TVal},#udp{}],
			[{ipv4_src,MVal,Mask}], Expected))
		|| {MVal,Mask,TVal,Expected} <- masked(32)],
	 [?_test(packet([#ether{},#ipv4{daddr =TVal},#udp{}],
			[{ipv4_dst,MVal,Mask}], Expected))
		|| {MVal,Mask,TVal,Expected} <- masked(32)] ].

trio_test_() ->
	Ipv6 = #ipv6{saddr = <<0:128>>,daddr = <<0:128>>},

	[?_test(packet([#ether{},#ipv4{},#tcp{sport =42}],
			[{tcp_src,42}], match)),
	 ?_test(packet([#ether{},Ipv6,#tcp{sport =42}],
			[{tcp_src,42}], match)),
	 ?_test(packet([#ether{},#ipv4{},#tcp{sport =1}],
			[{tcp_src,42}], miss)),
	 ?_test(packet([#ether{},#ipv4{},#tcp{dport =42}],
			[{tcp_dst,42}], match)),
	 ?_test(packet([#ether{},Ipv6,#tcp{dport =42}],
			[{tcp_dst,42}], match)),
	 ?_test(packet([#ether{},#ipv4{},#tcp{dport =1}],
			[{tcp_dst,42}], miss)),
	
	 %% UDP
	 ?_test(packet([#ether{},#ipv4{},#udp{sport =42}],
			[{udp_src,42}], match)),
	 ?_test(packet([#ether{},Ipv6,#udp{sport =42}],
			[{udp_src,42}], match)),
	 ?_test(packet([#ether{},#ipv4{},#udp{sport =1}],
			[{udp_src,42}], miss)),
	 ?_test(packet([#ether{},#ipv4{},#udp{dport =42}],
			[{udp_dst,42}], match)),
	 ?_test(packet([#ether{},Ipv6,#udp{dport =42}],
			[{udp_dst,42}], match)),
	 ?_test(packet([#ether{},#ipv4{},#udp{dport =1}],
			[{udp_dst,42}], miss)),

	 %% SCTP
	 ?_test(packet([#ether{},#ipv4{},#sctp{sport =42}],
			[{sctp_src,42}], match)),
	 ?_test(packet([#ether{},Ipv6,#sctp{sport =42}],
			[{sctp_src,42}], match)),
	 ?_test(packet([#ether{},#ipv4{},#sctp{sport =1}],
			[{sctp_src,42}], miss)),
	 ?_test(packet([#ether{},#ipv4{},#sctp{dport =42}],
			[{sctp_dst,42}], match)),
	 ?_test(packet([#ether{},Ipv6,#sctp{dport =42}],
			[{sctp_dst,42}], match)),
	 ?_test(packet([#ether{},#ipv4{},#sctp{dport =1}],
			[{sctp_dst,42}], miss)) ].

icmp_test_() ->
	 %%x clause suppressed
	[?_test(packet([#ether{},#ipv4{},#icmp{type =3}],
			[{icmpv4_type,3}], match)),
	 ?_test(packet([#ether{},#ipv4{},#icmp{type =5}],
			[{icmpv4_type,3}], miss)),
	 ?_test(packet([#ether{},#ipv4{},#icmp{code =3}],
			[{icmpv4_code,3}], match)),
	 ?_test(packet([#ether{},#ipv4{},#icmp{code =0}],
			[{icmpv4_code,3}], miss)),
	
	 %% ICMPv6
	 ?_test(packet([#ether{},#ipv4{},#icmpv6{type =3}],
			[{icmpv6_type,3}], match)),
	 ?_test(packet([#ether{},#ipv4{},#icmpv6{type =8}],
			[{icmpv6_type,3}], miss)),
	 ?_test(packet([#ether{},#ipv4{},#icmpv6{code =3}],
			[{icmpv6_code,3}], match)),
	 ?_test(packet([#ether{},#ipv4{},#icmpv6{code =0}],
			[{icmpv6_code,3}], miss)),

	 %% ICMPv6 NDP
	 %%x
	 ?_test(packet([#ether{},#ipv4{},#icmpv6{},#ndp_ns{tgt_addr = <<42:128>>}],
			[{ipv6_nd_target,42}], match)),
	 ?_test(packet([#ether{},#ipv4{},#icmpv6{},#ndp_ns{tgt_addr = <<42:128>>}],
			[{ipv6_nd_target,1}], miss)),
	 ?_test(packet([#ether{},#ipv4{},#icmpv6{},#ndp_na{src_addr = <<42:128>>}],
			[{ipv6_nd_target,42}], match)),
	 ?_test(packet([#ether{},#ipv4{},#icmpv6{},#ndp_na{src_addr = <<42:128>>}],
			[{ipv6_nd_target,1}], miss)),
	 ?_test(packet([#ether{},#ipv4{},#icmpv6{},#ndp_ns{sll = <<1,2,3,4,5,6>>}],
			[{ipv6_nd_sll,16#010203040506}], match)),
	 ?_test(packet([#ether{},#ipv4{},#icmpv6{},#ndp_ns{sll = <<1,2,3,4,5,0>>}],
			[{ipv6_nd_sll,16#010203040506}], miss)),
	 ?_test(packet([#ether{},#ipv4{},#icmpv6{},#ndp_na{tll = <<1,2,3,4,5,6>>}],
			[{ipv6_nd_tll,16#010203040506}], match)),
	 ?_test(packet([#ether{},#ipv4{},#icmpv6{},#ndp_na{tll = <<1,2,3,4,5,0>>}],
			[{ipv6_nd_tll,16#010203040506}], miss)) ].

port_info(Matches, PortInfo, Expected) ->
	AnyFrame = pkt:encapsulate([#ether{},#ipv4{},#udp{}]),
	AnyMeta = <<0:64>>,
	Ents = [{flow,Matches,#instr{}}],
	linc_max_generator:update_flow_table(flow42, Ents),
	R = linc_max_preparser:inject(AnyFrame, AnyMeta, PortInfo,
								  #actions{}, flow42),
	expected(Expected, R).

metadata(Matches, Meta, Expected) ->
	AnyFrame = pkt:encapsulate([#ether{},#ipv4{},#udp{}]),
	AnyPortInfo = {1,2,undefined},
	Ents = [{flow,Matches,#instr{}}],
	linc_max_generator:update_flow_table(flow42, Ents),
	R = linc_max_preparser:inject(AnyFrame, Meta, AnyPortInfo,
								  #actions{}, flow42),
	expected(Expected, R).

packet(Pkt, Matches, Expected) ->
	AnyMeta = <<0:64>>,
	AnyPortInfo = {1,2,undefined},
	Frame = pkt:encapsulate(Pkt),
	Ents = [{flow,Matches,#instr{}}],
	linc_max_generator:update_flow_table(flow42, Ents),
	R = linc_max_preparser:inject(Frame, AnyMeta, AnyPortInfo,
								  #actions{}, flow42),
	expected(Expected, R).

%% generated a set of values for masked matches
masked(Bits) ->
	lists:foldl(fun(nomask, Masked) ->
		V = random:uniform(1 bsl Bits) -1,
		Mask = (1 bsl Bits) -1,
		[{V,nomask,<<V:Bits>>,match},
		 {(bnot V) band Mask,nomask,<<V:Bits>>,miss}|Masked];
	(Ss, Masked) ->
		case lists:all(fun valid_bits/1, Ss) of
		true ->
			Mask = bits_to_mask(Ss),
			V = random:uniform(1 bsl Bits) -1,
			[{V band Mask,Mask,<<V:Bits>>,match},
			 {(bnot V) band Mask,Mask,<<V:Bits>>,miss}|Masked];
		false ->
			Masked
		end
	end, [], [nomask,
			  [{0,Bits}],
			  [{0,Bits -1}],
			  [{1,Bits -1}],
			  [{1,Bits -2}],
			  [{0,1},{2,Bits -3}],
			  [{1,1},{3,Bits -3}],
			  [{1,1},{3,Bits -4}]]).

valid_bits({B,L}) -> B >= 0 andalso L >= 1.

bits_to_mask(S) ->
	bits_to_mask(S, 0).

bits_to_mask([], Acc) ->
	Acc;
bits_to_mask([{B,L}|S], Acc) ->
	bits_to_mask(S, (((1 bsl L) -1) bsr B) bor Acc).

expected(match, miss) -> erlang:error(nomatch);
expected(match, _) -> ok;
expected(miss, miss) -> ok;
expected(miss, _) -> erlang:error(nomiss).

%%EOF
