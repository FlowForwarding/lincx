%%
%%
%%

%% @author Cloudozer, LLP. <info@cloudozer.com>
%% @copyright 2014 FlowForwarding.org
-module(linc_max_generator_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("pkt/include/pkt.hrl").

-include_lib("of_protocol/include/ofp_v4.hrl").
-include("linc_max.hrl").
-include("fast_path.hrl").

ports_test() ->
	port_info([{in_port,<<126:32>>}], {126,2,undefined}, match),
	port_info([{in_port,<<126:32>>}], {1,2,undefined}, miss),
	port_info([{in_phy_port,<<126:32>>}], {1,126,undefined}, match),
	port_info([{in_phy_port,<<126:32>>}], {1,2,undefined}, miss).

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
			[{eth_type,<<16#8100:16>>}], miss)),
	 ?_test(packet([#ether{},#ieee802_1q_tag{},#ipv4{},#udp{}],
			[{eth_type,<<16#800:16>>}], match)),
	 ?_test(packet([#ether{},Ipv6,#udp{}],
			[{eth_type,<<16#86dd:16>>}], match)),
	 ?_test(packet([#ether{},#arp{}],
			[{eth_type,<<16#806:16>>}], match)) ].

vlan_test_() ->
	 [?_test(packet([#ether{},#ieee802_1q_tag{vid = <<42:12>>},#ipv4{},#udp{}],
			[{vlan_vid,<<16#1000:13>>,<<16#1000:13>>}], match)),	%% any VLAN
	  ?_test(packet([#ether{},#ieee802_1q_tag{vid = <<126:12>>},#ipv4{},#udp{}],
			[{vlan_vid,<<16#1000:13>>,<<16#1000:13>>}], match)),	%% any VLAN
	  ?_test(packet([#ether{},#ipv4{},#udp{}],
			[{vlan_vid,<<16#1000:13>>,<<16#1000:13>>}], miss)),	%% any VLAN
	  ?_test(packet([#ether{},#ipv4{},#udp{}],
			[{vlan_vid,<<16#0000:13>>,nomask}], match)),	%% no VLAN
	  ?_test(packet([#ether{},#ieee802_1q_tag{},#ipv4{},#udp{}],
			[{vlan_vid,<<16#0000:13>>,nomask}], miss)),	%% no VLAN
	  ?_test(packet([#ether{},#ieee802_1q_tag{vid = <<42:12>>},#ipv4{},#udp{}],
			[{vlan_vid,<<42:13>>,nomask}], match)),
	  ?_test(packet([#ether{},#ieee802_1q_tag{vid = <<42:12>>},#ipv4{},#udp{}],
			[{vlan_vid,<<126:13>>,nomask}], miss)),
	  ?_test(packet([#ether{},#ieee802_1q_tag{pcp =5},#ipv4{},#udp{}],
			[{vlan_pcp,<<5:3>>}], match)),
	  ?_test(packet([#ether{},#ieee802_1q_tag{pcp =5},#ipv4{},#udp{}],
			[{vlan_pcp,<<3:3>>}], miss)) ].

pbb_test_() ->
	[[?_test(packet([#pbb{i_sid =TVal},#ether{},#ipv4{},#udp{}],
			[{pbb_isid,MVal,Mask}], Expected))
		|| {MVal,Mask,TVal,Expected} <- masked(24)],
	 ?_test(packet([#pbb{i_uca =1},#ether{},#ipv4{},#udp{}],
			[{pbb_uca,<<1:1>>}], match)),
	 ?_test(packet([#pbb{i_uca =1},#ether{},#ipv4{},#udp{}],
			[{pbb_uca,<<0:1>>}], miss)) ].

mpls_test_() ->
	E0 = #mpls_stack_entry{label = <<1:20>>,qos =0,pri =1,ecn =0,bottom =1},
	E1 = #mpls_stack_entry{label = <<42:20>>,bottom =0},
	E2 = #mpls_stack_entry{label = <<126:20>>,bottom =1},

	[?_test(packet([#ether{},#mpls_tag{stack =[E0]},#ipv4{},#udp{}],
			[{mpls_label,<<1:20>>}], match)),
	 ?_test(packet([#ether{},#mpls_tag{stack =[E0]},#ipv4{},#udp{}],
			[{mpls_label,<<2:20>>}], miss)),
	 ?_test(packet([#ether{},#mpls_tag{stack =[E1,E2]},#ipv4{},#udp{}],
			[{mpls_label,<<42:20>>}], match)),
	 ?_test(packet([#ether{},#mpls_tag{stack =[E1,E2]},#ipv4{},#udp{}],
			[{mpls_label,<<126:20>>}], miss)),
	 ?_test(packet([#ether{},#mpls_tag{stack =[E0]},#ipv4{},#udp{}],
			[{mpls_tc,<<2:3>>}], match)),
	 ?_test(packet([#ether{},#mpls_tag{stack =[E0]},#ipv4{},#udp{}],
			[{mpls_tc,<<1:3>>}], miss)),
	 ?_test(packet([#ether{},#mpls_tag{stack =[E0]},#ipv4{},#udp{}],
			[{mpls_bos,<<1:1>>}], match)),
	 ?_test(packet([#ether{},#mpls_tag{stack =[E0]},#ipv4{},#udp{}],
			[{mpls_bos,<<0:1>>}], miss)),
	 ?_test(packet([#ether{},#mpls_tag{stack =[E1,E2]},#ipv4{},#udp{}],
			[{mpls_bos,<<0:1>>}], match)),
	 ?_test(packet([#ether{},#mpls_tag{stack =[E1,E2]},#ipv4{},#udp{}],
			[{mpls_bos,<<1:1>>}], miss)) ].

traffic_class_test_() ->
	Ipv6 = #ipv6{saddr = <<0:128>>,daddr = <<0:128>>},

	[?_test(packet([#ether{},#ipv4{dscp =63},#udp{}],
			[{ip_dscp,<<63:6>>}], match)),
	 ?_test(packet([#ether{},#ipv4{dscp =63},#udp{}],
			[{ip_dscp,<<42:6>>}], miss)),
	 ?_test(packet([#ether{},Ipv6#ipv6{class =63*4},#udp{}],
			[{ip_dscp,<<63:6>>}], match)),
	 ?_test(packet([#ether{},Ipv6#ipv6{class =63*4},#udp{}],
			[{ip_dscp,<<42:6>>}], miss)),
	 ?_test(packet([#ether{},#arp{}],
			[{ip_dscp,<<42:6>>}], miss)),
	 ?_test(packet([#ether{},#ipv4{ecn =3},#udp{}],
			[{ip_ecn,<<3:2>>}], match)),
	 ?_test(packet([#ether{},#ipv4{ecn =3},#udp{}],
			[{ip_ecn,<<2:2>>}], miss)),
	 ?_test(packet([#ether{},Ipv6#ipv6{class =3},#udp{}],
			[{ip_ecn,<<3:2>>}], match)),
	 ?_test(packet([#ether{},Ipv6#ipv6{class =3},#udp{}],
			[{ip_ecn,<<2:2>>}], miss)),
	 ?_test(packet([#ether{},#arp{}],
			[{ip_ecn,<<2:2>>}], miss)) ].

ip_proto_test_() ->
	Ipv6 = #ipv6{saddr = <<0:128>>,daddr = <<0:128>>},

	[?_test(packet([#ether{},#ipv4{},#udp{}],
			[{ip_proto,<<17:8>>}], match)),
	 ?_test(packet([#ether{},#ipv4{},#tcp{}],
			[{ip_proto,<<6:8>>}], match)),
	 ?_test(packet([#ether{},#ipv4{},#sctp{}],
			[{ip_proto,<<6:8>>}], miss)),
	 ?_test(packet([#ether{},#arp{}],
			[{ip_proto,<<6:8>>}], miss)),
	 ?_test(packet([#ether{},Ipv6,#udp{}],
			[{ip_proto,<<17:8>>}], match)),
	 ?_test(packet([#ether{},Ipv6,#tcp{}],
			[{ip_proto,<<6:8>>}], match)) ].

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
			[{tcp_src,<<42:16>>}], match)),
	 ?_test(packet([#ether{},Ipv6,#tcp{sport =42}],
			[{tcp_src,<<42:16>>}], match)),
	 ?_test(packet([#ether{},#ipv4{},#tcp{sport =1}],
			[{tcp_src,<<42:16>>}], miss)),
	 ?_test(packet([#ether{},#ipv4{},#tcp{dport =42}],
			[{tcp_dst,<<42:16>>}], match)),
	 ?_test(packet([#ether{},Ipv6,#tcp{dport =42}],
			[{tcp_dst,<<42:16>>}], match)),
	 ?_test(packet([#ether{},#ipv4{},#tcp{dport =1}],
			[{tcp_dst,<<42:16>>}], miss)),
	
	 %% UDP
	 ?_test(packet([#ether{},#ipv4{},#udp{sport =42}],
			[{udp_src,<<42:16>>}], match)),
	 ?_test(packet([#ether{},Ipv6,#udp{sport =42}],
			[{udp_src,<<42:16>>}], match)),
	 ?_test(packet([#ether{},#ipv4{},#udp{sport =1}],
			[{udp_src,<<42:16>>}], miss)),
	 ?_test(packet([#ether{},#ipv4{},#udp{dport =42}],
			[{udp_dst,<<42:16>>}], match)),
	 ?_test(packet([#ether{},Ipv6,#udp{dport =42}],
			[{udp_dst,<<42:16>>}], match)),
	 ?_test(packet([#ether{},#ipv4{},#udp{dport =1}],
			[{udp_dst,<<42:16>>}], miss)),

	 %% SCTP
	 ?_test(packet([#ether{},#ipv4{},#sctp{sport =42}],
			[{sctp_src,<<42:16>>}], match)),
	 ?_test(packet([#ether{},Ipv6,#sctp{sport =42}],
			[{sctp_src,<<42:16>>}], match)),
	 ?_test(packet([#ether{},#ipv4{},#sctp{sport =1}],
			[{sctp_src,<<42:16>>}], miss)),
	 ?_test(packet([#ether{},#ipv4{},#sctp{dport =42}],
			[{sctp_dst,<<42:16>>}], match)),
	 ?_test(packet([#ether{},Ipv6,#sctp{dport =42}],
			[{sctp_dst,<<42:16>>}], match)),
	 ?_test(packet([#ether{},#ipv4{},#sctp{dport =1}],
			[{sctp_dst,<<42:16>>}], miss)) ].

icmp_test_() ->
	NdpNs = #ndp_ns{tgt_addr = <<42:128>>,sll = <<1,2,3,4,5,6>>},
	NdpNa = #ndp_na{src_addr = <<42:128>>,tll = <<1,2,3,4,5,6>>},

	[?_test(packet([#ether{},#ipv4{},#icmp{type =3}],
			[{icmpv4_type,<<3:8>>}], match)),
	 ?_test(packet([#ether{},#ipv4{},#icmp{type =5}],
			[{icmpv4_type,<<3:8>>}], miss)),
	 ?_test(packet([#ether{},#ipv4{},#icmp{code =3}],
			[{icmpv4_code,<<3:8>>}], match)),
	 ?_test(packet([#ether{},#ipv4{},#icmp{code =0}],
			[{icmpv4_code,<<3:8>>}], miss)),
	
	 %% ICMPv6
	 ?_test(packet([#ether{},#ipv4{},#icmpv6{type =3}],
			[{icmpv6_type,<<3:8>>}], match)),
	 ?_test(packet([#ether{},#ipv4{},#icmpv6{type =8}],
			[{icmpv6_type,<<3:8>>}], miss)),
	 ?_test(packet([#ether{},#ipv4{},#icmpv6{code =3}],
			[{icmpv6_code,<<3:8>>}], match)),
	 ?_test(packet([#ether{},#ipv4{},#icmpv6{code =0}],
			[{icmpv6_code,<<3:8>>}], miss)),

	 %% ICMPv6 NDP
	 %%x
	 ?_test(packet([#ether{},#ipv4{},#icmpv6{},NdpNs],
			[{ipv6_nd_target,<<42:128>>}], match)),
	 ?_test(packet([#ether{},#ipv4{},#icmpv6{},NdpNs],
			[{ipv6_nd_target,<<1:128>>}], miss)),
	 ?_test(packet([#ether{},#ipv4{},#icmpv6{},NdpNa],
			[{ipv6_nd_target,<<42:128>>}], match)),
	 ?_test(packet([#ether{},#ipv4{},#icmpv6{},NdpNa],
			[{ipv6_nd_target,<<1:128>>}], miss)),
	 ?_test(packet([#ether{},#ipv4{},#icmpv6{},NdpNs],
			[{ipv6_nd_sll,<<16#010203040506:48>>}], match)),
	 ?_test(packet([#ether{},#ipv4{},#icmpv6{},NdpNs#ndp_ns{sll = <<1,2,3,4,5,0>>}],
			[{ipv6_nd_sll,<<16#010203040506:48>>}], miss)),
	 ?_test(packet([#ether{},#ipv4{},#icmpv6{},NdpNa],
			[{ipv6_nd_tll,<<16#010203040506:48>>}], match)),
	 ?_test(packet([#ether{},#ipv4{},#icmpv6{},NdpNa#ndp_na{tll = <<1,2,3,4,5,0>>}],
			[{ipv6_nd_tll,<<16#010203040506:48>>}], miss)) ].

arp_test_() ->
	[?_test(packet([#ether{},#arp{op =1}],
			[{arp_op,<<1:16>>}], match)),
	 ?_test(packet([#ether{},#arp{op =2}],
			[{arp_op,<<1:16>>}], miss)),
	 [?_test(packet([#ether{},#arp{sip =TVal}],
			[{arp_spa,MVal,Mask}], Expected))
		|| {MVal,Mask,TVal,Expected} <- masked(32)],
	 [?_test(packet([#ether{},#arp{tip =TVal}],
			[{arp_tpa,MVal,Mask}], Expected))
		|| {MVal,Mask,TVal,Expected} <- masked(32)],
	 [?_test(packet([#ether{},#arp{sha =TVal}],
			[{arp_sha,MVal,Mask}], Expected))
		|| {MVal,Mask,TVal,Expected} <- masked(48)],
	 [?_test(packet([#ether{},#arp{tha =TVal}],
			[{arp_tha,MVal,Mask}], Expected))
		|| {MVal,Mask,TVal,Expected} <- masked(48)] ].

ipv6_test_() ->
	Ipv6 = #ipv6{saddr = <<0:128>>,daddr = <<0:128>>},

	[[?_test(packet([#ether{},Ipv6#ipv6{saddr =TVal},#udp{}],
			[{ipv6_src,MVal,Mask}], Expected))
		|| {MVal,Mask,TVal,Expected} <- masked(128)],
	 [?_test(packet([#ether{},Ipv6#ipv6{daddr =TVal},#udp{}],
			[{ipv6_dst,MVal,Mask}], Expected))
		|| {MVal,Mask,TVal,Expected} <- masked(128)],
	 [?_test(packet([#ether{},Ipv6#ipv6{flow =flow_label(TVal)},#udp{}],
			[{ipv6_flabel,MVal,Mask}], Expected))
		|| {MVal,Mask,TVal,Expected} <- masked(20)] ].

flow_label(<<N:20>>) -> N.

port_info(Matches, PortInfo, Expected) ->
	AnyFrame = pkt:encapsulate([#ether{},#ipv4{},#udp{}]),
	AnyMeta = <<0:64>>,
	Ents = [flow_entry(Matches)],
	linc_max_generator:update_flow_table(flow42, Ents),
	R = linc_max_preparser:inject(AnyFrame, AnyMeta, PortInfo,
								  #fast_actions{}, flow42, #blaze{}),
	expected(Expected, R).

metadata(Matches, Meta, Expected) ->
	AnyFrame = pkt:encapsulate([#ether{},#ipv4{},#udp{}]),
	AnyPortInfo = {1,2,undefined},
	Ents = [flow_entry(Matches)],
	linc_max_generator:update_flow_table(flow42, Ents),
	R = linc_max_preparser:inject(AnyFrame, Meta, AnyPortInfo,
								  #fast_actions{}, flow42, #blaze{}),
	expected(Expected, R).

packet(Pkt, Matches, Expected) ->
	AnyMeta = <<0:64>>,
	AnyPortInfo = {1,2,undefined},
	Frame = pkt:encapsulate(Pkt),
	Ents = [flow_entry(Matches)],
	linc_max_generator:update_flow_table(flow42, Ents),
	R = linc_max_preparser:inject(Frame, AnyMeta, AnyPortInfo,
								  #fast_actions{}, flow42, #blaze{}),
	expected(Expected, R).

flow_entry(Matches) ->
	#flow_entry{match =
			#ofp_match{fields =
					lists:map(fun({Fld,Val}) ->
							#ofp_field{name =Fld,value =Val,has_mask =false};
						({Fld,Val,nomask}) ->
							#ofp_field{name =Fld,value =Val,has_mask =false};
						({Fld,Val,Mask}) ->
							#ofp_field{name =Fld,value =Val,
									has_mask =true,mask =Mask}
						end, Matches)},
				instructions = []}.

%% generated a set of values for masked matches
masked(Bits) ->
	lists:foldl(fun(nomask, Masked) ->
		V = random:uniform(1 bsl Bits) -1,
		Mask = (1 bsl Bits) -1,
		[{<<V:Bits>>,nomask,<<V:Bits>>,match},
		 {<<((bnot V) band Mask):Bits>>,nomask,<<V:Bits>>,miss}|Masked];
	(Ss, Masked) ->
		case lists:all(fun valid_bits/1, Ss) of
		true ->
			Mask = bits_to_mask(Ss),
			V = random:uniform(1 bsl Bits) -1,
			[{<<(V band Mask):Bits>>,<<Mask:Bits>>,<<V:Bits>>,match},
			 {<<((bnot V) band Mask):Bits>>,<<Mask:Bits>>,<<V:Bits>>,miss}|Masked];
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
