%%
%%
%%

%% @author Cloudozer, LLP. <info@cloudozer.com>
%% @copyright 2014 FlowForwarding.org
-module(linc_max_splicer_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("pkt/include/pkt.hrl").

ether_test_() ->
	Mac = <<0,1,2,3,4,5>>,
	[?_test(edit([#ether{},#arp{}], eth_src, Mac,
				fun(#ether{shost =Mac1}) -> ?assertEqual(Mac, Mac1);
							(_) -> ok end)),
	 ?_test(edit([#ether{},#arp{}], eth_dst, Mac,
				fun(#ether{dhost =Mac1}) -> ?assertEqual(Mac, Mac1);
							(_) -> ok end)),
	 ?_test(protected([#ether{},#arp{}], eth_type, 42)) ].

vlan_test_() ->
	Pkt = [#ether{},#ieee802_1q_tag{},#arp{}],
	[?_test(edit(Pkt, vlan_vid, 42,
				fun(#ieee802_1q_tag{vid = Vid}) -> ?assertEqual(<<42:12>>, Vid);
							(_) -> ok end)),
	 ?_test(edit(Pkt, vlan_pcp, 5,
				fun(#ieee802_1q_tag{pcp = Pcp}) -> ?assertEqual(5, Pcp);
							(_) -> ok end)) ].

pbb_test_() ->
	Pkt = [#pbb{},#ether{},#arp{}],
	[?_test(edit(Pkt, pbb_uca, 1,
				fun(#pbb{i_uca = Uca}) -> ?assertEqual(1, Uca);
							(_) -> ok end)),
	 ?_test(edit(Pkt, pbb_uca, 0,
				fun(#pbb{i_uca = Uca}) -> ?assertEqual(0, Uca);
							(_) -> ok end)),
	 ?_test(edit(Pkt, pbb_isid, 42,
				fun(#pbb{i_sid = Isid}) -> ?assertEqual(<<42:24>>, Isid);
							(_) -> ok end)) ].

mpls_test_() ->
	Pkt = [#ether{},#mpls_tag{},#ipv4{},#udp{}],
	[?_test(edit(Pkt, mpls_label, 42,
				fun(#mpls_tag{stack =[#mpls_stack_entry{label =Label}|_]}) ->
					?assertEqual(<<42:20>>, Label);
				(_) ->
					ok
				end)),
	 ?_test(edit(Pkt, mpls_tc, 5,
				fun(#mpls_tag{stack =[#mpls_stack_entry{qos =Qos,pri =Pri,ecn =Ecn}|_]}) ->
					?assertEqual(1, Qos),
					?assertEqual(0, Pri),
					?assertEqual(1, Ecn);
				(_) ->
					ok
				end)),
	 ?_test(edit(Pkt, mpls_tc, 2,
				fun(#mpls_tag{stack =[#mpls_stack_entry{qos =Qos,pri =Pri,ecn =Ecn}|_]}) ->
					?assertEqual(0, Qos),
					?assertEqual(1, Pri),
					?assertEqual(0, Ecn);
				(_) ->
					ok
				end)),
	 ?_test(protected(Pkt, mpls_bos, 0)) ].

arp_test_() ->
	Addr = <<42,0,0,1>>,
	Mac = <<0,16,32,1,2,3>>,
	Pkt = [#ether{},#arp{}],
	[?_test(edit(Pkt, arp_op, 1,
				fun(#arp{op =Op}) -> ?assertEqual(1, Op);
						(_) -> ok end)),
	 ?_test(edit(Pkt, arp_op, 2,
				fun(#arp{op =Op}) -> ?assertEqual(2, Op);
						(_) -> ok end)),
	 ?_test(edit(Pkt, arp_spa, Addr,
				fun(#arp{sip =Addr1}) -> ?assertEqual(Addr, Addr1);
						(_) -> ok end)),
	 ?_test(edit(Pkt, arp_tpa, Addr,
				fun(#arp{tip =Addr1}) -> ?assertEqual(Addr, Addr1);
						(_) -> ok end)),
	 ?_test(edit(Pkt, arp_sha, Mac,
				fun(#arp{sha =Mac1}) -> ?assertEqual(Mac, Mac1);
						(_) -> ok end)),
	 ?_test(edit(Pkt, arp_tha, Mac,
				fun(#arp{tha =Mac1}) -> ?assertEqual(Mac, Mac1);
						(_) -> ok end)) ].

ipv4_test_fix() ->
	Addr = <<126,0,1,42>>,
	GetSum = fun(#ipv4{sum =Sum}) -> Sum;
					(_) -> undefined end,
	Pkt = [#ether{},#ipv4{},#udp{}],
	[?_test(edit(Pkt, ipv4_src, Addr,
				fun(#ipv4{saddr =Addr1}) -> ?assertEqual(Addr, Addr1);
						(_) -> ok end, GetSum)),
	 ?_test(edit(Pkt, ipv4_dst, Addr,
				fun(#ipv4{daddr =Addr1}) -> ?assertEqual(Addr, Addr1);
						(_) -> ok end, GetSum)),
	 ?_test(edit(Pkt, ip_dscp, 42,
				fun(#ipv4{dscp =Dscp}) -> ?assertEqual(42, Dscp);
						(_) -> ok end, GetSum)),
	 ?_test(edit(Pkt, ip_ecn, 3,
				fun(#ipv4{ecn =Ecn}) -> ?assertEqual(3, Ecn);
						(_) -> ok end, GetSum)),
	 ?_test(edit(Pkt, ip_ecn, 1,
				fun(#ipv4{ecn =Ecn}) -> ?assertEqual(1, Ecn);
						(_) -> ok end, GetSum)),
	 ?_test(protected(Pkt, ip_proto, 42)) ].

ipv6_test_() ->
	Addr = <<0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3>>,
	Pkt = [#ether{},#ipv6{saddr = <<0:128>>,daddr = <<0:128>>},#udp{}],
	[?_test(edit(Pkt, ipv6_src, Addr,
				fun(#ipv6{saddr =Addr1}) -> ?assertEqual(Addr, Addr1);
						(_) -> ok end)),
	 ?_test(edit(Pkt, ipv6_dst, Addr,
				fun(#ipv6{daddr =Addr1}) -> ?assertEqual(Addr, Addr1);
						(_) -> ok end)),
	 ?_test(edit(Pkt, ipv6_flabel, 126,
				fun(#ipv6{flow =Flow}) -> ?assertEqual(126, Flow);
						(_) -> ok end)),
	 ?_test(edit(Pkt, ip_dscp, 42,
				fun(#ipv6{class =Class}) -> ?assertEqual(42, Class bsr 2);
						(_) -> ok end)),
	 ?_test(edit(Pkt, ip_ecn, 3,
				fun(#ipv6{class =Class}) -> ?assertEqual(3, Class band 3);
						(_) -> ok end)),
	 ?_test(edit(Pkt, ip_ecn, 1,
				fun(#ipv6{class =Class}) -> ?assertEqual(1, Class band 3);
						(_) -> ok end)),
	 ?_test(protected(Pkt, ip_proto, 42)) ].

tcp_test_() ->
	Pkt = [#ether{},#ipv4{},#tcp{},<<1,2,3>>],
	GetSum = fun(#tcp{sum =Sum}) -> Sum;
					(_) -> undefined end,
	[?_test(edit(Pkt, tcp_src, 42,
				fun(#tcp{sport =Port}) -> ?assertEqual(42, Port);
						(_) -> ok end, GetSum)),
	 ?_test(edit(Pkt, tcp_dst, 42,
				fun(#tcp{dport =Port}) -> ?assertEqual(42, Port);
						(_) -> ok end, GetSum)) ].

udp_test_() ->
	Pkt = [#ether{},#ipv4{},#udp{},<<1,2,3>>],
	GetSum = fun(#udp{sum =Sum}) -> Sum;
					(_) -> undefined end,
	[?_test(edit(Pkt, udp_src, 42,
				fun(#udp{sport =Port}) -> ?assertEqual(42, Port);
						(_) -> ok end, GetSum)),
	 ?_test(edit(Pkt, udp_dst, 42,
				fun(#udp{dport =Port}) -> ?assertEqual(42, Port);
						(_) -> ok end, GetSum)) ].

sctp_test_() ->
	Pkt = [#ether{},#ipv4{},#sctp{},<<1,2,3>>],
	GetSum = fun(#sctp{sum =Sum}) -> Sum;
					(_) -> undefined end,
	[?_test(edit(Pkt, sctp_src, 42,
				fun(#sctp{sport =Port}) -> ?assertEqual(42, Port);
						(_) -> ok end, GetSum)),
	 ?_test(edit(Pkt, sctp_dst, 42,
				fun(#sctp{dport =Port}) -> ?assertEqual(42, Port);
						(_) -> ok end, GetSum)) ].

icmp_test_() ->
	Pkt = [#ether{},#ipv4{},#icmp{}],
	[?_test(protected(Pkt, icmpv4_type, 42)),
	 ?_test(protected(Pkt, icmpv4_code, 42)) ].

icmpv6_test_() ->
	Pkt1 = [#ether{},#ipv6{saddr = <<1:128>>,daddr = <<2:128>>},
					#icmpv6{},#ndp_na{}],
	Pkt2 = [#ether{},#ipv6{saddr = <<1:128>>,daddr = <<2:128>>},
					#icmpv6{},#ndp_na{}],
	Addr = <<42:128>>,
	GetSum = fun(#icmpv6{checksum =Sum}) -> Sum;
					(_) -> undefined end,
	[?_test(edit(Pkt1, ipv6_nd_target, Addr,
				fun(#ndp_na{src_addr =Addr1}) -> ?assertEqual(Addr, Addr1);
						(_) -> ok end, GetSum)),
	 ?_test(edit(Pkt2, ipv6_nd_target, Addr,
				fun(#ndp_ns{tgt_addr =Addr1}) -> ?assertEqual(Addr, Addr1);
						(_) -> ok end, GetSum)) ].

%%------------------------------------------------------------------------------

edit(OldPkt, Field, Value, CheckFun) ->
	P = pkt:encapsulate(OldPkt),
	P2 = linc_max_splicer:edit(P, Field, Value),
	Pkt2 = pkt:decapsulate(P2),
	CheckFun(Pkt2).

%% with checksum
edit(OldPkt, Field, Value, CheckFun, GetSum) ->
	P = pkt:encapsulate(OldPkt),
	P2 = linc_max_splicer:edit(P, Field, Value),
	Pkt2 = pkt:decapsulate(P2),
	CheckFun(Pkt2),
	%% Pkt2 has checksum calculated by splicer
	P3 = pkt:encapsulate(Pkt2),
	%% checksum recalculated by pkt:*
	Pkt3 = pkt:decapsulate(P3),
	A = [GetSum(Hdr) || Hdr <- Pkt2],
	B = [GetSum(Hdr) || Hdr <- Pkt3],
	?assertEqual(B, A).

protected(OldPkt, Field, Value) ->
	P = pkt:encapsulate(OldPkt),
	R = linc_max_splicer:edit(P, Field, Value),
	%?assertEqual(protected, R).
	ok.

%%EOF
