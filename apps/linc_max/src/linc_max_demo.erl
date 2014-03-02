%%
%% This is a temporary/experimentation module
%%
-module(linc_max_demo).
-export([help/0,generate_flows/1]).

-include_lib("of_protocol/include/ofp_v4.hrl").
-include("linc_max.hrl").

help() ->
	[{num_flows,100},
	 {match,{1.0,eth_dst,16#ffffff}},
	 {match,{0.5,vlan_vid,nomask}},
	 allow_arp].

generate_flows(actions) ->
	Flows = all_actions(),
	Flows;
generate_flows(Canned) when is_atom(Canned) ->
	FlowConf = flow_config(Canned),
	Flows = generate(FlowConf),
	Flows;
generate_flows(FlowConf) ->
	Flows = generate(FlowConf),
	Flows.

all_actions() ->
	ActionList =[#ofp_action_output{port =7},
				 #ofp_action_output{port =42},
				 #ofp_action_set_queue{queue_id =3},
				 #ofp_action_group{group_id =42},
				 #ofp_action_group{group_id =2},
				 #ofp_action_push_vlan{ethertype =16#8100},
				 #ofp_action_pop_vlan{},
				 #ofp_action_push_mpls{ethertype =16#8847},
				 #ofp_action_pop_mpls{ethertype =16#806},
				 #ofp_action_push_pbb{ethertype =16#88e7},
				 #ofp_action_pop_pbb{},
				 #ofp_action_set_field{field =
						#ofp_field{name =tcp_src,value = <<32000:16>>}},
				 #ofp_action_set_field{field =
						#ofp_field{name =ipv4_src,value = <<1,1,1,1>>}},
				 #ofp_action_set_mpls_ttl{mpls_ttl =3},
				 #ofp_action_dec_mpls_ttl{},
				 #ofp_action_set_nw_ttl{nw_ttl =3},
				 #ofp_action_dec_nw_ttl{},
				 #ofp_action_copy_ttl_out{},
				 #ofp_action_copy_ttl_in{}],

	Instr = #ofp_instruction_apply_actions{actions =ActionList},
	[#flow_entry{match =#ofp_match{},
				 instructions =[Instr]}].

generate(FlowConf) ->
	generate(1, 2, FlowConf) ++ generate(2, 1, FlowConf).

generate(InPort, OutPort, FlowConf) ->
	NumFlows = proplists:get_value(num_flows, FlowConf, 100),
	Matches = proplists:get_all_values(match, FlowConf),
	AllowArp = proplists:get_bool(allow_arp, FlowConf),

	[#flow_entry{match =#ofp_match{fields =
							[#ofp_field{name =in_port,value = <<InPort:32>>}]
					++ more_matches(Matches)},instructions =[]}
		|| _ <- lists:seq(1, NumFlows)]
	
		++ if AllowArp ->
			[#flow_entry{match =#ofp_match{fields =
							[#ofp_field{name =in_port,value = <<InPort:32>>},
							 #ofp_field{name =eth_type,value = <<8,6>>}]},
						 instructions =[#ofp_instruction_write_actions{actions =
								[#ofp_action_output{port = OutPort}]}]}];
			true -> [] end

		++ [#flow_entry{match =#ofp_match{fields =
							[#ofp_field{name =in_port,value = <<InPort:32>>}]},
						instructions =[#ofp_instruction_write_actions{actions =
								[#ofp_action_output{port = OutPort}]}]}].

more_matches(Matches) ->
	more_matches(Matches, []).

more_matches([], Acc) ->
	lists:reverse(Acc);
more_matches([{Prob,Fld}|Matches], Acc) ->
	N = bit_len(Fld),
	Val = random:uniform(1 bsl N) -1,
	Spec = #ofp_field{name =Fld,value = <<Val:N>>,has_mask =false},
	more_matches1(Prob, Spec, Matches, Acc);
more_matches([{Prob,Fld,nomask}|Matches], Acc) ->
	N = bit_len(Fld),
	Val = random:uniform(1 bsl N) -1,
	Spec = #ofp_field{name =Fld,value = <<Val:N>>,has_mask =false},
	more_matches1(Prob, Spec, Matches, Acc);
more_matches([{Prob,Fld,Mask}|Matches], Acc) ->
	N = bit_len(Fld),
	Val = random:uniform(1 bsl N) -1,
	Spec = #ofp_field{name =Fld,
					  value = <<(Val band Mask):N>>,
					  has_mask =true,
					  mask = <<Mask:N>>},
	more_matches1(Prob, Spec, Matches, Acc).

more_matches1(Prob, Spec, Matches, Acc) ->
	case random:uniform() < Prob of
	true ->
		more_matches(Matches, [Spec|Acc]);
	_ ->
		more_matches(Matches, Acc)
	end.

bit_len(in_port) -> 32;
bit_len(in_phy_port) -> 32;
bit_len(metadata) -> 64;
bit_len(eth_dst) -> 48;
bit_len(eth_src) -> 48;
bit_len(eth_type) -> 16;
bit_len(vlan_vid) -> 13;
bit_len(vlan_pcp) -> 3;
bit_len(ip_dscp) -> 6;
bit_len(ip_ecn) -> 2;
bit_len(ip_proto) -> 8;
bit_len(ipv4_src) -> 32;
bit_len(ipv4_dst) -> 32;
bit_len(tcp_src) -> 16;
bit_len(tcp_dst) -> 16;
bit_len(udp_src) -> 16;
bit_len(udp_dst) -> 16;
bit_len(sctp_src) -> 16;
bit_len(sctp_dst) -> 16;
bit_len(icmpv4_type) -> 8;
bit_len(icmpv4_code) -> 8;
bit_len(arp_op) -> 16;
bit_len(arp_spa) -> 32;
bit_len(arp_tpa) -> 32;
bit_len(arp_sha) -> 48;
bit_len(arp_tha) -> 48;
bit_len(ipv6_src) -> 128;
bit_len(ipv6_dst) -> 128;
bit_len(ipv6_flabel) -> 20;
bit_len(icmpv6_type) -> 8;
bit_len(icmpv6_code) -> 8;
bit_len(ipv6_nd_target) -> 128;
bit_len(ipv6_nd_sll) -> 48;
bit_len(ipv6_nd_tll) -> 48;
bit_len(mpls_label) -> 20;
bit_len(mpls_tc) -> 3;
bit_len(mpls_bos) -> 1;
bit_len(pbb_isid) -> 24;
bit_len(tunnel_id) -> 64;
bit_len(ipv6_exthdr) -> 9;
bit_len(pbb_uca) -> 1.

%% predefined flow configurations

flow_config(test0) ->
	[{num_flows,0}];
flow_config(test1) ->
	[{num_flows,128},
	 {match,{1.0,eth_dst,16#ffffff}},
	 {match,{0.5,vlan_vid,nomask}},
	 {match,{0.1,ip_dscp}},
	 allow_arp].

%%EOF
