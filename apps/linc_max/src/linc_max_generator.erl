-module(linc_max_generator).
-export([update_flow_table/2]).
-export([flow_table_forms/2]).

-include_lib("of_protocol/include/ofp_v4.hrl").
-include_lib("linc/include/linc_logger.hrl").
-include("linc_max.hrl").
-include("fast_path.hrl").

update_flow_table(TabName, FlowEnts) ->
	{ok,Forms} = flow_table_forms(TabName, FlowEnts),
	{ok,TabName,Bin} = compile:forms(Forms, [report_errors]),
	case erlang:check_old_code(TabName) of
	true ->
		erlang:purge_module(TabName);
	_ ->
		ok
	end,
	{module,_} = erlang:load_module(TabName, Bin),
	ok.

flow_table_forms(TabName, FlowEnts) ->

	%%
	%% module(flow_table_0).
	%% -export([match/23]).
	%% match(...) ->
	%% ...
	%%

	MatchArity = length(match_arguments()),

	F1 = {attribute,0,module,TabName},
	F2 = {attribute,0,export,[{match,MatchArity}]},
	F3 = {function,0,match,MatchArity,clauses(FlowEnts)},

	Forms = [F1,F2,F3],
	{ok,Forms}.

clauses(FlowEnts) ->
	clauses(FlowEnts, []).

clauses([], Acc) ->

	%%
	%% OFv1.4: Every flow table must support a table-miss flow entry to process
	%% table misses.
	%%
	%% match(_, _, _, ...) -> miss.
	%%

	Wildcards = [{var,0,'_'} || _ <- match_arguments()],
	Miss = {clause,0,Wildcards,[],[{atom,0,miss}]},
	lists:reverse([Miss|Acc]);

clauses([#flow_entry{match =#ofp_match{fields =Matches},
					 instructions =InstrList}|FlowEnts], Acc) ->
	case catch build_patterns(Matches, InstrList) of
	nomatch ->
		%% no chance of a match, suppress the clause
		clauses(FlowEnts, Acc);

	{'EXIT',Reason} ->
		erlang:error(Reason);

	Patterns when is_list(Patterns) ->
		Clause = {clause,0,Patterns,[],body(InstrList)},
		clauses(FlowEnts, [Clause|Acc]);

	{Patterns,Guard} ->
		Clause = {clause,0,Patterns,Guard,body(InstrList)},
		clauses(FlowEnts, [Clause|Acc])
	end.

build_patterns(Matches, InstrList) ->
	%% openflow_basic class only, enforced elsewhere

	HasGoto = lists:any(fun(#ofp_instruction_goto_table{}) -> true;
					(_) -> false end, InstrList),

	Ps = [pat(M) || M <- Matches],
	%% Ps :: [{Arg,[{Start,Len,Value}]} | {Arg,[Value]}
	ArgZones =
		[begin
			Xs = lists:concat([Zs || {Arg1,Zs} <- Ps, Arg1 =:= Arg]),
			{Arg,combine(sort_zones(Xs))}
		 end
			|| Arg <- lists:usort([A || {A,_} <- Ps])],

	Patterns =
		[begin
			Pat = case lists:keyfind(Arg, 1, ArgZones) of
			false ->
				{var,0,'_'};
			{_,[Value]} when is_integer(Value) ->
				value_pattern(Value, Arg);
			{_,[undefined]} ->
				{atom,0,undefined};
			{_,Zs} ->
				{bin,0,bin_elems(Zs)}
			end,
			wrap_pattern(Pat, Arg, HasGoto)
		 end
			|| Arg <- match_arguments()],

	add_guard(Patterns, Matches).

add_guard(Patterns, Matches) ->
	case lists:keymember(ipv6_nd_target, #ofp_field.name, Matches) of
	true ->
		%%
		%% When matching on IPV6_ND_TARGET we need to ensure that the type of
		%% ICMPv6 packet is either 135 or 136. For these packet types both
		%% IPV6_ND_SLL and IPV6_ND_TLL fields cannot be 'undefined'.
		%%
		Guard =
			[[{op,0,'=/=',{var,0,var_name(icmp6_sll)},{atom,0,undefined}}],
             [{op,0,'=/=',{var,0,var_name(icmp6_tll)},{atom,0,undefined}}]],
		{Patterns,Guard};
	false ->
		Patterns
	end.

%% Only IPV6_ND_SLL and IPV6_ND_TLL value fields are matched as binaries 
value_pattern(Value, icmp6_sll) ->
	value_pattern_as_binary(Value, 48);
value_pattern(Value, icmp6_tll) ->
	value_pattern_as_binary(Value, 48);
value_pattern(Value, _) ->
	{integer,0,Value}.

value_pattern_as_binary(Value, Bits) ->
	Zs = [{0,Bits,Value}],
	{bin,0,bin_elems(Zs)}.

wrap_pattern(Pat, Arg, true) ->
	wrap_pattern_1(Pat, Arg);
wrap_pattern(Pat, packet =Arg, _) ->
	wrap_pattern_1(Pat, Arg);
wrap_pattern(Pat, actions =Arg, _) ->
	wrap_pattern_1(Pat, Arg);
wrap_pattern(Pat, blaze =Arg, _) ->
	wrap_pattern_1(Pat, Arg);
wrap_pattern(Pat, icmp6_sll =Arg, _) ->
	wrap_pattern_1(Pat, Arg);
wrap_pattern(Pat, icmp6_tll =Arg, _) ->
	wrap_pattern_1(Pat, Arg);
wrap_pattern(Pat, _Arg, _HasGoto) ->
	Pat.

wrap_pattern_1(Pat, Arg) ->
	{match,0,Pat,{var,0,var_name(Arg)}}.

sort_zones([T|_] =Ts) when is_tuple(T) ->
	lists:keysort(1, Ts);
sort_zones(Ts) ->
	lists:sort(Ts).

pat(#ofp_field{name =in_port,value = <<Value:32>>}) ->
	{in_port,[Value]};
pat(#ofp_field{name =in_phy_port,value = <<Value:32>>}) ->
	{in_phy_port,[Value]};
pat(#ofp_field{name =metadata,value = <<Value:64>>,has_mask =HasMask,mask =Mask}) ->
	bit_pat(metadata, 0, 64, Value, mask(HasMask, Mask, 64));
pat(#ofp_field{name =eth_dst,value = <<Value:48>>,has_mask =HasMask,mask =Mask}) ->
	bit_pat(packet, 0, 48, Value, mask(HasMask, Mask, 48));
pat(#ofp_field{name =eth_src,value = <<Value:48>>,has_mask =HasMask,mask =Mask}) ->
	bit_pat(packet, 48, 48, Value, mask(HasMask, Mask, 48));
pat(#ofp_field{name =eth_type,value = <<Value:16>>}) ->
	{eth_type,[Value]};
pat(#ofp_field{name =vlan_vid,value = <<0:13>>,has_mask =false}) ->
	%% OFv1.4: Table 13: Match combinations for VLAN tags.
	{vlan_tag,[undefined]};
pat(#ofp_field{name =vlan_vid,value = <<16#1000:13>>,has_mask =true,mask = <<16#1000:13>>}) ->
	%% OFv1.4: Table 13: Match combinations for VLAN tags.
	{vlan_tag,[{0,0,0}]};
pat(#ofp_field{name =vlan_vid,value = <<Value:13>>,has_mask =false}) ->
	bit_pat(vlan_tag, 3, 13, Value, nomask);
pat(#ofp_field{name =vlan_pcp,value = <<Value:3>>}) ->
	bit_pat(vlan_tag, 0, 3, Value, nomask);
pat(#ofp_field{name =ip_dscp,value = <<Value:6>>}) ->
	bit_pat(ip_tclass, 0, 6, Value, nomask);
pat(#ofp_field{name =ip_ecn,value = <<Value:2>>}) ->
	bit_pat(ip_tclass, 6, 2, Value, nomask);
pat(#ofp_field{name =ip_proto,value = <<Value:8>>}) ->
	{ip_proto,[Value]};
pat(#ofp_field{name =ipv4_src,value = <<Value:32>>,has_mask =HasMask,mask =Mask}) ->
	bit_pat(ip4_hdr, 96, 32, Value, mask(HasMask, Mask, 32));
pat(#ofp_field{name =ipv4_dst,value = <<Value:32>>,has_mask =HasMask,mask =Mask}) ->
	bit_pat(ip4_hdr, 128, 32, Value, mask(HasMask, Mask, 32));
pat(#ofp_field{name =tcp_src,value = <<Value:16>>}) ->
	bit_pat(tcp_hdr, 0, 16, Value, nomask);
pat(#ofp_field{name =tcp_dst,value = <<Value:16>>}) ->
	bit_pat(tcp_hdr, 16, 16, Value, nomask);
pat(#ofp_field{name =udp_src,value = <<Value:16>>}) ->
	bit_pat(udp_hdr, 0, 16, Value, nomask);
pat(#ofp_field{name =udp_dst,value = <<Value:16>>}) ->
	bit_pat(udp_hdr, 16, 16, Value, nomask);
pat(#ofp_field{name =sctp_src,value = <<Value:16>>}) ->
	bit_pat(sctp_hdr, 0, 16, Value, nomask);
pat(#ofp_field{name =sctp_dst,value = <<Value:16>>}) ->
	bit_pat(sctp_hdr, 16, 16, Value, nomask);
pat(#ofp_field{name =icmpv4_type,value = <<Value:8>>}) ->
	bit_pat(icmp_msg, 0, 8, Value, nomask);
pat(#ofp_field{name =icmpv4_code,value = <<Value:8>>}) ->
	bit_pat(icmp_msg, 8, 8, Value, nomask);
pat(#ofp_field{name =arp_op,value = <<Value:16>>}) ->
	bit_pat(arp_msg, 48, 16, Value, nomask);
pat(#ofp_field{name =arp_spa,value = <<Value:32>>,has_mask =HasMask,mask =Mask}) ->
	bit_pat(arp_msg, 112, 32, Value, mask(HasMask, Mask, 32));
pat(#ofp_field{name =arp_tpa,value = <<Value:32>>,has_mask =HasMask,mask =Mask}) ->
	bit_pat(arp_msg, 192, 32, Value, mask(HasMask, Mask, 32));
pat(#ofp_field{name =arp_sha,value = <<Value:48>>,has_mask =HasMask,mask =Mask}) ->
	bit_pat(arp_msg, 64, 48, Value, mask(HasMask, Mask, 48));
pat(#ofp_field{name =arp_tha,value = <<Value:48>>,has_mask =HasMask,mask =Mask}) ->
	bit_pat(arp_msg, 144, 48, Value, mask(HasMask, Mask, 48));
pat(#ofp_field{name =ipv6_src,value = <<Value:128>>,has_mask =HasMask,mask =Mask}) ->
	bit_pat(ip6_hdr, 64, 128, Value, mask(HasMask, Mask, 128));
pat(#ofp_field{name =ipv6_dst,value = <<Value:128>>,has_mask =HasMask,mask =Mask}) ->
	bit_pat(ip6_hdr, 192, 128, Value, mask(HasMask, Mask, 128));
pat(#ofp_field{name =ipv6_flabel,value = <<Value:20>>,has_mask =HasMask,mask =Mask}) ->
	bit_pat(ip6_hdr, 12, 20, Value, mask(HasMask, Mask, 20));
pat(#ofp_field{name =icmpv6_type,value = <<Value:8>>}) ->
	bit_pat(icmp6_hdr, 0, 8, Value, nomask);
pat(#ofp_field{name =icmpv6_code,value = <<Value:8>>}) ->
	bit_pat(icmp6_hdr, 8, 8, Value, nomask);
pat(#ofp_field{name =ipv6_nd_target,value = <<Value:128>>}) ->
	%% requires a guard: icmp6_sll =/= undefined; icmp6_tll =/= undefined
	bit_pat(icmp6_hdr, 64, 128, Value, nomask);
pat(#ofp_field{name =ipv6_nd_sll,value = <<Value:48>>}) ->
	{icmp6_sll,[Value]};
pat(#ofp_field{name =ipv6_nd_tll,value = <<Value:48>>}) ->
	{icmp6_tll,[Value]};
pat(#ofp_field{name =mpls_label,value = <<Value:20>>}) ->
	bit_pat(mpls_tag, 0, 20, Value, nomask);
pat(#ofp_field{name =mpls_tc,value = <<Value:3>>}) ->
	bit_pat(mpls_tag, 20, 3, Value, nomask);
pat(#ofp_field{name =mpls_bos,value = <<Value:1>>}) ->
	bit_pat(mpls_tag, 23, 1, Value, nomask);
pat(#ofp_field{name =pbb_isid,value = <<Value:24>>,has_mask =HasMask,mask =Mask}) ->
	bit_pat(pbb_tag, 8, 24, Value, mask(HasMask, Mask, 24));
pat(#ofp_field{name =tunnel_id,value = <<Value:64>>,has_mask =HasMask,mask =Mask}) ->
	bit_pat(tunnel_id, 0, 64, Value, mask(HasMask, Mask, 64));
pat(#ofp_field{name =ipv6_exthdr,value = <<Value:9>>,has_mask =HasMask,mask =Mask}) ->
	bit_pat(ip6_ext, 0, 9, Value, mask(HasMask, Mask, 9));
pat(#ofp_field{name =pbb_uca,value = <<Value:1>>}) ->
	bit_pat(pbb_tag, 4, 1, Value, nomask).

mask(false, _, _) ->
	nomask;
mask(true, Bin, N) ->
	<<Mask:N>> =Bin,
	Mask.

bit_pat(Arg, Start, Bits, Value, nomask) ->
	{Arg,[{Start,Bits,Value}]};
bit_pat(Arg, Start, Bits, Value, Mask) ->
	{Arg,[{Start +Bits -Pos -Len,
		   Len,
		   (Value band sub_mask(Pos, Len)) bsr Pos}
				|| {Pos,Len} <- split_mask(Mask)]}.

split_mask(Mask) ->
	split_mask(Mask, 0, 1, 0, []).

split_mask(0, _, _, 0, Acc) ->
	Acc;
split_mask(0, N, _, Ones, Acc) ->
	[{N -Ones,Ones}|Acc];
split_mask(Mask, N, Probe, Ones, Acc) when Mask band Probe =/= 0 ->
	split_mask(Mask band (bnot Probe), N +1, Probe bsl 1, Ones +1, Acc);
split_mask(Mask, N, Probe, 0, Acc) ->
	split_mask(Mask, N +1, Probe bsl 1, 0, Acc);
split_mask(Mask, N, Probe, Ones, Acc) ->
	split_mask(Mask, N +1, Probe bsl 1, 0, [{N -Ones,Ones}|Acc]).

sub_mask(N, L) ->
	((1 bsl L) -1) bsl N.

%%
%% Combines bit patterns and values that refer to the same argument:
%%
%%	- adjacent or overlapping bit patterns are merged
%%	- overlapping bit patterns with different values throw 'nomatch'
%%	- coincident value patterns are merged
%%	- values patterns demanding different values for the field throw 'nomatch'
%%
%% The function is slightly to rigorous.
%%

combine(Ts) ->	%% can throw 'nomatch'
	combine(Ts, []).

combine([T], Acc) ->
	lists:reverse([T|Acc]);
combine([Val|[Val|_] =Ts], Acc) when is_integer(Val) ->
	combine(Ts, Acc);
combine([Val1,Val2|_], _Acc) when is_integer(Val1), is_integer(Val2) ->
	throw(nomatch);
combine([{Start1,Len1,_Val1} =T|[{Start2,_Len2,_Val2}|_] =Ts], Acc)
		when Start1 +Len1 < Start2 ->
	combine(Ts, [T|Acc]);
combine([T1,T2|Ts], Acc) ->
	combine([combine1(T1, T2)|Ts], Acc).

combine1({S1,L1,V1} =T1, {S2,L2,_V2} =T2) ->
	IS = S2,
	X = S1 +L1 -S2,
	IL = if X < L2 -> X; true -> L2 end,
	IV1 = cut(T1, IS, IL),
	IV2 = cut(T2, IS, IL),
	TS = S1 +L1,
	TL = S2 +L2 -TS,
	if IV1 =/= IV2 ->
		throw(nomatch);
	TL =< 0 ->
		T1;
	true ->
		{S1,
		 L1 +TL,
		 (V1 bsl TL) bor cut(T2, TS, TL)}
	end.

cut({S,L,V}, SA, LA) ->
	X = S +L -SA -LA,
	(V bsr X) band ((1 bsl LA) -1).

bin_elems(Zs) ->
	bin_elems(0, Zs, []).

bin_elems(O, [], Acc) when O rem 8 =:= 0 ->
	E = {bin_element,0,{var,0,'_'},default,[binary]},
	lists:reverse([E|Acc]);
bin_elems(_O, [], Acc) ->
	E = {bin_element,0,{var,0,'_'},default,[bits]},
	lists:reverse([E|Acc]);
bin_elems(O, [{S,_,_}|_] =Zs, Acc) when S > O ->
	E = {bin_element,0,{var,0,'_'},{integer,0,S -O},[bits]},
	bin_elems(S, Zs, [E|Acc]);
bin_elems(S, [{S,0,0}|Zs], Acc) ->
	bin_elems(S, Zs, Acc);
bin_elems(S, [{S,L,V}|Zs], Acc) when L >= 8 ->
	S1 = S +8,
	L1 = L -8,
	V0 = V bsr L1,
	V1 = V band ((1 bsl L1) -1),
	E = {bin_element,0,{integer,0,V0},default,default},
	bin_elems(S1, [{S1,L1,V1}|Zs], [E|Acc]);
bin_elems(S, [{S,L,V}|Zs], Acc) ->
	E = {bin_element,0,{integer,0,V},{integer,0,L},default},
	bin_elems(S +L, Zs, [E|Acc]).

%% Instruction compilation -----------------------------------------------------

%-record(ofp_instruction_apply_actions,{seq = 2,actions}).
%-record(ofp_instruction_clear_actions,{seq = 3}).
%-record(ofp_instruction_experimenter,{seq = 7,experimenter,data = <<>>}).
%-record(ofp_instruction_goto_table,{seq = 6,table_id}).
%-record(ofp_instruction_meter,{seq = 1,meter_id}).
%-record(ofp_instruction_write_actions,{seq = 4,actions}).
%-record(ofp_instruction_write_metadata,{seq = 5,
%                                        metadata,
%                                        metadata_mask = <<1:64>>}).

body(InstrList) ->
	body(InstrList,
		undefined,	%% MeterInstr
		undefined,	%% ApplyInstr
		undefined,	%% ClearWriteInstr
		undefined,	%% MetadataInstr
		undefined). %% GotoInstr

body([], MeterInstr, ApplyInstr, ClearWriteInstr, MetadataInstr, GotoInstr) ->
	compile_body(MeterInstr, ApplyInstr, ClearWriteInstr, MetadataInstr, GotoInstr);
body([#ofp_instruction_meter{meter_id =Id}|InstrList],
		_MeterInstr, ApplyInstr, ClearWriteInstr, MetadataInstr, GotoInstr) ->
	body(InstrList, {meter,Id}, ApplyInstr, ClearWriteInstr, MetadataInstr, GotoInstr);
body([#ofp_instruction_apply_actions{actions =As}|InstrList],
		MeterInstr, _ApplyInstr, ClearWriteInstr, MetadataInstr, GotoInstr) ->
	body(InstrList, MeterInstr, {apply,fast_action_list(As)}, ClearWriteInstr, MetadataInstr, GotoInstr);
body([#ofp_instruction_clear_actions{}|InstrList],
		MeterInstr, ApplyInstr, undefined, MetadataInstr, GotoInstr) ->
	body(InstrList, MeterInstr, ApplyInstr, clear, MetadataInstr, GotoInstr);
body([#ofp_instruction_write_actions{actions =Actions}|InstrList],
		MeterInstr, ApplyInstr, clear, MetadataInstr, GotoInstr) ->
	body(InstrList, MeterInstr, ApplyInstr, {clear,Actions}, MetadataInstr, GotoInstr);
body([#ofp_instruction_write_actions{actions =Actions}|InstrList],
		MeterInstr, ApplyInstr, _ClearWriteInstr, MetadataInstr, GotoInstr) ->
	body(InstrList, MeterInstr, ApplyInstr, {write,Actions}, MetadataInstr, GotoInstr);
body([#ofp_instruction_write_metadata{metadata = <<Value:64>>,
									  metadata_mask = <<Mask:64>>}|InstrList],
		MeterInstr, ApplyInstr, ClearWriteInstr, _MetadataInstr, GotoInstr) ->
	body(InstrList, MeterInstr, ApplyInstr, ClearWriteInstr, {metadata,Value,Mask}, GotoInstr);
body([#ofp_instruction_goto_table{table_id =Id}|InstrList],
		MeterInstr, ApplyInstr, ClearWriteInstr, MetadataInstr, _GotoInstr) ->
	body(InstrList, MeterInstr, ApplyInstr, ClearWriteInstr, MetadataInstr, {goto,Id}).


compile_body(MeterInstr, undefined, ClearWriteInstr, _MetadataInstr, undefined) ->
	%%
	%% No goto table - return (updated) action set
	%%
	%% match(Packet,...) ->
	%%		{do,Packet,Actions}.
	%%
	Goto = [{tuple,0,[{atom,0,do},
					   {var,0,var_name(packet)},
					   updated_actions(ClearWriteInstr)]}],
	compile_meter(MeterInstr, Goto);

compile_body(MeterInstr, {apply,ActionList}, ClearWriteInstr, _MetadataInstr, undefined) ->
	%%
	%% No goto table - apply action list and return (updated) action set
	%%
	%% match(Packet,...) ->
	%%		Packet1 = linc_max_fast_actions:apply_list(ActionList, Frame, Blaze),
	%%		{do,Packet1,Actions}.
	%%
	ApplyList = {call,0,{remote,0,{atom,0,linc_max_fast_actions},
								  {atom,0,apply_list}},
							[ast(ActionList),
							 {var,0,var_name(packet)},
							 {var,0,var_name(blaze)}]},
	Return = [{tuple,0,[{atom,0,do},
					   ApplyList,
					   updated_actions(ClearWriteInstr)]}],
	compile_meter(MeterInstr, Return);

compile_body(MeterInstr, _ApplyInstr, ClearWriteInstr, MetadataInstr, {goto,Id}) ->
	%%
	%% match(...) ->
	%%		flow_table_1:match(...).
	%%

	%%TODO: use linc function here
	TabName = list_to_atom("flow_table_" ++ integer_to_list(Id)),

	%% update the metadata and actions before calling the next flow table
	As = lists:map(fun(actions) ->
		updated_actions(ClearWriteInstr);
	
	(metadata) when MetadataInstr =/= undefined ->
		{_,Value,Mask} = MetadataInstr,
		{call,0,{remote,0,{atom,0,linc_max_fast_actions},
						  {atom,0,update_metadata}},[{var,0,var_name(metadata)},
												     {integer,0,(bnot Mask)},
													 {integer,0,Value}]};
	(A) ->
		{var,0,var_name(A)}
	end, match_arguments()),

	Call = {call,0,{remote,0,{atom,0,TabName},{atom,0,match}},As},
	Goto = [Call],
	compile_meter(MeterInstr, Goto).

compile_meter(undefined, Goto) ->
	Goto;
compile_meter({meter,Id}, Goto) ->
	%%
	%% match(...) ->
	%%		case meter(N) of
	%%		ok ->
	%%			...
	%%		_ ->
	%%			drop
	%%		end.
	%%
	Call = {call,0,{remote,0,{atom,0,linc_max_fast_actions},
						     {atom,0,meter}},[{integer,0,Id}]},
	C1 = {clause,0,[{atom,0,ok}],[],Goto},
	C2 = {clause,0,[{var,0,'_'}],[],[{atom,0,drop}]},
	Case = {'case',0,Call,[C1,C2]},
	[Case].

%-record(ofp_action_copy_ttl_in, { seq = 1 }).
%-record(ofp_action_pop_mpls, { seq = 2, ethertype :: integer() }).
%-record(ofp_action_pop_pbb, { seq = 3 }).
%-record(ofp_action_pop_vlan, { seq = 4 }).
%-record(ofp_action_push_mpls, { seq = 5, ethertype :: integer() }).
%-record(ofp_action_push_pbb, { seq = 6, ethertype :: integer() }).
%-record(ofp_action_push_vlan, { seq = 7, ethertype :: integer() }).
%-record(ofp_action_copy_ttl_out, { seq = 8 }).
%-record(ofp_action_dec_mpls_ttl, { seq = 9 }).
%-record(ofp_action_dec_nw_ttl, { seq = 10 }).
%-record(ofp_action_set_mpls_ttl, { seq = 11, mpls_ttl :: integer() }).
%-record(ofp_action_set_nw_ttl, { seq = 12, nw_ttl :: integer() }).
%-record(ofp_action_set_field, { seq = 13, field :: ofp_field() }).
%-record(ofp_action_set_queue, { seq = 14, queue_id :: integer() }).
%-record(ofp_action_group, { seq = 15, group_id :: integer() }).
%-record(ofp_action_output, { seq = 16, port :: ofp_port_no(),
%						 max_len = no_buffer :: ofp_packet_in_bytes() }).

%%TODO: packet modifications

updated_actions(undefined) ->
	{var,0,var_name(actions)};

updated_actions({clear,Specs}) ->
	updated_actions(clear, Specs, #fast_actions{});
updated_actions({write,Specs}) ->
	updated_actions(write, Specs, #fast_actions{}).

updated_actions(clear, [], Actions) ->
	ast(Actions);
updated_actions(write, [], Actions) ->
	Size = record_info(size, fast_actions),
	Fields = record_info(fields, fast_actions), 
	Rs = lists:zip(lists:seq(2, Size), Fields),

	OldElems = [{var,0,'_'}]	%% record tag (fast_actions)
			++
		lists:map(fun({N,F}) ->
			case element(N, Actions) of
			undefined ->
				{var,0,var_name(F)};
			_ ->
				{var,0,'_'}
			end
		end, Rs),

	NewElems = [{atom,0,fast_actions}]
			++
		lists:map(fun({N,F}) ->
			case element(N, Actions) of
			undefined ->
				{var,0,var_name(F)};
			X ->
				ast(X)
			end
		end, Rs),
	{block,0,[{match,0,{tuple,0,OldElems},{var,0,var_name(actions)}},
			  {tuple,0,NewElems}]};

updated_actions(Tag, [#ofp_action_set_queue{queue_id =Id}|Specs], Actions) ->
	updated_actions(Tag, Specs, Actions#fast_actions{queue =Id});
updated_actions(Tag, [#ofp_action_output{port = Port}|Specs], Actions) ->
	updated_actions(Tag, Specs, Actions#fast_actions{output =Port});
updated_actions(Tag, [#ofp_action_group{group_id =Id}|Specs], Actions) ->
	updated_actions(Tag, Specs, Actions#fast_actions{group =Id}).

%% @doc convert from #ofp_action{} to the fast path layout
fast_action_list(As) ->
	fast_action_list(As, []).

fast_action_list([], Acc) ->
	lists:reverse(Acc);
fast_action_list([#ofp_action_output{port =PortNo}|As], Acc) ->
	fast_action_list(As, [{output,PortNo}|Acc]);
fast_action_list([#ofp_action_set_queue{queue_id =Queue}|As], Acc) ->
	fast_action_list(As, [{set_queue,Queue}|Acc]);
fast_action_list([#ofp_action_group{group_id =Group}|As], Acc) ->
	fast_action_list(As, [{group,Group}|Acc]);
fast_action_list([#ofp_action_push_vlan{ethertype =EthType}|As], Acc) ->
	fast_action_list(As, [{push_vlan,EthType}|Acc]);
fast_action_list([#ofp_action_pop_vlan{}|As], Acc) ->
	fast_action_list(As, [pop_vlan|Acc]);
fast_action_list([#ofp_action_push_mpls{ethertype =EthType}|As], Acc) ->
	fast_action_list(As, [{push_mpls,EthType}|Acc]);
fast_action_list([#ofp_action_pop_mpls{ethertype =EthType}|As], Acc) ->
	fast_action_list(As, [{pop_mpls,EthType}|Acc]);
fast_action_list([#ofp_action_push_pbb{ethertype =EthType}|As], Acc) ->
	fast_action_list(As, [{push_pbb,EthType}|Acc]);
fast_action_list([#ofp_action_pop_pbb{}|As], Acc) ->
	fast_action_list(As, [pop_pbb|Acc]);
fast_action_list([#ofp_action_set_field{field =
					#ofp_field{name =Name,value =Value}}|As], Acc) ->
	fast_action_list(As, [{set_field,Name,fast_value(Value)}|Acc]);
fast_action_list([#ofp_action_set_mpls_ttl{mpls_ttl =TTL}|As], Acc) ->
	fast_action_list(As, [{set_mpls_ttl,TTL}|Acc]);
fast_action_list([#ofp_action_dec_mpls_ttl{}|As], Acc) ->
	fast_action_list(As, [decrement_mpls_ttl|Acc]);
fast_action_list([#ofp_action_set_nw_ttl{nw_ttl =TTL}|As], Acc) ->
	fast_action_list(As, [{set_ip_ttl,TTL}|Acc]);
fast_action_list([#ofp_action_dec_nw_ttl{}|As], Acc) ->
	fast_action_list(As, [decrement_ip_ttl|Acc]);
fast_action_list([#ofp_action_copy_ttl_out{}|As], Acc) ->
	fast_action_list(As, [copy_ttl_outwards|Acc]);
fast_action_list([#ofp_action_copy_ttl_in{}|As], Acc) ->
	fast_action_list(As, [copy_ttl_inwards|Acc]).

%% linc_max_splicer uses integers if the bit size of the field is 28 bits or
%% less. The rest of LINC switch always uses binaries and bitstrings.
fast_value(Value) when is_bitstring(Value) ->
	case bit_size(Value) of
	N when N =< 28 ->
		<<Int:N>> = Value,
		Int;
	_ ->
		Value
	end;
fast_value(Value) ->
	Value.

var_name(packet) -> 'Packet';
var_name(vlan_tag) -> 'VlanTag';
var_name(eth_type) -> 'EthType';
var_name(pbb_tag) -> 'PbbTag';
var_name(mpls_tag) -> 'MplsTag';
var_name(ip4_hdr) -> 'Ip4Hdr';
var_name(ip6_hdr) -> 'Ip6Hdr';
var_name(ip6_ext) -> 'Ip6Ext';
var_name(ip_proto) -> 'IpProto';
var_name(ip_tclass) -> 'IpTclass';
var_name(arp_msg) -> 'ArpMsg';
var_name(icmp6_hdr) -> 'Icmp6Hdr';
var_name(icmp6_sll) -> 'Icmp6OptSll';
var_name(icmp6_tll) -> 'Icmp6OptTll';
var_name(icmp_msg) -> 'IcmpMsg';
var_name(tcp_hdr) -> 'TcpHdr';
var_name(udp_hdr) -> 'UdpHdr';
var_name(sctp_hdr) -> 'SctpHdr';
var_name(metadata) -> 'Metadata';
var_name(in_port) -> 'InPort';
var_name(in_phy_port) -> 'InPhyPort';
var_name(tunnel_id) -> 'TunnelId';
var_name(actions) -> 'Actions';
var_name(blaze) -> 'Blaze';
%% fast_actions fields
var_name(queue) -> 'Queue';
var_name(output) -> 'Output';
var_name(group) -> 'Group'.

match_arguments() ->
	[packet,
	 vlan_tag,
	 eth_type,
	 pbb_tag,
	 mpls_tag,
	 ip4_hdr,
	 ip6_hdr,
	 ip6_ext,
	 ip_tclass,
	 ip_proto,
	 arp_msg,		%% ARP only
	 icmp_msg,		%% ICMP only
	 icmp6_hdr,		%% ICMPv6 only
	 icmp6_sll,		%% ICMPv6 only (135)
	 icmp6_tll,		%% ICMPv6 only (136)
	 tcp_hdr,		%% TCP
	 udp_hdr,		%% UDP
	 sctp_hdr,		%% SCTP
	 metadata,
	 in_port,
	 in_phy_port,
	 tunnel_id,
	 actions,
	 blaze].

ast(Term) ->
	erl_syntax:revert(erl_syntax:abstract(Term)).

%%EOF
