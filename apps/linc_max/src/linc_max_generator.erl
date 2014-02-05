-module(linc_max_generator).
-export([update_flow_table/2]).
-export([flow_table_forms/2]).

-define(ETH_P_IP,			16#0800).
-define(ETH_P_ARP,			16#0806).
-define(ETH_P_IPV6,			16#86dd).

-define(VLAN_VID_NONE,		16#0000).
-define(VLAN_VID_PRESENT,	16#1000).

-record(instr, {meter,
				apply,
				clear_write,
				metadata,
				goto}).

update_flow_table(TabName, FlowEnts) ->
	{ok,Forms} = flow_table_forms(TabName, FlowEnts),

	{ok,TabName,Bin} = compile:forms(Forms, []),

	case erlang:check_old_code(TabName) of
	true ->
		erlang:purge_module(TabName);
	_ ->
		ok
	end,
	{module,_} = erlang:load_module(TabName, Bin),
	ok.

flow_table_forms(TabName, FlowEnts) ->
	EntsWithVersions = versions(FlowEnts),

	F1 = {attribute,0,module,TabName},
	F2 = {attribute,0,export,
			[{FName,length(Args)}
				|| {FName,Args} <- function_signatures()]},
	Fs = [{function,0,FName,length(Args),
						clauses(EntsWithVersions, FName, Args)}
				|| {FName,Args} <- function_signatures()],

	Forms = [F1,F2|Fs],
	{ok,Forms}.

%% duplicate clauses that depend on IPv4/IPv6
versions(Ents) ->
	versions(Ents, []).

versions([], Acc) ->
	lists:reverse(Acc);
versions([{flow,Matches,Action} =Flow|Ents], Acc) ->
	case lists:keymember(ip_proto, 1, Matches) orelse
		 lists:keymember(ip_dscp, 1, Matches) orelse
		 lists:keymember(ip_ecn, 1, Matches) of
	true ->
		versions(Ents, [{flowv4,Matches,Action},
						{flowv6,Matches,Action}|Acc]);
	false ->
		versions(Ents, [Flow|Acc])
	end.

clauses(Ents, FName, Args) ->
	clauses(Ents, FName, Args, []).

clauses([], _, Args, Acc) ->
	Miss = {clause,0,[{var,0,'_'} || _ <- Args],[],[{atom,0,miss}]},
	lists:reverse([Miss|Acc]);
clauses([{Version,Matches,Instr}|Ents], FName, Args, Acc) ->
	case catch build_patterns(Version, Matches, Instr, FName, Args) of
	nomatch ->
		%% no chance of the match, suppress the clause
		clauses(Ents, FName, Args, Acc);
	Pats ->
		Clause = {clause,0,Pats,[],todo(Instr, FName, Args)},
		clauses(Ents, FName, Args, [Clause|Acc])
	end.

build_patterns(Version, Matches, Instr, _FName, Args) ->
	Specs = [spec(M, Version) || M <- Matches]
				++
			 match_version(Version),
	RefArgs = lists:usort([A || {A,_} <- Specs]),
	XtraArgs = RefArgs -- Args,
	if XtraArgs =/= [] ->
		throw(nomatch);
			true -> ok end,

	ArgZones = lists:foldr(fun(Arg, ArgZones) ->
		Ys = lists:concat([Xs
					|| {Arg1,Xs} <- Specs,Arg1 =:= Arg]),
		Zones = combine(sort_zones(Ys)),
		[{Arg,Zones}|ArgZones]
	end, [], RefArgs),

	lists:map(fun(A) ->
		P = case lists:keyfind(A, 1, ArgZones) of
		false ->
			{var,0,'_'};
		{_,[Value]} when is_integer(Value) ->
			{integer,0,Value};
		{_,[none]} ->
			{atom,0,none};
		{_,Zs} ->
			{bin,0,bin_elems(Zs)}
		end,
		if Instr#instr.goto =/= undefined;
					A =:= actions; A =:= state ->
			{match,0,P,{var,0,var_name(A)}};
				true -> P end
	end, Args).

sort_zones([T|_] =Ts) when is_tuple(T) ->
	lists:keysort(1, Ts);
sort_zones(Ts) ->
	lists:sort(Ts).

todo(#instr{meter =MI,
			apply =_AI,
			clear_write =CI,
			metadata =DI,
			goto =GI}, FName, Args) ->

	Goto = goto_instr(GI, CI, DI, FName, Args),
	Meter = meter_instr(MI, Goto),
	Meter.

goto_instr(undefined, CI, _DI, _FName, _Args) ->
	[{tuple,0,[{atom,0,do},
			   updated_actions(CI)]}];
goto_instr({goto,N}, CI, DI, FName, Args) ->
	TabName = list_to_atom("flow" ++ integer_to_list(N)),

	%% update metadata before calling the next flow table
	As = lists:map(fun(actions) when CI =/= undefined ->
		updated_actions(CI);
	
	(metadata) when DI =/= undefined ->
		{_,Value,Mask} = DI,
		{call,0,{remote,0,{atom,0,linc_max},
						  {atom,0,update_metadata}},[{var,0,var_name(metadata)},
												     {integer,0,(bnot Mask)},
													 {integer,0,Value}]};
	(A) ->
		{var,0,var_name(A)}
	end, Args),

	Call = {call,0,{remote,0,{atom,0,TabName},{atom,0,FName}},As},
	[Call].

meter_instr(undefined, Inner) ->
	Inner;
meter_instr({meter,N}, Inner) ->
	Call = {call,0,{remote,0,{atom,0,linc_max},
						     {atom,0,meter}},[{integer,0,N},
											  {var,0,var_name(state)}]},
	C1 = {clause,0,[{atom,0,ok}],[],Inner},
	C2 = {clause,0,[{var,0,'_'}],[],[{atom,0,drop}]},
	Case = {'case',0,Call,[C1,C2]},
	[Case].

updated_actions({clear,Specs}) ->
	Es = lists:map(fun({Fld,_VarName}) ->
		case lists:keyfind(Fld, 1, Specs) of
		false ->
			{atom,0,undefined};
		{_,Term} ->
			erl_syntax:revert(erl_syntax:abstract(Term))
		end
	end, actions_fields()),
	{tuple,0,Es};

updated_actions({write,Specs}) ->
	OldEs = lists:map(fun({Fld,VarName}) ->
		case lists:keyfind(Fld, 1, Specs) of
		false ->
			{var,0,VarName};
		_ ->
			{var,0,'_'}
		end
	end, actions_fields()),

	NewEs = lists:map(fun({Fld,VarName}) ->
		case lists:keyfind(Fld, 1, Specs) of
		false ->
			{var,0,VarName};
		{_,Term} ->
			erl_syntax:revert(erl_syntax:abstract(Term))
		end
	end, actions_fields()),
	{block,0,[{match,0,{tuple,0,OldEs},{var,0,var_name(actions)}},
			  {tuple,0,NewEs}]}.

actions_fields() ->
	[{tag,'Tag'},
	 {queue,'Queue'},
	 {output,'Output'},
	 {group,'Group'}].

match_version(flowv4) ->
	[{eth_type,[?ETH_P_IP]}];
match_version(flowv6) ->
	[{eth_type,[?ETH_P_IPV6]}];
match_version(_) ->
	[].

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
	%io:format("combine ~p and ~p~n", [T1,T2]),
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

spec({in_port,Value}, _) ->
	{in_port,[Value]};
spec({in_phy_port,Value}, _) ->
	{in_phy_port,[Value]};
spec({metadata,Value,Mask}, _) ->
	masq(metadata, 0, 64, Value, Mask);
spec({eth_dst,Value,Mask}, _) ->
	masq(packet, 0, 48, Value, Mask);
spec({eth_src,Value,Mask}, _) ->
	masq(packet, 48, 48, Value, Mask);
spec({eth_type,Value}, _) ->
	{eth_type,[Value]};
spec({vlan_vid,?VLAN_VID_NONE,nomask}, _) ->
	{vlan_tag,[none]};
spec({vlan_vid,?VLAN_VID_PRESENT,?VLAN_VID_PRESENT}, _) ->
	{vlan_tag,[{0,0,0}]};
spec({vlan_vid,Value,nomask}, _) ->
	masq(vlan_tag, 4, 12, Value, nomask);
spec({vlan_pcp,Value}, _) ->
	masq(vlan_tag, 0, 3, Value, nomask);
spec({ip_dscp,Value}, flowv4) ->
	masq(ip4_hdr, 8, 6, Value, nomask);
spec({ip_dscp,Value}, flowv6) ->
	masq(ip6_hdr, 4, 6, Value, nomask);
spec({ip_ecn,Value}, flowv4) ->
	masq(ip4_hdr, 14, 2, Value, nomask);
spec({ip_ecn,Value}, flowv6) ->
	masq(ip6_hdr, 10, 2, Value, nomask);
spec({ip_proto,Value}, flowv4) ->
	masq(ip4_hdr, 72, 8, Value, nomask);
spec({ip_proto,Value}, flowv6) ->
	%%TODO: this is the first header, not the last;
	%% yet another argument is needed
	masq(ip6_hdr, 48, 8, Value, nomask);
spec({ipv4_src,Value,Mask}, _) ->
	masq(ip4_hdr, 96, 32, Value, Mask);
spec({ipv4_dst,Value,Mask}, _) ->
	masq(ip4_hdr, 128, 32, Value, Mask);
spec({tcp_src,Value}, _) ->
	masq(tcp_hdr, 0, 16, Value, nomask);
spec({tcp_dst,Value}, _) ->
	masq(tcp_hdr, 16, 16, Value, nomask);
spec({udp_src,Value}, _) ->
	masq(udp_hdr, 0, 16, Value, nomask);
spec({udp_dst,Value}, _) ->
	masq(udp_hdr, 16, 16, Value, nomask);
spec({sctp_src,Value}, _) ->
	masq(sctp_hdr, 0, 16, Value, nomask);
spec({sctp_dst,Value}, _) ->
	masq(sctp_hdr, 16, 16, Value, nomask);
spec({icmpv4_type,Value}, _) ->
	masq(icmp_hdr, 0, 8, Value, nomask);
spec({icmpv4_code,Value}, _) ->
	masq(icmp_msg, 8, 8, Value, nomask);
spec({arp_op,Value}, _) ->
	masq(arp_msg, 48, 16, Value, nomask);
spec({arp_spa,Value,Mask}, _) ->
	masq(arp_msg, 112, 32, Value, Mask);
spec({arp_tpa,Value,Mask}, _) ->
	masq(arp_msg, 192, 32, Value, Mask);
spec({arp_sha,Value,Mask}, _) ->
	masq(arp_msg, 48, 48, Value, Mask);
spec({arp_tha,Value,Mask}, _) ->
	masq(arp_msg, 144, 48, Value, Mask);
spec({ipv6_src,Value,Mask}, _) ->
	masq(ip6_hdr, 64, 128, Value, Mask);
spec({ipv6_dst,Value,Mask}, _) ->
	masq(ip6_hdr, 192, 128, Value, Mask);
spec({ipv6_flabel,Value,Mask}, _) ->
	masq(ip6_hdr, 12, 20, Value, Mask);
spec({icmpv6_type,Value}, _) ->
	masq(icmp6_hdr, 0, 8, Value, nomask);
spec({icmpv6_code,Value}, _) ->
	masq(icmp6_hdr, 8, 8, Value, nomask);
spec({ipv6_nd_target,_Value}, _) ->
	todo; %%TODO use guard that nd_sll or nd_tll is not 'none'
spec({ipv6_nd_sll,Value}, _) ->
	{ip6_sll,[Value]};
spec({ipv6_nd_tll,Value}, _) ->
	{ip6_tll,[Value]};
spec({mpls_label,Value}, _) ->
	masq(mpls_tag, 0, 20, Value, nomask);
spec({mpls_tc,Value}, _) ->
	masq(mpls_tag, 20, 3, Value, nomask);
spec({mpls_bos,Value}, _) ->
	masq(mpls_tag, 23, 1, Value, nomask);
spec({pbb_isid,Value,Mask}, _) ->
	masq(pbb_tag, 8, 24, Value, Mask);
spec({tunnel_id,Value,Mask}, _) ->
	masq(tunnel_id, 0, 64, Value, Mask);
spec({ipv6_exthdr,Value,Mask}, _) ->
	masq(ip6_ext, 0, 9, Value, Mask);
spec({pbb_uca,Value}, _) ->
	masq(pbb_tag, 4, 1, Value, nomask).

masq(Arg, Start, Bits, Value, nomask) ->
	{Arg,[{Start,Bits,Value}]};
masq(Arg, Start, Bits, Value, Mask) ->
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

var_name(packet) -> 'Packet';
var_name(actions) -> 'Actions';
var_name(vlan_tag) -> 'VlanTag';
var_name(eth_type) -> 'EthType';
var_name(pbb_tag) -> 'PbbTag';
var_name(mpls_tag) -> 'MplsTag';
var_name(ip4_hdr) -> 'Ip4Hdr';
var_name(ip6_hdr) -> 'Ip6Hdr';
var_name(ip6_ext) -> 'Ip6Ext';
var_name(arp_msg) -> 'ArpMsg';
var_name(icmp6_hdr) -> 'Icmp6Hdr';
var_name(icmp6_sll) -> 'Icmp6OptSll';
var_name(icmp6_tll) -> 'Icmp6OptTll';
var_name(icmp_msg) -> 'IcmpMsg';
var_name(tcp_hdr) -> 'TcpHdr';
var_name(udp_hdr) -> 'UdpHdr';
var_name(sctp_hdr) -> 'SctpHdr';
var_name(in_port) -> 'InPort';
var_name(in_phy_port) -> 'InPhyPort';
var_name(metadata) -> 'Metadata';
var_name(tunnel_id) -> 'TunnelId';
var_name(state) -> 'St'.

function_signatures() ->
	[{arp,
		[packet,
		 actions,
		 vlan_tag,
		 eth_type,
		 pbb_tag,
		 mpls_tag,
		 ip4_hdr,
		 ip6_hdr,
		 ip6_ext,
		 arp_msg,
		 in_port,
		 in_phy_port,
		 metadata,
		 tunnel_id,
		 state]},
	 {icmp,
		[packet,
		 actions,
		 vlan_tag,
		 eth_type,
		 pbb_tag,
		 mpls_tag,
		 ip4_hdr,
		 ip6_hdr,
		 ip6_ext,
		 icmp_msg,
		 in_port,
		 in_phy_port,
		 metadata,
		 tunnel_id,
		 state]},
	 {icmpv6,
		[packet,
		 actions,
		 vlan_tag,
		 eth_type,
		 pbb_tag,
		 mpls_tag,
		 ip4_hdr,
		 ip6_hdr,
		 ip6_ext,
		 icmp6_hdr,
		 icmp6_sll,
		 icmp6_tll,
		 in_port,
		 in_phy_port,
		 metadata,
		 tunnel_id,
		 state]},
	 {tcp,
		[packet,
		 actions,
		 vlan_tag,
		 eth_type,
		 pbb_tag,
		 mpls_tag,
		 ip4_hdr,
		 ip6_hdr,
		 ip6_ext,
		 tcp_hdr,
		 in_port,
		 in_phy_port,
		 metadata,
		 tunnel_id,
		 state]},
	 {udp,
		[packet,
		 actions,
		 vlan_tag,
		 eth_type,
		 pbb_tag,
		 mpls_tag,
		 ip4_hdr,
		 ip6_hdr,
		 ip6_ext,
		 udp_hdr,
		 in_port,
		 in_phy_port,
		 metadata,
		 tunnel_id,
		 state]},
	 {sctp,
		[packet,
		 actions,
		 vlan_tag,
		 eth_type,
		 pbb_tag,
		 mpls_tag,
		 ip4_hdr,
		 ip6_hdr,
		 ip6_ext,
		 sctp_hdr,
		 in_port,
		 in_phy_port,
		 metadata,
		 tunnel_id,
		 state]},
	 {nonext,
		[packet,
		 actions,
		 vlan_tag,
		 eth_type,
		 pbb_tag,
		 mpls_tag,
		 ip4_hdr,
		 ip6_hdr,
		 ip6_ext,
		 in_port,
		 in_phy_port,
		 metadata,
		 tunnel_id,
		 state]}].

%%EOF
