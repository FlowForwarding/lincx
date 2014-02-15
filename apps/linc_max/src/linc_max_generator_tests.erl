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
		|| {MVal,Mask,TVal,Expected} <- masked(64)].

ether_test_() ->
	[[?_test(packet([#ether{dhost =TVal},#ipv4{},#udp{}],
			[{eth_dst,MVal,Mask}], Expected))
		|| {MVal,Mask,TVal,Expected} <- masked(48)],
	 [?_test(packet([#ether{shost =TVal},#ipv4{},#udp{}],
			[{eth_src,MVal,Mask}], Expected))
		|| {MVal,Mask,TVal,Expected} <- masked(48)]

	%% TODO: eth_type

].

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
