-module(linc_max_demo).
-export([update_metadata/3]).
-export([meter/2]).

%% Testing interface
-export([update/1]).
-export([start/0]).

-include_lib("of_protocol/include/ofp_v4.hrl").
-include("linc_max.hrl").

%% FAST PATH
%%
%% The function is needed because metadata are represented as binary in the
%% argument list of a flow entry.
%%
update_metadata(<<MetaInt:64>>, AndMe, OrMe) ->
	<<(MetaInt band AndMe bor OrMe):64>>.

%% FAST PATH
%%
%% Meters are processes. The call should lookup the meter process using the
%% state and exchange messages with it to check that the packet fits the bands.
%%
meter(_MeterId, _St) -> ok.

%%%%
%%%% Testing
%%%%

-define(RESTART_AFTER, 16384).

update(N) ->
	TestTabFile = "priv/test" ++ integer_to_list(N) ++ ".tab",
	{ok,Ents} = file:consult(TestTabFile),
	linc_max_generator:update_flow_table(flow0, Ents).

start() ->
	spawn(fun() ->
		{ok,P1} = net_vif:open(eth1, [binary]),
		{ok,P2} = net_vif:open(eth2, [binary]),

		io:format("plug: ~w|~w\n", [P1,P2]),
		plug(P1, P2)
	end).


plug(P1, P2) ->
	plug(P1, P2, 0).

plug(P1, P2, ?RESTART_AFTER) ->
	NewPid = spawn(fun() ->
		plug(P1, P2)
	end),

	erlang:port_connect(P1, NewPid),
	erlang:port_connect(P2, NewPid),
	unlink(P1),
	unlink(P2),

	drain_packets(NewPid);

plug(P1, P2, N) ->
	receive
	{P1,{data,Frame}} ->
		case linc_max_preparser:inject(Frame,
				undefined, {1,10,undefined}, #fast_actions{}, flow0) of
		{do,Actions} ->
			do(Frame, Actions, P1, P2);
		miss ->
			io:format("MISS1: ~p\n", [pkt:decapsulate(Frame)]);
		_ ->
			drop
		end,
		plug(P1, P2, N +1);

	{P2,{data,Frame}} ->
		case linc_max_preparser:inject(Frame,
				undefined, {2,20,undefined}, #fast_actions{}, flow0) of
		{do,Actions} ->
			do(Frame, Actions, P1, P2);
		miss ->
			io:format("MISS2: ~p\n", [pkt:decapsulate(Frame)]);
		_ ->
			drop
		end,
		plug(P1, P2, N +1)
	
	end.

do(Frame, #fast_actions{output =1}, P1, _P2) ->
	port_command(P1, Frame);

do(Frame, #fast_actions{output =2}, _P1, P2) ->
	port_command(P2, Frame);

do(Frame, #fast_actions{output =controller}, _P1, _P2) ->
	io:format("Packet-in: ~p~n", [Frame]);

do(_Frame, Actions, _P1, _P2) ->
	io:format("? ~p\n", [Actions]).

drain_packets(NewPid) ->
	receive
	X ->
		NewPid ! X,
		drain_packets(NewPid)
	after 0 ->
		ok
	end.

%%EOF
