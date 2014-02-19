%%
%%
%%

%% @author Cloduozer LLP. <info@cloudozer.com>
%% @copyright 2012 FlowForwarding.org
-module(linc_max_fast_actions).
-export([apply_set/3]).

-include("fast_path.hrl").

%% FAST PATH
%%
apply_set(#fast_actions{output =PortNo}, Frame, Ports) when is_integer(PortNo) ->
	{_,Outlet,_} = lists:keyfind(PortNo, 1, Ports),
	erlang:port_command(Outlet, Frame);

apply_set(#fast_actions{output =controller}, Frame, Ports) ->
	io:format("Packet-in: ~p~n", [Frame]);

apply_set(Actions, _Frame, _Ports) ->
	io:format("? ~p\n", [Actions]).

%%EOF
