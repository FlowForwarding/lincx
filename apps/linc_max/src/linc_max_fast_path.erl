-module(linc_max_fast_path).
-export([start/2]).

-include_lib("linc/include/linc_logger.hrl").

-include("fast_path.hrl").

%% Restart the fast path after this many packets to avoid GC
-define(REIGNITE_AFTER, 16384).

start(SwitchConfig, FlowTab0) ->
	%%NB: Config contains data for the current switch only
	PortConfig = proplists:get_value(ports, SwitchConfig, []),

	spawn(fun() ->
		try
			Ports = open_ports(PortConfig),
			blaze(#blaze{ports =Ports,start_at =FlowTab0})
		catch _:Error ->
			?ERROR("blaze dies: ~p\n~p", [Error,erlang:get_stacktrace()])
		end
	end).

open_ports(PortConfig) ->
	lists:foldl(fun({port,PortNo,Opts}, Ports) ->
		IfName = proplists:get_value(interface, Opts),
		case proplists:get_value(type, Opts) of
		vif ->
			case linc_max_port_native:vif(IfName) of
			{ok,Outlet,Mac} ->
				?INFO("Open vif port ~p (~p)\n", [Outlet,Mac]),
				[{PortNo,Outlet,[{mac,Mac}|Opts]}|Ports];
			{error,Error} ->
				?ERROR("Cannot open port: ~p", [Error]),
				Ports
			end;
		Type ->
			?ERROR("Unsupported port type: ~p", [Type]),
			Ports
		end
	end, [], PortConfig).

%%
%% blaze() loops hundreds thousand times per second
%%

blaze(Blaze) ->
	blaze(Blaze, 0).	%% add restart counter

blaze(Blaze, ?REIGNITE_AFTER) ->
	reignite(Blaze);
blaze(Blaze, ReigniteCounter) ->
	receive
	{Outlet,{data,Frame}} ->
		{PortNo,_,_} = lists:keyfind(Outlet, 2, Blaze#blaze.ports),

		Metadata = <<0:64>>,
		%% in_phy_port and tunnel_id are undefined
		PortInfo = {PortNo,undefined,undefined},

		%% Inject the frame into the pipeline
		case linc_max_preparser:inject(Frame,
				Metadata, PortInfo, #fast_actions{}, Blaze) of
		{do,Frame1,Actions} ->
			linc_max_fast_actions:apply_set(Actions, Frame1, Blaze);
		miss ->
			%%io:format("MISS: ~p\n", [pkt:decapsulate(Frame)]);

			%%TODO: send Packet-in message?
			drop;
		_ ->
			drop
		end,

		blaze(Blaze, ReigniteCounter +1)
	end.

reignite(#blaze{ports =Ports} =Blaze) ->
	NewPid = spawn(fun() ->
		try
			blaze(Blaze)
		catch _:Error ->
			?ERROR("blaze extinguishes: ~p\n~p\n", [Error,erlang:get_stacktrace()]),
			reignite(Blaze)
		end
	end),

	reconnect_ports(Ports, NewPid),
	drain_packets(NewPid).

reconnect_ports(Ports, NewPid) ->
	lists:foreach(fun({_,Outlet,_}) ->
		erlang:port_connect(Outlet, NewPid),
		unlink(Outlet)
	end, Ports).

drain_packets(NewPid) ->
	receive
	Any ->
		NewPid ! Any,
		drain_packets(NewPid)
	after 0 ->
		ok
	end.

%%EOF
