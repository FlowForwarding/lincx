-module(linc_max_fast_path).
-export([start/2]).

-include_lib("linc/include/linc_logger.hrl").

-include("fast_path.hrl").

%% Restart the fast path after this many packets (to mimic suppressed GC)
-define(REIGNITE_AFTER, 16384).

-define(BLAZE_PRIORITY, high).
-define(SUPPRESS_GC, true).

start(SwitchConfig, FlowTab0) ->
	%%NB: Config contains data for the current switch only
	PortConfig = proplists:get_value(ports, SwitchConfig, []),

	%% It is not possible to catch no_memory exception from inside the process.
	%% Thus, we need this.
	register(last_will, spawn(fun() ->
		process_flag(trap_exit, true),
		last_will()
	end)),

	spawn(fun() ->
		Ports = open_ports(PortConfig),
		blaze(#blaze{ports =Ports,start_at =FlowTab0})
	end).

last_will() ->
	receive
	{'EXIT',_Pid,normal} -> %% reignited
		last_will();
	{'EXIT',Pid,Reason} ->
		?ERROR("blaze ~p dies: ~p\n (fast path stopped)\n", [Pid,Reason])
	end.

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
	link(whereis(last_will)),

	%% the blaze is a peculiar process
	process_flag(priority, ?BLAZE_PRIORITY),
	process_flag(suppress_gc, ?SUPPRESS_GC),

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
		malformed ->
			%% preparser detected a malformed packet
			%%io:format("MALFORMED: ~p\n", [pkt:decapsulate(Frame)]);
			drop;
		_ ->
			drop
		end,

		blaze(Blaze, ReigniteCounter +1)
	end.

reignite(#blaze{ports =Ports} =Blaze) ->
%	%% Check that GC never happens
%	case process_info(self(), garbage_collection) of
%	{_,0} ->
%		ok;
%	{_,N} ->
%		?INFO("blaze ~w ran gc ~w time(s)\n", [self(),N])
%	end,

	NewPid = spawn(fun() ->
		blaze(Blaze)
	end),

	reconnect_ports(Ports, NewPid),
	drain_packets(NewPid).

reconnect_ports(Ports, NewPid) ->
	lists:foreach(fun({_,Outlet,_}) ->
		erlang:port_connect(Outlet, NewPid),
		unlink(Outlet)
	end, Ports).

drain_packets(NewPid) ->
	drain_packets(NewPid, 0).

drain_packets(NewPid, N) ->
	receive
	Any ->
		NewPid ! Any,
		drain_packets(NewPid, N +1)
	after 0 ->
		case whereis(blaze_statistics) of
		undefined ->
			ok;
		Pid ->
			Pid ! {message_count,N},
			ok
		end
	end.

%%EOF
