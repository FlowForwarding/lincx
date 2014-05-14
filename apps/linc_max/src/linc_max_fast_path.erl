-module(linc_max_fast_path).
-export([start/2,stop/0]).
-export([describe_ports/0]).

-include_lib("linc/include/linc_logger.hrl").

-include("fast_path.hrl").

%% Restart the fast path after this many packets (to mimic suppressed GC)
-define(REIGNITE_AFTER, 16384).

-define(BLAZE_PRIORITY, high).
-define(SUPPRESS_GC, true).

start(SwitchConfig, FlowTab0) ->
	%%NB: Config contains data for the current switch only
	PortConfig = proplists:get_value(ports, SwitchConfig, []),
	QueueConfig = proplists:get_value(queues, SwitchConfig, []),

	%%NB:each queue can be attached to a single port only. Two ports can not
	%% refer to the same queue_id. Not checked.

	%% It is not possible to catch no_memory exception from inside the process.
	%% Thus, we need this.
	register(last_will, spawn(fun() ->
		process_flag(trap_exit, true),
		last_will()
	end)),

	spawn(fun() ->
		Ports = open_ports(PortConfig),
		QueueMap = start_queues(Ports, QueueConfig),
		blaze(#blaze{ports =Ports,
					 queue_map =QueueMap,
					 start_at =FlowTab0})
	end).

stop() ->
	case erlang:whereis(last_will) of
	undefined ->
		ok;
	Pid ->
		Pid ! {stop,self()},
		receive stopped -> ok end
	end,
	
	%% the blaze process stops queues and closes ports
	send_to_blaze({stop,self()}),
	receive stopped -> ok end.

describe_ports() ->
	send_to_blaze({describe_ports,self()}),
	receive {ports,Ports} -> Ports end.

last_will() ->
	receive
	{stop,From} ->
		unregister(last_will),
		From ! stopped;
	{'EXIT',_Pid,normal} -> %% reignited
		last_will();
	{'EXIT',Pid,Reason} ->
		?ERROR("blaze ~p dies: ~p\n (fast path stalled)\n", [Pid,Reason])
	end.

open_ports(PortConfig) ->
	lists:foldl(fun({port,PortNo,Opts}, Ports) ->
		IfName = proplists:get_value(interface, Opts),
		case proplists:get_value(type, Opts) of
		vif ->
			case linc_max_port_native:vif(IfName) of
			{ok,Outlet,Mac} ->
				?INFO("Open vif port ~p (~p)\n", [Outlet,Mac]),
				Port = #port_info{port_no =PortNo,
								  outlet =Outlet,
								  hw_addr =Mac,
								  rx_pkt_ref =erlang:new_counter(),
								  rx_data_ref =erlang:new_counter(),
								  tx_pkt_ref =erlang:new_counter(),
								  tx_data_ref =erlang:new_counter()},
				[Port|Ports];
			{error,Error} ->
				?ERROR("Cannot open port: ~p", [Error]),
				Ports
			end;
		Type ->
			?ERROR("Unsupported port type: ~p", [Type]),
			Ports
		end
	end, [], PortConfig).

close_ports(Ports) ->
	lists:foreach(fun(#port_info{port_no =PortNo,
								 outlet =Outlet,
								 rx_pkt_ref =RxPktRef,
								 rx_data_ref =RxDataRef,
								 tx_pkt_ref =TxPktRef,
								 tx_data_ref =TxDataRef}) ->

		erlang:release_counter(RxPktRef),
		erlang:release_counter(RxDataRef),
		erlang:release_counter(TxPktRef),
		erlang:release_counter(TxDataRef),

		net_vif:close(Outlet),
		?INFO("port ~w [~w] closed\n", [PortNo,Outlet])
	end, Ports).

start_queues(Ports, QueueConfig) ->

	%QueueConfig = [{port,2,
	%                     [{port_rate,{100,mbps}},
	%                      {port_queues,[{2,[{min_rate,20},{max_rate,50}]}]}]},
	%               {port,1,
	%                     [{port_rate,{100,mbps}},
	%                      {port_queues,[{1,[{max_rate,10}]}]}]}]

	lists:flatmap(fun({port,PortNo,Os}) ->
		Queues = proplists:get_value(port_queues, Os, []),
		#port_info{outlet =Outlet} = lists:keyfind(PortNo, #port_info.port_no, Ports),
		lists:map(fun({QueueNo,QueueOpts}) ->
			?INFO("Starting queue ~w for port ~w ~p\n", [QueueNo,PortNo,QueueOpts]),
			{ok,Pid} = linc_max_queue:start_link(Outlet, QueueOpts),
			?INFO("Queue ~w started: ~w\n", [QueueNo,Pid]),
			{QueueNo,Pid}
		end, Queues)
	end, QueueConfig).

stop_queues(QueueMap) ->
	lists:foreach(fun({_,QueuePid}) ->
		linc_max_queue:stop(QueuePid)
	end, QueueMap).

%%
%% blaze() loops hundreds thousand times per second
%%

%% FAST PATH
%%
blaze(Blaze) ->
	register(blaze, self()),

	link(whereis(last_will)),

	%% the blaze is a peculiar process
	process_flag(priority, ?BLAZE_PRIORITY),
	process_flag(suppress_gc, ?SUPPRESS_GC),

	blaze(Blaze, 0).	%% add restart counter

blaze(Blaze, ?REIGNITE_AFTER) ->
	reignite(Blaze);
blaze(#blaze{queue_map =QueueMap,ports =Ports} =Blaze, ReigniteCounter) ->
	receive
	{queue_restarting,OldPid,NewPid} ->
		{value,{Outlet,_},QueueMap1} = lists:keytake(OldPid, 2, QueueMap),
		OldPid ! queue_restarted,
		%% #blaze{} copied - happens rarely
		blaze(Blaze#blaze{queue_map =[{Outlet,NewPid}|QueueMap1]}, ReigniteCounter);

	{'EXIT',_,normal} ->
		blaze(Blaze, ReigniteCounter);	%% queue restarted normally

	{'EXIT',QueuePid,Reason} ->
		?ERROR("queue ~p exits with reason ~p\n", [QueuePid,Reason]),
		QueueMap1 = lists:keydelete(QueuePid, 2, QueueMap),
		blaze(Blaze#blaze{queue_map =QueueMap1}, ReigniteCounter);

	{Outlet,{data,Frame}} ->
		#port_info{port_no =PortNo,
				   rx_pkt_ref =RxPktRef,
				   rx_data_ref =RxDataRef} = 
						lists:keyfind(Outlet, #port_info.outlet, Ports),

		erlang:update_counter(RxPktRef),
		erlang:update_counter(RxDataRef, byte_size(Frame)),

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

		blaze(Blaze, ReigniteCounter +1);

	{stop,From} ->
		stop_queues(QueueMap),
		close_ports(Ports),

		{message_queue_len,MQLen} = process_info(self(), message_queue_len),
		?INFO("blaze process stopped: ~w message(s) lost\n", [MQLen]),

		From ! stopped;

	{describe_ports,From} ->
		From ! {ports,Ports},
		blaze(Blaze, ReigniteCounter)
	end.

reignite(#blaze{ports =Ports} =Blaze) ->
	unregister(blaze),

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

send_to_blaze(Msg) ->
	try
		blaze ! Msg
	catch _:_ ->
		%% happens rarely; blaze is restarting, between unregister/register
		receive after 0 -> ok end,	%% yield
		send_to_blaze(Msg)
	end.

%%EOF
