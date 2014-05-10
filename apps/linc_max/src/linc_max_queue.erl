%%
%% See queues_in_lincx.pdf. The code uses variable names from the paper.
%%

%% @author Cloduozer LLP. <info@cloudozer.com>
%% @copyright 2014 FlowForwarding.org
-module(linc_max_queue).
-export([start_link/2,stop/1]).

-include_lib("linc/include/linc_logger.hrl").

-define(MIN_R_MAX, 0.05).				%% 48kbps
-define(MIN_TIMEOUT, 5).				%% 5ms
-define(MAX_BUFFERED, 8 *1024 *1024).	%% 8MB

-define(WINDOW, 250.0).		%% 250ms

%% Restart the queue after this many packets
-define(RESTART_AFTER, 16384).

start_link(Outlet, Opts) when is_port(Outlet), is_list(Opts) ->	%% {ok,Pid}
	case check_options(Opts) of
	{ok,Rmin,Rmax} when Rmin =/= undefined, Rmax =/= undefined, Rmin > Rmax ->
		{error,badarg};
	{ok,Rmin,Rmax} ->
		Pid = spawn_link(fun() ->
			loop(Outlet, Rmin, Rmax)
		end),
		{ok,Pid};
	Error ->
		{error,Error}
	end;
start_link(_, _) ->
	{error,badarg}.

stop(QueuePid) ->
	QueuePid ! {stop,self()},
	receive
	stopped ->
		ok
	after 5000 ->
		{error,timout}
	end.

check_options(Opts) ->
	check_options(Opts, undefined, undefined).

check_options([], Rmin, Rmax) ->
	{ok,Rmin,Rmax};
check_options([{min_rate,R}|Opts], _, Rmax)
		when is_number(R), R >= 0 ->
	check_options(Opts, float(R), Rmax);
check_options([{max_rate,R}|Opts], Rmin, _)
		when is_number(R), R >= ?MIN_R_MAX ->
	check_options(Opts, Rmin, float(R));
check_options(_, _, _) ->
	badarg.

%% FAST PATH
%%
loop(Outlet, Rmin, Rmax) ->
	loop(Outlet, Rmin, Rmax, 0.0, now(), queue:new(), 0, 0, undefined).

loop(Outlet, Rmin, Rmax, R, T, Q, B, ?RESTART_AFTER, TRef) ->
	stop_timer(TRef),
	NewPid = spawn(fun() ->
		loop(Outlet, Rmin, Rmax, R, T, Q, B, 0, undefined)
	end),
	linc_max_fast_path:send_to_blaze({queue_restarting,self(),NewPid}),
	receive queue_restarted -> ok end,
	drain_mailbox(NewPid);

loop(Outlet, Rmin, Rmax, R, T, Q, B, N, TRef) ->
	receive
	{stop,From} ->
		stop_timer(TRef),
		if B > 0 ->
			?INFO("Queue ~w stopped (~w buffered byte(s) lost)\n", [self(),B]);
		true ->
			?INFO("Queue ~w stopped\n", [self()])
		end,
		From ! stopped;

	timeout ->
		new_rate(Outlet, Rmin, Rmax, R, T, Q, B, N);

	Frame when is_binary(Frame) ->
		stop_timer(TRef),
		if B > ?MAX_BUFFERED ->
			%% drop
			new_rate(Outlet, Rmin, Rmax, R, T, Q, B, N +1);
		true ->
			new_rate(Outlet, Rmin, Rmax, R, T, queue:in(Frame, Q), B +size(Frame), N)
		end
	end.

%% FAST PATH
%%
new_rate(Outlet, Rmin, Rmax, R, T, Q, B, N) ->
	T1 = now(),
	R1 = case delta(T1, T) of
	Delta when Delta > ?WINDOW ->
		0.0;
	Delta ->
		R - R *Delta /?WINDOW
	end,
	case queue:is_empty(Q) of
	true ->
		loop(Outlet, Rmin, Rmax, R1, T1, Q, B, N, undefined);
	false ->
		limit_rate(Outlet, Rmin, Rmax, R1, T1, Q, B, N)
	end.

%% FAST PATH
%%
limit_rate(Outlet, Rmin, Rmax, R, T, Q, B, N) ->
	Frame = queue:get(Q),
	P = size(Frame),

	R1 = R + P /?WINDOW /125,	%% P - bytes, ?WINDOW - ms, 125 = 8 *1000 /1000000 
	R2 = R + B /?WINDOW /125,	%% B - bytes

	%?INFO("R1 = ~w; R2 = ~w\n", [R1,R2]),

	if (Rmax =:= undefined orelse R1 =< Rmax) andalso
	   (Rmin =:= undefined orelse R2 >= Rmin) ->

		erlang:port_command(Outlet, Frame),
		new_rate(Outlet, Rmin, Rmax, R1, T, queue:drop(Q), B -P, N +1);
	
	Rmax =/= undefined andalso R1 > Rmax ->
		%% R > 0 due to MIN_R_MAX check
		Wait = round(?WINDOW - ?WINDOW *Rmax /R + P /R /125),	%% P - bytes, R - Mbps

		%% Wait is likely under 1ms
		TRef = if Wait < ?MIN_TIMEOUT ->
			erlang:send_after(?MIN_TIMEOUT, self(), timeout);
		true ->
			erlang:send_after(Wait, self(), timeout)
		end,

		loop(Outlet, Rmin, Rmax, R, T, Q, B, N, TRef);

	true ->
		%% R2 < Rmin - wait forever
		loop(Outlet, Rmin, Rmax, R, T, Q, B, N, undefined)
	end.

%% FAST PATH
%%
delta({M,S,U1}, {M,S,U}) when U1 > U ->
	(U1 -U) /1000;
delta({M,S1,U1}, {M,S,U}) when S1 > S ->
	(S1 -S) *1000.0 + (U1 -U) /1000;
delta({M1,S1,U1}, {M,S,U}) when M1 > M ->
%% happens once a week
	(M1 -M) *1.0e9 + (S1 -S) *1000.0 + (U1 -U) /1000.

drain_mailbox(NewPid) ->
	receive
	Any ->
		NewPid ! Any,
		drain_mailbox(NewPid)
	after 0 ->
		ok
	end.

stop_timer(undefined) ->
	ok;
stop_timer(TRef) ->
	erlang:cancel_timer(TRef).

%%EOF
