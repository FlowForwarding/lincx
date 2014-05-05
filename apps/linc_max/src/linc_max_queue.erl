%%
%% See queues_in_lincx.pdf. The code uses variable names from the paper.
%%

%% @author Cloduozer LLP. <info@cloudozer.com>
%% @copyright 2014 FlowForwarding.org
-module(linc_max_queue).
-export([start/2]).

-define(MIN_R_MAX, 0.05).	%% 48kbps

-define(WINDOW, 250.0).		%% 250ms

%% restart the queue after this many packets
-define(RESTART_AFTER, 16384).

start(Outlet, Opts) when is_port(Outlet), is_list(Opts) ->	%% {ok,Pid}
	case check_options(Opts) of
	{ok,Rmin,Rmax} when Rmin > Rmax ->
		{error,badarg};
	{ok,Rmin,Rmax} ->
		Pid = spawn(fun() ->
			loop(Outlet, Rmin, Rmax)
		end),
		{ok,Pid};
	Error ->
		{error,Error}
	end;
start(_, _) ->
	{error,badarg}.

check_options(Opts) ->
	check_options(Opts, undefined, undefined).

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
	if TRef =/= undefined -> erlang:cancel_timer(TRef);
			true -> ok end,
	NewPid = spawn(fun() ->
		loop(Outlet, Rmin, Rmax, R, T, Q, B, 0, undefined)
	end),
	send_to_blaze({queue_restarting,self(),NewPid}),
	receive queue_restarted -> ok end,
	drain_mailbox(NewPid);

loop(Outlet, Rmin, Rmax, R, T, Q, B, N, TRef) ->
	receive
	timeout ->
		new_rate(Outlet, Rmin, Rmax, R, T, Q, B, N);

	Frame when is_binary(Frame) ->
		if TRef =/= undefined -> erlang:cancel_timer(TRef);
				true -> ok end,
		new_rate(Outlet, Rmin, Rmax, R, T, queue:in(Q, Frame), B +size(Frame), N)
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
	limit_rate(Outlet, Rmin, Rmax, R1, T1, Q, B, N).

%% FAST PATH
%%
limit_rate(Outlet, Rmin, Rmax, R, T, Q, B, N) ->
	case queue:is_empty(Q) of
	true ->
		loop(Outlet, Rmin, Rmax, R, T, Q, B, N, undefined);
	false ->
		Frame = queue:get(Q),
		P = size(Frame),

		R1 = R + P /?WINDOW /125,	%% P - bytes, ?WINDOW - ms, 125 = 8 *1000 /1000000 
		R2 = R + B /?WINDOW /125,	%% B - bytes

		if (Rmax =:= undefined orelse R1 =< Rmax) andalso
	       (Rmin =:= undefined orelse R2 >= Rmin) ->

			erlang:port_command(Outlet, Frame),
			limit_rate(Outlet, Rmin, Rmax, R1, T, queue:drop(Q), B -P, N +1);
		
		Rmax =/= undefined andalso R1 > Rmax ->
			%% R > 0 due to MIN_R_MAX check
			Wait = ?WINDOW - ?WINDOW *Rmax /R + P /R /125,	%% P - bytes, R - Mbps
			TRef = erlang:send_after(Wait, self(), timeout),
			loop(Outlet, Rmin, Rmax, R1, T, Q, B, N, TRef);

		true ->
			%% R2 < Rmin - wait forever
			loop(Outlet, Rmin, Rmax, R1, T, Q, B, N, undefined)
		end
	end.

%% FAST PATH
%%
delta({M,S,U1}, {M,S,U}) when U1 > U ->
	(U1 -U) /1000;
delta({M,S1,U1}, {M,S,U}) when S1 > S ->
	(S1 -S) *1000.0 + (U1 -U) /1000;
delta({M1,S1,U1}, {M,S,U}) when M1 > M ->
%% happens once a week
	(M1 -S) *1.0e9 + (S1 -S) *1000.0 + (U1 -U) /1000.

send_to_blaze(Msg) ->
	try
		blaze ! Msg
	catch _:badarg ->
		%% The blaze process restarting. The new process will be registered
		%% soon. Keep trying.
		send_to_blaze(Msg)
	end.

drain_mailbox(NewPid) ->
	receive
	Any ->
		NewPid ! Any,
		drain_mailbox(NewPid)
	after 0 ->
		ok
	end.

%%EOF
