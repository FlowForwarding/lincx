%%
%% See queues_in_lincx.pdf
%%

%% @author Cloduozer LLP. <info@cloudozer.com>
%% @copyright 2014 FlowForwarding.org
-module(linc_max_queue).

-define(MIN_R_MAX, 0.05).	%% 48kbps

-define(WINDOW, 0.250).		%% 250ms

start(DataPort, Opts) when is_port(DataPort), is_list(Opts) ->	%% {ok,Pid}
	case check_options(Opts) of
	{ok,Rmin,Rmax} when Rmin > Rmax ->
		{error,badarg};
	{ok,Rmin,Rmax} ->
		loop(DataPort, Rmin, Rmax);
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

loop(DataPort, Rmin, Rmax) ->
	todo.

%	{_,_,TS} = now(),
%	loop(DataPort, Rmin, Rmax, 0.0, TS).
%
%loop(DataPort, Rmin, Rmax, 
