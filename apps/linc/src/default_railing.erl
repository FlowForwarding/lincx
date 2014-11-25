-module(default_railing).
-export([railing/0]).

railing() ->
	ErlCfg = "lincx.config",
	YmlCfg = "lincx.yml",
	try
		case filelib:is_file(YmlCfg) of
			true ->
				io:format("Parse: ~s\n", [YmlCfg]),
				application:start(yamerl),
				read_yml(yamerl_constr:file(YmlCfg));
			_ ->
				case filelib:is_file(ErlCfg) of
					true ->
						io:format("Parse: ~s\n", [ErlCfg]),
						case file:consult(ErlCfg) of
							{ok, Terms} ->
								read_erl(Terms);
							_ ->
								io:format("can't parse '~s'\n", [ErlCfg])
						end;
					_ ->
						io:format("can't find ~s\n", [ErlCfg]),
						halt(1)
				end
		end
	catch
		throw:{yamerl_exception, ErrList} ->
			lists:foreach(
				fun({_, _, Text, Line, Col, _, _, _}) ->
					io:format("~s:~p:~p: ~s\n", [YmlCfg, Line, Col, Text])
				end,
				ErrList
			),
			halt(1);
		throw:Text ->
			io:format("~s:~s\n", [YmlCfg, lists:flatten(Text)]),
			halt(1)
	end.

read_yml([Opts]) ->
	Ports = ports(proplists:get_value("ports", Opts),[]),
	Controllers = controllers(proplists:get_value("controllers", Opts),[]),
	Listen = listen(proplists:get_value("listen", Opts)),
	Memory = memory(proplists:get_value("memory", Opts)),
	%PortIds = lists:seq(1, length(Ports)),
	Ipconf = ipconf(proplists:get_value("ipconf", Opts)),

	Name = proplists:get_value("name", Opts),
	Domain = proplists:get_value("domain", Opts),

	conf(Name, Domain, Ports, [], Controllers, Ipconf, "0.0.0.0", Listen, Memory);
read_yml(_) ->
	throw("Invalid number of documents").

memory(undefined) ->
	1024;
memory(Memory) when is_integer(Memory) andalso Memory > 0 ->
	Memory;
memory(Memory) ->
	throw(io_lib:format("memory: invalid value ~p", [Memory])).

listen(undefined) ->
	6653;
listen(Port) when is_integer(Port) andalso Port > 0 andalso Port < 65536 ->
	Port;
listen(Port) ->
	throw(io_lib:format("listen: invalid value ~p", [Port])).

controllers([], Controlers) ->
	lists:reverse(Controlers);
controllers([C | Rest], Controllers) when is_list(C) ->
	try
		[Addr, PortString] = string:tokens(C, ":"),
		{ok, _} = inet_parse:address(Addr),
		PortInteger = list_to_integer(PortString),
		true = PortInteger > 0 andalso PortInteger < 65536,
		controllers(Rest, [{Addr, PortInteger} | Controllers])
	catch _:_ ->
		throw(io_lib:format("controllers: invalid value ~p", [C]))
	end;
controllers(_, _) ->
	throw("controllers: malformed").

ports([], Ports) ->
	lists:reverse(Ports);
ports([P | Rest], Ports) when is_list(P) ->
	ports(Rest, [P | Ports]);
ports(_, _) ->
	throw("ports: malformed").

ipconf("dhcp") ->
	" -dhcp";
ipconf(undefined) ->
	"";
ipconf(Opts) when is_list(Opts) ->
	Arg =
		fun(Opt) ->
			case proplists:get_value(Opt, Opts) of
				undefined ->
					throw("ipconf:" ++ Opt ++ ": not found");
				Val ->
					case inet_parse:address(Val) of
						{ok, _} ->
							" -" ++ Opt ++ " " ++ Val;
						_ ->
							throw("ipconf:" ++ Opt ++ io_lib:format(": invalid value: ~p", [Val]))
					end
			end
		end,

	Arg("ipaddr") ++ Arg("netmask") ++ Arg("gateway");
ipconf(_) ->
	throw("ipconf: malformed").

read_erl(Conf) ->
	Queues =
		lists:foldl(
			fun
				({queue, Id, Min, Max}, Acc) ->
					Acc ++ [{Id, Min, Max}];
				(_, Acc) ->
					Acc
			end,
			[],
			Conf
		),

	PortsConf =
		lists:foldl(
			fun
				({ports, N}, Acc) ->
					Acc ++ [{port, S} || S <- lists:seq(1, N)];
				({ports, N, Aux}, Acc) ->
					Start = proplists:get_value(start, Aux, 1),
					Prefix = proplists:get_value(prefix, Aux, br),
					Acc ++ [{port, S + Start, [{bridge, Prefix}]} || S <- lists:seq(1, N)];
				(_, Acc) ->
					Acc
			end,
			[],
			Conf
		),

	Ports =
		lists:foldl(
			fun
				({port, PortNo}, Ports) ->
					[{PortNo, bridge(PortNo), []} | Ports];
				({port, PortNo, PortOps}, Ports) ->
					Bridge =
						case proplists:get_value(bridge, PortOps) of
							undefined ->
								bridge(PortNo);
							B ->
								bridge(B)
						end,

					Queue =
						case proplists:get_value(queue, PortOps) of
							undefined ->
								[];
							Q ->
								[Q]
						end,

					[{PortNo, Bridge, Queue} | Ports];
				(_, Ports) ->
					Ports
			end,
			[],
			Conf ++ PortsConf
		),

	Controllers = [{addr(Addr), Port} || {controler, Addr, Port} <- Conf],

	{ListenerIp, ListenerPort} =
		lists:foldl(
			fun
				({listener, Port}, _) ->
					{"0.0.0.0", Port};
				({listener, Addr, Port}, _) ->
					{addr(Addr), Port};
				(_, Res) ->
					Res
			end,
			{"0.0.0.0", 6653},
			Conf
		),

	Ipconf =
		lists:foldl(
			fun
				({ipconf, dhcp}, _) ->
					" -dhcp";
				({ipconf,IPAddr,NetMask,Gateway}, _) ->
					" -ipaddr " ++ addr(IPAddr) ++ " -netmask " ++ NetMask ++ " -gateway " ++ Gateway;
				(_, Res) ->
					Res
			end,
			"",
			Conf
		),

	Memory =
		lists:foldl(
			fun
				({memory, M}, _) ->
					M;
				(_, Res) ->
					Res
			end,
			1024,
			Conf
		),

	Name = undefined,
	Domain = undefined,

	conf(Name, Domain, Ports, Queues, Controllers, Ipconf, ListenerIp, ListenerPort, Memory).

argumentize(App, Key, Val) ->
	io_lib:format(" -~p ~p '~s'", [App, Key, io_lib:write(Val)]).

bridge(B) when is_atom(B) ->
	atom_to_list(B);
bridge(N) when is_integer(N) ->
	"br" ++ integer_to_list(N);
bridge(B) ->
	B.

addr({A,B,C,D}) ->
	lists:flatten(io_lib:format("~p.~p.~p.~p",[A,B,C,D]));
addr(Addr) ->
	Addr.

conf(Name, Domain, Ports, Queues, Controllers, Ipconf, ListenerIp, ListenerPort, Memory) ->
	[{vif, 'bridge=xenbr0'}] ++
	[{vif, list_to_atom("bridge=" ++ B)} || {_,B,_} <- Ports] ++
	[
		{name, Name},
		{domain, Domain},
		{extra, " -config " ++ "/lincx/priv/sys.config"},
		{extra,
			" -eval \\\"lists:map(fun application:start/1, ["
				"crypto,asn1,public_key,ssh,compiler,syntax_tools,xmerl,mnesia,lager,linc"
			"])\\\""
		},
		{extra, Ipconf},
		{extra, argumentize(
			linc, capable_switch_ports,
			[{port,Id,[{interface,"eth" ++ integer_to_list(Id)},{type,vif}]} || {Id,_,_} <- Ports]
		)},
		{extra, argumentize(
			linc, capable_switch_queues,
			[{queue, Id, [{min_rate, Min}, {max_rate, Max}]} || {Id, Min, Max} <- Queues]
		)},
		{extra, argumentize(
			linc, logical_switches,
			[{switch,0,[
				{backend,linc_max},
				{controllers,
					[{Addr, Addr, Port, tcp} || {Addr, Port} <- Controllers]
				},
				{controllers_listener, {ListenerIp, ListenerPort, tcp}},
				{queues_status, disabled},
				{ports, [{port,Id,{queues,Queue}} || {Id,_,Queue} <- Ports]}
			]}]
		)},
		{memory, Memory},
		{lib, eunit},
		{lib, tools},
		{lib, public_key},
		{lib, asn1},
		{lib, crypto},
		{lib, ssh},
		{lib, xmerl},
		{lib, mnesia},
		{lib, syntax_tools},
		{lib, compiler},
		{include, "priv"},
		{exclude, "deps/yamerl/ebin"},
		{exclude, "apps/linc/ebin/lincx_railing.beam"}
	].
