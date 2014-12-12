-module(default_railing).
-export([railing/0]).

railing() ->
	ErlCfg = "lincx.config",
	YmlCfg = "lincx.yml",
	try
		case filelib:is_file(YmlCfg) of
			true ->
				yml(YmlCfg);
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

yml(Cfg) ->
	io:format("Parse: ~s\n", [Cfg]),
	application:start(yamerl),
	Opts =
		case yamerl_constr:file(Cfg) of
			[Doc] ->
				Doc;
			_ ->
				throw("invalid number of docs")
		end,

	%io:format("~p\n", [Opts]),
	%%[check_opt(O) || O <- Opts],

	Queues =
		lists:map(
			fun(Fields) ->
				{
					proplists:get_value("id", Fields),
					proplists:get_value("min", Fields),
					proplists:get_value("max", Fields)
				}
			end,
			proplists:get_value("queues", Opts, [])
		),

	Ports =
		lists:map(
			fun(Fields) ->
				{
					proplists:get_value("id", Fields),
					proplists:get_value("bridge", Fields),
					proplists:get_value("queue", Fields, [])
				}
			end,
			proplists:get_value("ports", Opts, [])
		),

	Controllers =
		lists:map(
			fun(C) ->
				case string:tokens(C, ":") of
					[Ip, Port] ->
						{Ip, list_to_integer(Port)};
					[Ip] ->
						{Ip, 6653}
				end
			end,
			proplists:get_value("controllers", Opts, [])
		),

	Ipconf =
		case proplists:get_value("ipconf", Opts) of
			undefined ->
				"";
			"dhcp" ->
				"-dhcp";
			Fields ->
				"-ipaddr " ++ proplists:get_value("ipaddr", Fields) ++
				" -netmask " ++ proplists:get_value("netmask", Fields) ++
				" -gateway " ++ proplists:get_value("gateway", Fields)
		end,

	{ListenIp, ListenPort} =
		case proplists:get_value("listen", Opts) of
			undefined ->
				{"0.0.0.0", 6653};
			IpPort ->
				case string:tokens(IpPort, ":") of
					[Ip, Port] ->
						{Ip, list_to_integer(Port)};
					[Ip] ->
						{Ip, 6653}
				end
		end,

	Memory =
		case proplists:get_value("memory", Opts) of
			undefined ->
				1024;
			M ->
				M
		end,

	NineP =
		lists:foldl(
			fun(Mount, Acc) ->
				Acc ++ " -9p " ++ Mount
			end,
			"",
			proplists:get_value("9p", Opts, [])
		),

	Secret =
		case proplists:get_value("secret", Opts) of
			undefined ->
				"";
			S ->
				"-secret " ++ S
		end,

	conf(Ports, Queues, Controllers, Ipconf, ListenIp, ListenPort, Memory, NineP ++ " " ++ Secret).

check_opt({"ipconf", Ipconf}) when is_list(Ipconf) ->
	lists:foreach(
		fun
			({"ipaddr", Ipaddr}) ->
				check_ip(Ipaddr);
			({"netmask", Netmask}) ->
				check_ip(Netmask);
			({"gateway", Gateway}) ->
				check_ip(Gateway);
			(Unknown) ->
				throw(io_lib:format("unknown ipconf field: ~p", [Unknown]))
		end,
		Ipconf
	);
check_opt({"queues", Queues}) when is_list(Queues) ->
	[check_queue(Q) || Q <- Queues];
check_opt({"ports", Ports}) when is_list(Ports) ->
	[check_port(P) || P <- Ports];
check_opt({"controllers", Controllers}) when is_list(Controllers) ->
	[check_controller(C) || C <- Controllers];
check_opt({"listen", Listen}) when is_integer(Listen) ->
	ok;
check_opt({"memory", Memory}) when is_integer(Memory) ->
	ok;
check_opt(Unknown) ->
	throw(io_lib:format("unknown option: ~p", [Unknown])).

check_controller(Controller) when is_list(Controller) ->
	lists:foreach(
		fun
			({"ip", Ip}) ->
				check_ip(Ip);
			({"port", Port}) when is_integer(Port) ->
				ok;
			(Unknown) ->
				throw(io_lib:format("unknown controller field: ~p", [Unknown]))
		end,
		Controller
	);
check_controller(Unknown) ->
	throw(io_lib:format("invalid controller: ~p", [Unknown])).

check_port(Port) when is_list(Port) ->
	lists:foreach(
		fun
			({"id", Id}) when is_integer(Id) ->
				ok;
			({"bridge", Bridge}) when is_list(Bridge) ->
				ok;
			({"queue", Queue}) when is_integer(Queue) ->
				ok;
			(Unknown) ->
				throw(io_lib:format("unknown port field: ~p", [Unknown]))
		end,
		Port
	);
check_port(Unknown) ->
	throw(io_lib:format("invalid port: ~p", [Unknown])).

check_queue(Queue) when is_list(Queue) ->
	lists:foreach(
		fun
			({"id", Id}) when is_integer(Id) ->
				ok;
			({"min", Min}) when is_integer(Min) ->
				ok;
			({"max", Max}) when is_integer(Max) ->
				ok;
			(Unknown) ->
				throw(io_lib:format("unknown queue field: ~p", [Unknown]))
		end,
		Queue
	);
check_queue(Unknown) ->
	throw(io_lib:format("invalid queue: ~p", [Unknown])).

check_ip(Ip) ->
	try
		[A,B,C,D] = string:tokens(Ip, "."),
		Octet =
			fun(O) ->
				I = list_to_integer(O),
				true = I >= 0 andalso I =< 255
			end,
		Octet(A),Octet(B),Octet(C),Octet(D)
	catch _:_ ->
		throw(io_lib:format("invalid IP: ~p", [Ip]))
	end.

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

					Ports ++ [{PortNo, Bridge, Queue}];
				(_, Ports) ->
					Ports
			end,
			[],
			Conf ++ PortsConf
		),

	Controllers = [{addr(Addr), Port} || {controller, Addr, Port} <- Conf],

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
					"-dhcp";
				({ipconf,IPAddr,NetMask,Gateway}, _) ->
					"-ipaddr " ++ addr(IPAddr) ++ " -netmask " ++ NetMask ++ " -gateway " ++ Gateway;
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

	Mount =
		lists:foldl(
			fun
				({mount, IP, Linux, Ling, Secret}, _) ->
					"-9p " ++ addr(IP) ++ " " ++ Linux ++ " " ++ Ling ++ " -secret " ++ Secret;
				(_, Res) ->
					Res
			end,
			"",
			Conf
		),

	conf(Ports, Queues, Controllers, Ipconf, ListenerIp, ListenerPort, Memory, Mount).

argumentize(App, Key, Val) ->
	%% this weird transform required to properly escape string values
	EscapedVal =
		string:strip(
			lists:flatten(io_lib:format("~p", [lists:flatten(io_lib:print(Val,1,1024,-1))])),
			both, $"
		),
	io_lib:format("-~p ~p '~s'", [App, Key, EscapedVal]).

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

conf(Ports, Queues, Controllers, Ipconf, ListenerIp, ListenerPort, Memory, Mount) ->
	[{vif, 'bridge=xenbr0'}] ++
	[{vif, list_to_atom("bridge=" ++ B)} || {_,B,_} <- Ports] ++
	[
		{extra, Ipconf},
		{extra, Mount},
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
		{extra,
			"-eval \\\"lists:map(fun application:start/1, ["
				"crypto,asn1,public_key,ssh,compiler,syntax_tools,xmerl,mnesia,lager,linc"
			"])\\\""
		},
		{extra, "-config " ++ "/lincx/priv/sys.config"},
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
		{exclude, "apps/linc/ebin/default_railing.beam"}
	].
