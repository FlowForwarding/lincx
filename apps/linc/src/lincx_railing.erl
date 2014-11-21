-module(lincx_railing).
-export([rail/1]).

rail(Conf) ->
	Ports = [P || {port, P} <- Conf],
	PortNums = lists:seq(1, length(Ports)),

	[
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
		{app, [
			{linc, capable_switch_ports,
				[{port,P,[{interface,"eth" ++ integer_to_list(P)},{type,vif}]} || P <- PortNums]
			},
			{linc, logical_switches, [{switch,0,[
				{backend,linc_max},
				{controllers,
					[{Addr, Addr, Port, tcp} || {controler, Addr, Port} <- Conf]
				},
				{controllers_listener, {"0.0.0.0", proplists:get_value(listener, Conf, 6653), tcp}},
				{queues_status, disabled},
				{ports, [{port,P,{queues,[]}} || P <- PortNums]}
			]}]}
		]},
		{config, "/lincx/priv/sys.config"},
		{eval, "lists:map(fun application:start/1, [crypto,asn1,public_key,ssh,compiler,syntax_tools,xmerl,mnesia,lager,linc])"},
		{vif, [xenbr0 | [P || {port, P} <- Conf]]}
	].
