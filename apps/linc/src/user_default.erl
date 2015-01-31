-module(user_default).
-export([vif/0]).

vif() ->
	{ok,Ifs} = inet:getifaddrs(),
	io:format("Name     Mac               Address         Netmask\n"),
			%% 01234567 00:00:00:00:00:00 192.192.192.192 255.255.255.255
	lists:foreach(fun({Name,Props}) ->
						Mac	    = proplists:get_value(hwaddr, Props, [0,0,0,0,0,0]),
						IpAddr  = proplists:get_value(addr, Props),
						Netmask = proplists:get_value(netmask, Props),
						io:format("~-8s ~s ~-15s ~-15s\n",
									[Name,hex(Mac),ip(IpAddr),ip(Netmask)]) end, Ifs).

hex(Mac) ->
	string:join([ io_lib:format("~2.16.0b", [X]) || X <- Mac ], ":").

ip(undefined) -> "";
ip({A,B,C,D}) -> io_lib:format("~w.~w.~w.~w", [A,B,C,D]);
ip(_)		  -> "". %% IPv6?

