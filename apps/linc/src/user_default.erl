-module(user_default).
-export([vif/0]).

vif() ->
	{ok,Ifs} = inet:getifaddrs(),
	lists:foreach(fun({Name,Props}) ->
						Mac = proplists:get_value(hwaddr, Props, [0,0,0,0,0,0]),
						io:format("~-8s~s\n", [Name,hex(Mac)]) end, Ifs).

hex(Mac) ->
	string:join([ io_lib:format("~2.16.0b", [X]) || X <- Mac ], ":").

