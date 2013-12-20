-module(m1).
-compile(export_all).

foo() ->
	spawn(fun() ->
		receive
			X -> erlang:display(X), foo()
		end
	end).

%%EOF
