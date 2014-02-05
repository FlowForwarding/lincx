-module(m1).
-compile(export_all).

foo() ->
	begin
		X = 2,
		2*X
	end.

%%EOF
