-module(m1).
-compile(export_all).

foo(X) when X =:= 135; X =:= 136 -> ok.

%%EOF
