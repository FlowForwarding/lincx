
Definitions.

D		= [0-9]
C		= [a-zA-Z_]
A		= [a-zA-Z_0-9]
WS		= ([\000-\s]|//.*)

Rules.

<<		: {token,{list_to_atom(TokenChars),TokenLine}}.

[\*(),;\{\}\[\]=]
		: {token,{list_to_atom(TokenChars),TokenLine}}.

(typedef|enum|struct|union)
		: {token,{list_to_atom(TokenChars),TokenLine}}.

{C}{A}+	: {token,{ident,list_to_atom(TokenChars),TokenLine}}.

{D}+	: {token,{int,atom_int(TokenChars),TokenLine}}.
-{D}+	: {token,{int,atom_int(TokenChars),TokenLine}}.

{WS}+	: skip_token.

#.*(\\\n.*)*	: skip_token.

/\*([^*]|(\*+([^*/])))*\*+/	: skip_token.

Erlang code.

atom_int(X) ->
	list_to_integer(X).
