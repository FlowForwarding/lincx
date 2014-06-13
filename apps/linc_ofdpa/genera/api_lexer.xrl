
Definitions.

D		= [0-9]
C		= [a-zA-Z_]
A		= [a-zA-Z_0-9]
WS		= ([\000-\s]|//.*)

Rules.

[\*(),;\[\]]
		: {token,{list_to_atom(TokenChars),TokenLine}}.

(void|struct|in|out|inout)
		: {token,{list_to_atom(TokenChars),TokenLine}}.

{C}{A}+	: {token,{ident,list_to_atom(TokenChars),TokenLine}}.

{WS}+	: skip_token.

Erlang code.
