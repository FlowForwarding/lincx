
Nonterminals functions function type arguments argument dir.

Terminals ident void struct in out inout '*' '(' ')' '[' ']' ',' ';'.

Rootsymbol functions.

functions -> function
	: ['$1'].
functions -> function functions
	: ['$1'|'$2'].

function -> type ident '(' ')' ';'
	: {function,id('$2'),'$1',[]}.
function -> type ident '(' arguments ')' ';'
	: {function,id('$2'),'$1','$4'}.

arguments -> argument
	: ['$1'].
arguments -> argument ',' arguments
	: ['$1'|'$3'].

argument -> type ident
	: {arg,id('$2'),'$1',in}.
argument -> type '*' ident
	: {arg_ptr,id('$3'),'$1',out}.
argument -> type '*' ident '[' dir ']'
	: {arg_ptr,id('$3'),'$1','$5'}.

type -> void
	: void.
type -> ident
	: id('$1').
type -> struct ident
	: id('$2').

dir -> in
	: in.
dir -> out
	: out.
dir -> inout
	: inout.

Erlang code.

id({ident,Id,_}) -> Id.

