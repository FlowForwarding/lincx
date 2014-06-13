
Nonterminals decls decl constants constant fields field type.

Terminals typedef enum struct union ',' '{' '}' '=' ';' '<<' ident int.

Rootsymbol decls.

decls -> decl
	: ['$1'].
decls -> decl decls
	: ['$1'|'$2'].

decl -> typedef enum '{' constants '}' ident ';'
	: {enum,id('$6'),'$4'}.
decl -> typedef struct ident '{' fields '}' ident ';'
	: {struct,id('$7'),'$5'}.
decl -> typedef struct '{' fields '}' ident ';'
	: {struct,id('$6'),'$4'}.

constants -> constant
	: ['$1'].
constants -> constant ','
	: ['$1'].
constants -> constant ',' constants
	: ['$1'|'$3'].

constant -> ident '=' int '<<' int
	: {constant,id('$1'),val('$3') bsl val('$5')}.
constant -> ident '=' int
	: {constant,id('$1'),val('$3')}.
constant -> ident '=' ident
	: {constant,id('$1'),{ref,id('$3')}}.
constant -> ident
	: {constant,id('$1'),default}.

fields -> field
	: ['$1'].
fields -> field fields
	: ['$1'|'$2'].

field -> type ident ';'
	: {field,id('$2'),'$1'}.
field -> union '{' fields '}' ident ';'
	: {field,id('$5'),{union,'$3'}}.

type -> struct ident
	: id('$2').
type -> ident
	: id('$1').

Erlang code.

id({ident,Id,_}) -> Id.

val({int,N,_}) -> N.

