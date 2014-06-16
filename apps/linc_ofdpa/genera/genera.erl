-module(genera).

-export([action/3]).

-record(g, {
		api =[],
		enums =[],
		structs =[],
		sizes =[]}).

action(Action, Api, DataTypes) ->
	ActiveTypes = active_types(Api, DataTypes),
	%%io:format("ActiveTypes = ~p\n", [ActiveTypes]),

	Enums = [E || {enum,T,_} =E <- DataTypes, lists:member(T, ActiveTypes)],
	Structs = [S || {struct,T,_} =S <- DataTypes, lists:member(T, ActiveTypes)],

	Enums1 = renumber_enums(Enums),
	EnumSizes = [{T,32} || {enum,T,_} <- Enums],

	{Structs1,AllSizes} = layout_structs(Structs, EnumSizes),

	action(Action, #g{api =Api,
					  enums =Enums1,
					  structs =Structs1,
					  sizes =AllSizes}).

action(enums, #g{enums =Enums}) ->
	Clauses1 = lists:concat([
		begin
			[{clause,0,[{atom,0,atomize(E)},
						{atom,0,atomize(Name)}],[],
								[{integer,0,Value}]}
					|| {constant,Name,Value} <- Cs]
		end
			|| {enum,E,Cs} <- Enums]),
	F1 = {function,0,enum_to_integer,2,Clauses1},
	Clauses2 = lists:concat([
		begin
			[{clause,0,[{atom,0,atomize(E)},
						{integer,0,Value}],[],
								[{atom,0,atomize(Name)}]}
					|| {constant,Name,Value} <- Cs]
		end
			|| {enum,E,Cs} <- Enums]),
	F2 = {function,0,integer_to_enum,2,Clauses2},
	io:format("~s\n", [erl_pp:form(F1)]),
	io:format("~s\n", [erl_pp:form(F2)]);

action(structs, #g{structs =Structs,sizes =Sizes}) ->
	%% struct_to_binary(#flow_entry{}) -> Bin
	Clauses1 = [
		begin
			Name = decamelize(S),
			case lists:keyfind(S, 1, Sizes) of
			{_,manual} ->
				Fields = [{record_field,0,{atom,0,N},{var,0,var_name(N)}}
								|| {field,N,_} <- Fs],
				Body = [{atom,0,implement_manually}],
				{clause,0,[{record,0,Name,Fields}],[],Body};
			_ ->
				Fields = [{record_field,0,{atom,0,N},{var,0,var_name(N)}}
								|| {field,N,_,_} <- Fs],
				Body = [pack_binary(Fs)],
				{clause,0,[{record,0,Name,Fields}],[],Body}
			end
		end
			|| {struct,S,Fs} <- Structs],
	F1 = {function,0,struct_to_binary,1,Clauses1},
	%% binary_to_struct(Tag, Bin) -> #flow_entry{}
	Clauses2 = [
		begin
			Name = decamelize(S),
			case lists:keyfind(S, 1, Sizes) of
			{_,manual} ->
				{clause,0,[{atom,0,Name},{var,0,'_'}],[],[{atom,0,implement_manually}]};
			_ ->
				Pat = binary_pattern(Fs),
				Fields = [{record_field,0,{atom,0,N},{var,0,var_name(N)}}
								|| {field,N,_,_} <- Fs],
				Body = [{record,0,Name,Fields}],
				{clause,0,[{atom,0,Name},Pat],[],Body}
			end
		end
			|| {struct,S,Fs} <- Structs],
	F2 = {function,0,binary_to_struct,2,Clauses2},
	io:format("~s\n", [erl_pp:form(F1)]),
	io:format("~s\n", [erl_pp:form(F2)]);

action(records, #g{structs =Structs}) ->
	lists:foreach(fun({struct,S,Fs}) ->
		Name = decamelize(S),
		Ns = lists:foldr(fun({field,N,_}, Ns) ->
			[N|Ns];
		({field,N,_,_}, Ns) ->
			[N|Ns];
		({pad,_}, Ns) ->
			Ns
		end, [], Fs),
		Fields = [{record_field,0,{atom,0,N}} || N <- Ns],
		F = {attribute,0,record,{Name,Fields}},
		io:format("~s\n", [erl_pp:form(F)])
	end, Structs);

%% {N,function,ofdpaQueueRateSet,'OFDPA_ERROR_t',
%%           [{arg,portNum,uint32_t,in},
%%            {arg,queueId,uint32_t,in},
%%            {arg,minRate,uint32_t,in},
%%            {arg,maxRate,uint32_t,in}]},

%% ofdpaQueueRateSet(PortNum, QueueId, MinRate, MaxRate) ->
%%		call([{enum,error_t}], ?QUEUE_RATE_SET,
%%				PortNum, QueueId, MinRate, MaxRate).

action(stubs, #g{api =Api,enums =Enums,structs =Structs}) ->
	lists:foreach(fun({K,function,Name,RetType,Args}) ->
		ArgInfo = [{N,T,D,type_class(T, Enums, Structs)} || {_,N,T,D} <- Args],
		%%NB: RetType is always enum
		CTs = [{C,T} || {_,T,D,C} <- ArgInfo, D =:= out orelse D =:= inout],
		RetVals = ret_type(RetType)
						++
			lists:map(fun({scalar,T}) ->
				T;
			({enum,E}) ->
				{enum,atomize(E)};
			({struct,S}) ->
				{struct,decamelize(S)};
			(T) ->
				T 
			end, CTs),
		EncArgs = lists:map(fun({N,T,_,enum}) ->
			P = {call,0,{atom,0,enum_to_integer},[{atom,0,atomize(T)},{var,0,var_name(N)}]},
			{tuple,0,[{atom,0,uint32_t},P]};
		({N,_,_,struct}) ->
			{call,0,{atom,0,struct_to_binary},[{var,0,var_name(N)}]};
		({N,T,_,scalar}) ->
			{tuple,0,[{atom,0,T},{var,0,var_name(N)}]}
		end, ArgInfo),
		Body = [{call,0,{atom,0,call},
					[erl_parse:abstract(RetVals),{integer,0,K},consify(EncArgs)]}],
		ArgVars = [{var,0,var_name(N)} || {_,N,_,_} <- Args],
		Clause = {clause,0,ArgVars,[],Body},
		F = {function,0,Name,length(ArgVars),[Clause]},
		io:format("~s\n", [erl_pp:form(F)])
	end, Api);

action(defines, #g{api =Api}) ->
	lists:foreach(fun({K,function,Name,_,_}) ->
		io:format("#define ~s\t~w\n", [decamelize1(Name),K])
	end, Api);

action(cases, #g{api =Api,enums =Enums,structs =Structs}) ->
	lists:foreach(fun({_,function,Name,RetType,Args}) ->
		io:format("\tcase ~s:\n\t{\n", [decamelize1(Name)]),
		ArgInfo = [{N,T,D,type_class(T, Enums, Structs)} || {_,N,T,D} <- Args],

		if RetType =/= void ->
			io:format("\t\tassert(roff +4 <= *ret_len);\n", []),
			io:format("\t\t// make space for OFDPA_ERROR_t\n", []),
			io:format("\t\troff += 4;\n\n", []);
		true ->
			ok
		end,

		%% inout/out parameters
		lists:foreach(fun({N,T,_,struct}) ->
			io:format("\t\tassert(roff +4 <= *ret_len);\n", []),
			io:format("\t\tPUT32(ret_buf +roff, sizeof(~s));\n", [T]),
			io:format("\t\troff += 4;\n", []),
			io:format("\t\tassert(roff +sizeof(~s) <= *ret_len);\n", [T]),
			io:format("\t\t~s *~w = (~s *)(ret_buf +roff);\n", [T,N,T]),
			io:format("\t\troff += sizeof(~s);\n", [T]);
		({N,T,_,enum}) ->
			io:format("\t\tassert(roff +4 <= *ret_len);\n", []),
			io:format("\t\t~s *~w = (~s *)(ret_buf +roff);\n", [T,N,T]),
			io:format("\t\troff += 4;\n", []);

		({N,ofdpaMacAddr_t,_,_}) ->
			io:format("\t\tassert(roff +6 <= *ret_len);\n", []),
			io:format("\t\tofdpaMacAddr_t *~w = (ofdpaMacAddr_t *)(ret_buf +roff);\n", [N]),
			io:format("\t\troff += 6;\n", []);
		({N,timeval,_,_}) ->
			io:format("\t\tassert(roff +6 <= *ret_len);\n", []),
			io:format("\t\tstruct timeval *~w = (struct timeval *)(ret_buf +roff);\n", [N]),
			io:format("\t\troff += 6;\n", []);

		({N,uint16_t,_,scalar}) ->
			io:format("\t\tassert(roff +2 <= *ret_len);\n", []),
			io:format("\t\tuint16_t *~w = (uint16_t *)(ret_buf +roff);\n", [N]),
			io:format("\t\troff += 2;\n", []);
		({N,uint32_t,_,scalar}) ->
			io:format("\t\tassert(roff +4 <= *ret_len);\n", []),
			io:format("\t\tuint32_t *~w = (uint32_t *)(ret_buf +roff);\n", [N]),
			io:format("\t\troff += 4;\n", []);
		({N,uint64_t,_,scalar}) ->
			io:format("\t\tassert(roff +8 <= *ret_len);\n", []),
			io:format("\t\tuint64_t *~w = (uint64_t *)(ret_buf +roff);\n", [N]),
			io:format("\t\troff += 8;\n", []);
		(X) ->
			io:format("\t\t//TODO: ~p\n", [X])
		end, [X || {_,_,D,_} =X <- ArgInfo, D =:= out orelse D =:= inout]),

		%% in/inout parameters
		io:format("\n", []),
		lists:foreach(fun({N,T,inout,struct}) ->
			io:format("\t\tassert(off +4 <= arg_len);\n", []),
			io:format("\t\tsz = GET32(arg_buf +off);\n", []),
			io:format("\t\toff += 4;\n", []),
			io:format("\t\tassert(sz <= sizeof(~s));\n", [T], []),
			io:format("\t\tmemcpy((void *)~s, (void *)arg_buf, sz);\n", [N]),
			io:format("\t\tassert(off +sz <= arg_len);\n", []),
			io:format("\t\toff += sz;\n", []);
		({N,T,_,struct}) ->
			io:format("\t\tassert(off +4 <= arg_len);\n", []),
			io:format("\t\tsz = GET32(arg_buf +off);\n", []),
			io:format("\t\toff += 4;\n", []),
			io:format("\t\tassert(off +sz <= arg_len);\n", []),
			io:format("\t\t~s *~w = (~s *)(arg_buf +off);\n", [T,N,T]),
			io:format("\t\toff += sz;\n", []);

		({N,T,in,enum}) ->
			io:format("\t\tassert(off +4 <= arg_len);\n", []),
			io:format("\t\t~s ~w = (~s)GET32(arg_buf +off);\n", [T,N,T]),
			io:format("\t\toff += 4;\n", []);

		({N,uint16_t,inout,scalar}) ->
			io:format("\t\tassert(off +2 <= arg_len);\n", []),
			io:format("\t\t*~w = GET16(arg_buf +off);\n", [N]),
			io:format("\t\toff += 2;\n", []);
		({N,uint16_t,_,scalar}) ->
			io:format("\t\tassert(off +2 <= arg_len);\n", []),
			io:format("\t\tuint16_t ~w = GET16(arg_buf +off);\n", [N]),
			io:format("\t\toff += 2;\n", []);

		({N,uint32_t,inout,scalar}) ->
			io:format("\t\tassert(off +4 <= arg_len);\n", []),
			io:format("\t\t*~w = GET32(arg_buf +off);\n", [N]),
			io:format("\t\toff += 4;\n", []);
		({N,uint32_t,_,scalar}) ->
			io:format("\t\tassert(off +4 <= arg_len);\n", []),
			io:format("\t\tuint32_t ~w = GET32(arg_buf +off);\n", [N]),
			io:format("\t\toff += 4;\n", []);

		({N,uint64_t,inout,scalar}) ->
			io:format("\t\tassert(off +8 <= arg_len);\n", []),
			io:format("\t\t*~w = GET64(arg_buf +off);\n", [N]),
			io:format("\t\toff += 8;\n", []);
		({N,uint64_t,_,scalar}) ->
			io:format("\t\tassert(off +8 <= arg_len);\n", []),
			io:format("\t\tuint64_t ~w = GET64(arg_buf +off);\n", [N]),
			io:format("\t\toff += 8;\n", []);
		(X) ->
			io:format("\t\t//TODO: ~p\n", [X])
		end, [X || {_,_,D,_} =X <- ArgInfo, D =:= in orelse D =:= inout]),

		As = string:join([atom_to_list(N) || {N,_,_,_} <- ArgInfo], ", "),
		if RetType =:= void ->
			io:format("\n\t\t~s(~s);\n\n", [Name,As]);
		true ->
			io:format("\n\t\tOFDPA_ERROR_t err = ~s(~s);\n", [Name,As]),
			io:format("\t\tPUT32(ret_buf, err);\n\n", [])
		end,

		io:format("\t\t*ret_len = roff;\n", []),
		io:format("\t\tbreak;\n\t}\n", [])
	end, Api);

action(exports, #g{api =Api}) ->
	F = {attribute,0,export,[{F,length(As)} || {_,function,F,_,As} <- Api]},
	io:format("~s\n", [erl_pp:form(F)]);

action('dump-enums', #g{enums =Enums}) ->
	io:format("~p\n", [Enums]);

action('dump-structs', #g{structs =Structs}) ->
	io:format("~p\n", [Structs]);

action('dump-sizes', #g{sizes =Sizes}) ->
	io:format("~p\n", [Sizes]);

action(Action, _G) ->
	io:format("Error: action '~s' unknown\n", [Action]).

%% {enum,'OFDPA_PORT_TYPE_t',
%%       [{constant,'OFDPA_PORT_TYPE_PHYSICAL',0},
%%        {constant,'OFDPA_PORT_TYPE_LOGICAL_TUNNEL',1}]},

ret_type(void) ->
	[];
ret_type('OFDPA_ERROR_t') ->
	[{enum,error_t}].

consify([]) ->
	{nil,0};
consify([P|Ps]) ->
	{cons,0,P,consify(Ps)}.

type_class(T, Enums, Structs) ->
	case lists:keymember(T, 2, Enums) of
	true ->
		enum;
	false ->
		case lists:keymember(T, 2, Structs) of
		true ->
			struct;
		false ->
			scalar
		end
	end.

pack_binary(Fs) ->
	pack_binary(Fs, []).

pack_binary([], Acc) ->
	{bin,0,lists:reverse(Acc)};
pack_binary([{field,Name,_,{Sz,BinOpts}}|Fs], Acc) ->
	Elem = {bin_element,0,{var,0,var_name(Name)},
						  {integer,0,Sz},
						  BinOpts},
	pack_binary(Fs, [Elem|Acc]);
pack_binary([{pad,Sz}|Fs], Acc) ->
	Elem = {bin_element,0,{integer,0,0},
						  {integer,0,Sz},
						  default},
	pack_binary(Fs, [Elem|Acc]).

binary_pattern(Fs) ->
	binary_pattern(Fs, []).

binary_pattern([], Acc) ->
	{bin,0,lists:reverse(Acc)};
binary_pattern([{field,Name,_,{Sz,BinOpts}}|Fs], Acc) ->
	Elem = {bin_element,0,{var,0,var_name(Name)},
						  {integer,0,Sz},
						  BinOpts},
	binary_pattern(Fs, [Elem|Acc]);
binary_pattern([{pad,Sz}|Fs], Acc) ->
	Elem = {bin_element,0,{var,0,'_'},
						  {integer,0,Sz},
						  default},
	binary_pattern(Fs, [Elem|Acc]).

active_types(Api, DTs) ->
	Ts = lists:usort(lists:concat([
			[RetType] ++ [ArgType || {_,_,ArgType,_} <- Args]
					|| {_,function,_Name,RetType,Args} <- Api])),
	active_types1(Ts, DTs, []).

active_types1([], _, Acc) ->
	lists:usort(Acc);
active_types1([{union,Fs} = T|Ts], DTs, Acc) ->
	active_types1([X || {field,_,X} <- Fs] ++ Ts, DTs, [T|Acc]);
active_types1([T|Ts], DTs, Acc) ->
	case lists:keyfind(T, 2, DTs) of
	{struct,_,Fs} ->
		active_types1([X || {field,_,X} <- Fs] ++ Ts, DTs, [T|Acc]);
	_ ->
		active_types1(Ts, DTs, [T|Acc])
	end.

renumber_enums(Enums) ->
	renumber_enums(Enums, []).

renumber_enums([], Acc) ->
	lists:reverse(Acc);
renumber_enums([{enum,Name,Cs}|Enums], Acc) ->
	renumber_enums(Enums, [{enum,Name,enum_vals(Cs)}|Acc]).

enum_vals(Cs) ->
	enum_vals(Cs, 0, []).

enum_vals([], _, Acc) ->
	lists:reverse(Acc);
enum_vals([{constant,_Name,V} =C|Cs], _N, Acc) when is_integer(V) ->
	enum_vals(Cs, V +1, [C|Acc]);
enum_vals([{constant,Name,default}|Cs], N, Acc) ->
	enum_vals(Cs, N +1, [{constant,Name,N}|Acc]);
enum_vals([{constant,Name,{ref,OtherName}}|Cs], _N, Acc) ->
	{_,_,V} = lists:keyfind(OtherName, 2, Acc),
	enum_vals(Cs, V +1, [{constant,Name,V}|Acc]).

layout_structs(Structs, Sizes) ->
	layout_structs(Structs, Sizes, []).

layout_structs([], Sizes, Acc) ->
	{lists:reverse(Acc),Sizes};
layout_structs([{struct,T,Fs} =Struct|Structs], Sizes, Acc) ->
	%%io:format("layout ~w\n", [T]),
	try
		{Fs1,Sz} = align_fields(Fs, Sizes),
		layout_structs(Structs, [{T,Sz}|Sizes], [{struct,T,Fs1}|Acc])

	catch error:manual_type ->	%% ofdpa_buffdesc
		layout_structs(Structs, [{T,manual}|Sizes], [Struct|Acc])
	end.

align_fields(Fs, Sizes) ->
	align_fields(Fs, Sizes, 0, []).

align_fields([], _, Off, Acc) ->
	{lists:reverse(Acc),Off};
align_fields([{field,Name,T}|Fs], Sizes, Off, Acc) ->
	{Bits,Align,BinOpts} = type_size(T, Sizes),
	Off1 = (Off +Align -1) band (bnot (Align -1)),
	if Off1 > Off ->
		align_fields(Fs, Sizes, Off1 +Bits, [{field,Name,T,{Bits,BinOpts}},{pad,Off1 -Off}|Acc]);
	true ->
		align_fields(Fs, Sizes, Off1 +Bits, [{field,Name,T,{Bits,BinOpts}}|Acc])
	end.

type_size(uint8_t, _Sizes) -> {8,8,[little]};
type_size(uint16_t, _Sizes) -> {16,16,[little]};
type_size(uint32_t, _Sizes) -> {32,32,[little]};
type_size(uint64_t, _Sizes) -> {64,64,[little]};
type_size(ofdpaMacAddr_t, _Sizes) -> {48,8,[binary]};
type_size(in_addr_t, _Sizes) -> {32,8,[binary]};
type_size(in6_addr, _Sizes) -> {128,8,[binary]};
type_size({union,Fs}, Sizes) ->
	Ps = [type_size(X, Sizes) || {field,_,X} <- Fs],
	{lists:max([X || {X,_,_} <- Ps]),8,[binary]};
type_size(ofdpa_buffdesc, _Sizes) ->
	erlang:error(manual_type);
type_size(T, Sizes) ->
	case lists:keyfind(T, 1, Sizes) of
	{_,Size} ->
		{Size,8,[binary]};
	_ ->
		erlang:error({undef_type,T,Sizes})
	end.

atomize(N) when is_atom(N) ->
	case atom_to_list(N) of
	"OFDPA_" ++ Rest ->
		list_to_atom(string:to_lower(Rest))
	end.

decamelize(S) ->
	%% ofdpaFlowEntry_t -> flow_entry
	X = atom_to_list(S),
	Y = decam(string:substr(X, 6, length(X) -7)),
	list_to_atom(string:to_lower(Y)).

decamelize1(S) ->
	%% ofdpaFlowEntryDelete -> FLOW_ENTRY_DELETE
	X = atom_to_list(S),
	Y = decam(string:substr(X, 6, length(X) -5)),
	string:to_upper(Y).

decam(S) ->
	decam(S, []).

decam([], Acc) ->
	lists:reverse(Acc);
decam([C|Cs], []) when C < $a ->
	decam(Cs, [C]);
decam([C|Cs], Acc) when C < $a ->
	decam(Cs, [C,$_|Acc]);
decam([C|Cs], Acc) ->
	decam(Cs, [C|Acc]).

var_name(X) ->
	[C|Cs] = atom_to_list(X),
	list_to_atom([string:to_upper(C)|Cs]).

%%EOF
