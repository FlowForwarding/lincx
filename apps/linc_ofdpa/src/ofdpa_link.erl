-module(ofdpa_link).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(st, {sock}).

-define(TAG_CALL, 16#ca11).
-define(TAG_RETURN,	16#bac6).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Host, Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Host,Port], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Host,Port]) ->
	{ok,Sock} = gen_tcp:connect(Host, Port, [{active,false},binary]),
    {ok,#st{sock =Sock}}.

%%
%%  4: len without len field)
%%	2: #0xca11
%%	2: function
%%	4: cookie
%%	-: args
%%

handle_call({call,What,ArgBin}, _From, #st{sock =Sock} =St) ->

	ReqSz = 2 +2 +4 +size(ArgBin),
	CookieBin = crypto:rand_bytes(4),
	<<Cookie:32/little>> = CookieBin,
	ReqBin = <<ReqSz:32/little,
			   ?TAG_CALL:16/little,
			   What:16/little,
			   CookieBin/binary,
			   ArgBin/binary>>,

	ok = gen_tcp:send(Sock, ReqBin),
	{ok,<<RepSz:32/little>>} = gen_tcp:recv(Sock, 4),
	{ok,<<_:16/little,
		  _:16/little,
		  Cookie:32/little,
		  RetBin/binary>>} = gen_tcp:recv(Sock, RepSz),

	{reply,{ok,RetBin},St}.

handle_cast(_Msg, St) ->
    {noreply,St}.

handle_info(_Info, St) ->
    {noreply,St}.

terminate(_Reason, _St) ->
    ok.

code_change(_OldVsn, St, _Extra) ->
    {ok,St}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

