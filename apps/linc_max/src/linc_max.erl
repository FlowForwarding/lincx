-module(linc_max).
-export([update_metadata/3]).
-export([meter/2]).

%% FAST PATH
%%
%% The function is needed because metadata are represented as binary in the
%% argument list of a flow entry.
%%
update_metadata(<<MetaInt:64>>, AndMe, OrMe) ->
	<<(MetaInt band AndMe bor OrMe):64>>.

%% FAST PATH
%%
%% Meters are processes. The call should lookup the meter process using the
%% state and exchange messages with it to check that the packet fit the bands.
%%
meter(_MeterId, _St) -> ok.

%%EOF
