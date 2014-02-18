%%------------------------------------------------------------------------------
%% Copyright 2012 FlowForwarding.org
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%-----------------------------------------------------------------------------

%% @author Erlang Solutions Ltd. <openflow@erlang-solutions.com>
%% @copyright 2012 FlowForwarding.org
%% @doc Supervisor module for the userspace switch implementation.
-module(linc_max_sup).

-behaviour(supervisor).

%% API
-export([start_link/1,
         start_backend_sup/1]).

%% Supervisor callbacks
-export([init/1]).

%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

-spec start_link(integer()) -> {ok, pid()} | ignore | {error, term()}.
start_link(SwitchId) ->
    supervisor:start_link(?MODULE, SwitchId).

-spec start_backend_sup(integer()) -> {ok, pid()}.
start_backend_sup(SwitchId) ->
    LincSup = linc:lookup(SwitchId, linc_sup),
    %% This should be a 'temporary' child, since it is added by one of
    %% the children of linc_sup.  Otherwise we would get an
    %% already_started error, when linc_sup restarts one copy of
    %% linc_max_sup, and linc_max tries to add another linc_max_sup.
    BackendSup = {?MODULE, {?MODULE, start_link, [SwitchId]},
                  temporary, 5000, supervisor, [?MODULE]},
    supervisor:start_child(LincSup, BackendSup).

%%------------------------------------------------------------------------------
%% Supervisor callbacks
%%------------------------------------------------------------------------------

init(SwitchId) ->
    linc:register(SwitchId, linc_max_sup, self()),
    MeterSup = {linc_max_meter_sup,
                {linc_max_meter_sup, start_link, [SwitchId]},
                permanent, 5000, supervisor, [linc_max_meter_sup]},
    PortSup = {linc_max_port_sup,
               {linc_max_port_sup, start_link, [SwitchId]},
               permanent, 5000, supervisor, [linc_max_port_sup]},
    {ok, {{one_for_one, 5, 10}, [PortSup,MeterSup]}}.
