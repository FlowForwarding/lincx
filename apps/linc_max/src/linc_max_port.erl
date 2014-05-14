%%
%%
%%

%% @author Cloudozer LLP <info@cloudozer.org>
%% @copyright 2014 FlowForwarding.org
-module(linc_max_port).
-export([initialize/2,is_valid/2]).
-export([get_desc/1,get_stats/2]).
-export([modify/2]).

-include_lib("of_protocol/include/ofp_v4.hrl").
-include("fast_path.hrl").

initialize(_SwitchId, _Config) ->

%% Config = [{switch,0,
%%               [{datapath_id,"00:16:3E:23:44:00:00:00"},
%%                {backend,linc_max},
%%                {controllers,[]},
%%                {controllers_listener,{"0.0.0.0",6634,tcp}},
%%                {queues_status,disabled},
%%                {ports,
%%                    [{port,1,
%%                         [{config,
%%                              {port_configuration,undefined,up,false,false,
%%                                  false}},
%%                          {features,
%%                              {features,undefined,'100Mb-FD',true,copper,
%%                                  unsupported}},
%%                          {interface,"eth1"},
%%                          {type,vif}]},

	ok.

is_valid(_SwitchId, PortNo) ->
	Ports = linc_max_fast_path:describe_ports(),
	lists:keymember(PortNo, #port_info.port_no, Ports).

get_desc(_SwitchId) ->
	Ports = linc_max_fast_path:describe_ports(),
	Body = [#ofp_port{port_no =PortNo,
					  hw_addr =Mac} || #port_info{port_no =PortNo,
												  hw_addr =Mac} <- Ports], 
	#ofp_port_desc_reply{body =Body}.

get_stats(_Switch, #ofp_port_stats_request{port_no =any}) ->
	Ports = linc_max_fast_path:describe_ports(),
	port_stats_reply(Ports);
get_stats(_Switch, #ofp_port_stats_request{port_no =PortNo}) ->
	Ports = linc_max_fast_path:describe_ports(),
	case lists:keyfind(PortNo, #port_info.port_no, Ports) of
	false ->
		#ofp_error_msg{type =bad_request,code =bad_port};

	Port ->
		port_stats_reply([Port])
	end.

port_stats_reply(Ports) ->
	Body = [#ofp_port_stats{port_no =PortNo,
							duration_sec =0,	%%TODO
							duration_nsec =0,	%%TODO
							rx_packets =erlang:read_counter(RxPktRef),
							rx_bytes =erlang:read_counter(RxDataRef),
							tx_packets =erlang:read_counter(TxPktRef),
							tx_bytes =erlang:read_counter(TxDataRef)}
			|| #port_info{port_no =PortNo,
						  rx_pkt_ref =RxPktRef,
					      rx_data_ref =RxDataRef,
			   			  tx_pkt_ref =TxPktRef,
			   			  tx_data_ref =TxDataRef} <- Ports],
	#ofp_port_stats_reply{body =Body}.

modify(_Switch, #ofp_port_mod{port_no =_PortNo}) ->
	%%TODO
	{error,{port_mod_failed,eperm}}.

%%%------------------------------------------------------------------------------
%%% Copyright 2012 FlowForwarding.org
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%-----------------------------------------------------------------------------
%
%%% @author Erlang Solutions Ltd. <openflow@erlang-solutions.com>
%%% @author Cloudozer LLP. <info@cloudozer.com>
%%% @copyright 2012 FlowForwarding.org
%%% @doc Port-related routines. 
%-module(linc_max_port).
%
%%% Port API
%-export([initialize/2,
%         get_desc/1,
%         get_stats/2,
%         get_state/2,
%         set_state/3,
%         get_config/2,
%         set_config/3,
%         get_features/2,
%         get_advertised_features/2,
%         set_advertised_features/3,
%         get_all_ports_state/1,
%         get_all_queues_state/1,
%         is_valid/2]).
%
%-include_lib("of_config/include/of_config.hrl").
%-include_lib("of_protocol/include/of_protocol.hrl").
%-include_lib("of_protocol/include/ofp_v4.hrl").
%-include_lib("linc/include/linc_logger.hrl").
%-include("linc_max.hrl").
%-include("linc_max_port.hrl").
%
%%%%-----------------------------------------------------------------------------
%%%% API functions
%%%%-----------------------------------------------------------------------------
%
%-spec initialize(integer(), tuple(config, list(linc_port_config()))) -> ok.
%initialize(SwitchId, Config) ->
%    LincPorts = ets:new(linc_ports, [public,
%                                     {keypos, #linc_port.port_no},
%                                     {read_concurrency, true}]),
%    LincPortStats = ets:new(linc_port_stats,
%                            [public,
%                             {keypos, #ofp_port_stats.port_no},
%                             {read_concurrency, true}]),
%    linc:register(SwitchId, linc_ports, LincPorts),
%    linc:register(SwitchId, linc_port_stats, LincPortStats),
%    case queues_enabled(SwitchId) of
%        true ->
%            linc_max_queue:initialize(SwitchId);
%        false ->
%            ok
%    end,
%
%	%% Add ports to ETS tables
%	%% The actual port management happens in linc_max_fast_path
%	lists:foreach(fun({port, PortNo, _PortOpts}) ->
%		ets:insert(linc:lookup(SwitchId, linc_ports),
%        				#linc_port{port_no = PortNo, pid = undefined}),
%        ets:insert(linc:lookup(SwitchId, linc_port_stats),
%                        #ofp_port_stats{port_no = PortNo,
%										duration_sec = erlang:now()})
%	end, ports_for_switch(SwitchId, Config)).
%
%%% @doc Return list of all OFP ports present in the switch.
%-spec get_desc(integer()) -> ofp_port_desc_reply().
%get_desc(SwitchId) ->
%    L = ets:foldl(fun(#linc_port{pid = Pid}, Ports) ->
%                          Port = gen_server:call(Pid, get_port),
%                          [Port | Ports]
%                  end, [], linc:lookup(SwitchId, linc_ports)),
%    #ofp_port_desc_reply{body = L}.
%
%%% @doc Return port stats record for the given OF port.
%-spec get_stats(integer(), ofp_port_stats_request()) -> ofp_port_stats_reply() |
%                                                        ofp_error_msg().
%get_stats(SwitchId, #ofp_port_stats_request{port_no = any}) ->
%    PortStats = ets:tab2list(linc:lookup(SwitchId, linc_port_stats)),
%    #ofp_port_stats_reply{body = convert_duration(PortStats)};
%get_stats(SwitchId, #ofp_port_stats_request{port_no = PortNo}) ->
%    case ets:lookup(linc:lookup(SwitchId, linc_port_stats), PortNo) of
%        [] ->
%            #ofp_error_msg{type = bad_request, code = bad_port};
%        [#ofp_port_stats{}] = PortStats ->
%            #ofp_port_stats_reply{body = convert_duration(PortStats)}
%    end.
%
%-spec get_state(integer(), ofp_port_no()) -> [ofp_port_state()].
%get_state(SwitchId, PortNo) ->
%    case get_port_pid(SwitchId, PortNo) of
%        {error, _} ->
%            {error, {bad_request, bad_port}};
%        Pid ->
%            gen_server:call(Pid, get_port_state)
%    end.
%
%-spec set_state(integer(), ofp_port_no(), [ofp_port_state()]) -> ok.
%set_state(SwitchId, PortNo, PortState) ->
%    case get_port_pid(SwitchId, PortNo) of
%        {error, _} ->
%            {error, {bad_request, bad_port}};
%        Pid ->
%            gen_server:call(Pid, {set_port_state, PortState})
%    end.
%
%-spec get_config(integer(), ofp_port_no()) -> [ofp_port_config()].
%get_config(SwitchId, PortNo) ->
%    case get_port_pid(SwitchId, PortNo) of
%        {error, _} ->
%            {error, {bad_request, bad_port}};
%        Pid ->
%            gen_server:call(Pid, get_port_config)
%    end.
%
%-spec set_config(integer(), ofp_port_no(), [ofp_port_config()]) -> ok.
%set_config(SwitchId, PortNo, PortConfig) ->
%    case get_port_pid(SwitchId, PortNo) of
%        {error, _} ->
%            {error, {bad_request, bad_port}};
%        Pid ->
%            gen_server:call(Pid, {set_port_config, PortConfig})
%    end.
%
%-spec get_features(integer(), ofp_port_no()) -> tuple([ofp_port_feature()],
%                                                      [ofp_port_feature()],
%                                                      [ofp_port_feature()],
%                                                      [ofp_port_feature()]).
%get_features(SwitchId, PortNo) ->
%    case get_port_pid(SwitchId, PortNo) of
%        {error, _} ->
%            {error, {bad_request, bad_port}};
%        Pid ->
%            gen_server:call(Pid, get_features)
%    end.
%
%-spec get_advertised_features(integer(), ofp_port_no()) -> [ofp_port_feature()].
%get_advertised_features(SwitchId, PortNo) ->
%    case get_port_pid(SwitchId, PortNo) of
%        {error, _} ->
%            {error, {bad_request, bad_port}};
%        Pid ->
%            gen_server:call(Pid, get_advertised_features)
%    end.
%
%-spec set_advertised_features(integer(), ofp_port_no(), [ofp_port_feature()]) -> ok.
%set_advertised_features(SwitchId, PortNo, AdvertisedFeatures) ->
%    case get_port_pid(SwitchId, PortNo) of
%        {error, _} ->
%            {error, {bad_request, bad_port}};
%        Pid ->
%            gen_server:call(Pid, {set_advertised_features, AdvertisedFeatures})
%    end.
%
%-spec get_all_ports_state(integer()) -> list({ResourceId :: string(),
%                                              ofp_port()}).
%get_all_ports_state(SwitchId) ->
%    lists:map(fun(PortNo) ->
%                      Pid = get_port_pid(SwitchId, PortNo),
%                      gen_server:call(Pid, get_info)
%              end, get_all_port_no(SwitchId)).
%
%-spec get_all_queues_state(integer()) -> list(tuple(string(), integer(), integer(),
%                                                    integer(), integer())).
%get_all_queues_state(SwitchId) ->
%    lists:flatmap(fun(PortNo) ->
%                          linc_max_queue:get_all_queues_state(SwitchId, PortNo)
%                  end, get_all_port_no(SwitchId)).
%
%%% @doc Test if a port exists.
%-spec is_valid(integer(), ofp_port_no()) -> boolean().
%is_valid(_SwitchId, PortNo) when is_atom(PortNo)->
%    true;
%is_valid(SwitchId, PortNo) when is_integer(PortNo)->
%    ets:member(linc:lookup(SwitchId, linc_ports), PortNo).
%
%%%%-----------------------------------------------------------------------------
%%%% Internal functions
%%%%-----------------------------------------------------------------------------
%
%%% @doc Return list of all OFP port numbers present in the switch.
%-spec get_all_port_no(integer()) -> [integer()].
%get_all_port_no(SwitchId) ->
%    ets:foldl(fun(#linc_port{port_no = PortNo}, Acc) ->
%                      [PortNo | Acc]
%              end, [], linc:lookup(SwitchId, linc_ports)).
%
%-spec update_port_rx_counters(integer(), integer(), integer()) -> any().
%update_port_rx_counters(SwitchId, PortNum, Bytes) ->
%    ets:update_counter(linc:lookup(SwitchId, linc_port_stats), PortNum,
%                       [{#ofp_port_stats.rx_packets, 1},
%                        {#ofp_port_stats.rx_bytes, Bytes}]).
%
%-spec update_port_tx_counters(integer(), integer(), integer()) -> any().
%update_port_tx_counters(SwitchId, PortNum, Bytes) ->
%    ets:update_counter(linc:lookup(SwitchId, linc_port_stats), PortNum,
%                       [{#ofp_port_stats.tx_packets, 1},
%                        {#ofp_port_stats.tx_bytes, Bytes}]).
%
%-spec get_port_pid(integer(), ofp_port_no()) -> pid() | {error, invalid | nonexistent}.
%get_port_pid(_SwitchId, PortNo) when is_atom(PortNo); PortNo > ?OFPP_MAX ->
%    {error, invalid};
%get_port_pid(SwitchId, PortNo) ->
%    case ets:lookup(linc:lookup(SwitchId, linc_ports), PortNo) of
%        [] ->
%            {error, nonexistent};
%        [#linc_port{pid = Pid}] ->
%            Pid
%    end.
%
%-spec convert_duration(list(#ofp_port_stats{})) -> list(#ofp_port_stats{}).
%convert_duration(PortStatsList) ->
%    lists:map(fun(#ofp_port_stats{duration_sec = DSec} = PortStats) ->
%                      MicroDuration = timer:now_diff(erlang:now(), DSec),
%                      Sec = microsec_to_sec(MicroDuration),
%                      NSec = microsec_to_nsec(MicroDuration),
%                      PortStats#ofp_port_stats{duration_sec = Sec,
%                                               duration_nsec = NSec}
%              end, PortStatsList).
%
%microsec_to_sec(Micro) ->
%    Micro div 1000000.
%
%microsec_to_nsec(Micro) ->
%    (Micro rem 1000000) * 1000.
%
%%maybe_buffer(_SwitchId, action, Packet, no_buffer) ->
%%    {no_buffer,pkt:encapsulate(Packet)};
%%maybe_buffer(SwitchId, action, Packet, Bytes) ->
%%    maybe_buffer(SwitchId, Packet, Bytes);
%%maybe_buffer(SwitchId, no_match, Packet, _Bytes) ->
%%    maybe_buffer(SwitchId, Packet, get_switch_config(miss_send_len));
%%maybe_buffer(SwitchId, invalid_ttl, Packet, _Bytes) ->
%%    %% The spec does not specify how many bytes to include for invalid_ttl,
%%    %% so we use miss_send_len here as well.
%%    maybe_buffer(SwitchId, Packet, get_switch_config(miss_send_len)).
%%
%%maybe_buffer(_SwitchId, Packet, no_buffer) ->
%%    {no_buffer, pkt:encapsulate(Packet)};
%%maybe_buffer(SwitchId, Packet, Bytes) ->
%%    BufferId = linc_buffer:save_buffer(SwitchId, Packet),
%%    {BufferId, truncate_packet(Packet,Bytes)}.
%%
%%truncate_packet(Packet,Bytes) ->
%%    Bin = pkt:encapsulate(Packet),
%%    case byte_size(Bin) > Bytes of
%%        true ->
%%            <<Head:Bytes/bytes, _/binary>> = Bin,
%%            Head;
%%        false ->
%%            Bin
%%    end.
%
%-spec queues_enabled(integer()) -> boolean().
%queues_enabled(SwitchId) ->
%    {ok, Switches} = application:get_env(linc, logical_switches),
%    {switch, SwitchId, Opts} = lists:keyfind(SwitchId, 2, Switches),
%    case lists:keyfind(queues_status, 1, Opts) of
%        false ->
%            false;
%        {queues_status, enabled} ->
%            true;
%        _ ->
%            false
%    end.
%
%-spec queues_config(integer(), list(linc_port_config())) -> [term()] |
%                                                            disabled.
%queues_config(SwitchId, PortOpts) ->
%    case queues_enabled(SwitchId) of
%        true ->
%            case lists:keyfind(queues, 1, PortOpts) of
%                false ->
%                    disabled;
%                {queues, Queues} ->
%                    Queues
%            end;
%        false ->
%            disabled
%    end.
%
%ports_for_switch(SwitchId, Config) ->
%    {switch, SwitchId, Opts} = lists:keyfind(SwitchId, 2, Config),
%    {ports, Ports} = lists:keyfind(ports, 1, Opts),
%    QueuesStatus = lists:keyfind(queues_status, 1, Opts),
%    Queues = lists:keyfind(queues, 1, Opts),
%    [begin
%         {port, PortNo, [QueuesStatus | [Queues | PortConfig]]}
%     end || {port, PortNo, PortConfig} <- Ports].

%%EOF
