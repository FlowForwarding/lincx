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

%% @author Cloduozer LLP. <info@cloudozer.com>
%% @copyright 2014 FlowForwarding.org
%% @doc An OpenFlow switch based on the Broadcom's OF-DPA.

-module(linc_ofdpa).
-behaviour(gen_switch).

-define(SWITCH_ID, 0).	%% gradually move away from multiple switches

%% gen_switch callbacks
-export([start/1,
         stop/1,
         handle_message/2]).

%% Backend API
-export([is_port_valid/2,
         is_queue_valid/3,
         set_datapath_mac/2,
         log_message_sent/1]).

%% Handle all message types
-export([ofp_features_request/2,
         ofp_flow_mod/2,
         ofp_table_mod/2,
         ofp_port_mod/2,
         ofp_group_mod/2,
         ofp_packet_out/2,
         ofp_echo_request/2,
         ofp_get_config_request/2,
         ofp_set_config/2,
         ofp_barrier_request/2,
         ofp_queue_get_config_request/2,
         ofp_desc_request/2,
         ofp_flow_stats_request/2,
         ofp_aggregate_stats_request/2,
         ofp_table_stats_request/2,
         ofp_table_features_request/2,
         ofp_port_desc_request/2,
         ofp_port_stats_request/2,
         ofp_queue_stats_request/2,
         ofp_group_stats_request/2,
         ofp_group_desc_request/2,
         ofp_group_features_request/2,
         ofp_meter_mod/2,
         ofp_meter_stats_request/2,
         ofp_meter_config_request/2,
         ofp_meter_features_request/2]).

-include_lib("of_protocol/include/of_protocol.hrl").
-include_lib("of_protocol/include/ofp_v4.hrl").
-include_lib("linc/include/linc_logger.hrl").
-include("linc_ofdpa.hrl").

-record(state, {datapath_mac,
				switch_config}).
-type state() :: #state{}.

%%%-----------------------------------------------------------------------------
%%% gen_switch callbacks
%%%-----------------------------------------------------------------------------

%% @doc Start the switch.
-spec start(any()) -> {ok, Version :: 4, state()}.
start(BackendOpts) ->
    try
        {datapath_mac, DatapathMac} = lists:keyfind(datapath_mac, 1, BackendOpts),
		{ok,4,#state{datapath_mac =DatapathMac}}
    catch
        _:Error ->
			?ERROR("linc_ofdpa:start(): ~p\n\t~p\n", [Error,erlang:get_stacktrace()]),
            {error, Error}
    end.

%% @doc Stop the switch.
-spec stop(state()) -> any().
stop(_State) ->
    ok.

-spec handle_message(ofp_message_body(), state()) ->
                            {noreply, state()} |
                            {reply, ofp_message(), state()}.
handle_message(MessageBody, State) ->
    MessageName = element(1, MessageBody),
    erlang:apply(?MODULE, MessageName, [State, MessageBody]).

%%%-----------------------------------------------------------------------------
%%% Backend API
%%%-----------------------------------------------------------------------------

-spec is_port_valid(integer(), ofp_port_no()) -> boolean().
is_port_valid(?SWITCH_ID, PortNo) -> 
	case ofdpa:ofdpaPortStateGet(PortNo) of
	{ok,e_none,_} ->
		true;
	_ ->
		false
	end.

-spec is_queue_valid(integer(), ofp_port_no(), ofp_queue_id()) -> boolean().
is_queue_valid(?SWITCH_ID, PortNo, QueueId) ->
	case ofdpa:ofdpaQueueRateGet(PortNo, QueueId) of
	{ok,e_none,_,_} ->
		true;
	_ ->
		false
	end.

set_datapath_mac(State, NewMac) ->
    State#state{datapath_mac = NewMac}.

-spec log_message_sent(ofp_message()) -> term().
log_message_sent(#ofp_message{body = Body} = Message)
  when is_record(Body, ofp_error_msg) ->
    ?DEBUG("[OF_ERROR] Sent message to controller: ~w~n", [Message]);
log_message_sent(Message) ->
    ?DEBUG("Sent message to controller: ~w~n", [Message]).


%%%-----------------------------------------------------------------------------
%%% Handling of messages
%%%-----------------------------------------------------------------------------

ofp_features_request(#state{datapath_mac = DatapathMac} = State,
                     #ofp_features_request{}) ->
    FeaturesReply = #ofp_features_reply{datapath_mac = DatapathMac,
                                        datapath_id = ?SWITCH_ID,
                                        n_buffers = 0,
                                        n_tables = 255,
                                        auxiliary_id = 0,
                                        capabilities = ?CAPABILITIES},
    {reply, FeaturesReply, State}.

%% @doc Modify flow entry in the flow table.
-spec ofp_flow_mod(state(), ofp_flow_mod()) ->
                          {noreply, #state{}} |
                          {reply, ofp_message(), #state{}}.
ofp_flow_mod(State,
             #ofp_flow_mod{} = _FlowMod) ->
	%%TODO
	{reply,#ofp_error_msg{type =flow_mod_failed,code =eperm},State}.

%% @doc Modify flow table configuration.
-spec ofp_table_mod(state(), ofp_table_mod()) ->
                           {noreply, #state{}} |
                           {reply, ofp_message(), #state{}}.
ofp_table_mod(State, #ofp_table_mod{} = _TableMod) ->
	%%TODO
	{noreply,State}.

%% @doc Modify port configuration.
-spec ofp_port_mod(state(), ofp_port_mod()) ->
                          {noreply, #state{}} |
                          {reply, ofp_message(), #state{}}.
ofp_port_mod(State,
             #ofp_port_mod{port_no = PortNo,
						   hw_addr =Mac,
						   config =Config,
						   advertise =Advertise}) ->
	Reply = case ofdpa:ofdpaPortMacGet(PortNo) of
	{ok,e_none,Mac} ->

		%%TODO
		todo;

	{ok,e_none,_WrongMac} ->
		#ofp_error_msg{type =port_mod_failed,code =bad_hw_addr};
	{ok,e_not_found,_} ->
		#ofp_error_msg{type =port_mod_failed,code =bad_port};
	_ ->
		#ofp_error_msg{type =bad_request,code =bad_port}
	end,
	{reply,Reply,State}.

%% @doc Modify group entry in the group table.
-spec ofp_group_mod(state(), ofp_group_mod()) ->
                           {noreply, #state{}} |
                           {reply, ofp_message(), #state{}}.
ofp_group_mod(State,
              #ofp_group_mod{} = _GroupMod) ->
	%%TODO
	{reply,#ofp_error_msg{type =group_mod_failed,code =eperm},State}.

%% @doc Handle a packet received from controller.
-spec ofp_packet_out(state(), ofp_packet_out()) ->
                            {noreply, #state{}} |
                            {reply, ofp_message(), #state{}}.
ofp_packet_out(State,
               #ofp_packet_out{buffer_id = no_buffer,
                               actions = _Actions,
                               in_port = _InPort,
                               data = _Data}) ->
	%%TODO
    {noreply,State}.

%% @doc Reply to echo request.
-spec ofp_echo_request(state(), ofp_echo_request()) ->
                              {reply, ofp_echo_reply(), #state{}}.
ofp_echo_request(State, #ofp_echo_request{data = Data}) ->
    EchoReply = #ofp_echo_reply{data = Data},
    {reply,EchoReply,State}.

%% @doc Reply to get config request.
-spec ofp_get_config_request(state(), ofp_get_config_request()) ->
                                    {reply, ofp_get_config_reply(), #state{}}.
ofp_get_config_request(#state{switch_config = SwitchConfig} = State,
                       #ofp_get_config_request{}) ->
    ConfigReply = #ofp_get_config_reply{flags = proplists:get_value(
                                                  flags,
                                                  SwitchConfig),
                                        miss_send_len = proplists:get_value(
                                                          miss_send_len,
                                                          SwitchConfig)},
    {reply,ConfigReply,State}.

%% @doc Set switch configuration.
-spec ofp_set_config(state(), ofp_set_config()) -> {noreply, state()}.
ofp_set_config(State, #ofp_set_config{flags =Flags,
                                      miss_send_len =MissSendLength}) ->
    SwitchConfig = [{flags,Flags},{miss_send_len,MissSendLength}],
    {noreply,State#state{switch_config =SwitchConfig}}.

%% @doc Reply to barrier request.
-spec ofp_barrier_request(state(), ofp_barrier_request()) ->
                                 {reply, ofp_barrier_reply(), #state{}}.
ofp_barrier_request(State, #ofp_barrier_request{}) ->
	%%TODO
    BarrierReply = #ofp_barrier_reply{},
    {reply,BarrierReply,State}.

%% @doc Reply to get queue config request.
-spec ofp_queue_get_config_request(state(), ofp_queue_get_config_request()) ->
                                          {reply, ofp_get_config_reply(),
                                           #state{}}.
ofp_queue_get_config_request(State,
                             #ofp_queue_get_config_request{port =Port}) ->
	%%TODO
    QueueConfigReply = #ofp_queue_get_config_reply{port =Port,
                                                   queues =[]},
    {reply,QueueConfigReply,State}.

%% @doc Get switch description statistics.
-spec ofp_desc_request(state(), ofp_desc_request()) ->
                              {reply, ofp_desc_reply(), #state{}}.
ofp_desc_request(State, #ofp_desc_request{}) ->
    {reply, #ofp_desc_reply{flags = [],
                            mfr_desc = get_env(manufacturer_desc),
                            hw_desc = get_env(hardware_desc),
                            sw_desc = get_env(software_desc),
                            serial_num = get_env(serial_number),
                            dp_desc = get_env(datapath_desc)
                           }, State}.

%% @doc Get flow entry statistics.
-spec ofp_flow_stats_request(state(), ofp_flow_stats_request()) ->
                                    {reply, ofp_flow_stats_reply(), #state{}}.
ofp_flow_stats_request(State,
                       #ofp_flow_stats_request{} = _Request) ->
	%%TODO
	Reply = #ofp_flow_stats_reply{body =[]},
    {reply,Reply,State}.

%% @doc Get aggregated flow statistics.
-spec ofp_aggregate_stats_request(state(), ofp_aggregate_stats_request()) ->
                                         {reply, ofp_aggregate_stats_reply(),
                                          #state{}}.
ofp_aggregate_stats_request(State,
                            #ofp_aggregate_stats_request{} = _Request) ->
	%%TODO
	Reply = #ofp_aggregate_stats_reply{packet_count =0,
									   byte_count =0,
									   flow_count =0},
    {reply,Reply,State}.

%% @doc Get flow table statistics.
-spec ofp_table_stats_request(state(), ofp_table_stats_request()) ->
                                     {reply, ofp_table_stats_reply(), #state{}}.
ofp_table_stats_request(State,
                        #ofp_table_stats_request{} = _Request) ->
	%%TODO
	Reply = #ofp_table_stats_reply{body =[]},
    {reply,Reply,State}.

-spec ofp_table_features_request(state(), #ofp_table_features_request{}) ->
                                        {reply, #ofp_table_features_reply{},
                                         #state{}}.
ofp_table_features_request(State,
                           #ofp_table_features_request{} = _Request) ->
	%%TODO
	Reply = #ofp_table_features_reply{body =[]},
    {reply,Reply,State}.

%% @doc Get port description.
-spec ofp_port_desc_request(state(), ofp_port_desc_request()) ->
                                   {reply, ofp_port_desc_reply(), #state{}}.
ofp_port_desc_request(State,
                      #ofp_port_desc_request{}) ->
	%%TODO
	Reply = #ofp_port_desc_reply{body =[]},
    {reply,Reply,State}.

%% @doc Get port statistics.
-spec ofp_port_stats_request(state(), ofp_port_stats_request()) ->
                                    {reply, ofp_port_stats_reply(), #state{}}.
ofp_port_stats_request(State,
                       #ofp_port_stats_request{} = _Request) ->
	%%TODO
	Reply = #ofp_port_stats_reply{body =[]},
    {reply,Reply,State}.

%% @doc Get queue statistics.
-spec ofp_queue_stats_request(state(), ofp_queue_stats_request()) ->
                                     {reply, ofp_queue_stats_reply(), #state{}}.
ofp_queue_stats_request(State,
                        #ofp_queue_stats_request{} = _Request) ->
	%%TODO
    Reply =	#ofp_queue_stats_reply{body =[]}, 
    {reply,Reply,State}.

%% @doc Get group statistics.
-spec ofp_group_stats_request(state(), ofp_group_stats_request()) ->
                                     {reply, ofp_group_stats_reply(), #state{}}.
ofp_group_stats_request(State,
                        #ofp_group_stats_request{} = _Request) ->
	%%TODO
    Reply = #ofp_group_stats_reply{body =[]},
    {reply,Reply,State}.

%% @doc Get group description statistics.
-spec ofp_group_desc_request(state(), ofp_group_desc_request()) ->
                                    {reply, ofp_group_desc_reply(), #state{}}.
ofp_group_desc_request(State,
                       #ofp_group_desc_request{} = _Request) ->
	%%TODO
    Reply = #ofp_group_desc_reply{body =[]},
    {reply,Reply,State}.

%% @doc Get group features statistics.
-spec ofp_group_features_request(state(),
                                 ofp_group_features_request()) ->
                                        {reply, ofp_group_features_reply(),
                                         #state{}}.
ofp_group_features_request(State,
                           #ofp_group_features_request{} = _Request) ->
	%%TODO
    Reply = #ofp_group_features_reply{
       types = [all, select, indirect, ff],
       capabilities = [select_weight, chaining], %select_liveness, chaining_checks
       max_groups = {?MAX, ?MAX, ?MAX, ?MAX},
       actions = {?SUPPORTED_WRITE_ACTIONS, ?SUPPORTED_WRITE_ACTIONS,
                  ?SUPPORTED_WRITE_ACTIONS, ?SUPPORTED_WRITE_ACTIONS}
      },
	{reply,Reply,State}.

%% Meters ----------------------------------------------------------------------

ofp_meter_mod(State,
              #ofp_meter_mod{} = _MeterMod) ->
	%%TODO
	Reply = #ofp_error_msg{type =meter_mod_failed,code =eperm},
	{reply,Reply,State}.

ofp_meter_stats_request(State,
                        #ofp_meter_stats_request{meter_id = _Id}) ->
	%%TODO
	Reply = #ofp_meter_stats_reply{body =[]},
	{reply,Reply,State}.

ofp_meter_config_request(State,
                         #ofp_meter_config_request{meter_id = _Id}) ->
	%%TODO
	Reply = #ofp_meter_config_reply{body =[]},
	{reply,Reply,State}.

ofp_meter_features_request(State, #ofp_meter_features_request{}) ->
    Reply = #ofp_meter_features_reply{max_meter = ?MAX,
                              band_types = [], %% ?SUPPORTED_BANDS,
                              capabilities = [], %% ?SUPPORTED_FLAGS,
                              max_bands = ?MAX_BANDS,
                              max_color = 0},
	{reply,Reply,State}.

%%%-----------------------------------------------------------------------------
%%% Helpers
%%%-----------------------------------------------------------------------------

get_env(Env) ->
    {ok, Value} = application:get_env(linc, Env),
    Value.

%%EOF
