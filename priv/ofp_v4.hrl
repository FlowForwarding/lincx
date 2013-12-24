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
%% @author Krzysztof Rutka <krzysztof.rutka@erlang-solutions.com>
%% @copyright 2012 FlowForwarding.org

-define(VERSION, 4).

%% Maximum values --------------------------------------------------------------

-define(OFPP_MAX, 16#ffffff00). %% port number
-define(OFPQ_MAX, 16#fffffffe). %% queue id
-define(OFPG_MAX, 16#ffffff00). %% group id
-define(OFPTT_MAX, 16#fe).      %% table id
-define(OFPCML_MAX, 16#ffe5).   %% buffer id
-define(OFPCML_NO_BUFFER, 16#ffff).   %% buffer id
-define(OFPM_MAX, 16#ffff0000). %% flow meter number

%% Message sizes (in bytes) ----------------------------------------------------

-define(FEATURES_REQUEST_SIZE, 8).
-define(FEATURES_REPLY_SIZE, 32).
-define(GET_CONFIG_REQUEST_SIZE, 8).
-define(GET_CONFIG_REPLY_SIZE, 12).
-define(SET_CONFIG_SIZE, 12).
-define(TABLE_MOD_SIZE, 16).
-define(FLOW_MOD_SIZE, 56).
-define(GROUP_MOD_SIZE, 16).
-define(PORT_MOD_SIZE, 40).
-define(DESC_STATS_REQUEST_SIZE, 16).
-define(DESC_STATS_REPLY_SIZE, 1072).
-define(FLOW_STATS_REQUEST_SIZE, 56).
-define(FLOW_STATS_REPLY_SIZE, 16).
-define(AGGREGATE_STATS_REQUEST_SIZE, 56).
-define(AGGREGATE_STATS_REPLY_SIZE, 40).
-define(TABLE_STATS_REQUEST_SIZE, 16).
-define(TABLE_STATS_REPLY_SIZE, 16).
-define(PORT_STATS_REQUEST_SIZE, 24).
-define(PORT_STATS_REPLY_SIZE, 16).
-define(QUEUE_STATS_REQUEST_SIZE, 24).
-define(QUEUE_STATS_REPLY_SIZE, 16).
-define(GROUP_STATS_REQUEST_SIZE, 24).
-define(GROUP_STATS_REPLY_SIZE, 16).
-define(GROUP_DESC_STATS_REQUEST_SIZE, 16).
-define(GROUP_DESC_STATS_REPLY_SIZE, 16).
-define(GROUP_FEATURES_STATS_REQUEST_SIZE, 16).
-define(GROUP_FEATURES_STATS_REPLY_SIZE, 56).
-define(EXPERIMENTER_STATS_REQUEST_SIZE, 24).
-define(EXPERIMENTER_STATS_REPLY_SIZE, 24).
-define(QUEUE_GET_CONFIG_REQUEST_SIZE, 16).
-define(QUEUE_GET_CONFIG_REPLY_SIZE, 16).
-define(PACKET_OUT_SIZE, 24).
-define(BARRIER_REQUEST_SIZE, 8).
-define(BARRIER_REPLY_SIZE, 8).
-define(ROLE_REQUEST_SIZE, 24).
-define(ROLE_REPLY_SIZE, 24).
-define(PACKET_IN_SIZE, 32).
-define(FLOW_REMOVED_SIZE, 56).
-define(PORT_STATUS_SIZE, 80).
-define(ERROR_SIZE, 12).
-define(ERROR_EXPERIMENTER_SIZE, 16).
-define(HELLO_SIZE, 8).
-define(ECHO_REQUEST_SIZE, 8).
-define(ECHO_REPLY_SIZE, 8).
-define(EXPERIMENTER_SIZE, 16).

%% Structure sizes (in bytes) --------------------------------------------------

-define(PORT_SIZE, 64).
-define(PACKET_QUEUE_SIZE, 16).
-define(QUEUE_PROP_MIN_RATE_SIZE, 16).
-define(QUEUE_PROP_MAX_RATE_SIZE, 16).
-define(QUEUE_PROP_EXPERIMENTER_SIZE, 16).
-define(OXM_FIELD_SIZE, 4).
-define(MATCH_SIZE, 8).
-define(INSTRUCTION_GOTO_TABLE_SIZE, 8).
-define(INSTRUCTION_WRITE_METADATA_SIZE, 24).
-define(INSTRUCTION_WRITE_ACTIONS_SIZE, 8).
-define(INSTRUCTION_APPLY_ACTIONS_SIZE, 8).
-define(INSTRUCTION_CLEAR_ACTIONS_SIZE, 8).
-define(INSTRUCTION_METER_SIZE, 8).
-define(INSTRUCTION_EXPERIMENTER_SIZE, 8).
-define(ACTION_COPY_TTL_IN_SIZE, 8).
-define(ACTION_POP_MPLS_SIZE, 8).
-define(ACTION_POP_VLAN_SIZE, 8).
-define(ACTION_POP_PBB_SIZE, 8).
-define(ACTION_PUSH_MPLS_SIZE, 8).
-define(ACTION_PUSH_VLAN_SIZE, 8).
-define(ACTION_PUSH_PBB_SIZE, 8).
-define(ACTION_COPY_TTL_OUT_SIZE, 8).
-define(ACTION_DEC_MPLS_TTL_SIZE, 8).
-define(ACTION_DEC_NW_TTL_SIZE, 8).
-define(ACTION_SET_MPLS_TTL_SIZE, 8).
-define(ACTION_SET_NW_TTL_SIZE, 8).
-define(ACTION_SET_FIELD_SIZE, 8).
-define(ACTION_SET_QUEUE_SIZE, 8).
-define(ACTION_GROUP_SIZE, 8).
-define(ACTION_OUTPUT_SIZE, 16).
-define(ACTION_EXPERIMENTER_SIZE, 8).
-define(BUCKET_SIZE, 16).
-define(BUCKET_COUNTER_SIZE, 16).
-define(FLOW_STATS_SIZE, 56).
-define(TABLE_STATS_SIZE, 24).
-define(OFP_TABLE_FEATURES_SIZE, 64).
-define(PORT_STATS_SIZE, 112).
-define(QUEUE_STATS_SIZE, 40).
-define(GROUP_STATS_SIZE, 40).
-define(GROUP_DESC_STATS_SIZE, 8).
-define(METER_BAND_SIZE, 16).
-define(METER_STATS_SIZE, 40).
-define(METER_CONFIG_SIZE, 8).

%% Misc sizes (in bytes) -------------------------------------------------------

-define(OFP_ETH_ALEN, 6).            %% ethernet address
-define(OFP_MAX_PORT_NAME_LEN, 16).  %% port name string
-define(OFP_MAX_TABLE_NAME_LEN, 32). %% table name string
-define(DESC_STR_LEN, 256).          %% switch description string
-define(SERIAL_NUM_LEN, 32).         %% serial number string

%% OXM Vlan Id values -----------------------------------------------------------

-define(OFPVID_PRESENT, 16#1000).
-define(OFPVID_NONE, 16#0000).

%%%-----------------------------------------------------------------------------
%%% OFP Message Body
%%%-----------------------------------------------------------------------------

-type ofp_message_body() ::
        %% Immutable messages
        ofp_error_msg()
      | ofp_echo_request()
      | ofp_echo_reply()
      | ofp_experimenter()
        %% Switch configuration messages
      | ofp_features_request()
      | ofp_features_reply()
      | ofp_get_config_request()
      | ofp_get_config_reply()
      | ofp_set_config()
        %% Asynchronous messages
      | ofp_packet_in()
      | ofp_flow_removed()
      | ofp_port_status()
        %% Controller command messages
      | ofp_packet_out()
      | ofp_flow_mod()
      | ofp_group_mod()
      | ofp_port_mod()
      | ofp_table_mod()
        %% Multipart messages
      | ofp_multipart_request()
      | ofp_multipart_reply()
        %% Barrier messages
      | ofp_barrier_request()
      | ofp_barrier_reply()
        %% Queue configuration messages
      | ofp_queue_get_config_request()
      | ofp_queue_get_config_reply()
        %% Controller role change request messages
      | ofp_role_request()
      | ofp_role_reply()
        %% Asynchronous message configuration
      | ofp_get_async_request()
      | ofp_get_async_reply()
      | ofp_set_async()
        %% Meters and rate limiters configuration messages
      | ofp_meter_mod().

%%%-----------------------------------------------------------------------------
%%% Common Structures (A 2)
%%%-----------------------------------------------------------------------------

%%%-----------------------------------------------------------------------------
%%% Port Structures (A 2.1)
%%%-----------------------------------------------------------------------------

-type ofp_port_config() :: port_down
                         | no_recv
                         | no_fwd
                         | no_packet_in.

-type ofp_port_state() :: link_down
                        | blocked
                        | live.

-type ofp_port_reserved() :: in_port
                           | table
                           | normal
                           | flood
                           | all
                           | controller
                           | local
                           | any.

-type ofp_port_no() :: integer()
                     | ofp_port_reserved().

-type ofp_port_feature() :: '10mb_hd'
                          | '10mb_fd'
                          | '100mb_hd'
                          | '100mb_fd'
                          | '1gb_hd'
                          | '1gb_fd'
                          | '10gb_fd'
                          | '40gb_fd'
                          | '100gb_fd'
                          | '1tb_fd'
                          | other
                          | copper
                          | fiber
                          | autoneg
                          | pause
                          | pause_asym.

-record(ofp_port, {
          port_no :: ofp_port_no(),
          hw_addr :: binary(),
          name :: binary(),
          config = [] :: [ofp_port_config()],
          state = [] :: [ofp_port_state()],
          curr = [] :: [ofp_port_feature()],
          advertised = [] :: [ofp_port_feature()],
          supported = [] :: [ofp_port_feature()],
          peer = [] :: [ofp_port_feature()],
          curr_speed = 0 :: integer(),
          max_speed = 0 :: integer()
         }).
-type ofp_port() :: #ofp_port{}.

%%%-----------------------------------------------------------------------------
%%% Queue Structures (A 2.2)
%%%-----------------------------------------------------------------------------

%% Queue rates are given in permiles. Value > 1000 means QoS is disabled.

-type ofp_queue_id() :: integer()
                      | all.

-record(ofp_queue_prop_min_rate, {
          rate :: integer()
         }).

-record(ofp_queue_prop_max_rate, {
          rate :: integer()
         }).

-record(ofp_queue_prop_experimenter, {
          experimenter :: integer(),
          data = <<>> :: binary()
         }).

-type ofp_queue_property() :: #ofp_queue_prop_min_rate{} |
                              #ofp_queue_prop_max_rate{} |
                              #ofp_queue_prop_experimenter{}.

-record(ofp_packet_queue, {
          queue_id :: ofp_queue_id(),
          port_no :: ofp_port_no(),
          properties :: [ofp_queue_property()]
         }).
-type ofp_packet_queue() :: #ofp_packet_queue{}.

%%%-----------------------------------------------------------------------------
%%% Flow Match Structures (A 2.3)
%%%-----------------------------------------------------------------------------

%% Flow Match Header (A 2.3.1) -------------------------------------------------

-record(ofp_match, {
          fields = []  :: [ofp_field()]
         }).
-type ofp_match() :: #ofp_match{}.

%% OXM classes (A 2.3.3) -------------------------------------------------------

-type ofp_field_class() :: nxm_0
                         | nxm_1
                         | openflow_basic
                         | experimenter.

%% Flow Match Fields (A 2.3.7) -------------------------------------------------

-type openflow_basic_type() :: in_port
                             | in_phy_port
                             | metadata
                             | eth_dst
                             | eth_src
                             | eth_type
                             | vlan_vid
                             | vlan_pcp
                             | ip_dscp
                             | ip_ecn
                             | ip_proto
                             | ipv4_src
                             | ipv4_dst
                             | tcp_src
                             | tcp_dst
                             | udp_src
                             | udp_dst
                             | sctp_src
                             | sctp_dst
                             | icmpv4_type
                             | icmpv4_code
                             | arp_op
                             | arp_spa
                             | arp_tpa
                             | arp_sha
                             | arp_tha
                             | ipv6_src
                             | ipv6_dst
                             | ipv6_label
                             | icmpv6_type
                             | icmpv6_code
                             | ipv6_nd_target
                             | ipv6_nd_sll
                             | ipv6_nd_tll
                             | mpls_label
                             | mpls_tc
                             | mpls_bos
                             | pbb_isid
                             | tunnel_id
                             | ipv6_exthdr.

-type ofp_field_type() :: openflow_basic_type().

-type ofp_vlan_id() :: present
                     | none.

-type ofp_ipv6exthdr_flags() :: nonext
                              | esp
                              | auth
                              | dest
                              | frag
                              | router
                              | hop
                              | unrep
                              | unseq.

%% OXM field
-record(ofp_field, {
          class = openflow_basic :: ofp_field_class(),
          name :: ofp_field_type(),
          has_mask = false :: boolean(),
          value :: bitstring(),
          mask :: bitstring()
         }).
-type ofp_field() :: #ofp_field{}.

%%%-----------------------------------------------------------------------------
%%% Flow Instruction Structures (A 2.4)
%%%-----------------------------------------------------------------------------

-record(ofp_instruction_meter, {
          seq = 1,
          meter_id :: meter_id()
         }).

-record(ofp_instruction_apply_actions, {
          seq = 2,
          actions :: [ofp_action()]
         }).

-record(ofp_instruction_clear_actions, {
          seq = 3
         }).

-record(ofp_instruction_write_actions, {
          seq = 4,
          actions :: [ofp_action()]
         }).

-record(ofp_instruction_write_metadata, {
          seq = 5,
          metadata                 :: binary(),
          metadata_mask = <<1:64>> :: binary()
         }).

-record(ofp_instruction_goto_table, {
          seq = 6,
          table_id :: integer()
         }).

-record(ofp_instruction_experimenter, {
          seq = 7,
          experimenter :: integer(),
          data = <<>> :: binary()
         }).

-type ofp_instruction() :: #ofp_instruction_meter{}
                          | #ofp_instruction_apply_actions{}
                          | #ofp_instruction_clear_actions{}
                          | #ofp_instruction_write_actions{}
                          | #ofp_instruction_write_metadata{}
                          | #ofp_instruction_goto_table{}
                          | #ofp_instruction_experimenter{}.

%%%-----------------------------------------------------------------------------
%%% Action Structures (A 2.5)
%%%-----------------------------------------------------------------------------

-record(ofp_action_copy_ttl_in, {
          seq = 1
         }).

-record(ofp_action_pop_mpls, {
          seq = 2,
          ethertype :: integer()
         }).

-record(ofp_action_pop_pbb, {
          seq = 3
         }).

-record(ofp_action_pop_vlan, {
          seq = 4
         }).

-record(ofp_action_push_mpls, {
          seq = 5,
          ethertype :: integer()
         }).

-record(ofp_action_push_pbb, {
          seq = 6,
          ethertype :: integer()
         }).

-record(ofp_action_push_vlan, {
          seq = 7,
          ethertype :: integer()
         }).

-record(ofp_action_copy_ttl_out, {
          seq = 8
         }).

-record(ofp_action_dec_mpls_ttl, {
          seq = 9
         }).

-record(ofp_action_dec_nw_ttl, {
          seq = 10
         }).

-record(ofp_action_set_mpls_ttl, {
          seq = 11,
          mpls_ttl :: integer()
         }).

-record(ofp_action_set_nw_ttl, {
          seq = 12,
          nw_ttl :: integer()
         }).

-record(ofp_action_set_field, {
          seq = 13,
          field :: ofp_field()
         }).

-record(ofp_action_set_queue, {
          seq = 14,
          queue_id :: integer()
         }).

-record(ofp_action_group, {
          seq = 15,
          group_id :: integer()
         }).

-record(ofp_action_output, {
          seq = 16,
          port :: ofp_port_no(),
          max_len = no_buffer :: ofp_packet_in_bytes()
         }).

-record(ofp_action_experimenter, {
          seq = 99,
          experimenter :: integer(),
          data = <<>> :: binary()
         }).

-type ofp_action_type() :: copy_ttl_in
                         | pop_mpls
                         | pop_pbb
                         | pop_vlan
                         | push_mpls
                         | push_pbb
                         | push_vlan
                         | copy_ttl_out
                         | dec_mpls_ttl
                         | dec_nw_ttl
                         | set_mpls_ttl
                         | set_nw_ttl
                         | set_field
                         | set_queue
                         | group
                         | output
                         | experimenter.

-type ofp_action() :: #ofp_action_copy_ttl_in{}
                    | #ofp_action_pop_mpls{}
                    | #ofp_action_pop_pbb{}
                    | #ofp_action_pop_vlan{}
                    | #ofp_action_push_mpls{}
                    | #ofp_action_push_pbb{}
                    | #ofp_action_push_vlan{}
                    | #ofp_action_copy_ttl_out{}
                    | #ofp_action_dec_mpls_ttl{}
                    | #ofp_action_dec_nw_ttl{}
                    | #ofp_action_set_mpls_ttl{}
                    | #ofp_action_set_nw_ttl{}
                    | #ofp_action_set_field{}
                    | #ofp_action_set_queue{}
                    | #ofp_action_group{}
                    | #ofp_action_output{}
                    | #ofp_action_experimenter{}.

%%%-----------------------------------------------------------------------------
%%% Controller-to-Switch Messages (A 3)
%%%-----------------------------------------------------------------------------

%%%-----------------------------------------------------------------------------
%%% Handshake (A 3.1)
%%%-----------------------------------------------------------------------------

-record(ofp_features_request, {}).
-type ofp_features_request() :: #ofp_features_request{}.

-type ofp_switch_capability() :: flow_stats
                               | table_stats
                               | port_stats
                               | group_stats
                               | ip_reasm
                               | queue_stats
                               | port_blocked.

-record(ofp_features_reply, {
          datapath_mac :: binary(),
          datapath_id :: integer(),
          n_buffers :: integer(),
          n_tables :: integer(),
          auxiliary_id :: integer(),
          capabilities = [] :: [ofp_switch_capability()]
         }).
-type ofp_features_reply() :: #ofp_features_reply{}.

%%%-----------------------------------------------------------------------------
%%% Switch Configuration (A 3.2)
%%%-----------------------------------------------------------------------------

-record(ofp_get_config_request, {}).
-type ofp_get_config_request() :: #ofp_get_config_request{}.

-type ofp_config_flags() :: frag_drop
                          | frag_reasm.

-type ofp_buffer_id() :: integer()
                       | no_buffer.

-record(ofp_get_config_reply, {
          flags = [] :: [ofp_config_flags()],
          miss_send_len :: ofp_packet_in_bytes()
         }).
-type ofp_get_config_reply() :: #ofp_get_config_reply{}.

-record(ofp_set_config, {
          flags = [] :: [ofp_config_flags()],
          miss_send_len :: ofp_packet_in_bytes()
         }).
-type ofp_set_config() :: #ofp_set_config{}.

%%%-----------------------------------------------------------------------------
%%% Flow Table Configuration (A 3.3)
%%%-----------------------------------------------------------------------------

-type ofp_table_id() :: all
                      | integer().

-record(ofp_table_mod, {
          table_id = all :: ofp_table_id()
         }).
-type ofp_table_mod() :: #ofp_table_mod{}.

%%%-----------------------------------------------------------------------------
%%% Nodify State Messages (A 3.4)
%%%-----------------------------------------------------------------------------

%%% Modify Flow Entry Message (A 3.4.1) ----------------------------------------

-type ofp_flow_mod_command() :: add
                              | modify
                              | modify_strict
                              | delete
                              | delete_strict.

-type ofp_flow_mod_flag() :: send_flow_rem
                           | check_overlap
                           | reset_counts
                           | no_pkt_counts
                           | no_byt_counts.

-record(ofp_flow_mod, {
          cookie = <<0:64>> :: binary(),
          cookie_mask = <<0:64>> :: binary(),
          table_id = all :: ofp_table_id(),
          command :: ofp_flow_mod_command(),
          idle_timeout = 0 :: integer(),
          hard_timeout = 0 :: integer(),
          priority = 0 :: integer(),
          buffer_id = no_buffer :: ofp_buffer_id(),
          out_port = any :: ofp_port_no(),
          out_group = any :: ofp_group_id(),
          flags = [] :: [ofp_flow_mod_flag()],
          match = #ofp_match{} :: ofp_match(),
          instructions = [] :: [ofp_instruction()]
         }).
-type ofp_flow_mod() :: #ofp_flow_mod{}.

%%% Modify Group Entry Message (A 3.4.2) ---------------------------------------

-type ofp_group_mod_command() :: add
                               | modify
                               | delete.

-type ofp_group_type() :: all
                        | select
                        | indirect
                        | ff.

-type ofp_group_id() :: integer()
                      | any
                      | all.

-record(ofp_bucket, {
          weight = 1 :: integer(),
          watch_port = 0 :: integer(),
          watch_group = 0 :: integer(),
          actions = [] :: [ofp_action()]
         }).
-type ofp_bucket() :: #ofp_bucket{}.

-record(ofp_group_mod, {
          command :: ofp_group_mod_command(),
          type :: ofp_group_type(),
          group_id :: ofp_group_id(),
          buckets = [] :: [ofp_bucket()]
         }).
-type ofp_group_mod() :: #ofp_group_mod{}.

%%% Port Modification Message (A 3.4.3) ----------------------------------------

-record(ofp_port_mod, {
          port_no :: ofp_port_no(),
          hw_addr :: binary(),
          config = [] :: [ofp_port_config()],
          mask = [] :: [ofp_port_config()],
          advertise = [] :: [ofp_port_feature()]
         }).
-type ofp_port_mod() :: #ofp_port_mod{}.

%%% Meter Modification Message (A 3.4.4) ---------------------------------------

-type meter_id() :: integer()
                  | slowpath
                  | controller
                  | all.

-type ofp_meter_mod_command() :: add
                               | modify
                               | delete.

-type ofp_meter_flag() :: kbps
                        | pktps
                        | burst
                        | stats.

-type ofp_meter_band_type() :: drop
                             | dscp_remark
                             | experimenter.

-record(ofp_meter_band_drop, {
          type = drop :: ofp_meter_band_type(),
          rate = -1 :: integer(),
          burst_size = -1 :: integer()
         }).

-record(ofp_meter_band_dscp_remark, {
          type = dscp_remark :: ofp_meter_band_type(),
          rate = -1 :: integer(),
          burst_size = -1 :: integer(),
          prec_level = 1 :: integer()
         }).

-record(ofp_meter_band_experimenter, {
          type = experimenter :: ofp_meter_band_type(),
          rate = -1 :: integer(),
          burst_size = -1 :: integer(),
          experimenter :: integer()
         }).

-type ofp_meter_band() :: #ofp_meter_band_drop{}
                        | #ofp_meter_band_dscp_remark{}
                        | #ofp_meter_band_experimenter{}.

-record(ofp_meter_mod, {
          command :: ofp_meter_mod_command(),
          %% Warning: OpenFlow 1.3.0 spec defines flags field as a field that
          %% contains only one value. Page 58 section A.3.4.4.
          flags = [kbps, stats] :: [ofp_meter_flag()],
          meter_id :: meter_id(),
          bands = [] :: [ofp_meter_band()]
         }).
-type ofp_meter_mod() :: #ofp_meter_mod{}.

%%%-----------------------------------------------------------------------------
%%% Multipart Messages (A 3.5)
%%%-----------------------------------------------------------------------------

-type ofp_multipart_request_flag() :: more.

-type ofp_multipart_reply_flag() :: more.

%%% Description (A 3.5.1) ------------------------------------------------------

-record(ofp_desc_request, {
          flags = [] :: [ofp_multipart_request_flag()]
         }).
-type ofp_desc_request() :: #ofp_desc_request{}.

-record(ofp_desc_reply, {
          flags = [] :: [ofp_multipart_reply_flag()],
          mfr_desc :: binary(),
          hw_desc :: binary(),
          sw_desc :: binary(),
          serial_num :: binary(),
          dp_desc :: binary()
         }).
-type ofp_desc_reply() :: #ofp_desc_reply{}.

%%% Individual Flow Statistics (A 3.5.2) ---------------------------------------

-record(ofp_flow_stats, {
          table_id :: ofp_table_id(),
          duration_sec :: integer(),
          duration_nsec :: integer(),
          priority :: integer(),
          idle_timeout :: integer(),
          hard_timeout :: integer(),
          flags = [] :: [ofp_flow_mod_flag()],
          cookie :: binary(),
          packet_count :: integer(),
          byte_count :: integer(),
          match :: ofp_match(),
          instructions = [] :: [ofp_instruction()]
         }).
-type ofp_flow_stats() :: #ofp_flow_stats{}.

-record(ofp_flow_stats_request, {
          flags = [] :: [ofp_multipart_request_flag()],
          table_id = all :: ofp_table_id(),
          out_port = any :: ofp_port_no(),
          out_group = any :: ofp_group_id(),
          cookie = <<0:64>> :: binary(),
          cookie_mask = <<0:64>> :: binary(),
          match = #ofp_match{} :: ofp_match()
         }).
-type ofp_flow_stats_request() :: #ofp_flow_stats_request{}.

-record(ofp_flow_stats_reply, {
          flags = [] :: [ofp_multipart_reply_flag()],
          body = [] :: [ofp_flow_stats()]
         }).
-type ofp_flow_stats_reply() :: #ofp_flow_stats_reply{}.

%%% Aggregate Flow Statistics (A 3.5.3) ----------------------------------------

-record(ofp_aggregate_stats_request, {
          flags = [] :: [ofp_multipart_request_flag()],
          table_id = all :: ofp_table_id(),
          out_port = any :: ofp_port_no(),
          out_group = any :: ofp_group_id(),
          cookie = <<0:64>> :: binary(),
          cookie_mask = <<0:64>> :: binary(),
          match = #ofp_match{} :: ofp_match()
         }).
-type ofp_aggregate_stats_request() :: #ofp_aggregate_stats_request{}.

-record(ofp_aggregate_stats_reply, {
          flags = [] :: [ofp_multipart_reply_flag()],
          packet_count = 0 :: integer(),
          byte_count = 0 :: integer(),
          flow_count = 0 :: integer()
         }).
-type ofp_aggregate_stats_reply() :: #ofp_aggregate_stats_reply{}.

%%% Table Statistics (A 3.5.4) -------------------------------------------------

-record(ofp_table_stats, {
          table_id :: ofp_table_id(),
          active_count :: integer(),
          lookup_count :: integer(),
          matched_count :: integer()
         }).
-type ofp_table_stats() :: #ofp_table_stats{}.

-record(ofp_table_stats_request, {
          flags = [] :: [ofp_multipart_request_flag()]
         }).
-type ofp_table_stats_request() :: #ofp_table_stats_request{}.

-record(ofp_table_stats_reply, {
          flags = [] :: [ofp_multipart_reply_flag()],
          body = [] :: [ofp_table_stats()]
         }).
-type ofp_table_stats_reply() :: #ofp_table_stats_reply{}.

%% A.3.5.5 Table Features ------------------------------------------------------

-record(ofp_table_feature_prop_instructions, {
          instruction_ids = [] :: [atom() | {experimenter, integer()}]
         }).

-record(ofp_table_feature_prop_instructions_miss, {
          instruction_ids = [] :: [atom() | {experimenter, integer()}]
         }).

-record(ofp_table_feature_prop_next_tables, {
          next_table_ids = [] :: [integer()]
         }).

-record(ofp_table_feature_prop_next_tables_miss, {
          next_table_ids = [] :: [integer()]
         }).

-record(ofp_table_feature_prop_write_actions, {
          action_ids = [] :: [atom() | {experimenter, integer()}]
         }).

-record(ofp_table_feature_prop_write_actions_miss, {
          action_ids = [] :: [atom() | {experimenter, integer()}]
         }).

-record(ofp_table_feature_prop_apply_actions, {
          action_ids = [] :: [atom() | {experimenter, integer()}]
         }).

-record(ofp_table_feature_prop_apply_actions_miss, {
          action_ids = [] :: [atom() | {experimenter, integer()}]
         }).

-record(ofp_table_feature_prop_match, {
          oxm_ids = [] :: [atom() | {experimenter, integer()}]
         }).

-record(ofp_table_feature_prop_wildcards, {
          oxm_ids = [] :: [atom() | {experimenter, integer()}]
         }).

-record(ofp_table_feature_prop_write_setfield, {
          oxm_ids = [] :: [atom() | {experimenter, integer()}]
         }).

-record(ofp_table_feature_prop_write_setfield_miss, {
          oxm_ids = [] :: [atom() | {experimenter, integer()}]
         }).

-record(ofp_table_feature_prop_apply_setfield, {
          oxm_ids = [] :: [atom() | {experimenter, integer()}]
         }).

-record(ofp_table_feature_prop_apply_setfield_miss, {
          oxm_ids = [] :: [atom() | {experimenter, integer()}]
         }).

-record(ofp_table_feature_prop_experimenter, {
          experimenter :: integer(),
          exp_type :: integer(),
          data = <<>> :: binary()
         }).

-record(ofp_table_feature_prop_experimenter_miss, {
          experimenter :: integer(),
          exp_type :: integer(),
          data = <<>> :: binary()
         }).

-type ofp_table_feature_property() ::
        #ofp_table_feature_prop_instructions{}
      | #ofp_table_feature_prop_instructions_miss{}
      | #ofp_table_feature_prop_next_tables{}
      | #ofp_table_feature_prop_next_tables_miss{}
      | #ofp_table_feature_prop_write_actions{}
      | #ofp_table_feature_prop_write_actions_miss{}
      | #ofp_table_feature_prop_apply_actions{}
      | #ofp_table_feature_prop_apply_actions_miss{}
      | #ofp_table_feature_prop_match{}
      | #ofp_table_feature_prop_wildcards{}
      | #ofp_table_feature_prop_write_setfield{}
      | #ofp_table_feature_prop_write_setfield_miss{}
      | #ofp_table_feature_prop_apply_setfield{}
      | #ofp_table_feature_prop_apply_setfield_miss{}
      | #ofp_table_feature_prop_experimenter{}
      | #ofp_table_feature_prop_experimenter_miss{}.

-record(ofp_table_features, {
          table_id :: ofp_table_id(),
          name :: bitstring(),
          metadata_match :: binary(),
          metadata_write :: binary(),
          max_entries :: integer(),
          properties = [] :: [ofp_table_feature_property()]
         }).

-record(ofp_table_features_request, {
          flags = [] :: [ofp_multipart_request_flag()],
          body = [] :: [#ofp_table_features{}]
         }).

-record(ofp_table_features_reply, {
          flags = [] :: [ofp_multipart_reply_flag()],
          body = [] :: [#ofp_table_features{}]
         }).

%%% Port Statistics (A 3.5.6) --------------------------------------------------

-record(ofp_port_stats, {
          port_no          :: ofp_port_no(),
          rx_packets    = 0 :: integer(),
          tx_packets    = 0 :: integer(),
          rx_bytes      = 0 :: integer(),
          tx_bytes      = 0 :: integer(),
          rx_dropped    = 0 :: integer(),
          tx_dropped    = 0 :: integer(),
          rx_errors     = 0 :: integer(),
          tx_errors     = 0 :: integer(),
          rx_frame_err  = 0 :: integer(),
          rx_over_err   = 0 :: integer(),
          rx_crc_err    = 0 :: integer(),
          collisions    = 0 :: integer(),
          duration_sec  = 0 :: integer(),
          duration_nsec = 0 :: integer()
         }).
-type ofp_port_stats() :: #ofp_port_stats{}.

-record(ofp_port_stats_request, {
          flags = [] :: [ofp_multipart_request_flag()],
          port_no :: ofp_port_no()
         }).
-type ofp_port_stats_request() :: #ofp_port_stats_request{}.

-record(ofp_port_stats_reply, {
          flags = [] :: [ofp_multipart_reply_flag()],
          body = [] :: [ofp_port_stats()]
         }).
-type ofp_port_stats_reply() :: #ofp_port_stats_reply{}.

%%% Port Description (A 3.5.7) -------------------------------------------------

-record(ofp_port_desc_request, {
          flags = [] :: [ofp_multipart_request_flag()]
         }).
-type ofp_port_desc_request() :: #ofp_port_desc_request{}.

-record(ofp_port_desc_reply, {
          flags = [] :: [ofp_multipart_reply_flag()],
          body = [] :: [ofp_port()]
         }).
-type ofp_port_desc_reply() :: #ofp_port_desc_reply{}.

%%% Queue Statistics (A 3.5.8) -------------------------------------------------

-record(ofp_queue_stats, {
          port_no        :: ofp_port_no(),
          queue_id       :: ofp_queue_id(),
          tx_bytes   = 0 :: integer(),
          tx_packets = 0 :: integer(),
          tx_errors  = 0 :: integer(),
          duration_sec  = 0 :: integer(),
          duration_nsec = 0 :: integer()
         }).
-type ofp_queue_stats() :: #ofp_queue_stats{}.

-record(ofp_queue_stats_request, {
          flags = [] :: [ofp_multipart_request_flag()],
          port_no :: ofp_port_no(),
          queue_id :: ofp_queue_id()
         }).
-type ofp_queue_stats_request() :: #ofp_queue_stats_request{}.

-record(ofp_queue_stats_reply, {
          flags = [] :: [ofp_multipart_reply_flag()],
          body = [] :: [ofp_queue_stats()]
         }).
-type ofp_queue_stats_reply() :: #ofp_queue_stats_reply{}.

%%% Group Statistics (A 3.5.9) -------------------------------------------------

-record(ofp_bucket_counter, {
          packet_count = 0 :: integer(),
          byte_count   = 0 :: integer()
         }).
-type ofp_bucket_counter() :: #ofp_bucket_counter{}.

-record(ofp_group_stats, {
          group_id :: ofp_group_id(),
          ref_count :: integer(),
          packet_count :: integer(),
          byte_count :: integer(),
          duration_sec = 0 :: integer(),
          duration_nsec = 0 :: integer(),
          bucket_stats = [] :: [ofp_bucket_counter()]
         }).
-type ofp_group_stats() :: #ofp_group_stats{}.

-record(ofp_group_stats_request, {
          flags = [] :: [ofp_multipart_request_flag()],
          group_id = all :: ofp_group_id()
         }).
-type ofp_group_stats_request() :: #ofp_group_stats_request{}.

-record(ofp_group_stats_reply, {
          flags = [] :: [ofp_multipart_reply_flag()],
          body = [] :: [ofp_group_stats()]
         }).
-type ofp_group_stats_reply() :: #ofp_group_stats_reply{}.

%%% Group Description (A 3.5.10) -----------------------------------------------

-record(ofp_group_desc_stats, {
          type :: ofp_group_type(),
          group_id :: ofp_group_id(),
          buckets = [] :: [ofp_bucket()]
         }).
-type ofp_group_desc_stats() :: #ofp_group_desc_stats{}.

-record(ofp_group_desc_request, {
          flags = [] :: [ofp_multipart_request_flag()]
         }).
-type ofp_group_desc_request() :: #ofp_group_desc_request{}.

-record(ofp_group_desc_reply, {
          flags = [] :: [ofp_multipart_reply_flag()],
          body = [] :: [ofp_group_desc_stats()]
         }).
-type ofp_group_desc_reply() :: #ofp_group_desc_reply{}.

%%% Group Features (A 3.5.11) --------------------------------------------------

-type ofp_group_capabilities() :: select_weight
                                | select_liveness
                                | chaining
                                | chaining_checks.

-record(ofp_group_features_request, {
          flags = [] :: [ofp_multipart_request_flag()]
         }).
-type ofp_group_features_request() :: #ofp_group_features_request{}.

-record(ofp_group_features_reply, {
          flags = [] :: [ofp_multipart_reply_flag()],
          types        = []               :: [ofp_group_type()],
          capabilities = []               :: [ofp_group_capabilities()],
          max_groups   = {0,0,0,0}        :: {integer(), integer(),
                                              integer(), integer()},
          actions      = {[], [], [], []} :: {[ofp_action_type()],
                                              [ofp_action_type()],
                                              [ofp_action_type()],
                                              [ofp_action_type()]}
         }).
-type ofp_group_features_reply() :: #ofp_group_features_reply{}.

%%% Meter Statistics (A 3.5.12) ------------------------------------------------

-record(ofp_meter_band_stats, {
          packet_band_count :: integer(),
          byte_band_count :: integer()
         }).
-type ofp_meter_band_stats() :: #ofp_meter_band_stats{}.

-record(ofp_meter_stats, {
          meter_id :: meter_id(),
          flow_count :: integer(),
          packet_in_count :: integer(),
          byte_in_count :: integer(),
          duration_sec :: integer(),
          duration_nsec :: integer(),
          band_stats = [] :: [ofp_meter_band_stats()]
         }).
-type ofp_meter_stats() :: #ofp_meter_stats{}.

-record(ofp_meter_stats_request, {
          flags = [] :: [ofp_multipart_request_flag()],
          meter_id :: meter_id()
         }).
-type ofp_meter_stats_request() :: #ofp_meter_stats_request{}.

-record(ofp_meter_stats_reply, {
          flags = [] :: [ofp_multipart_reply_flag()],
          body = [] :: [ofp_meter_stats()]
         }).
-type ofp_meter_stats_reply() :: #ofp_meter_stats_reply{}.

%%% Meter Configuration Statistics (A 3.5.13) ----------------------------------

-record(ofp_meter_config, {
          flags = [] :: [ofp_meter_mod_command()],
          meter_id :: meter_id(),
          bands = [] :: [ofp_meter_band()]
         }).
-type ofp_meter_config() :: #ofp_meter_config{}.

-record(ofp_meter_config_request, {
          flags = [] :: [ofp_multipart_request_flag()],
          meter_id :: meter_id()
         }).
-type ofp_meter_config_request() :: #ofp_meter_config_request{}.

-record(ofp_meter_config_reply, {
          flags = [] :: [ofp_multipart_reply_flag()],
          body = [] :: [ofp_meter_config()]
         }).
-type ofp_meter_config_reply() :: #ofp_meter_config_reply{}.

%%% Meter Features Statistics (A 3.5.14) ---------------------------------------

-record(ofp_meter_features_request, {
          flags = [] :: [ofp_multipart_request_flag()]
         }).
-type ofp_meter_features_request() :: #ofp_meter_features_request{}.

-record(ofp_meter_features_reply, {
          flags = [] :: [ofp_multipart_reply_flag()],
          max_meter :: integer(),
          band_types = [] :: [ofp_meter_band_type()],
          capabilities = [] :: [ofp_meter_flag()],
          max_bands :: integer(),
          max_color :: integer()
         }).
-type ofp_meter_features_reply() :: #ofp_meter_features_reply{}.

%%% Experimenter Multipart (A 3.5.15) ------------------------------------------

-record(ofp_experimenter_request, {
          flags = [] :: [ofp_multipart_request_flag()],
          experimenter :: integer(),
          exp_type :: integer(),
          data = <<>> :: binary()
         }).
-type ofp_experimenter_request() :: #ofp_experimenter_request{}.

-record(ofp_experimenter_reply, {
          flags = [] :: [ofp_multipart_reply_flag()],
          experimenter :: integer(),
          exp_type :: integer(),
          data = <<>> :: binary()
         }).
-type ofp_experimenter_reply() :: #ofp_experimenter_reply{}.

-type ofp_multipart_request() :: ofp_desc_request()
                               | ofp_flow_stats_request()
                               | ofp_aggregate_stats_request()
                               | ofp_table_stats_request()
                               | #ofp_table_features_request{}
                               | ofp_port_desc_request()
                               | ofp_port_stats_request()
                               | ofp_queue_stats_request()
                               | ofp_group_stats_request()
                               | ofp_group_desc_request()
                               | ofp_group_features_request()
                               | ofp_meter_stats_request()
                               | ofp_meter_config_request()
                               | ofp_meter_features_request()
                               | ofp_experimenter_request().

-type ofp_multipart_reply() :: ofp_desc_reply()
                             | ofp_flow_stats_reply()
                             | ofp_aggregate_stats_reply()
                             | ofp_table_stats_reply()
                             | #ofp_table_features_reply{}
                             | ofp_port_stats_reply()
                             | ofp_port_desc_reply()
                             | ofp_queue_stats_reply()
                             | ofp_group_stats_reply()
                             | ofp_group_desc_reply()
                             | ofp_group_features_reply()
                             | ofp_meter_stats_reply()
                             | ofp_meter_config_reply()
                             | ofp_meter_features_reply()
                             | ofp_experimenter_reply().

%%%-----------------------------------------------------------------------------
%%% Queue Configuration Messages (A 3.6)
%%%-----------------------------------------------------------------------------

-record(ofp_queue_get_config_request, {
          port :: ofp_port_no()
         }).
-type ofp_queue_get_config_request() :: #ofp_queue_get_config_request{}.

-record(ofp_queue_get_config_reply, {
          port :: ofp_port_no(),
          queues = [] :: [ofp_packet_queue()]
         }).
-type ofp_queue_get_config_reply() :: #ofp_queue_get_config_reply{}.

%%%-----------------------------------------------------------------------------
%%% Packet-Out Message (A 3.7)
%%%-----------------------------------------------------------------------------

-record(ofp_packet_out, {
          buffer_id = no_buffer :: ofp_buffer_id(),
          in_port = controller :: controller,
          actions = [] :: [ofp_action()],
          data = <<>> :: binary()
         }).
-type ofp_packet_out() :: #ofp_packet_out{}.

%%%-----------------------------------------------------------------------------
%%% Barrier Message (A 3.8)
%%%-----------------------------------------------------------------------------

-record(ofp_barrier_request, {}).
-type ofp_barrier_request() :: #ofp_barrier_request{}.

-record(ofp_barrier_reply, {}).
-type ofp_barrier_reply() :: #ofp_barrier_reply{}.

%%%-----------------------------------------------------------------------------
%%% Role Request Message (A 3.9)
%%%-----------------------------------------------------------------------------

-type ofp_controller_role() :: nochange
                             | equal
                             | master
                             | slave.

-record(ofp_role_request, {
          role :: ofp_controller_role(),
          generation_id :: integer()
         }).
-type ofp_role_request() :: #ofp_role_request{}.

-record(ofp_role_reply, {
          role :: ofp_controller_role(),
          generation_id :: integer()
         }).
-type ofp_role_reply() :: #ofp_role_reply{}.

%%%-----------------------------------------------------------------------------
%%% Set Asynchronous Configuration Message (A 3.10)
%%%-----------------------------------------------------------------------------

-record(ofp_get_async_request, {}).
-type ofp_get_async_request() :: #ofp_get_async_request{}.

-record(ofp_get_async_reply, {
          packet_in_mask = {[], []} :: {[ofp_packet_in_reason()],
                                        [ofp_packet_in_reason()]},
          port_status_mask = {[], []} :: {[ofp_port_status_reason()],
                                          [ofp_port_status_reason()]},
          flow_removed_mask = {[], []} :: {[ofp_flow_removed_reason()],
                                           [ofp_flow_removed_reason()]}
         }).
-type ofp_get_async_reply() :: #ofp_get_async_reply{}.

-record(ofp_set_async, {
          packet_in_mask = {[], []} :: {[ofp_packet_in_reason()],
                                        [ofp_packet_in_reason()]},
          port_status_mask = {[], []} :: {[ofp_port_status_reason()],
                                          [ofp_port_status_reason()]},
          flow_removed_mask = {[], []} :: {[ofp_flow_removed_reason()],
                                           [ofp_flow_removed_reason()]}
         }).
-type ofp_set_async() :: #ofp_set_async{}.

%%%-----------------------------------------------------------------------------
%%% Asynchronous Messages (A 4)
%%%-----------------------------------------------------------------------------

%%%-----------------------------------------------------------------------------
%%% Packet-In Message (A 4.1)
%%%-----------------------------------------------------------------------------

-type ofp_packet_in_reason() :: no_match
                              | action
                              | invalid_ttl.

-type ofp_packet_in_bytes() :: integer()
                             | no_buffer.

-record(ofp_packet_in, {
          buffer_id = no_buffer :: ofp_buffer_id(),
          reason                :: ofp_packet_in_reason(),
          table_id              :: integer(),
          cookie = <<-1:64>>    :: binary(),
          match = #ofp_match{}  :: ofp_match(),
          data = <<>>           :: binary()
         }).
-type ofp_packet_in() :: #ofp_packet_in{}.

%%%-----------------------------------------------------------------------------
%%% Flow Removed Message (A 4.2)
%%%-----------------------------------------------------------------------------

-type ofp_flow_removed_reason() :: idle_timeout
                                 | hard_timeout
                                 | delete
                                 | group_delete.

-record(ofp_flow_removed, {
          cookie :: binary(),
          priority :: integer(),
          reason :: ofp_flow_removed_reason(),
          table_id :: ofp_table_id(),
          duration_sec :: integer(),
          duration_nsec :: integer(),
          idle_timeout :: integer(),
          hard_timeout :: integer(),
          packet_count :: integer(),
          byte_count :: integer(),
          match :: ofp_match()
         }).
-type ofp_flow_removed() :: #ofp_flow_removed{}.

%%%-----------------------------------------------------------------------------
%%% Port Status Message (A 4.3)
%%%-----------------------------------------------------------------------------

-type ofp_port_status_reason() :: add
                                | delete
                                | modify.

-record(ofp_port_status, {
          reason :: ofp_port_status_reason(),
          desc :: ofp_port()
         }).
-type ofp_port_status() :: #ofp_port_status{}.

%%%-----------------------------------------------------------------------------
%%% Error Message (A 4.4)
%%%-----------------------------------------------------------------------------

-type ofp_hello_failed() :: incompatible
                          | eperm.

-type ofp_bad_request() :: bad_version
                         | bad_type
                         | bad_multipart
                         | bad_experimenter
                         | bad_exp_type
                         | eperm
                         | bad_len
                         | buffer_empty
                         | buffer_unknown
                         | bad_table_id
                         | is_slave
                         | bad_port
                         | bad_packet
                         | multipart_buffer_overflow.

-type ofp_bad_action() :: bad_type
                        | bad_len
                        | bad_experimenter
                        | bad_exp_type
                        | bad_out_port
                        | bad_argument
                        | eperm
                        | too_many
                        | bad_queue
                        | bad_out_group
                        | match_inconsistent
                        | unsupported_order
                        | bad_tag
                        | bad_set_type
                        | bad_set_len
                        | bad_set_argument.

-type ofp_bad_instruction() :: unknown_inst
                             | unsup_inst
                             | bad_table_id
                             | unsup_metadata
                             | unsup_metadata_mask
                             | bad_experimenter
                             | bad_exp_type
                             | bad_len
                             | eperm.

-type ofp_bad_match() :: bad_type
                       | bad_len
                       | bad_tag
                       | bad_dl_addr_mask
                       | bad_nw_addr_mask
                       | bad_wildcards
                       | bad_field
                       | bad_value
                       | bad_mask
                       | bad_prereq
                       | dup_field
                       | eperm.

-type ofp_flow_mod_failed() :: unknown
                             | table_full
                             | bad_table_id
                             | overlap
                             | eperm
                             | bad_timeout
                             | bad_command
                             | bad_flags.

-type ofp_group_mod_failed() :: group_exists
                              | invalid_group
                              | weight_unsupported
                              | out_of_groups
                              | out_of_buckets
                              | chaining_unsupported
                              | watch_unsupported
                              | loop
                              | unknown_group
                              | chained_group
                              | bad_type
                              | bad_command
                              | bad_bucket
                              | bad_watch
                              | eperm.

-type ofp_port_mod_failed() :: bad_port
                             | bad_hw_addr
                             | bad_config
                             | bad_advertise
                             | eperm.

-type ofp_table_mod_failed() :: bad_table
                              | bad_config
                              | eperm.

-type ofp_queue_op_failed() :: bad_port
                             | bad_queue
                             | eperm.

-type ofp_switch_config_failed() :: bad_flags
                                  | bad_len
                                  | eperm.

-type ofp_role_request_failed() :: stale
                                 | unsup
                                 | bad_role.

-type ofp_meter_mod_failed() :: unknown
                              | meter_exists
                              | invalid_meter
                              | unknown_meter
                              | bad_command
                              | bad_flags
                              | bad_rate
                              | bad_burst
                              | bad_band
                              | bad_band_value
                              | out_of_meters
                              | out_of_bands.

-type ofp_table_features_failed() :: bad_table
                                   | bad_metadata
                                   | bad_type
                                   | bad_len
                                   | bad_argument
                                   | eperm.

-type ofp_error_code() :: ofp_hello_failed()
                        | ofp_bad_request()
                        | ofp_bad_action()
                        | ofp_bad_instruction()
                        | ofp_bad_match()
                        | ofp_flow_mod_failed()
                        | ofp_group_mod_failed()
                        | ofp_port_mod_failed()
                        | ofp_table_mod_failed()
                        | ofp_queue_op_failed()
                        | ofp_switch_config_failed()
                        | ofp_role_request_failed()
                        | ofp_meter_mod_failed()
                        | ofp_table_features_failed().

-type ofp_error_type() :: hello_failed
                        | bad_request
                        | bad_action
                        | bad_instruction
                        | bad_match
                        | flow_mod_failed
                        | group_mod_failed
                        | port_mod_failed
                        | table_mod_failed
                        | queue_op_failed
                        | switch_config_failed
                        | role_request_failed
                        | meter_mod_failed
                        | table_features_failed
                        | experimenter.

-record(ofp_error_msg, {
          type :: ofp_error_type(),
          code :: ofp_error_code(),
          data = <<>> :: binary()
         }).

-record(ofp_error_msg_experimenter, {
          exp_type :: integer(),
          experimenter :: integer(),
          data = <<>> :: binary()
         }).

-type ofp_error_msg() :: #ofp_error_msg{}
                       | #ofp_error_msg_experimenter{}.

%%%-----------------------------------------------------------------------------
%%% Symmetric Messages (A 5)
%%%-----------------------------------------------------------------------------

%%%-----------------------------------------------------------------------------
%%% Hello (A 5.1)
%%%-----------------------------------------------------------------------------

%%%-----------------------------------------------------------------------------
%%% Echo Request (A 5.2)
%%%-----------------------------------------------------------------------------

-record(ofp_echo_request, {
          data = <<>> :: binary()
         }).
-type ofp_echo_request() :: #ofp_echo_request{}.

%%%-----------------------------------------------------------------------------
%%% Echo Reply (A 5.3)
%%%-----------------------------------------------------------------------------

-record(ofp_echo_reply, {
          data = <<>> :: binary()
         }).
-type ofp_echo_reply() :: #ofp_echo_reply{}.

%%%-----------------------------------------------------------------------------
%%% Experimenter (A 5.4)
%%%-----------------------------------------------------------------------------

-record(ofp_experimenter, {
          experimenter :: integer(),
          exp_type :: integer(),
          data = <<>> :: binary()
         }).
-type ofp_experimenter() :: #ofp_experimenter{}.
