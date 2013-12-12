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

-define(VERSION, 3).

%% Maximum values --------------------------------------------------------------

-define(OFPP_MAX, 16#ffffff00). %% port number
-define(OFPQ_MAX, 16#fffffffe). %% queue id
-define(OFPG_MAX, 16#fffffffd). %% group id
-define(OFPTT_MAX, 16#fe).      %% table id
-define(OFPCML_MAX, 16#ffe5).   %% buffer id
-define(OFPCML_NO_BUFFER, 16#ffff).   %% buffer id

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
-define(PACKET_IN_SIZE, 24).
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
-define(INSTRUCTION_EXPERIMENTER_SIZE, 8).
-define(ACTION_COPY_TTL_IN_SIZE, 8).
-define(ACTION_POP_MPLS_SIZE, 8).
-define(ACTION_POP_VLAN_SIZE, 8).
-define(ACTION_PUSH_MPLS_SIZE, 8).
-define(ACTION_PUSH_VLAN_SIZE, 8).
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
-define(TABLE_STATS_SIZE, 128).
-define(PORT_STATS_SIZE, 104).
-define(QUEUE_STATS_SIZE, 32).
-define(GROUP_STATS_SIZE, 32).
-define(GROUP_DESC_STATS_SIZE, 8).

%% Field lengths (in bits) -----------------------------------------------------

-define(IN_PORT_FIELD_LENGTH, 32).
-define(IN_PHY_PORT_FIELD_LENGTH, 32).
-define(METADATA_FIELD_LENGTH, 64).
-define(ETH_DST_FIELD_LENGTH, 48).
-define(ETH_SRC_FIELD_LENGTH, 48).
-define(ETH_TYPE_FIELD_LENGTH, 16).
-define(VLAN_VID_FIELD_LENGTH, 13).
-define(VLAN_PCP_FIELD_LENGTH, 3).
-define(IP_DSCP_FIELD_LENGTH, 6).
-define(IP_ECN_FIELD_LENGTH, 2).
-define(IP_PROTO_FIELD_LENGTH, 8).
-define(IPV4_SRC_FIELD_LENGTH, 32).
-define(IPV4_DST_FIELD_LENGTH, 32).
-define(TCP_SRC_FIELD_LENGTH, 16).
-define(TCP_DST_FIELD_LENGTH, 16).
-define(UDP_SRC_FIELD_LENGTH, 16).
-define(UDP_DST_FIELD_LENGTH, 16).
-define(SCTP_SRC_FIELD_LENGTH, 16).
-define(SCTP_DST_FIELD_LENGTH, 16).
-define(ICMPV4_TYPE_FIELD_LENGTH, 8).
-define(ICMPV4_CODE_FIELD_LENGTH, 8).
-define(ARP_OP_FIELD_LENGTH, 16).
-define(ARP_SPA_FIELD_LENGTH, 32).
-define(ARP_TPA_FIELD_LENGTH, 32).
-define(ARP_SHA_FIELD_LENGTH, 48).
-define(ARP_THA_FIELD_LENGTH, 48).
-define(IPV6_SRC_FIELD_LENGTH, 128).
-define(IPV6_DST_FIELD_LENGTH, 128).
-define(IPV6_FLABEL_FIELD_LENGTH, 20).
-define(ICMPV6_TYPE_FIELD_LENGTH, 8).
-define(ICMPV6_CODE_FIELD_LENGTH, 8).
-define(IPV6_ND_TARGET_FIELD_LENGTH, 128).
-define(IPV6_ND_SLL_FIELD_LENGTH, 48).
-define(IPV6_ND_TLL_FIELD_LENGTH, 48).
-define(MPLS_LABEL_FIELD_LENGTH, 20).
-define(MPLS_TC_FIELD_LENGTH, 3).

%% Misc sizes (in bytes) -------------------------------------------------------

-define(OFP_ETH_ALEN, 6).            %% ethernet address
-define(OFP_MAX_PORT_NAME_LEN, 16).  %% port name string
-define(OFP_MAX_TABLE_NAME_LEN, 32). %% table name string
-define(DESC_STR_LEN, 256).          %% switch description string
-define(SERIAL_NUM_LEN, 32).         %% serial number string

%%% Port Structures ------------------------------------------------------------

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

%% Port
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

%%% Queue Structures -----------------------------------------------------------

%% Queue rates are given in permiles. Value > 1000 means QoS is disabled.

-type ofp_queue_id() :: integer()
                      | all.

%% Min-Rate queue property
-record(ofp_queue_prop_min_rate, {
          rate :: integer()
         }).

%% Max-Rate queue property
-record(ofp_queue_prop_max_rate, {
          rate :: integer()
         }).

%% Experimenter queue property
-record(ofp_queue_prop_experimenter, {
          experimenter :: integer(),
          data = <<>> :: binary()
         }).

-type ofp_queue_property() :: #ofp_queue_prop_min_rate{} |
                              #ofp_queue_prop_max_rate{} |
                              #ofp_queue_prop_experimenter{}.

%% Packet queue
-record(ofp_packet_queue, {
          queue_id :: ofp_queue_id(),
          port_no :: ofp_port_no(),
          properties :: [ofp_queue_property()]
         }).
-type ofp_packet_queue() :: #ofp_packet_queue{}.

%%% Flow Match Structures ------------------------------------------------------

-type ofp_field_class() :: nxm_0
                         | nxm_1
                         | openflow_basic
                         | experimenter.

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
                             | mpls_tc.

-type ofp_field_type() :: openflow_basic_type().

%% OXM field
-record(ofp_field, {
          class = openflow_basic :: ofp_field_class(),
          name :: ofp_field_type(),
          has_mask = false :: boolean(),
          value :: bitstring(),
          mask :: bitstring()
         }).
-type ofp_field() :: #ofp_field{}.

%% Match
-record(ofp_match, {
          fields = [] :: [ofp_field()]
         }).

-type ofp_match() :: #ofp_match{}.

%%% Flow Instruction Structures ------------------------------------------------

%% Instruction structure for apply actions
-record(ofp_instruction_apply_actions, {
          seq = 1,
          actions :: [ofp_action()]
         }).

%% Instruction structure for clear actions
-record(ofp_instruction_clear_actions, {
          seq = 2
         }).

%% Instruction structure for write actions
-record(ofp_instruction_write_actions, {
          seq = 3,
          actions :: [ofp_action()]
         }).

%% Instruction structure for write metadata
-record(ofp_instruction_write_metadata, {
          seq = 4,
          metadata                 :: binary(),
          metadata_mask = <<1:64>> :: binary()
         }).

%% Instruction structure for goto table
-record(ofp_instruction_goto_table, {
          seq = 5,
          table_id :: integer()
         }).

%% Instruction structure for experimenter
-record(ofp_instruction_experimenter, {
          seq = 6,
          experimenter :: integer(),
          data = <<>> :: binary()
         }).

-type ofp_instruction() :: #ofp_instruction_goto_table{}
                         | #ofp_instruction_write_metadata{}
                         | #ofp_instruction_write_actions{}
                         | #ofp_instruction_apply_actions{}
                         | #ofp_instruction_clear_actions{}
                         | #ofp_instruction_experimenter{}.

%%% Action Structures ----------------------------------------------------------

%% Copy TTL inwards action
-record(ofp_action_copy_ttl_in, {
          seq = 1
         }).

%% Pop MPLS header action
-record(ofp_action_pop_mpls, {
          seq = 2,
          ethertype :: integer()
         }).

%% Pop VLAN header action
-record(ofp_action_pop_vlan, {
          seq = 3
         }).

%% Push MPLS header action
-record(ofp_action_push_mpls, {
          seq = 4,
          ethertype :: integer()
         }).

%% Push VLAN header action
-record(ofp_action_push_vlan, {
          seq = 5,
          ethertype :: integer()
         }).

%% Copy TTL outwards action
-record(ofp_action_copy_ttl_out, {
          seq = 6
         }).

%% Decrement MPLS TTL action
-record(ofp_action_dec_mpls_ttl, {
          seq = 7
         }).

%% Decrement IPv4 TTL action
-record(ofp_action_dec_nw_ttl, {
          seq = 8
         }).

%% Set MPLS TTL action
-record(ofp_action_set_mpls_ttl, {
          seq = 9,
          mpls_ttl :: integer()
         }).

%% Set IPv4 TTL action
-record(ofp_action_set_nw_ttl, {
          seq = 10,
          nw_ttl :: integer()
         }).

%% Set field action
-record(ofp_action_set_field, {
          seq = 11,
          field :: ofp_field()
         }).

%% Set queue action
-record(ofp_action_set_queue, {
          seq = 12,
          queue_id :: integer()
         }).

%% Group action
-record(ofp_action_group, {
          seq = 13,
          group_id :: integer()
         }).

%% Output action
-record(ofp_action_output, {
          seq = 14,
          port :: ofp_port_no(),
          max_len = no_buffer :: ofp_packet_in_bytes()
         }).

%% Experimenter action
-record(ofp_action_experimenter, {
          seq = 99,
          experimenter :: integer(),
          data = <<>> :: binary()
         }).

-type ofp_action_type() :: output
                         | group
                         | set_queue
                         | set_mpls_ttl
                         | dec_mpls_ttl
                         | set_nw_ttl
                         | dec_nw_ttl
                         | copy_ttl_out
                         | copy_ttl_in
                         | push_vlan
                         | pop_vlan
                         | push_mpls
                         | pop_mpls
                         | set_field
                         | experimenter.

-type ofp_action() :: #ofp_action_output{}
                    | #ofp_action_group{}
                    | #ofp_action_set_queue{}
                    | #ofp_action_set_mpls_ttl{}
                    | #ofp_action_dec_mpls_ttl{}
                    | #ofp_action_set_nw_ttl{}
                    | #ofp_action_dec_nw_ttl{}
                    | #ofp_action_copy_ttl_out{}
                    | #ofp_action_copy_ttl_in{}
                    | #ofp_action_push_vlan{}
                    | #ofp_action_pop_vlan{}
                    | #ofp_action_push_mpls{}
                    | #ofp_action_pop_mpls{}
                    | #ofp_action_set_field{}
                    | #ofp_action_experimenter{}.

%%% Other Structures -----------------------------------------------------------

%% Bucket for use in groups
-record(ofp_bucket, {
          weight :: integer(),
          watch_port :: integer(),
          watch_group :: integer(),
          actions = [] :: [ofp_action()]
         }).
-type ofp_bucket() :: #ofp_bucket{}.

%% Bucket counter for use in group stats
-record(ofp_bucket_counter, {
          packet_count = 0 :: integer(),
          byte_count   = 0 :: integer()
         }).
-type ofp_bucket_counter() :: #ofp_bucket_counter{}.

-type ofp_table_id() :: all
                      | integer().

%% Flow stats
-record(ofp_flow_stats, {
          table_id :: ofp_table_id(),
          duration_sec :: integer(),
          duration_nsec :: integer(),
          priority :: integer(),
          idle_timeout :: integer(),
          hard_timeout :: integer(),
          cookie :: binary(),
          packet_count :: integer(),
          byte_count :: integer(),
          match :: ofp_match(),
          instructions = [] :: [ofp_instruction()]
         }).
-type ofp_flow_stats() :: #ofp_flow_stats{}.

%% Table stats
-record(ofp_table_stats, {
          table_id :: ofp_table_id(),
          name :: binary(),
          match = [] :: [atom()],
          wildcards = [] :: [atom()],
          write_actions = [] :: [atom()],
          apply_actions = [] :: [atom()],
          write_setfields = [] :: [atom()],
          apply_setfields = [] :: [atom()],
          metadata_match :: binary(),
          metadata_write :: binary(),
          instructions = [] :: [atom()],
          config :: ofp_table_config(),
          max_entries :: integer(),
          active_count = 0 :: integer(),
          lookup_count = 0 :: integer(),
          matched_count = 0 :: integer()
         }).
-type ofp_table_stats() :: #ofp_table_stats{}.

%% Port stats
-record(ofp_port_stats, {
          port_no          :: ofp_port_no(),
          rx_packets   = 0 :: integer(),
          tx_packets   = 0 :: integer(),
          rx_bytes     = 0 :: integer(),
          tx_bytes     = 0 :: integer(),
          rx_dropped   = 0 :: integer(),
          tx_dropped   = 0 :: integer(),
          rx_errors    = 0 :: integer(),
          tx_errors    = 0 :: integer(),
          rx_frame_err = 0 :: integer(),
          rx_over_err  = 0 :: integer(),
          rx_crc_err   = 0 :: integer(),
          collisions   = 0 :: integer()
         }).
-type ofp_port_stats() :: #ofp_port_stats{}.

%% Queue stats
-record(ofp_queue_stats, {
          port_no        :: ofp_port_no(),
          queue_id       :: ofp_queue_id(),
          tx_bytes   = 0 :: integer(),
          tx_packets = 0 :: integer(),
          tx_errors  = 0 :: integer()
         }).
-type ofp_queue_stats() :: #ofp_queue_stats{}.

%% Group stats
-record(ofp_group_stats, {
          group_id :: ofp_group_id(),
          ref_count :: integer(),
          packet_count :: integer(),
          byte_count :: integer(),
          bucket_stats = [] :: [ofp_bucket_counter()]
         }).
-type ofp_group_stats() :: #ofp_group_stats{}.

%% Group desc stats
-record(ofp_group_desc_stats, {
          type :: atom(),
          group_id :: ofp_group_id(),
          buckets = [] :: [ofp_bucket()]
         }).
-type ofp_group_desc_stats() :: #ofp_group_desc_stats{}.

%%%-----------------------------------------------------------------------------
%%% Controller-to-Switch Messages
%%%-----------------------------------------------------------------------------

%%% Features (Handshake) -------------------------------------------------------

%% Features request
-record(ofp_features_request, {}).
-type ofp_features_request() :: #ofp_features_request{}.

-type ofp_switch_capability() :: flow_stats
                               | table_stats
                               | port_stats
                               | group_stats
                               | ip_reasm
                               | queue_stats
                               | port_blocked.

%% Switch features (Features reply)
-record(ofp_features_reply, {
          datapath_mac :: binary(),
          datapath_id :: integer(),
          n_buffers :: integer(),
          n_tables :: integer(),
          capabilities = [] :: [ofp_switch_capability()],
          ports = [] :: [ofp_port()]
         }).
-type ofp_features_reply() :: #ofp_features_reply{}.

%%% Switch Configuration -------------------------------------------------------

%% Configuration request
-record(ofp_get_config_request, {}).
-type ofp_get_config_request() :: #ofp_get_config_request{}.

-type ofp_switch_configuration() :: frag_drop
                                  | frag_reasm
                                  | invalid_ttl_to_controller.

%% Configuration reply
-record(ofp_get_config_reply, {
          flags = [] :: [ofp_switch_configuration()],
          miss_send_len :: ofp_packet_in_bytes()
         }).
-type ofp_get_config_reply() :: #ofp_get_config_reply{}.

%% Set configuration
-record(ofp_set_config, {
          flags = [] :: [ofp_switch_configuration()],
          miss_send_len :: ofp_packet_in_bytes()
         }).
-type ofp_set_config() :: #ofp_set_config{}.

%%% Modify-State ---------------------------------------------------------------

-type ofp_flow_mod_command() :: add
                              | modify
                              | modify_strict
                              | delete
                              | delete_strict.

-type ofp_flow_mod_flag() :: send_flow_rem
                           | check_overlap
                           | reset_counts.

%% Flow mod
-record(ofp_flow_mod, {
          cookie = <<0:64>> :: binary(),
          cookie_mask = <<0:64>> :: binary(),
          table_id = all :: ofp_table_id(),
          command :: ofp_flow_mod_command(),
          idle_timeout = 0 :: integer(),
          hard_timeout = 0 :: integer(),
          priority = 16#ffff :: integer(),
          buffer_id = no_buffer :: ofp_buffer_id(),
          out_port = any :: ofp_port_no(),
          out_group = any :: ofp_group_id(),
          flags = [] :: [ofp_flow_mod_flag()],
          match = #ofp_match{} :: ofp_match(),
          instructions = [] :: [ofp_instruction()]
         }).
-type ofp_flow_mod() :: #ofp_flow_mod{}.

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

%% Group mod
-record(ofp_group_mod, {
          command :: ofp_group_mod_command(),
          type :: ofp_group_type(),
          group_id :: ofp_group_id(),
          buckets = [] :: [ofp_bucket()]
         }).
-type ofp_group_mod() :: #ofp_group_mod{}.

%% Port mod
-record(ofp_port_mod, {
          port_no :: ofp_port_no(),
          hw_addr :: binary(),
          config = [] :: [ofp_port_config()],
          mask = [] :: [ofp_port_config()],
          advertise = [] :: [ofp_port_feature()]
         }).
-type ofp_port_mod() :: #ofp_port_mod{}.

-type ofp_table_config() :: continue
                          | drop
                          | controller.

%% Table mod
-record(ofp_table_mod, {
          table_id = all :: ofp_table_id(),
          config :: ofp_table_config()
         }).
-type ofp_table_mod() :: #ofp_table_mod{}.

%%% Read-State -----------------------------------------------------------------

-type ofp_stats_request_flags() :: any(). %% For future use
-type ofp_stats_reply_flags() :: more.

%% Request for desc stats
-record(ofp_desc_stats_request, {
          flags = [] :: [ofp_stats_request_flags()]
         }).
-type ofp_desc_stats_request() :: #ofp_desc_stats_request{}.

%% Desc stats
-record(ofp_desc_stats_reply, {
          flags = [] :: [ofp_stats_reply_flags()],
          mfr_desc :: binary(),
          hw_desc :: binary(),
          sw_desc :: binary(),
          serial_num :: binary(),
          dp_desc :: binary()
         }).
-type ofp_desc_stats_reply() :: #ofp_desc_stats_reply{}.

%% Request for flow stats
-record(ofp_flow_stats_request, {
          flags       = []           :: [ofp_stats_request_flags()],
          table_id    = all          :: ofp_table_id(),
          out_port    = any          :: ofp_port_no(),
          out_group   = any          :: ofp_group_id(),
          cookie      = <<0:64>>     :: binary(),
          cookie_mask = <<0:64>>     :: binary(),
          match       = #ofp_match{} :: ofp_match()
         }).
-type ofp_flow_stats_request() :: #ofp_flow_stats_request{}.

%% Flow stats reply
-record(ofp_flow_stats_reply, {
          flags = [] :: [ofp_stats_reply_flags()],
          stats = [] :: [ofp_flow_stats()]
         }).
-type ofp_flow_stats_reply() :: #ofp_flow_stats_reply{}.

%% Request for aggregate stats
-record(ofp_aggregate_stats_request, {
          flags = [] :: [ofp_stats_request_flags()],
          table_id = all :: ofp_table_id(),
          out_port = any :: ofp_port_no(),
          out_group = any :: ofp_group_id(),
          cookie = <<0:64>> :: binary(),
          cookie_mask = <<0:64>> :: binary(),
          match = #ofp_match{} :: ofp_match()}).
-type ofp_aggregate_stats_request() :: #ofp_aggregate_stats_request{}.

%% Aggregate stats reply
-record(ofp_aggregate_stats_reply, {
          flags = [] :: [ofp_stats_reply_flags()],
          packet_count = 0 :: integer(),
          byte_count = 0 :: integer(),
          flow_count = 0 :: integer()
         }).
-type ofp_aggregate_stats_reply() :: #ofp_aggregate_stats_reply{}.

%% Request for table stats
-record(ofp_table_stats_request, {
          flags = [] :: [ofp_stats_request_flags()]
         }).
-type ofp_table_stats_request() :: #ofp_table_stats_request{}.

%% Table stats reply
-record(ofp_table_stats_reply, {
          flags = [] :: [atom()],
          stats = [] :: [#ofp_table_stats{}]
         }).
-type ofp_table_stats_reply() :: #ofp_table_stats_reply{}.

%% Request for port stats
-record(ofp_port_stats_request, {
          flags = [] :: [ofp_stats_request_flags()],
          port_no :: ofp_port_no()
         }).
-type ofp_port_stats_request() :: #ofp_port_stats_request{}.

%% Port stats reply
-record(ofp_port_stats_reply, {
          flags = [] :: [ofp_stats_reply_flags()],
          stats = [] :: [ofp_port_stats()]
         }).
-type ofp_port_stats_reply() :: #ofp_port_stats_reply{}.

%% Request for queue stats
-record(ofp_queue_stats_request, {
          flags = [] :: [ofp_stats_request_flags()],
          port_no = all :: ofp_port_no(),
          queue_id = all :: ofp_queue_id()
         }).
-type ofp_queue_stats_request() :: #ofp_queue_stats_request{}.

%% Queue stats reply
-record(ofp_queue_stats_reply, {
          flags = [] :: [ofp_stats_reply_flags()],
          stats = [] :: [ofp_queue_stats()]
         }).
-type ofp_queue_stats_reply() :: #ofp_queue_stats_reply{}.

%% Request for group stats
-record(ofp_group_stats_request, {
          flags = [] :: [ofp_stats_request_flags()],
          group_id = all :: ofp_group_id()
         }).
-type ofp_group_stats_request() :: #ofp_group_stats_request{}.

%% Group stats reply
-record(ofp_group_stats_reply, {
          flags = [] :: [ofp_stats_reply_flags()],
          stats = [] :: [ofp_group_stats()]
         }).
-type ofp_group_stats_reply() :: #ofp_group_stats_reply{}.

%% Request for group desc stats
-record(ofp_group_desc_stats_request, {
          flags = [] :: [ofp_stats_request_flags()]
         }).
-type ofp_group_desc_stats_request() :: #ofp_group_desc_stats_request{}.

%% Group desc stats reply
-record(ofp_group_desc_stats_reply, {
          flags = [] :: [ofp_stats_reply_flags()],
          stats = [] :: [ofp_group_desc_stats()]
         }).
-type ofp_group_desc_stats_reply() :: #ofp_group_desc_stats_reply{}.

%% Request for group features stats
-record(ofp_group_features_stats_request, {
          flags = [] :: [ofp_stats_request_flags()]
         }).
-type ofp_group_features_stats_request() :: #ofp_group_features_stats_request{}.

-type ofp_group_features_capabilities() :: select_weight
                                         | select_liveness
                                         | chaining
                                         | chaining_checks.

%% Group features stats reply
-record(ofp_group_features_stats_reply, {
          flags        = []               :: [ofp_stats_reply_flags()],
          types        = []               :: [atom()],
          capabilities = []               :: [ofp_group_features_capabilities()],
          max_groups   = {0,0,0,0}        :: {integer(), integer(),
                                              integer(), integer()},
          actions      = {[], [], [], []} :: {[atom()], [atom()],
                                              [atom()], [atom()]}
         }).
-type ofp_group_features_stats_reply() :: #ofp_group_features_stats_reply{}.

%% Request for experimenter stats
-record(ofp_experimenter_stats_request, {
          flags = [] :: [ofp_stats_request_flags()],
          experimenter :: integer(),
          exp_type :: integer(),
          data = <<>> :: binary()
         }).
-type ofp_experimenter_stats_request() :: #ofp_experimenter_stats_request{}.

%% Experimenter stats reply
-record(ofp_experimenter_stats_reply, {
          flags = [] :: [ofp_stats_reply_flags()],
          experimenter :: integer(),
          exp_type :: integer(),
          data = <<>> :: binary()
         }).
-type ofp_experimenter_stats_reply() :: #ofp_experimenter_stats_reply{}.

-type ofp_stats_request() :: ofp_desc_stats_request()
                           | ofp_flow_stats_request()
                           | ofp_aggregate_stats_request()
                           | ofp_table_stats_request()
                           | ofp_port_stats_request()
                           | ofp_queue_stats_request()
                           | ofp_group_stats_request()
                           | ofp_group_desc_stats_request()
                           | ofp_group_features_stats_request()
                           | ofp_experimenter_stats_request().

-type ofp_stats_reply() :: ofp_desc_stats_reply()
                         | ofp_flow_stats_reply()
                         | ofp_aggregate_stats_reply()
                         | ofp_table_stats_reply()
                         | ofp_port_stats_reply()
                         | ofp_queue_stats_reply()
                         | ofp_group_stats_reply()
                         | ofp_group_desc_stats_reply()
                         | ofp_group_features_stats_reply()
                         | ofp_experimenter_stats_reply().

%%% Queue Configuration --------------------------------------------------------

%% Get queue config request message
-record(ofp_queue_get_config_request, {
          port :: ofp_port_no()
         }).
-type ofp_queue_get_config_request() :: #ofp_queue_get_config_request{}.

%% Get queue config reply message
-record(ofp_queue_get_config_reply, {
          port :: ofp_port_no(),
          queues = [] :: [ofp_packet_queue()]
         }).
-type ofp_queue_get_config_reply() :: #ofp_queue_get_config_reply{}.

%%% Packet-out -----------------------------------------------------------------

-type ofp_buffer_id() :: integer()
                       | no_buffer.

%% Send packet
-record(ofp_packet_out, {
          buffer_id = no_buffer :: ofp_buffer_id(),
          in_port = controller :: controller,
          actions = [] :: [ofp_action()],
          data = <<>> :: binary()
         }).
-type ofp_packet_out() :: #ofp_packet_out{}.

%%% Barrier --------------------------------------------------------------------

%% Barrier request
-record(ofp_barrier_request, {}).
-type ofp_barrier_request() :: #ofp_barrier_request{}.

%% Barrier reply
-record(ofp_barrier_reply, {}).
-type ofp_barrier_reply() :: #ofp_barrier_reply{}.

%%% Role Request ---------------------------------------------------------------

-type ofp_controller_role() :: nochange
                             | equal
                             | master
                             | slave.

%% Role request messages
-record(ofp_role_request, {
          role :: ofp_controller_role(),
          generation_id :: integer()
         }).
-type ofp_role_request() :: #ofp_role_request{}.

%% Role reply message
-record(ofp_role_reply, {
          role :: ofp_controller_role(),
          generation_id :: integer()
         }).
-type ofp_role_reply() :: #ofp_role_reply{}.

%%%-----------------------------------------------------------------------------
%%% Asynchronous Messages
%%%-----------------------------------------------------------------------------

-type ofp_packet_in_reason() :: no_match
                              | action
                              | invalid_ttl.

-type ofp_packet_in_bytes() :: integer()
                             | no_buffer.

%% Packet-in
-record(ofp_packet_in, {
          buffer_id = no_buffer :: ofp_buffer_id(),
          reason :: ofp_packet_in_reason(),
          table_id :: integer(),
          match :: ofp_match(),
          data = <<>> :: binary()
         }).
-type ofp_packet_in() :: #ofp_packet_in{}.

-type ofp_flow_removed_reason() :: idle_timeout
                                 | hard_timeout
                                 | delete
                                 | group_delete.

%% Flow removed
-record(ofp_flow_removed, {
          cookie :: binary(),
          priority :: integer(),
          reason :: ofp_flow_removed_reason(),
          table_id :: integer(),
          duration_sec :: integer(),
          duration_nsec :: integer(),
          idle_timeout :: integer(),
          hard_timeout :: integer(),
          packet_count :: integer(),
          byte_count :: integer(),
          match :: ofp_match()
         }).
-type ofp_flow_removed() :: #ofp_flow_removed{}.

-type ofp_port_status_reason() :: add
                                | delete
                                | modify.

%% Port status change
-record(ofp_port_status, {
          reason :: ofp_port_status_reason(),
          desc :: ofp_port()
         }).
-type ofp_port_status() :: #ofp_port_status{}.

%% -type ofp_error_type() :: ...
%% -type ofp_bad_request_code() :: ...
%% -type ofp_error_code() :: bad_request_code()
%%                         | ...

%% Error message
-record(ofp_error_msg, {
          type :: atom(),
          code :: atom(),
          data = <<>> :: binary()
         }).

%% Experimenter error message
-record(ofp_error_msg_experimenter, {
          exp_type :: integer(),
          experimenter :: integer(),
          data = <<>> :: binary()
         }).

-type ofp_error_msg() :: #ofp_error_msg{}
                       | #ofp_error_msg_experimenter{}.

%%%-----------------------------------------------------------------------------
%%% Symmetric Messages
%%%-----------------------------------------------------------------------------

%% Echo Request
-record(ofp_echo_request, {
          data = <<>> :: binary()
         }).
-type ofp_echo_request() :: #ofp_echo_request{}.

%% Echo Reply
-record(ofp_echo_reply, {
          data = <<>> :: binary()
         }).
-type ofp_echo_reply() :: #ofp_echo_reply{}.

%% Experimenter
-record(ofp_experimenter, {
          experimenter :: integer(),
          exp_type :: integer(),
          data = <<>> :: binary()
         }).
-type ofp_experimenter() :: #ofp_experimenter{}.

-type ofp_message_body() :: ofp_error_msg()
                          | ofp_echo_request()
                          | ofp_echo_reply()
                          | ofp_experimenter()
                          | ofp_features_request()
                          | ofp_features_reply()
                          | ofp_get_config_request()
                          | ofp_get_config_reply()
                          | ofp_set_config()
                          | ofp_packet_in()
                          | ofp_flow_removed()
                          | ofp_port_status()
                          | ofp_packet_out()
                          | ofp_flow_mod()
                          | ofp_group_mod()
                          | ofp_port_mod()
                          | ofp_table_mod()
                          | ofp_stats_request()
                          | ofp_stats_reply()
                          | ofp_barrier_request()
                          | ofp_barrier_reply()
                          | ofp_queue_get_config_request()
                          | ofp_queue_get_config_reply()
                          | ofp_role_request()
                          | ofp_role_reply().
