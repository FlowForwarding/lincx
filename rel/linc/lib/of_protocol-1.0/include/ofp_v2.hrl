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
%% @doc OpenFlow Protocol 1.1 specific header.

%%------------------------------------------------------------------------------
%% OpenFlow Header
%%------------------------------------------------------------------------------

%% Protocol version
-define(VERSION, 2).

%%------------------------------------------------------------------------------
%% Common Structures
%%------------------------------------------------------------------------------

%% Port Structures -------------------------------------------------------------

-record(ofp_port, {
          port_no,
          hw_addr,
          name,
          config,
          state,
          curr,
          advertised,
          supported,
          peer,
          curr_speed,
          max_speed
         }).

%% Queue Structures ------------------------------------------------------------

-record(ofp_packet_queue, {
          queue_id,
          properties
         }).

-record(ofp_queue_prop_none, {}).

-record(ofp_queue_prop_min_rate, {
          rate
         }).

%% Flow Match Structures -------------------------------------------------------

-record(ofp_match, {
          in_port,
          wildcards,
          dl_src,
          dl_src_mask,
          dl_dst,
          dl_dst_mask,
          dl_vlan,
          dl_vlan_pcp,
          dl_type,
          nw_tos,
          nw_proto,
          nw_src,
          nw_src_mask,
          nw_dst,
          nw_dst_mask,
          tp_src,
          tp_dst,
          mpls_label,
          mpls_tc,
          metadata,
          metadata_mask
         }).

%% Flow Instruction Structures -------------------------------------------------

-record(ofp_instruction_goto_table, {
          table_id
         }).

-record(ofp_instruction_write_metadata, {
          metadata,
          metadata_mask
         }).

-record(ofp_instruction_write_actions, {
          actions
         }).

-record(ofp_instruction_apply_actions, {
          actions
         }).

-record(ofp_instruction_clear_actions, {}).

-record(ofp_instruction_experimenter, {
          experimenter,
          data
         }).

%% Action Structures -----------------------------------------------------------

-record(ofp_action_output, {
          port,
          max_len
         }).

-record(ofp_action_set_vlan_vid, {
          vlan_vid
         }).

-record(ofp_action_set_vlan_pcp, {
          vlan_pcp
         }).

-record(ofp_action_set_dl_src, {
          dl_src
         }).

-record(ofp_action_set_dl_dst, {
          dl_dst
         }).

-record(ofp_action_set_nw_src, {
          nw_src
         }).

-record(ofp_action_set_nw_dst, {
          nw_dst
         }).

-record(ofp_action_set_nw_tos, {
          nw_tos
         }).

-record(ofp_action_set_nw_ecn, {
          nw_ecn
         }).

-record(ofp_action_set_tp_src, {
          tp_src
         }).

-record(ofp_action_set_tp_dst, {
          tp_dst
         }).

-record(ofp_action_copy_ttl_out, {}).

-record(ofp_action_copy_ttl_in, {}).

-record(ofp_action_set_mpls_label, {
          mpls_label
         }).

-record(ofp_action_set_mpls_tc, {
          mpls_tc
         }).

-record(ofp_action_set_mpls_ttl, {
          mpls_ttl
         }).

-record(ofp_action_dec_mpls_ttl, {}).

-record(ofp_action_push_vlan, {
          ethertype
         }).

-record(ofp_action_pop_vlan, {}).

-record(ofp_action_push_mpls, {
          ethertype
         }).

-record(ofp_action_pop_mpls, {
          ethertype
         }).

-record(ofp_action_set_queue, {
          queue_id
         }).

-record(ofp_action_group, {
          group_id
         }).

-record(ofp_action_set_nw_ttl, {
          nw_ttl
         }).

-record(ofp_action_dec_nw_ttl, {}).

-record(ofp_action_experimenter, {
          experimenter,
          data
         }).

%%------------------------------------------------------------------------------
%% Controller-to-Switch Messages
%%------------------------------------------------------------------------------

%% Handshake -------------------------------------------------------------------

-record(ofp_features_request, {}).

-record(ofp_features_reply, {
          datapath_mac,
          datapath_id,
          n_buffers,
          n_tables,
          capabilities,
          ports
         }).

%% Switch Configuration --------------------------------------------------------

-record(ofp_get_config_request, {}).

-record(ofp_get_config_reply, {
          flags,
          miss_send_len
         }).

-record(ofp_set_config, {
          flags,
          miss_send_len
         }).

%% Flow Table Configuration ----------------------------------------------------

-record(ofp_table_mod, {
          table_id,
          config
         }).

%% Modify State Messages -------------------------------------------------------

-record(ofp_flow_mod, {
          cookie,
          cookie_mask,
          table_id,
          command,
          idle_timeout,
          hard_timeout,
          priority,
          buffer_id,
          out_port,
          out_group,
          flags,
          match,
          instructions
         }).

-record(ofp_group_mod, {
          command,
          type,
          group_id,
          buckets
         }).

-record(ofp_bucket, {
          weight,
          watch_port,
          watch_group,
          actions
         }).

-record(ofp_port_mod, {
          port_no,
          hw_addr,
          config,
          mask,
          advertise
         }).

%% Queue Configuration Messages ------------------------------------------------

-record(ofp_queue_get_config_request, {
          port
         }).

-record(ofp_queue_get_config_reply, {
          port,
          queues
         }).

%% Read State Messages ---------------------------------------------------------

-record(ofp_desc_stats_request, {
          flags
         }).

-record(ofp_flow_stats_request, {
          flags,
          table_id,
          out_port,
          out_group,
          cookie,
          cookie_mask,
          match
         }).

-record(ofp_aggregate_stats_request, {
          flags,
          table_id,
          out_port,
          out_group,
          cookie,
          cookie_mask,
          match
         }).

-record(ofp_table_stats_request, {
          flags
         }).

-record(ofp_port_stats_request, {
          flags,
          port_no
         }).

-record(ofp_queue_stats_request, {
          flags,
          port_no,
          queue_id
         }).

-record(ofp_group_stats_request, {
          flags,
          group_id
         }).

-record(ofp_group_desc_stats_request, {
          flags
         }).

-record(ofp_experimenter_stats_request, {
          flags,
          experimenter,
          data
         }).


-record(ofp_desc_stats_reply, {
          flags,
          mfr_desc,
          hw_desc,
          sw_desc,
          serial_num,
          dp_desc
         }).

-record(ofp_flow_stats_reply, {
          flags,
          stats
         }).

-record(ofp_flow_stats, {
          table_id,
          duration_sec,
          duration_nsec,
          priority,
          idle_timeout,
          hard_timeout,
          cookie,
          packet_count,
          byte_count,
          match,
          instructions
         }).

-record(ofp_aggregate_stats_reply, {
          flags,
          packet_count,
          byte_count,
          flow_count
         }).

-record(ofp_table_stats_reply, {
          flags,
          stats
         }).

-record(ofp_table_stats, {
          table_id,
          name,
          wildcards,
          match,
          instructions,
          write_actions,
          apply_actions,
          config,
          max_entries,
          active_count,
          lookup_count,
          matched_count
         }).

-record(ofp_port_stats_reply, {
          flags,
          stats
         }).

-record(ofp_port_stats, {
          port_no,
          rx_packets,
          tx_packets,
          rx_bytes,
          tx_bytes,
          rx_dropped,
          tx_dropped,
          rx_errors,
          tx_errors,
          rx_frame_err,
          rx_over_err,
          rx_crc_err,
          collisions
         }).

-record(ofp_queue_stats_reply, {
          flags,
          stats
         }).

-record(ofp_queue_stats, {
          port_no,
          queue_id,
          tx_bytes,
          tx_packets,
          tx_errors
         }).

-record(ofp_group_stats_reply, {
          flags,
          stats
         }).

-record(ofp_group_stats, {
          group_id,
          ref_count,
          packet_count,
          byte_count,
          bucket_stats
         }).

-record(ofp_bucket_counter, {
          packet_count,
          byte_count
         }).

-record(ofp_group_desc_stats_reply, {
          flags,
          stats
         }).

-record(ofp_group_desc_stats, {
          type,
          group_id,
          buckets
         }).

-record(ofp_experimenter_stats_reply, {
          flags,
          experimenter,
          data
         }).

%% Packet-Out Messages ---------------------------------------------------------

-record(ofp_packet_out, {
          buffer_id,
          in_port,
          actions,
          data
         }).

%% Barrier Messages ------------------------------------------------------------

-record(ofp_barrier_request, {}).

-record(ofp_barrier_reply, {}).

%%------------------------------------------------------------------------------
%% Asynchronous Messages
%%------------------------------------------------------------------------------

%% Packet-In Message -----------------------------------------------------------

-record(ofp_packet_in, {
          buffer_id,
          in_port,
          in_phy_port,
          reason,
          table_id,
          data
         }).

%% Flow Removed Message --------------------------------------------------------

-record(ofp_flow_removed, {
          cookie,
          priority,
          reason,
          table_id,
          duration_sec,
          duration_nsec,
          idle_timeout,
          packet_count,
          byte_count,
          match
         }).

%% Port Status Message ---------------------------------------------------------

-record(ofp_port_status, {
          reason,
          desc
         }).

%% Error Message ---------------------------------------------------------------

-record(ofp_error_msg, {
          type,
          code,
          data
         }).

%%------------------------------------------------------------------------------
%% Symmetric Messages
%%------------------------------------------------------------------------------

%% Hello -----------------------------------------------------------------------

%% Echo Request ----------------------------------------------------------------

-record(ofp_echo_request, {
          data
         }).

%% Echo Reply ------------------------------------------------------------------

-record(ofp_echo_reply, {
          data
         }).

%% Vendor ----------------------------------------------------------------------

-record(ofp_experimenter, {
          experimenter,
          data
         }).
