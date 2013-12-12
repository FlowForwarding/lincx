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
%% @doc OpenFlow Protocol 1.0 specific header.

%%------------------------------------------------------------------------------
%% 5.1 OpenFlow Header
%%------------------------------------------------------------------------------

%% Protocol version
-define(VERSION, 1).

%%------------------------------------------------------------------------------
%% 5.2 Common Structures
%%------------------------------------------------------------------------------

%% 5.2.1 Port Structures -------------------------------------------------------

-record(ofp_phy_port, {
          port_no,
          hw_addr,
          name,
          config,
          state,
          curr,
          advertised,
          supported,
          peer
         }).

%% 5.2.2 Queue Structures ------------------------------------------------------

-record(ofp_packet_queue, {
          queue_id,
          properties
         }).

-record(ofp_queue_prop_none, {}).

-record(ofp_queue_prop_min_rate, {
          rate
         }).

%% 5.2.3 Flow Match Structures -------------------------------------------------

-record(ofp_match, {
          wildcards,
          nw_src_wildcard,
          nw_dst_wildcard,
          in_port,
          dl_src,
          dl_dst,
          dl_vlan,
          dl_vlan_pcp,
          dl_type,
          nw_tos,
          nw_proto,
          nw_src,
          nw_dst,
          tp_src,
          tp_dst
         }).

%% 5.2.4 Flow Action Structures ------------------------------------------------

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

-record(ofp_action_strip_vlan, {}).

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

-record(ofp_action_set_tp_src, {
          tp_src
         }).

-record(ofp_action_set_tp_dst, {
          tp_dst
         }).

-record(ofp_action_enqueue, {
          port,
          queue_id
         }).

-record(ofp_action_vendor, {
          vendor
         }).

%%------------------------------------------------------------------------------
%% 5.3 Controller-to-Switch Messages
%%------------------------------------------------------------------------------

%% 5.3.1 Handshake -------------------------------------------------------------

-record(ofp_features_request, {}).

-record(ofp_features_reply, {
          datapath_mac,
          datapath_id,
          n_buffers,
          n_tables,
          capabilities,
          actions,
          ports
         }).

%% 5.3.2 Switch Configuration --------------------------------------------------

-record(ofp_get_config_request, {}).

-record(ofp_get_config_reply, {
          flags,
          miss_send_len
         }).

-record(ofp_set_config, {
          flags,
          miss_send_len
         }).

%% 5.3.3 Modify State Messages -------------------------------------------------

%% Modify Flow Entry Message ---------------------------------------------------

-record(ofp_flow_mod, {
          match,
          cookie,
          command,
          idle_timeout,
          hard_timeout,
          priority,
          buffer_id,
          out_port,
          flags,
          actions
         }).

%% Port Modification Message ---------------------------------------------------

-record(ofp_port_mod, {
          port_no,
          hw_addr,
          config,
          mask,
          advertise
         }).

%% 5.3.4 Queue Configuration Messages ------------------------------------------

-record(ofp_queue_get_config_request, {
          port
         }).

-record(ofp_queue_get_config_reply, {
          port,
          queues
         }).

%% 5.3.5 Read State Messages ---------------------------------------------------

%% Description Statistics ------------------------------------------------------

-record(ofp_desc_stats_request, {
          flags
         }).

-record(ofp_desc_stats_reply, {
          flags,
          mfr_desc,
          hw_desc,
          sw_desc,
          serial_num,
          dp_desc
         }).

%% Individual Flow Statistics --------------------------------------------------

-record(ofp_flow_stats_request, {
          flags,
          match,
          table_id,
          out_port
         }).

-record(ofp_flow_stats_reply, {
          flags,
          stats
         }).

-record(ofp_flow_stats, {
          table_id,
          match,
          duration_sec,
          duration_nsec,
          priority,
          idle_timeout,
          hard_timeout,
          cookie,
          packet_count,
          byte_count,
          actions
         }).

%% Aggregate Flow Statistics ---------------------------------------------------

-record(ofp_aggregate_stats_request, {
          flags,
          match,
          table_id,
          out_port
         }).

-record(ofp_aggregate_stats_reply, {
          flags,
          packet_count,
          byte_count,
          flow_count
         }).

%% Table Statistics ------------------------------------------------------------

-record(ofp_table_stats_request, {
          flags
         }).

-record(ofp_table_stats_reply, {
          flags,
          stats
         }).

-record(ofp_table_stats, {
          table_id,
          name,
          wildcards,
          max_entries,
          active_count,
          lookup_count,
          matched_count
         }).

%% Port Statistics -------------------------------------------------------------

-record(ofp_port_stats_request, {
          flags,
          port_no
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

%% Queue Statistics ------------------------------------------------------------

-record(ofp_queue_stats_request, {
          flags,
          port_no,
          queue_id
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

%% Vendor Statistics -----------------------------------------------------------

-record(ofp_vendor_stats_request, {
          flags,
          vendor,
          data
         }).

-record(ofp_vendor_stats_reply, {
          flags,
          vendor,
          data
         }).

%% 5.3.6 Send Packet Message ---------------------------------------------------

-record(ofp_packet_out, {
          buffer_id,
          in_port,
          actions,
          data
         }).

%% 5.3.7 Barrier Messages ------------------------------------------------------

-record(ofp_barrier_request, {}).

-record(ofp_barrier_reply, {}).

%%------------------------------------------------------------------------------
%% Asynchronous Messages
%%------------------------------------------------------------------------------

%% 5.4.1 Packet-In Message -----------------------------------------------------

-record(ofp_packet_in, {
          buffer_id,
          in_port,
          reason,
          data
         }).

%% 5.4.2 Flow Removed Message --------------------------------------------------

-record(ofp_flow_removed, {
          match,
          cookie,
          priority,
          reason,
          duration_sec,
          duration_nsec,
          idle_timeout,
          packet_count,
          byte_count
         }).

%% 5.4.3 Port Status Message ---------------------------------------------------

-record(ofp_port_status, {
          reason,
          desc
         }).

%% 5.4.4 Error Message ---------------------------------------------------------

-record(ofp_error_msg, {
          type,
          code,
          data
         }).

%%------------------------------------------------------------------------------
%% 5.5 Symmetric Messages
%%------------------------------------------------------------------------------

%% 5.5.1 Hello -----------------------------------------------------------------

%% 5.5.2 Echo Request ----------------------------------------------------------

-record(ofp_echo_request, {
          data
         }).

%% 5.5.3 Echo Reply ------------------------------------------------------------

-record(ofp_echo_reply, {
          data
         }).

%% 5.5.4 Vendor ----------------------------------------------------------------

-record(ofp_vendor, {
          vendor,
          data
         }).
