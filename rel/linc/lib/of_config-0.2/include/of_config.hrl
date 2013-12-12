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

-type id() :: string().
-type ip_address() :: string().

-type operation() :: undefined | merge | replace | create | delete | remove.

%% 7.12 OpenFlow Flow Table ----------------------------------------------------

-type match_field_type() :: input_port
                          | physical_input_port
                          | metadata
                          | ethernet_dest
                          | ethernet_src
                          | ethernet_frame_type
                          | vlan_id
                          | vlan_priority
                          | ip_dscp
                          | ip_ecn
                          | ip_protocol
                          | ipv4_src
                          | ipv4_dest
                          | tcp_src
                          | tcp_dest
                          | udp_src
                          | udp_dest
                          | sctp_src
                          | sctp_dest
                          | icmpv4_type
                          | icmpv4_code
                          | arp_op
                          | arp_src_ip_address
                          | arp_target_ip_address
                          | arp_src_hardware_address
                          | arp_target_hardware_address
                          | ipv6_src
                          | ipv6_dest
                          | ipv6_flow_label
                          | icmpv6_type
                          | icmpv6_code
                          | ipv6_nd_target
                          | ipv6_nd_source_link_layer
                          | ipv6_nd_target_link_layer
                          | mpls_label
                          | mpls_tc.

-record(flow_table, {
          resource_id     :: id(),
          max_entries     :: integer(),
          next_tables     :: [integer()],
          instructions    :: [instruction_type()],
          matches         :: [match_field_type()],
          write_actions   :: [action_type()],
          apply_actions   :: [action_type()],
          write_setfields :: [match_field_type()],
          apply_setfields :: [match_field_type()],
          wildcards       :: [match_field_type()],
          metadata_match  :: binary(),
          metadata_write  :: binary()
         }).

%% 7.10 External Certificate / 7.11 Owned Certificate --------------------------

-type certificate_type() :: external
                          | owned.

-record(private_key_rsa, {
          modulus  :: string(),
          exponent :: string()
         }).

-record(private_key_dsa, {
          p :: string(),
          q :: string(),
          g :: string(),
          y :: string(),
          j :: string(),
          seed :: string(),
          pgen_counter :: string()
         }).

-type private_key() :: #private_key_rsa{}
                     | #private_key_dsa{}.

-record(certificate, {
          operation   :: operation(),
          resource_id :: id(),
          type        :: certificate_type(),
          certificate :: binary(),
          private_key :: private_key() | undefined
         }).

%% 7.8 OpenFlow Port Feature ---------------------------------------------------

-type rate() :: '10Mb-HD'
              | '10Mb-FD'
              | '100Mb-HD'
              | '100Mb-FD'
              | '1Gb-HD'
              | '1Gb-FD'
              | '10Gb'
              | '40Gb'
              | '100Gb'
              | '1Tb'
              | 'other'.

-type medium() :: copper
                | fiber.

-type pause() :: unsupported
               | symmetric
               | asymmetric.

-record(features, {
          operation      :: operation(),
          rate           :: rate(),
          auto_negotiate :: boolean(),
          medium         :: medium(),
          pause          :: pause()
         }).

%% 7.7 OpenFlow Resource -------------------------------------------------------

-type oper_state() :: up
                    | down.

-record(port_configuration, {
          operation    :: operation(),
          admin_state  :: oper_state(),
          no_receive   :: boolean(),
          no_forward   :: boolean(),
          no_packet_in :: boolean()
         }).

-record(port_state, {
          oper_state :: oper_state(),
          blocked    :: boolean(),
          live       :: boolean()
         }).

-record(port_features, {
          operation       :: operation(),
          current         :: #features{},
          advertised      :: #features{},
          supported       :: #features{},
          advertised_peer :: #features{}
         }).

-record(ip_in_gre_tunnel, {
          local_endpoint_address  :: ip_address(),
          remote_endpoint_address :: ip_address(),
          checksum_present        :: boolean(),
          key_present             :: boolean(),
          key                     :: integer(),
          sequence_number_present :: boolean()
         }).

-record(vxlan_tunnel, {
          vni_valid           :: boolean(),
          vni                 :: integer(),
          vni_multicast_group :: binary(),
          udp_source_port     :: integer(),
          udp_dest_port       :: integer(),
          udp_checksum        :: boolean()
         }).

-record(nvgre_tunnel, {
          tni                 :: integer(),
          tni_user            :: integer(),
          tni_multicast_group :: binary()
         }).

-type tunnel() :: #ip_in_gre_tunnel{}
                | #vxlan_tunnel{}
                | #nvgre_tunnel{}.

-record(port, {
          operation     :: operation(),
          resource_id   :: id(),
          number        :: integer(),
          name          :: binary(),
          current_rate  :: integer(),
          max_rate      :: integer(),
          configuration :: #port_configuration{},
          state         :: #port_state{},
          features      :: #port_features{},
          tunnel        :: tunnel() | undefined
         }).

%% 7.9 OpenFlow Queue ----------------------------------------------------------

-record(queue_properties, {
          min_rate      :: integer(),
          max_rate      :: integer(),
          experimenters :: [integer()]
         }).
-type queue_properties() :: #queue_properties{}.


-record(queue, {
          operation   :: operation(),
          resource_id :: id(),
          id          :: id(),
          port        :: id(),
          properties  :: queue_properties()
         }).

%% 7.5 OpenFlow Controller -----------------------------------------------------

-type role() :: master
              | slave
              | equal.

-type controller_protocol() :: tcp
                             | tls.

-type connection_state() :: up
                          | down.

-type version() :: not_applicable
                 | '1.0'
                 | '1.0.1'
                 | '1.1'
                 | '1.2'
                 | '1.3'
                 | '1.3.1'.

-record(controller_state, {
          connection_state   :: connection_state(),
          current_version    :: version(),
          supported_versions :: [version()]
         }).

-record(controller, {
          operation        :: operation(),
          id               :: id(),
          role             :: role(),
          ip_address       :: ip_address(),
          port             :: integer(),
          local_ip_address :: ip_address(),
          local_port       :: integer(),
          protocol         :: controller_protocol(),
          state            :: #controller_state{}
         }).

%% 7.4 Logical Switch Capabilities ---------------------------------------------

-type reserved_port_type() :: all
                            | controller
                            | table
                            | inport
                            | any
                            | local
                            | normal
                            | flood.

-type group_type() :: all
                    | select
                    | indirect
                    | fast_failover.

-type group_capability() :: select_weight
                          | select_liveness
                          | chaining
                          | chaining_check.

-type action_type() :: output
                     | copy_ttl_out
                     | copy_ttl_in
                     | set_mpls_ttl
                     | dec_mpls_ttl
                     | push_vlan
                     | pop_vlan
                     | push_mpls
                     | pop_mpls
                     | set_queue
                     | group
                     | set_nw_ttl
                     | dec_nw_ttl
                     | set_field.

-type instruction_type() :: apply_actions
                          | clear_actions
                          | write_actions
                          | write_metadata
                          | goto_table.

-record(capabilities, {
          max_buffered_packets            :: integer(),
          max_tables                      :: integer(),
          max_ports                       :: integer(),
          flow_statistics         = false :: boolean(),
          table_statistics        = false :: boolean(),
          port_statistics         = false :: boolean(),
          group_statistics        = false :: boolean(),
          queue_statistics        = false :: boolean(),
          reassemble_ip_fragments = false :: boolean(),
          block_looping_ports     = false :: boolean(),
          reserved_port_types     = []    :: [reserved_port_type()],
          group_types             = []    :: [group_type()],
          group_capabilities      = []    :: [group_capability()],
          action_types            = []    :: [action_type()],
          instruction_types       = []    :: [instruction_type()]
         }).

%% 7.3 OpenFlow Logical Switch -------------------------------------------------

-type lost_connection_behaviour() :: failSecureMode
                                   | failStandaloneMode.

-record(logical_switch, {
          id                           :: id(),
          capabilities                 :: #capabilities{},
          datapath_id                  :: binary(),
          enabled                      :: boolean(),
          check_controller_certificate :: boolean(),
          lost_connection_behavior     :: lost_connection_behaviour(),
          controllers                  :: [#controller{}],
          resources                    :: [resource()]
         }).

%% 7.2 OpenFlow Configuration Point --------------------------------------------

-type configuration_point_protocol() :: ssh
                                      | soap
                                      | tls
                                      | beep.

-record(configuration_point, {
          id             :: id(),
          uri            :: string(),
          protocol = ssh :: configuration_point_protocol()
         }).

%% 7.1 OpenFlow Capable Switch -------------------------------------------------

-type resource() :: #port{}
                  | #queue{}
                  | #certificate{}
                  | #flow_table{}.

-record(capable_switch, {
          id                   :: id(),
          configuration_points :: [#configuration_point{}],
          resources            :: [resource()],
          logical_switches     :: [#logical_switch{}]
         }).
