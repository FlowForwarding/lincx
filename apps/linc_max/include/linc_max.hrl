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
%% @author Cloudozer LLP. <info@cloudozer.com>
%% @copyright 2012 FlowForwarding.org
%% @doc Header file for userspace implementation of OpenFlow switch.

-define(CAPABILITIES, [
	flow_stats,
	table_stats,
	port_stats,
	group_stats,
	%% ip_reasm,
	queue_stats
	%% port_blocked
]).

-define(SUPPORTED_ACTIONS, [
	output,
	group,
	set_queue,
	set_mpls_ttl,
	dec_mpls_ttl,
	set_nw_ttl,
	dec_nw_ttl,
	copy_ttl_out,
	copy_ttl_in,
	push_vlan,
	pop_vlan,
	push_mpls,
	pop_mpls,
	push_pbb,
	pop_pbb,
	set_field
]).

%% Description of internal representation of fast_actions record.
%% If you change this you should regen sources with ./scripts/fast_actions_gen
-define(ACTIONS_SCHEME, [
	{set, set1, 'Set1', [
		copy_ttl_inwards,
		pop_pbb,
		{pop_mpls, value},
		pop_vlan,
		{push_mpls, value},
		{push_pbb, value},
		{push_vlan, value},
		copy_ttl_outwards,
		dec_nw_ttl,
		dec_mpls_ttl,
		{set_nw_ttl, value},
		{set_mpls_ttl, value},
		{set_field, eth_dst, value},
		{set_field, eth_src, value},
		{set_field, eth_type, value},
		{set_field, vlan_vid, value},
		{set_field, vlan_pcp, value},
		{set_field, ip_dscp, value},
		{set_field, ip_ecn, value},
		{set_field, ip_proto, value},
		{set_field, ipv4_src, value},
		{set_field, ipv4_dst, value},
		{set_field, tcp_src, value},
		{set_field, tcp_dst, value},
		{set_field, udp_src, value},
		{set_field, udp_dst, value},
		{set_field, sctp_src, value},
		{set_field, sctp_dst, value},
		{set_field, icmpv4_type, value},
		{set_field, icmpv4_code, value},
		{set_field, arp_op, value},
		{set_field, arp_spa, value},
		{set_field, arp_tpa, value},
		{set_field, arp_sha, value},
		{set_field, arp_tha, value},
		{set_field, ipv6_src, value},
		{set_field, ipv6_dst, value},
		{set_field, ipv6_label, value},
		{set_field, icmpv6_type, value},
		{set_field, icmpv6_code, value},
		{set_field, ipv6_nd_target, value},
		{set_field, ipv6_nd_sll, value},
		{set_field, ipv6_nd_tll, value},
		{set_field, mpls_label, value},
		{set_field, mpls_tc, value},
		{set_field, mpls_bos, value},
		{set_field, pbb_isid, value},
		{set_field, tunnel_id, value},
		{set_field, ipv6_exthdr, value}
	]},
	{action, queue, 'Queue', {set_queue, value}},
	{action, group, 'Group', {group, value}},
	{action, output, 'PortNo', {output, value}}
]).

-define(SUPPORTED_WRITE_ACTIONS, ?SUPPORTED_ACTIONS).
-define(SUPPORTED_APPLY_ACTIONS, ?SUPPORTED_ACTIONS).
-define(SUPPORTED_MATCH_FIELDS, [
	in_port,
	%% in_phy_port,
	metadata,
	eth_dst,
	eth_src,
	eth_type,
	vlan_vid,
	vlan_pcp,
	ip_dscp,
	ip_ecn,
	ip_proto,
	ipv4_src,
	ipv4_dst,
	tcp_src,
	tcp_dst,
	udp_src,
	udp_dst,
	sctp_src,
	sctp_dst,
	icmpv4_type,
	icmpv4_code,
	arp_op,
	arp_spa,
	arp_tpa,
	arp_sha,
	arp_tha,
	ipv6_src,
	ipv6_dst,
	ipv6_flabel,
	icmpv6_type,
	icmpv6_code,
	ipv6_nd_target,
	ipv6_nd_sll,
	ipv6_nd_tll,
	mpls_label,
	mpls_tc,
	mpls_bos,
	pbb_isid
	%% tunnel_id
	%% ext_hdr
]).

-define(SUPPORTED_WILDCARDS, ?SUPPORTED_MATCH_FIELDS).
-define(SUPPORTED_WRITE_SETFIELDS, ?SUPPORTED_MATCH_FIELDS).
-define(SUPPORTED_APPLY_SETFIELDS, ?SUPPORTED_WRITE_SETFIELDS).
-define(SUPPORTED_INSTRUCTIONS, [
	goto_table,
	write_metadata,
	write_actions,
	apply_actions,
	clear_actions,
	meter
]).
-define(SUPPORTED_RESERVED_PORTS, [
	all,
	controller,
	table,
	inport,
	any
	%% local
	%% normal
	%% flood
]).
-define(SUPPORTED_GROUP_TYPES, [
	all,
	select,
	indirect,
	ff
]).
-define(SUPPORTED_GROUP_CAPABILITIES, [
	select_weight,
	select_liveness,
	chaining
	%% chaining-check
]).
-define(SUPPORTED_METADATA_MATCH, <<-1:64>>).
-define(SUPPORTED_METADATA_WRITE, <<-1:64>>).

-define(MAX, (1 bsl 24)). %% some arbitrary big number
-define(MAX_FLOW_TABLE_ENTRIES, ?MAX).
-define(MAX_TABLES, 255).
-define(MAX_BANDS, 255).
-define(MAX_PORTS, ?MAX).
-define(MAX_BUFFERED_PACKETS, 0).

-type priority() :: non_neg_integer().
-type flow_id() :: {priority(), reference()}.

-type linc_table_config() :: continue
						   | drop
						   | controller.

-record(flow_table_config, {
	id            :: non_neg_integer(),
	config = drop :: linc_table_config()
}).

-record(flow_entry, {
	id                       :: flow_id(),
	priority                 :: priority(),
	match = #ofp_match{}     :: ofp_match(),
	cookie = <<0:64>>        :: binary(),
	flags = []               :: [ofp_flow_mod_flag()],
	install_time             :: erlang:timestamp(),
	expires = {infinity,0,0} :: erlang:timestamp(),
	idle = {infinity,0,0}    :: erlang:timestamp(),
	instructions = []        :: ordsets:ordset(ofp_instruction())
}).

-record(flow_timer, {
	id                       :: flow_id(),
	table                    :: non_neg_integer(),
	idle_timeout = infinity  :: infinity | non_neg_integer(),
	hard_timeout = infinity  :: infinity | non_neg_integer(),
	expire = infinity        :: infinity | non_neg_integer(),
	remove = infinity        :: infinity | non_neg_integer()
}).

-record(flow_entry_counter, {
	id                   :: flow_id(),
	received_packets = 0 :: integer(),
	received_bytes   = 0 :: integer()
}).

-record(flow_table_counter, {
	id :: integer(),
	%% Reference count is dynamically generated for the sake of simplicity
	%% reference_count = 0 :: integer(),
	packet_lookups = 0 :: integer(),
	packet_matches = 0 :: integer()
}).

-record(linc_pkt, {
	in_port                     :: ofp_port_no(),
	fields = #ofp_match{}       :: ofp_match(),
	actions = []                :: ordsets:ordset(ofp_action()),
	packet = []                 :: pkt:packet(),
	size = 0                    :: integer(),
	queue_id = default          :: integer() | default,
	table_id                    :: integer(),
	no_packet_in = false        :: boolean(),
	packet_in_reason            :: ofp_packet_in_reason(),
	packet_in_bytes = no_buffer :: ofp_packet_in_bytes(),
	cookie = <<-1:64>>          :: binary(),
	switch_id = 0               :: integer()
}).
-type linc_pkt() :: #linc_pkt{}.

%%EOF
