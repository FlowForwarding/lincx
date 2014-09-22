%%
%%
%%

-record(port_info, {
	port_no,
	outlet,
	hw_addr,
	rx_pkt_ref,
	rx_data_ref,
	tx_pkt_ref,
	tx_data_ref
}).

-record(blaze, {
	ports		:: [#port_info{}],
	queue_map	:: [{port(),pid()}]
}).

-record(slow_actions, {
	copy_ttl_inwards,
	pop_pbb,
	pop_mpls,
	pop_vlan,
	push_mpls,
	push_pbb,
	push_vlan,
	copy_ttl_outwards,
	decrement_ip_ttl,
	decrement_mpls_ttl,
	set_ip_ttl,
	set_mpls_ttl,
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
	ipv6_label,
	icmpv6_type,
	icmpv6_code,
	ipv6_nd_target,
	ipv6_nd_sll,
	ipv6_nd_tll,
	mpls_label,
	mpls_tc,
	mpls_bos,
	pbb_isid,
	tunnel_id,
	ipv6_exthdr
}).

-record(fast_actions, {
	slow_actions,
	queue,
	group,
	output
}).

%%EOF
