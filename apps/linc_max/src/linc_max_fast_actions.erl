%%
%%
%%

%% @author Cloduozer LLP. <info@cloudozer.com>
%% @copyright 2012 FlowForwarding.org
-module(linc_max_fast_actions).
-export([update_metadata/3]).
-export([meter/2]).
-export([apply_set/3]).
-export([apply_list/3]).
%% these functions are used in linc_max_generator and always expected to return a binary
-export([
	packet_in/2,
	output/3,
	set_queue/3,
	group/3,
	push_vlan/2,
	pop_vlan/1,
	push_mpls/2,
	pop_mpls/2,
	push_pbb/2,
	pop_pbb/1,
	set_field/3,
	set_mpls_ttl/2,
	decrement_mpls_ttl/1,
	set_ip_ttl/2,
	decrement_ip_ttl/1,
	copy_ttl_outwards/1,
	copy_ttl_inwards/1
]).

-include_lib("of_protocol/include/of_protocol.hrl").
-include_lib("of_protocol/include/ofp_v4.hrl").
-include_lib("linc/include/linc_logger.hrl").
-include_lib("pkt/include/pkt.hrl").
-include("linc_max.hrl").
-include("fast_path.hrl").

apply_set(#fast_actions{
		slow_actions = undefined,
		queue = undefined,
		group = undefined,
		output = undefined
	}, Frame, _Blaze) ->
	Frame;
apply_set(#fast_actions{
		slow_actions = undefined,
		queue = undefined,
		group = undefined,
		output = PortNo
	}, Frame, Blaze) ->
	output(Frame, PortNo, Blaze);
apply_set(#fast_actions{
		slow_actions = undefined,
		queue = undefined,
		group = Group
	}, Frame, Blaze) ->
	group(Frame, Group, Blaze);
apply_set(#fast_actions{
		slow_actions = undefined,
		queue = Queue
	}, Frame, Blaze) ->
	set_queue(Frame, Queue, Blaze);
apply_set(#fast_actions{
		slow_actions = Slow,
		queue = undefined,
		group = undefined,
		output = PortNo
	}, Frame, Blaze) ->
	output(apply_slow(Frame, Slow), PortNo, Blaze);
apply_set(#fast_actions{
		slow_actions = Slow,
		queue = undefined,
		group = Group
	}, Frame, Blaze) ->
	group(apply_slow(Frame, Slow), Group, Blaze);
apply_set(#fast_actions{
		slow_actions = Slow,
		queue = Queue
	}, Frame, Blaze) ->
	set_queue(apply_slow(Frame, Slow), Queue, Blaze).

apply_slow(Frame, As) ->
	apply_copy_ttl_inwards(Frame, As).

apply_copy_ttl_inwards(Frame, #slow_actions{copy_ttl_inwards = undefined} = As) ->
	apply_pop_pbb(Frame, As);
apply_copy_ttl_inwards(Frame, #slow_actions{copy_ttl_inwards = _} = As) ->
	apply_pop_pbb(copy_ttl_inwards(Frame), As).

apply_pop_pbb(Frame, #slow_actions{pop_pbb = undefined} = As) ->
	apply_pop_mpls(Frame, As);
apply_pop_pbb(Frame, #slow_actions{pop_pbb = _} = As) ->
	apply_pop_mpls(pop_pbb(Frame), As).

apply_pop_mpls(Frame, #slow_actions{pop_mpls = undefined} = As) ->
	apply_pop_vlan(Frame, As);
apply_pop_mpls(Frame, #slow_actions{pop_mpls = EthType} = As) ->
	apply_pop_vlan(pop_mpls(Frame, EthType), As).

apply_pop_vlan(Frame, #slow_actions{pop_vlan = undefined} = As) ->
	apply_push_mpls(Frame, As);
apply_pop_vlan(Frame, #slow_actions{pop_vlan = _} = As) ->
	apply_push_mpls(pop_vlan(Frame), As).

apply_push_mpls(Frame, #slow_actions{push_mpls = undefined} = As) ->
	apply_push_pbb(Frame, As);
apply_push_mpls(Frame, #slow_actions{push_mpls = EthType} = As) ->
	apply_push_pbb(push_mpls(Frame, EthType), As).

apply_push_pbb(Frame, #slow_actions{push_pbb = undefined} = As) ->
	apply_push_vlan(Frame, As);
apply_push_pbb(Frame, #slow_actions{push_pbb = EthType} = As) ->
	apply_push_vlan(push_pbb(Frame, EthType), As).

apply_push_vlan(Frame, #slow_actions{push_vlan = undefined} = As) ->
	apply_copy_ttl_outwards(Frame, As);
apply_push_vlan(Frame, #slow_actions{push_vlan = EthType} = As) ->
	apply_copy_ttl_outwards(push_vlan(Frame, EthType), As).

apply_copy_ttl_outwards(Frame, #slow_actions{copy_ttl_outwards = undefined} = As) ->
	apply_decrement_ip_ttl(Frame, As);
apply_copy_ttl_outwards(Frame, #slow_actions{copy_ttl_outwards = _} = As) ->
	apply_decrement_ip_ttl(copy_ttl_outwards(Frame), As).

apply_decrement_ip_ttl(Frame, #slow_actions{decrement_ip_ttl = undefined} = As) ->
	apply_decrement_mpls_ttl(Frame, As);
apply_decrement_ip_ttl(Frame, #slow_actions{decrement_ip_ttl = _} = As) ->
	apply_decrement_mpls_ttl(decrement_ip_ttl(Frame), As).

apply_decrement_mpls_ttl(Frame, #slow_actions{decrement_mpls_ttl = undefined} = As) ->
	apply_set_ip_ttl(Frame, As);
apply_decrement_mpls_ttl(Frame, #slow_actions{decrement_mpls_ttl = _} = As) ->
	apply_set_ip_ttl(decrement_mpls_ttl(Frame), As).

apply_set_ip_ttl(Frame, #slow_actions{set_ip_ttl = undefined} = As) ->
	apply_set_mpls_ttl(Frame, As);
apply_set_ip_ttl(Frame, #slow_actions{set_ip_ttl = TTL} = As) ->
	apply_set_mpls_ttl(set_ip_ttl(Frame, TTL), As).

apply_set_mpls_ttl(Frame, #slow_actions{set_mpls_ttl = undefined} = As) ->
	apply_eth_dst(Frame, As);
apply_set_mpls_ttl(Frame, #slow_actions{set_mpls_ttl = TTL} = As) ->
	apply_eth_dst(set_mpls_ttl(Frame, TTL), As).

apply_eth_dst(Frame, #slow_actions{eth_dst = undefined} = As) ->
	apply_eth_src(Frame, As);
apply_eth_dst(Frame, #slow_actions{eth_dst = Value} = As) ->
	apply_eth_src(set_field(Frame, eth_dst, Value), As).

apply_eth_src(Frame, #slow_actions{eth_src = undefined} = As) ->
	apply_eth_type(Frame, As);
apply_eth_src(Frame, #slow_actions{eth_src = Value} = As) ->
	apply_eth_type(set_field(Frame, eth_src, Value), As).

apply_eth_type(Frame, #slow_actions{eth_type = undefined} = As) ->
	apply_vlan_vid(Frame, As);
apply_eth_type(Frame, #slow_actions{eth_type = Value} = As) ->
	apply_vlan_vid(set_field(Frame, eth_type, Value), As).

apply_vlan_vid(Frame, #slow_actions{vlan_vid = undefined} = As) ->
	apply_vlan_pcp(Frame, As);
apply_vlan_vid(Frame, #slow_actions{vlan_vid = Value} = As) ->
	apply_vlan_pcp(set_field(Frame, vlan_vid, Value), As).

apply_vlan_pcp(Frame, #slow_actions{vlan_pcp = undefined} = As) ->
	apply_ip_dscp(Frame, As);
apply_vlan_pcp(Frame, #slow_actions{vlan_pcp = Value} = As) ->
	apply_ip_dscp(set_field(Frame, vlan_pcp, Value), As).

apply_ip_dscp(Frame, #slow_actions{ip_dscp = undefined} = As) ->
	apply_ip_ecn(Frame, As);
apply_ip_dscp(Frame, #slow_actions{ip_dscp = Value} = As) ->
	apply_ip_ecn(set_field(Frame, ip_dscp, Value), As).

apply_ip_ecn(Frame, #slow_actions{ip_ecn = undefined} = As) ->
	apply_ip_proto(Frame, As);
apply_ip_ecn(Frame, #slow_actions{ip_ecn = Value} = As) ->
	apply_ip_proto(set_field(Frame, ip_ecn, Value), As).

apply_ip_proto(Frame, #slow_actions{ip_proto = undefined} = As) ->
	apply_ipv4_src(Frame, As);
apply_ip_proto(Frame, #slow_actions{ip_proto = Value} = As) ->
	apply_ipv4_src(set_field(Frame, ip_proto, Value), As).

apply_ipv4_src(Frame, #slow_actions{ipv4_src = undefined} = As) ->
	apply_ipv4_dst(Frame, As);
apply_ipv4_src(Frame, #slow_actions{ipv4_src = Value} = As) ->
	apply_ipv4_dst(set_field(Frame, ipv4_src, Value), As).

apply_ipv4_dst(Frame, #slow_actions{ipv4_dst = undefined} = As) ->
	apply_tcp_src(Frame, As);
apply_ipv4_dst(Frame, #slow_actions{ipv4_dst = Value} = As) ->
	apply_tcp_src(set_field(Frame, ipv4_dst, Value), As).

apply_tcp_src(Frame, #slow_actions{tcp_src = undefined} = As) ->
	apply_tcp_dst(Frame, As);
apply_tcp_src(Frame, #slow_actions{tcp_src = Value} = As) ->
	apply_tcp_dst(set_field(Frame, tcp_src, Value), As).

apply_tcp_dst(Frame, #slow_actions{tcp_dst = undefined} = As) ->
	apply_udp_src(Frame, As);
apply_tcp_dst(Frame, #slow_actions{tcp_dst = Value} = As) ->
	apply_udp_src(set_field(Frame, tcp_dst, Value), As).

apply_udp_src(Frame, #slow_actions{udp_src = undefined} = As) ->
	apply_udp_dst(Frame, As);
apply_udp_src(Frame, #slow_actions{udp_src = Value} = As) ->
	apply_udp_dst(set_field(Frame, udp_src, Value), As).

apply_udp_dst(Frame, #slow_actions{udp_dst = undefined} = As) ->
	apply_sctp_src(Frame, As);
apply_udp_dst(Frame, #slow_actions{udp_dst = Value} = As) ->
	apply_sctp_src(set_field(Frame, udp_dst, Value), As).

apply_sctp_src(Frame, #slow_actions{sctp_src = undefined} = As) ->
	apply_sctp_dst(Frame, As);
apply_sctp_src(Frame, #slow_actions{sctp_src = Value} = As) ->
	apply_sctp_dst(set_field(Frame, sctp_src, Value), As).

apply_sctp_dst(Frame, #slow_actions{sctp_dst = undefined} = As) ->
	apply_icmpv4_type(Frame, As);
apply_sctp_dst(Frame, #slow_actions{sctp_dst = Value} = As) ->
	apply_icmpv4_type(set_field(Frame, sctp_dst, Value), As).

apply_icmpv4_type(Frame, #slow_actions{icmpv4_type = undefined} = As) ->
	apply_icmpv4_code(Frame, As);
apply_icmpv4_type(Frame, #slow_actions{icmpv4_type = Value} = As) ->
	apply_icmpv4_code(set_field(Frame, icmpv4_type, Value), As).

apply_icmpv4_code(Frame, #slow_actions{icmpv4_code = undefined} = As) ->
	apply_arp_op(Frame, As);
apply_icmpv4_code(Frame, #slow_actions{icmpv4_code = Value} = As) ->
	apply_arp_op(set_field(Frame, icmpv4_code, Value), As).

apply_arp_op(Frame, #slow_actions{arp_op = undefined} = As) ->
	apply_arp_spa(Frame, As);
apply_arp_op(Frame, #slow_actions{arp_op = Value} = As) ->
	apply_arp_spa(set_field(Frame, arp_op, Value), As).

apply_arp_spa(Frame, #slow_actions{arp_spa = undefined} = As) ->
	apply_arp_tpa(Frame, As);
apply_arp_spa(Frame, #slow_actions{arp_spa = Value} = As) ->
	apply_arp_tpa(set_field(Frame, arp_spa, Value), As).

apply_arp_tpa(Frame, #slow_actions{arp_tpa = undefined} = As) ->
	apply_arp_sha(Frame, As);
apply_arp_tpa(Frame, #slow_actions{arp_tpa = Value} = As) ->
	apply_arp_sha(set_field(Frame, arp_tpa, Value), As).

apply_arp_sha(Frame, #slow_actions{arp_sha = undefined} = As) ->
	apply_arp_tha(Frame, As);
apply_arp_sha(Frame, #slow_actions{arp_sha = Value} = As) ->
	apply_arp_tha(set_field(Frame, arp_sha, Value), As).

apply_arp_tha(Frame, #slow_actions{arp_tha = undefined} = As) ->
	apply_ipv6_src(Frame, As);
apply_arp_tha(Frame, #slow_actions{arp_tha = Value} = As) ->
	apply_ipv6_src(set_field(Frame, arp_tha, Value), As).

apply_ipv6_src(Frame, #slow_actions{ipv6_src = undefined} = As) ->
	apply_ipv6_dst(Frame, As);
apply_ipv6_src(Frame, #slow_actions{ipv6_src = Value} = As) ->
	apply_ipv6_dst(set_field(Frame, ipv6_src, Value), As).

apply_ipv6_dst(Frame, #slow_actions{ipv6_dst = undefined} = As) ->
	apply_ipv6_label(Frame, As);
apply_ipv6_dst(Frame, #slow_actions{ipv6_dst = Value} = As) ->
	apply_ipv6_label(set_field(Frame, ipv6_dst, Value), As).

apply_ipv6_label(Frame, #slow_actions{ipv6_label = undefined} = As) ->
	apply_icmpv6_type(Frame, As);
apply_ipv6_label(Frame, #slow_actions{ipv6_label = Value} = As) ->
	apply_icmpv6_type(set_field(Frame, ipv6_label, Value), As).

apply_icmpv6_type(Frame, #slow_actions{icmpv6_type = undefined} = As) ->
	apply_icmpv6_code(Frame, As);
apply_icmpv6_type(Frame, #slow_actions{icmpv6_type = Value} = As) ->
	apply_icmpv6_code(set_field(Frame, icmpv6_type, Value), As).

apply_icmpv6_code(Frame, #slow_actions{icmpv6_code = undefined} = As) ->
	apply_ipv6_nd_target(Frame, As);
apply_icmpv6_code(Frame, #slow_actions{icmpv6_code = Value} = As) ->
	apply_ipv6_nd_target(set_field(Frame, icmpv6_code, Value), As).

apply_ipv6_nd_target(Frame, #slow_actions{ipv6_nd_target = undefined} = As) ->
	apply_ipv6_nd_sll(Frame, As);
apply_ipv6_nd_target(Frame, #slow_actions{ipv6_nd_target = Value} = As) ->
	apply_ipv6_nd_sll(set_field(Frame, ipv6_nd_target, Value), As).

apply_ipv6_nd_sll(Frame, #slow_actions{ipv6_nd_sll = undefined} = As) ->
	apply_ipv6_nd_tll(Frame, As);
apply_ipv6_nd_sll(Frame, #slow_actions{ipv6_nd_sll = Value} = As) ->
	apply_ipv6_nd_tll(set_field(Frame, ipv6_nd_sll, Value), As).

apply_ipv6_nd_tll(Frame, #slow_actions{ipv6_nd_tll = undefined} = As) ->
	apply_mpls_label(Frame, As);
apply_ipv6_nd_tll(Frame, #slow_actions{ipv6_nd_tll = Value} = As) ->
	apply_mpls_label(set_field(Frame, ipv6_nd_tll, Value), As).

apply_mpls_label(Frame, #slow_actions{mpls_label = undefined} = As) ->
	apply_mpls_tc(Frame, As);
apply_mpls_label(Frame, #slow_actions{mpls_label = Value} = As) ->
	apply_mpls_tc(set_field(Frame, mpls_label, Value), As).

apply_mpls_tc(Frame, #slow_actions{mpls_tc = undefined} = As) ->
	apply_mpls_bos(Frame, As);
apply_mpls_tc(Frame, #slow_actions{mpls_tc = Value} = As) ->
	apply_mpls_bos(set_field(Frame, mpls_tc, Value), As).

apply_mpls_bos(Frame, #slow_actions{mpls_bos = undefined} = As) ->
	apply_pbb_isid(Frame, As);
apply_mpls_bos(Frame, #slow_actions{mpls_bos = Value} = As) ->
	apply_pbb_isid(set_field(Frame, mpls_bos, Value), As).

apply_pbb_isid(Frame, #slow_actions{pbb_isid = undefined} = As) ->
	apply_tunnel_id(Frame, As);
apply_pbb_isid(Frame, #slow_actions{pbb_isid = Value} = As) ->
	apply_tunnel_id(set_field(Frame, pbb_isid, Value), As).

apply_tunnel_id(Frame, #slow_actions{tunnel_id = undefined} = As) ->
	apply_ipv6_exthdr(Frame, As);
apply_tunnel_id(Frame, #slow_actions{tunnel_id = _Value} = As) ->
	?ERROR("Tunnel ID in action set not implemented"),
	apply_ipv6_exthdr(Frame, As).

apply_ipv6_exthdr(Frame, #slow_actions{ipv6_exthdr = undefined}) ->
	Frame;
apply_ipv6_exthdr(Frame, #slow_actions{ipv6_exthdr = Value}) ->
	set_field(Frame, ipv6_exthdr, Value).

%% FAST PATH
%%
%% The function is needed because metadata are represented as binary in the
%% argument list of a flow entry.
%%
update_metadata(<<MetaInt:64>>, AndMe, OrMe) ->
	<<(MetaInt band AndMe bor OrMe):64>>.

%% FAST PATH
%%
%% Meters are processes. The call should lookup the meter process using the
%% state and exchange messages with it to check that the packet fits the bands.
%%
meter(_MeterId, _St) -> ok.

%%------------------------------------------------------------------------------

%% Apply-Actions

%% FAST PATH
%%
apply_list([], Frame, _Blaze) ->
	Frame;

apply_list([{output,PortNo}|ActionList], Frame, Blaze) ->
	output(Frame, PortNo, Blaze),
	apply_list(ActionList, Frame, Blaze);

apply_list([{set_queue,Queue}|ActionList], Frame, Blaze) ->
	Frame1 = set_queue(Frame, Queue, Blaze),
	apply_list(ActionList, Frame1, Blaze);

apply_list([{group,Group}|ActionList], Frame, Blaze) ->
	Frame1 = group(Frame, Group, Blaze),
	apply_list(ActionList, Frame1, Blaze);

apply_list([{push_vlan,EthType}|ActionList], Frame, Blaze) ->
	Frame1 = push_vlan(Frame, EthType),
	apply_list(ActionList, Frame1, Blaze);
apply_list([pop_vlan|ActionList], Frame, Blaze) ->
	Frame1 = pop_vlan(Frame),
	apply_list(ActionList, Frame1, Blaze);

apply_list([{push_mpls,EthType}|ActionList], Frame, Blaze) ->
	Frame1 = push_mpls(Frame, EthType),
	apply_list(ActionList, Frame1, Blaze);
apply_list([{pop_mpls,EthType}|ActionList], Frame, Blaze) ->
	Frame1 = pop_mpls(Frame, EthType),
	apply_list(ActionList, Frame1, Blaze);

apply_list([{push_pbb,EthType}|ActionList], Frame, Blaze) ->
	Frame1 = push_pbb(Frame, EthType),
	apply_list(ActionList, Frame1, Blaze);
apply_list([pop_pbb|ActionList], Frame, Blaze) ->
	Frame1 = pop_pbb(Frame),
	apply_list(ActionList, Frame1, Blaze);

apply_list([{set_field,Field,Value}|ActionList], Frame, Blaze) ->
	case set_field(Frame, Field, Value) of
	Error when is_atom(Error) ->
		Error;	%% missing, protected, fragmented
	Frame1 ->
		apply_list(ActionList, Frame1, Blaze)
	end;

apply_list([{set_mpls_ttl,TTL}|ActionList], Frame, Blaze) ->
	Frame1  = set_mpls_ttl(Frame, TTL),
	apply_list(ActionList, Frame1, Blaze);
apply_list([decrement_mpls_ttl|ActionList], Frame, Blaze) ->
	case decrement_mpls_ttl(Frame) of
	invalid_ttl ->
		?INFO("Packet has invalid TTL"),
		%%TODO: send packet-in message to controller
		invalid_ttl;
	Frame1 ->
		apply_list(ActionList, Frame1, Blaze)
	end;

apply_list([{set_ip_ttl,TTL}|ActionList], Frame, Blaze) ->
	Frame1  = set_ip_ttl(Frame, TTL),
	apply_list(ActionList, Frame1, Blaze);
apply_list([decrement_ip_ttl|ActionList], Frame, Blaze) ->
	case decrement_ip_ttl(Frame) of
	invalid_ttl ->
		?INFO("Packet has invalid TTL"),
		%%TODO: send packet-in message to controller
		invalid_ttl;
	Frame1 ->
		apply_list(ActionList, Frame1, Blaze)
	end;

apply_list([copy_ttl_outwards|ActionList], Frame, Blaze) ->
	Frame1  = copy_ttl_outwards(Frame),
	apply_list(ActionList, Frame1, Blaze);
apply_list([copy_ttl_inwards|ActionList], Frame, Blaze) ->
	Frame1  = copy_ttl_inwards(Frame),
	apply_list(ActionList, Frame1, Blaze).

%%------------------------------------------------------------------------------
packet_in(Frame, InPort) ->
	%% TODO: fill all fields
	PacketIn = #ofp_packet_in{
		reason = action,
		table_id = 0,
		match = #ofp_match{fields = [
			#ofp_field{name = in_port, value = <<InPort>>}
		]},
		data = Frame
	},
	SwitchId = 0,
    linc_logic:send_to_controllers(SwitchId, #ofp_message{body = PacketIn}),
    Frame.

output(Frame, controller, _Blaze) ->
	%% 
	%% This is a quick-n-dirty implementation for Packet-In messages. It follows
	%% the code of linc_us4 without much thought. Require a good review.
	%%

	SwitchId = 0,	%%XXX
	TableId = 0,	%%XXX
	PacketIn = #ofp_packet_in{
		reason = action,
		table_id = TableId,
		data = Frame
	},
    linc_logic:send_to_controllers(SwitchId, #ofp_message{body = PacketIn}),
    Frame;
output(Frame, Sink, _Blaze) when is_atom(Sink) ->
	?ERROR("Output to '~w' not implemented", [Sink]),
	Frame;
output(Frame, PortNo, Blaze) ->
	#port_info{
		outlet      =Outlet,
		tx_pkt_ref  =TxPktRef,
		tx_data_ref =TxDataRef
	} = lists:keyfind(PortNo, #port_info.port_no, Blaze#blaze.ports),

	erlang:port_command(Outlet, Frame),

	erlang:update_counter(TxPktRef),
	erlang:update_counter(TxDataRef, byte_size(Frame)),
	Frame.

set_queue(Frame, Queue, Blaze) ->
	{_,Pid} = lists:keyfind(Queue, 1, Blaze#blaze.queue_map),
	Pid ! Frame,
	Frame.

group(Frame, Group, Blaze) ->
	erlang:apply(Group, apply, [Frame, Blaze]).

set_field(Frame, Field, FastValue) ->
	linc_max_splicer:edit(Frame, Field, FastValue).

push_vlan(<<EthAddrs:12/binary,
			?ETH_P_802_1Q:16,VlanTag:2/binary,
			Rest/binary>>, EthType) ->
	%% inherited values
	<<EthAddrs/binary,EthType:16,VlanTag:2/binary,
					  ?ETH_P_802_1Q:16,VlanTag:2/binary,Rest/binary>>;
push_vlan(<<EthAddrs:12/binary,
			?ETH_P_PBB_B:16,VlanTag:2/binary,
			Rest/binary>>, EthType) ->
	%% inherited values
	<<EthAddrs/binary,EthType:16,VlanTag:2/binary,
					  ?ETH_P_PBB_B:16,VlanTag:2/binary,Rest/binary>>;
push_vlan(<<EthAddrs:12/binary,
			Rest/binary>>, EthType) ->
	%% default values
	
	%% OpenFlow 1.4.0 page 26: New fields ... should be set to zero.
	%% 
	%% The old implementation set VLAN ID to 1. Why?
	
	<<EthAddrs/binary,EthType:16,0:16,Rest/binary>>.

%push_vlan(Frame, EthType) ->
%	P = pkt:decapsulate(Frame),
%    %% When pushing, fields are based on existing tag if there is any
%    case linc_max_packet:find(P, ieee802_1q_tag) of
%        not_found ->
%            InheritVid = <<1:12>>,
%            InheritPrio = 0;
%        {_, BasedOnTag} ->
%            InheritVid = BasedOnTag#ieee802_1q_tag.vid,
%            InheritPrio = BasedOnTag#ieee802_1q_tag.pcp
%    end,
%    P2 = linc_max_packet:find_and_edit(
%           P, ether,
%           fun(T) -> 
%                   NewTag = #ieee802_1q_tag{
%                     pcp = InheritPrio,
%                     vid = InheritVid,
%                     ether_type = EthType
%                    },
%                   %% found ether element, return it plus VLAN tag for insertion
%                   [T, NewTag]
%           end),
%	pkt:encapsulate(P2).

pop_vlan(<<EthAddrs:12/binary,
			?ETH_P_802_1Q:16,_VlanTag:2/binary,
			Rest/binary>>) ->
	<<EthAddrs/binary,Rest/binary>>;
pop_vlan(<<EthAddrs:12/binary,
			?ETH_P_PBB_B:16,_VlanTag:2/binary,
			Rest/binary>>) ->
	<<EthAddrs/binary,Rest/binary>>;
pop_vlan(Frame) ->
	Frame.

%pop_vlan(Frame) ->
%	P = pkt:decapsulate(Frame),
%    P2 = linc_max_packet:find_and_edit(
%           P, ieee802_1q_tag,
%           %% returning 'delete' atom will work for first VLAN tag only
%           fun(_) -> 'delete' end),
%	pkt:encapsulate(P2).

push_pbb(<<EthAddrs:12/binary,
		   ?ETH_P_PBB_B:16,_:16,
		   ?ETH_P_PBB_I:16,_,ISID:24,
		   Rest/binary>>, ?ETH_P_PBB_I) ->
	%% See the OF 1.4.0 spec, p. 26
	IPCP = vlan_pcp(Rest),
	<<EthAddrs/binary,
	  ?ETH_P_PBB_I:16,IPCP:3,0:5,ISID:24,
	  Rest/binary>>;

push_pbb(<<EthAddrs:12/binary,
		   Rest/binary>>, ?ETH_P_PBB_I) ->
	%% See the OF 1.4.0 spec, p. 66
	IPCP = vlan_pcp(Rest),
	<<EthAddrs/binary,
	  ?ETH_P_PBB_I:16,IPCP:3,0:5,0:24,
	  EthAddrs/binary,
	  Rest/binary>>.

%% find the outmost 802.1q header and return its PCP field
vlan_pcp(<<?ETH_P_802_1Q:16,PCP:3,_:13,_/binary>>) ->
	PCP;
vlan_pcp(<<?ETH_P_PBB_B:16,_:16,Rest/binary>>) ->
	vlan_pcp(Rest);
vlan_pcp(_) ->
	0.

%push_pbb(Frame, 16#88e7) ->
%	%%
%	%% pkt module has separate entries for ether and pbb packets. Try pbb first.
%	%%
%	P =  try
%		pkt:decapsulate_pbb(Frame)
%	catch _:_ ->
%		pkt:decapsulate(Frame)
%	end,
%
%    %% If there was PBB tag, copy isid from it
%    {ISID, IsPreviousPBB} = case linc_max_packet:find(P, pbb) of
%                                not_found ->
%                                    {<<1:24>>, false};
%                                {_, PreviousPBB} ->
%                                    {PreviousPBB#pbb.i_sid, true}
%                            end,
%    %% If there was VLAN tag, copy PCP from it
%    IPCP = case linc_max_packet:find(P, ieee802_1q_tag) of
%               not_found ->
%                   0;
%               {_, PreviousVLAN} ->
%                   PreviousVLAN#ieee802_1q_tag.pcp
%           end,
%    PBB = #pbb{b_pcp = 0,
%               b_dei = 0,
%               i_pcp = IPCP,
%               i_dei = 0,
%               i_uca = 0,
%               i_sid = ISID},
%    NewPacket = case IsPreviousPBB of
%                    true ->
%                        [#pbb{} | PacketRest] = P,
%                        [PBB | PacketRest];
%                    false ->
%                        [PBB | P]
%                end,
%	pkt:encapsulate(NewPacket).

pop_pbb(<<_EthAddrs:12/binary,?ETH_P_PBB_I:16,_ISID:32,Rest/binary>>) ->
	Rest;
pop_pbb(Frame) ->
	Frame.

%pop_pbb(Frame) ->
%	%% see comment above
%	P =  try
%		pkt:decapsulate_pbb(Frame)
%	catch _:_ ->
%		pkt:decapsulate(Frame)
%	end,
%    P2 = case P of
%             [#pbb{} | PRest] ->
%                 PRest;
%             _ ->
%                 P
%         end,
%	pkt:encapsulate(P2).

push_mpls(<<_:12/binary,Rest/binary>> =Frame, EthType) ->
	push_mpls(Frame, 12, Rest, EthType).

push_mpls(Frame, Off,
	<<?ETH_P_802_1Q:16,_:16,Rest/binary>>, EthType) ->
	push_mpls(Frame, Off +2 +2, Rest, EthType);
push_mpls(Frame, Off,
	<<?ETH_P_PBB_B:16,_:16,Rest/binary>>, EthType) ->
	push_mpls(Frame, Off +2 +2, Rest, EthType);
push_mpls(Frame, Off,
	<<?ETH_P_PBB_I:16,_:32,_:(6+6)/binary,Rest/binary>>, EthType) ->
	push_mpls(Frame, Off +2 +4 +6 +6, Rest, EthType);
push_mpls(Frame, Off,
	<<MplsType:16,Label:20,TClass:3,BoS:1,TTL:8,Rest/binary>>, EthType)
			when MplsType =:= ?ETH_P_MPLS_UNI orelse MplsType =:= ?ETH_P_MPLS_MULTI ->
	Prefix = binary:part(Frame, 0, Off),
	<<Prefix/binary,
	  EthType:16,Label:20,TClass:3,0:1,TTL:8,
	  Label:20,TClass:3,BoS:1,TTL:8,
	  Rest/binary>>;
push_mpls(Frame, Off,
	<<?ETH_P_IP:16,4:4,_:60,TTL:8,_/binary>> =Rest, EthType) ->
	Prefix = binary:part(Frame, 0, Off),
	<<_:16,Suffix/binary>> = Rest,
	<<Prefix/binary,
	  EthType:16,0:20,0:3,1:1,TTL:8,
	  Suffix/binary>>;
push_mpls(Frame, Off,
	<<?ETH_P_ARP:16,1:16,_/binary>> =Rest, EthType) ->
	Prefix = binary:part(Frame, 0, Off),
	<<_:16,Suffix/binary>> = Rest,
	<<Prefix/binary,
	  EthType:16,0:20,0:3,1:1,0:8, % ARP has no TTL, so set to 0
	  Suffix/binary>>;
push_mpls(Frame, Off,
	<<?ETH_P_IPV6:16,6:4,_:52,TTL:8,_/binary>> =Rest, EthType) ->
	Prefix = binary:part(Frame, 0, Off),
	<<_:16,Suffix/binary>> = Rest,
	<<Prefix/binary,
	  EthType:16,0:20,0:3,1:1,TTL:8,
	  Suffix/binary>>;
push_mpls(Frame, _, _, _) ->
	Frame.

%push_mpls(Frame, _EthType) ->
%	P = pkt:decapsulate(Frame),
%	%% inherit IP or MPLS ttl value
%    FindOldMPLS = linc_max_packet:find(P, mpls_tag),
%    SetTTL = case linc_max_packet:find(P, ipv4) of
%                 not_found ->
%                     case FindOldMPLS of
%                         not_found ->
%                             0;
%                         {_, T} ->
%                             mpls_get_outermost_ttl(T)
%                     end;
%                 {_, T} ->
%                     T#ipv4.ttl
%             end,
%
%    case FindOldMPLS of
%        not_found ->
%            %% Must insert after ether or vlan tag,
%            %% whichever is deeper in the packet
%            InsertAfter = case linc_max_packet:find(P, ieee802_1q_tag) of
%                              not_found ->
%                                  ether;
%                              _ ->
%                                  ieee802_1q_tag
%                          end,
%            P2 = linc_max_packet:find_and_edit(
%                   P, InsertAfter,
%                   fun(T) -> 
%                           NewEntry = #mpls_stack_entry{ttl = SetTTL},
%                           NewTag = #mpls_tag{stack = [NewEntry]},
%                           %% found ether or vlan element, return it plus
%                           %% MPLS tag for insertion
%                           [T, NewTag]
%                   end);
%        %% found an MPLS shim header, and will push tag into it
%        _ ->
%            P2 = linc_max_packet:find_and_edit(
%                   P, mpls_tag,
%                   fun(T) -> 
%                           %% base the newly inserted entry on a previous one
%                           NewEntry = hd(T#mpls_tag.stack),
%                           T#mpls_tag{stack = [NewEntry | T#mpls_tag.stack]}
%                   end)
%    end,
%	pkt:encapsulate(P2).

pop_mpls(<<_:12/binary,Rest/binary>> =Frame, EthType) ->
	pop_mpls(Frame, 12, Rest, EthType).

pop_mpls(Frame, Off,
	<<?ETH_P_802_1Q:16,_:16,Rest/binary>>, EthType) ->
	pop_mpls(Frame, Off +2 +2, Rest, EthType);
pop_mpls(Frame, Off,
	<<?ETH_P_PBB_B:16,_:16,Rest/binary>>, EthType) ->
	pop_mpls(Frame, Off +2 +2, Rest, EthType);
pop_mpls(Frame, Off,
	<<?ETH_P_PBB_I:16,_:32,_:(6+6)/binary,Rest/binary>>, EthType) ->
	pop_mpls(Frame, Off +2 +4 +6 +6, Rest, EthType);
pop_mpls(Frame, Off,
	<<MplsType:16,_Label:20,_TClass:3,0:1,_TTL:8,Rest/binary>>, _EthType)
			when MplsType =:= ?ETH_P_MPLS_UNI orelse MplsType =:= ?ETH_P_MPLS_MULTI ->
	Prefix = binary:part(Frame, 0, Off),
	<<Prefix/binary,MplsType:16,Rest/binary>>;
pop_mpls(Frame, Off,
	<<MplsType:16,_Label:20,_TClass:3,1:1,_TTL:8,Rest/binary>>, EthType)
			when MplsType =:= ?ETH_P_MPLS_UNI orelse MplsType =:= ?ETH_P_MPLS_MULTI ->
	Prefix = binary:part(Frame, 0, Off),
	<<Prefix/binary,EthType:16,Rest/binary>>;
pop_mpls(Frame, _, _, _) ->
	Frame.

%pop_mpls(Frame, EthType) ->
%	P = pkt:decapsulate(Frame),
%    PopMPLSHeader = fun(T) ->
%                            Stk = T#mpls_tag.stack,
%                            %% based on how many elements were in stack,
%                            %% either pop a top most element or delete
%                            %% the whole tag (for empty)
%                            case Stk of
%                                [_OnlyOneElement] ->
%                                    'delete';
%                                [_|RestOfStack] ->
%                                    T#mpls_tag{stack = RestOfStack}
%                            end
%                    end,
%    ModifyEtherType = fun(T) ->
%                              case T of
%                                  #ether{} ->
%                                      T#ether{type = EthType};
%                                  #ieee802_1q_tag{} ->
%                                      T#ieee802_1q_tag{ether_type = EthType}
%                              end
%                      end,
%    P2 = case linc_max_packet:find_and_edit(P, mpls_tag, PopMPLSHeader) of
%             Unmodified when Unmodified =:= P ->
%                 Unmodified;
%             Modified ->
%                 BeforeMPLSTag = case linc_max_packet:find(P, ieee802_1q_tag) of
%                                     not_found ->
%                                         ether;
%                                     _ ->
%                                         ieee802_1q_tag
%                                 end,
%                 linc_max_packet:find_and_edit(Modified, BeforeMPLSTag,
%                                               ModifyEtherType)
%         end,
%	pkt:encapsulate(P2).

%%------------------------------------------------------------------------------
%% These are slow - the faster version should not use pkt:*
%%
%% Copied from linc_max_actions.erl
%%

set_mpls_ttl(Frame, NewTTL) ->
	P = pkt:decapsulate(Frame),
    P2 = linc_max_packet:find_and_edit(
           P, mpls_tag,
           fun(T) ->
                   [TopTag | StackTail] = T#mpls_tag.stack,
                   NewTag = TopTag#mpls_stack_entry{ ttl = NewTTL },
                   T#mpls_tag{ stack = [NewTag | StackTail] }
           end),
	pkt:encapsulate(P2).

decrement_mpls_ttl(Frame) ->
	P = pkt:decapsulate(Frame),
    try 
        P2 = linc_max_packet:find_and_edit(
               P, mpls_tag,
               fun(#mpls_tag{stack=[#mpls_stack_entry{ttl=0} | _]}) ->
                       throw(invalid_ttl);
                  (#mpls_tag{stack=[#mpls_stack_entry{ttl=TTL}=TopTag | StackTail]}=T) ->
                       NewTag = TopTag#mpls_stack_entry{ttl = TTL-1},
                       T#mpls_tag{ stack = [NewTag | StackTail] }
               end),
		pkt:encapsulate(P2)
    catch 
        throw:invalid_ttl =Error ->
		Error
	end.

set_ip_ttl(Frame, NewTTL) ->
	P = pkt:decapsulate(Frame),
    P2 = linc_max_packet:find_and_edit(
           P, ipv4,
           fun(T) ->
                   T#ipv4{ ttl = NewTTL }
           end),
	pkt:encapsulate(P2).

decrement_ip_ttl(Frame) ->
	P = pkt:decapsulate(Frame),
    try
        P2 = linc_max_packet:find_and_edit(
               P, ipv4,
               fun(#ipv4{ ttl = 0 }) ->
                       throw(invalid_ttl);
                  (#ipv4{ ttl = TTL } = T) ->
                       T#ipv4{ ttl = TTL-1 }
               end),
		pkt:encapsulate(P2)
    catch
        throw:invalid_ttl = Error ->
		Error
	end.

copy_ttl_outwards(Frame) ->
	P = pkt:decapsulate(Frame),
    Tags = filter_copy_fields(P),
    P2 = case Tags of
             [#mpls_tag{stack = S}, #ipv4{ttl = NextOutermostTTL} | _]
               when length(S) == 1 ->
                 linc_max_packet:find_and_edit(
                   P, mpls_tag,
                   fun(T) ->
                           [Stack1 | StackRest] = T#mpls_tag.stack,
                           Stack1b = Stack1#mpls_stack_entry{
                                       ttl = NextOutermostTTL
                                      },
                           T#mpls_tag{
                             stack = [Stack1b | StackRest]
                            }
                   end);
             [#mpls_tag{stack = S} | _] when length(S) > 1 ->
                 linc_max_packet:find_and_edit(
                   P, mpls_tag,
                   fun(T) ->
                           [Stack1, Stack2 | StackRest] = T#mpls_tag.stack,
                           Stack1b = Stack1#mpls_stack_entry{
                                       ttl = Stack2#mpls_stack_entry.ttl
                                      },
                           T#mpls_tag{
                             %% reconstruct the stack
                             stack = [Stack1b, Stack2 | StackRest] 
                            }
                   end);
             [#ipv4{}, #ipv4{ttl = NextOutermostTTL}] ->
                 linc_max_packet:find_and_edit(
                   P, ipv4,
                   fun(T) ->
                           T#ipv4{ ttl = NextOutermostTTL }
                   end);
			X ->
				?INFO("TODO: copy_ttl_outwards: ~p", [X]),
				P	%% packet not modified
         end,
	pkt:encapsulate(P2).

copy_ttl_inwards(Frame) ->
	P = pkt:decapsulate(Frame),
	Tags = filter_copy_fields(P),
    P2 = case Tags of
             [#mpls_tag{stack = S} = MPLS, #ipv4{} | _]
               when length(S) == 1 ->
                 linc_max_packet:find_and_edit(
                   P, ipv4,
                   fun(T) ->
                           T#ipv4{ ttl = mpls_get_outermost_ttl(MPLS) }
                   end);
             [#mpls_tag{stack = S} | _] when length(S) > 1 ->
                 linc_max_packet:find_and_edit(
                   P, mpls_tag,
                   fun(T) ->
                           [Stack1, Stack2 | StackRest] = T#mpls_tag.stack,
                           Stack2b = Stack2#mpls_stack_entry{
                                       ttl = Stack1#mpls_stack_entry.ttl
                                      },
                           T#mpls_tag{
                             stack = [Stack1, Stack2b | StackRest]
                            }
                   end);
             [#ipv4{ttl = OutermostTTL}, #ipv4{}] ->
                 linc_max_packet:find_and_edit_skip(
                   P, ipv4,
                   fun(T) ->
                           T#ipv4{ ttl = OutermostTTL }
                   end, 1);
			X ->
				?INFO("TODO: copy_ttl_inwards: ~p", [X]),
				P	%% packet not modified
         end,
	pkt:encapsulate(P2).
 
%% @doc Extracts a TTL value from given MPLS tag's stack topmost entry
mpls_get_outermost_ttl(T = #mpls_tag{}) ->
    [H | _] = T#mpls_tag.stack,
    H#mpls_stack_entry.ttl.

filter_copy_fields(List) ->
    lists:filter(fun(B) when is_binary(B) ->
					false;
				 (T) when is_tuple(T) ->
                         element(1,T) =:= mpls_tag orelse
                             element(1,T) =:= ipv4
                 end, List).

%%EOF
