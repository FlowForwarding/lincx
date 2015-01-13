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
-module(linc_max_fast_actions_tests).

-include_lib("of_protocol/include/of_protocol.hrl").
-include_lib("of_protocol/include/ofp_v4.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("pkt/include/pkt.hrl").
-include("linc_max.hrl").

-include("fast_path.hrl").

-define(INIT_VAL, 100).
-define(NEW_VAL, 200).
-define(INIT_VAL(Bits), <<1:Bits>>).
-define(NEW_VAL(Bits), <<42:Bits>>).

%%TODO: tests for the action set

packet_modifying_actions_test_() ->
	[{"Action Set-Field", fun action_set_field/0},

	 {"Action Push-Tag: PBB", fun action_push_tag_pbb/0},
	 {"Action Pop-Tag: PBB", fun action_pop_tag_pbb/0},
	 {"Action Push-Tag: VLAN", fun action_push_tag_vlan/0},
	 {"Action Pop-Tag: VLAN", fun action_pop_tag_vlan/0},
	 {"Action Push-Tag: MPLS", fun action_push_tag_mpls/0},
	 {"Action Pop-Tag: MPLS", fun action_pop_tag_mpls/0},

	 {"Action Change-TTL: set MPLS TTL", fun action_set_mpls_ttl/0},
	 {"Action Change-TTL: decrement MPLS TTL", fun action_decrement_mpls_ttl/0},
	 {"Action Change-TTL: invalid MPLS TTL", fun invalid_mpls_ttl/0},
	 {"Action Change-TTL: set IP TTL", fun action_set_ip_ttl/0},
	 {"Action Change-TTL: decrement IP TTL", fun action_decrement_ip_ttl/0},
	 {"Action Change-TTL: invalid IP TTL", fun invalid_ip_ttl/0},
	 {"Action Change-TTL: copy TTL outwards", fun action_copy_ttl_outwards/0},
	 {"Action Change-TTL: copy TTL inwards", fun action_copy_ttl_inwards/0}
	].

action_set_field() ->
    EthType = {[#ether{},#arp{}],
			{arp_spa,?NEW_VAL(32)}, [#ether{},#arp{sip = ?NEW_VAL(32)}]},
    EthDst = {[#ether{},#arp{}],
			{eth_dst,?NEW_VAL(48)}, [#ether{dhost = ?NEW_VAL(48)},#arp{}]},
    EthSrc = {[#ether{},#arp{}],
			{eth_src,?NEW_VAL(48)}, [#ether{shost = ?NEW_VAL(48)},#arp{}]},
    [begin
         Action = {set_field,Name,Value},
         check_action(Action, Packet, NewPacket)
     end || {Packet, {Name, Value}, NewPacket} <- [EthType, EthDst, EthSrc]].

action_push_tag_pbb() ->
    Ethertype = 16#88e7,
    Action = {push_pbb,Ethertype},

    %% No previous PBB nor VLAN
    Packet1 = [#ether{},#arp{}],
    NewPacket1 = [#pbb{i_sid = <<1:24>>},#ether{},#arp{}],
    check_action(Action, Packet1, NewPacket1),

    %% Previous PBB exists
    Packet2 = [#pbb{i_sid = <<100:24>>},#ether{},#arp{}],
    NewPacket2 = [#pbb{i_sid = <<100:24>>},#ether{},#arp{}],
    check_action(Action, Packet2, NewPacket2),

    %% Previous VLAN exists
    Packet3 = [#ether{},#ieee802_1q_tag{pcp = 100},#arp{}],
    NewPacket3 = [#pbb{i_pcp = 100, i_sid = <<1:24>>},
                  #ether{},#ieee802_1q_tag{pcp = 100},#arp{}],
    check_action(Action, Packet3, NewPacket3).

action_pop_tag_pbb() ->
    Action = pop_pbb,

    Packet1 = [#ether{},#arp{}],
    NewPacket1 = [#ether{},#arp{}],
    check_action(Action, Packet1, NewPacket1),

    Packet2 = [#pbb{},#ether{},#arp{}],
    NewPacket2 = [#ether{},#arp{}],
    check_action(Action, Packet2, NewPacket2).

action_push_tag_vlan() ->
    %% No initial VLAN
    VLAN1 = 16#8100,
    Action1 = {push_vlan,VLAN1},
    Packet1 = [#ether{},#arp{}],
    NewPacket1 = [#ether{},
                  #ieee802_1q_tag{pcp = 0, vid = <<1:12>>, ether_type = VLAN1},#arp{}],
    check_action(Action1, Packet1, NewPacket1),

    %% %% Initial VLAN present
    VLAN2 = 16#88a8,
    Action2 = {push_vlan,VLAN2},
    Packet2 = [#ether{},
               #ieee802_1q_tag{pcp = 100, vid = <<100:12>>, ether_type = VLAN1},#arp{}],
    NewPacket2 = [#ether{},
                  #ieee802_1q_tag{pcp = 100, vid = <<100:12>>, ether_type = VLAN2},
                  #ieee802_1q_tag{pcp = 100, vid = <<100:12>>, ether_type = VLAN1},#arp{}],
    check_action(Action2, Packet2, NewPacket2).

action_pop_tag_vlan() ->
    Action = pop_vlan,

    %% Pop with only one VLAN
    VLAN1 = 16#8100,
    Packet1 = [#ether{},
               #ieee802_1q_tag{pcp = 100, vid = <<100:12>>, ether_type = VLAN1},#arp{}],
    NewPacket1 = [#ether{},#arp{}],
    check_action(Action, Packet1, NewPacket1),

    %% Pop with two VLANs
    VLAN2 = 16#88a8,
    Packet2 = [#ether{},
               #ieee802_1q_tag{pcp = 100, vid = <<100:12>>, ether_type = VLAN2},
               #ieee802_1q_tag{pcp = 100, vid = <<100:12>>, ether_type = VLAN1},#arp{}],
    NewPacket2 = [#ether{},
                  #ieee802_1q_tag{pcp = 100, vid = <<100:12>>, ether_type = VLAN1},#arp{}],
    check_action(Action, Packet2, NewPacket2).

action_push_tag_mpls() ->
    MPLS1 = 16#8847,
    Action = {push_mpls,MPLS1},

	%%MK: MPLS header always followed by an IP header (?)

    %% Single MPLS tag without VLAN and IP headers 
    %Packet1 = [#ether{},#ipv4{},#udp{}],
    %NewPacket1 = [#ether{},
    %              #mpls_tag{stack = [#mpls_stack_entry{ttl = 0}]},#ipv4{},#udp{}],
    %check_action(Action, Packet1, NewPacket1),

    %% Multiple MPLS tags without VLAN and IP headers
    Packet2 = [#ether{},
               #mpls_tag{stack = [#mpls_stack_entry{}]},#ipv4{},#udp{}],
    NewPacket2 = [#ether{},
                  #mpls_tag{stack = [#mpls_stack_entry{},
                                     #mpls_stack_entry{}]},#ipv4{},#udp{}],
    check_action(Action, Packet2, NewPacket2),

	%%MK: MPLS header always followed by an IP header (?)

    %% Single MPLS tag with VLAN but whithout IP headers
    %% Should insert MPLS after VLAN.
    %VLAN1 = 16#8100,
    %Packet3 = [#ether{},
    %           #ieee802_1q_tag{pcp = 100, vid = <<100:12>>, ether_type = VLAN1}],
    %NewPacket3 = [#ether{},
    %              #ieee802_1q_tag{pcp = 100, vid = <<100:12>>, ether_type = VLAN1},
    %              #mpls_tag{stack = [#mpls_stack_entry{ttl = 0}]}],
    %check_action(Action, Packet3, NewPacket3),

    %% Single MPLS with IP header.
    %% Copy TTL from IP to MPLS.
    Packet4 = [#ether{},
               #ipv4{ttl = 500},#udp{}],
    NewPacket4 = [#ether{},
                  #mpls_tag{stack = [#mpls_stack_entry{ttl = 500}]},
                  #ipv4{ttl = 500},#udp{}],
    check_action(Action, Packet4, NewPacket4).

action_pop_tag_mpls() ->
    IPv4EtherType = 16#0800,
    MPLSEtherType = 16#8847,
    VLANEtherType = 16#8100,

    %% One MPLS label - delete whole header3
    Action1 = {pop_mpls,IPv4EtherType},
    
    Packet1 = [#ether{type = MPLSEtherType},
               #mpls_tag{stack = [#mpls_stack_entry{}]},
               #ipv4{},#udp{}],
    NewPacket1 = [#ether{type = IPv4EtherType},#ipv4{},#udp{}],
    check_action(Action1, Packet1, NewPacket1),

    %% Two MPLS labels - delete only the outermost one
    Action2 = {pop_mpls,MPLSEtherType},

	L1 = <<1:20>>,
	L2 = <<2:20>>,

    Packet2 = [#ether{type = MPLSEtherType},
               #mpls_tag{stack = [#mpls_stack_entry{label = L1},
                                  #mpls_stack_entry{label = L2}]},
               #ipv4{},#udp{}],
    NewPacket2 = [#ether{type = MPLSEtherType},
                  #mpls_tag{stack = [#mpls_stack_entry{label = L2}]},
                  #ipv4{},#udp{}],
    check_action(Action2, Packet2, NewPacket2),

    %% One MPLS header after VLAN tag - delete whole MPLS header after VLAN tag
    Action3 = {pop_mpls,IPv4EtherType},

    Packet3 = [#ether{type = VLANEtherType},
               #ieee802_1q_tag{vid = <<100:12>>, ether_type = MPLSEtherType},
               #mpls_tag{stack = [#mpls_stack_entry{label = L1}]},
               #ipv4{},#udp{}],
    NewPacket3 = [#ether{type = VLANEtherType},
                  #ieee802_1q_tag{vid = <<100:12>>, ether_type = IPv4EtherType},
                  #ipv4{},#udp{}],
    check_action(Action3, Packet3, NewPacket3).

action_set_mpls_ttl() ->
    Packet = [#ether{},#mpls_tag{stack =
						[#mpls_stack_entry{ttl = ?INIT_VAL}]},#ipv4{},#udp{}],
    NewPacket = [#ether{},#mpls_tag{stack = 
						[#mpls_stack_entry{ttl = ?NEW_VAL}]},#ipv4{},#udp{}],
    Action = {set_mpls_ttl,?NEW_VAL},
    check_action(Action, Packet, NewPacket).

action_decrement_mpls_ttl() ->
    Packet = [#ether{},#mpls_tag{stack =
						[#mpls_stack_entry{ttl = ?INIT_VAL}]},#ipv4{},#udp{}],
    NewPacket = [#ether{},#mpls_tag{stack =
						[#mpls_stack_entry{ttl = ?INIT_VAL - 1}]},#ipv4{},#udp{}],
    Action = decrement_mpls_ttl,
    check_action(Action, Packet, NewPacket).

invalid_mpls_ttl() ->
    Packet = [#ether{},#mpls_tag{stack =
						[#mpls_stack_entry{ttl = 0}]},#ipv4{},#udp{}],
    Action = decrement_mpls_ttl,
    check_action_error(Action, Packet, invalid_ttl).

action_set_ip_ttl() ->
    Packet = [#ether{},#ipv4{ttl = ?INIT_VAL},#udp{}],
    NewPacket = [#ether{},#ipv4{ttl = ?NEW_VAL},#udp{}],
    Action = {set_ip_ttl,?NEW_VAL},
    check_action(Action, Packet, NewPacket).

action_decrement_ip_ttl() ->
    Packet = [#ether{},#ipv4{ttl = ?INIT_VAL},#udp{}],
    NewPacket = [#ether{},#ipv4{ttl = ?INIT_VAL - 1},#udp{}],
    Action = decrement_ip_ttl,
    check_action(Action, Packet, NewPacket).

invalid_ip_ttl() ->
    Packet = [#ether{},#ipv4{ttl = 0},#udp{}],
    Action = decrement_ip_ttl,
    check_action_error(Action, Packet, invalid_ttl).

action_copy_ttl_outwards() ->
    Action  = copy_ttl_outwards,

	%% pkt does not support IP-in-IP

    %% from IPv4 to IPv4
    %Packet1 = [#ether{},#ipv4{ttl = ?INIT_VAL},#ipv4{ttl = ?NEW_VAL},#udp{}],
    %NewPacket1 = [#ether{},#ipv4{ttl = ?NEW_VAL},#ipv4{ttl = ?NEW_VAL},#udp{}],
    %check_action(Action, Packet1, NewPacket1),

    %% from IPv4 to MPLS
    Packet2 = [#ether{},#mpls_tag{stack =
					[#mpls_stack_entry{ttl = ?INIT_VAL}]},
						#ipv4{ttl = ?NEW_VAL},#udp{}],
    NewPacket2 = [#ether{},#mpls_tag{stack = 
					[#mpls_stack_entry{ttl = ?NEW_VAL}]},
						#ipv4{ttl = ?NEW_VAL},#udp{}],
    check_action(Action, Packet2, NewPacket2),

    %% from MPLS to MPLS
    Packet3 = [#ether{},#mpls_tag{stack =
					[#mpls_stack_entry{ttl = ?INIT_VAL},
                     #mpls_stack_entry{ttl = ?NEW_VAL}]},
               #ipv4{ttl = ?INIT_VAL},#udp{}],
    NewPacket3 = [#ether{},#mpls_tag{stack =
					[#mpls_stack_entry{ttl = ?NEW_VAL},
                     #mpls_stack_entry{ttl = ?NEW_VAL}]},
                  #ipv4{ttl = ?INIT_VAL},#udp{}],
    check_action(Action, Packet3, NewPacket3).

action_copy_ttl_inwards() ->
    Action  = copy_ttl_inwards,

	%% pkt does not support IP-in-IP

    %% from IPv4 to IPv4
    %Packet1 = [#ether{},#ipv4{ttl = ?NEW_VAL},#ipv4{ttl = ?INIT_VAL},#udp{}],
    %NewPacket1 = [#ether{},#ipv4{ttl = ?NEW_VAL}, #ipv4{ttl = ?NEW_VAL},#udp{}],
    %check_action(Action, Packet1, NewPacket1),

    %% from MPLS to IPv4
    Packet2 = [#ether{},#mpls_tag{stack =
					[#mpls_stack_entry{ttl = ?NEW_VAL}]},
               #ipv4{ttl = ?INIT_VAL},#udp{}],
    NewPacket2 = [#ether{},#mpls_tag{stack =
					[#mpls_stack_entry{ttl = ?NEW_VAL}]},
			   #ipv4{ttl = ?NEW_VAL},#udp{}],
    check_action(Action, Packet2, NewPacket2),

    %% from MPLS to MPLS
    Packet3 = [#ether{},#mpls_tag{stack =
					[#mpls_stack_entry{ttl = ?NEW_VAL},
                     #mpls_stack_entry{ttl = ?INIT_VAL}]},
               #ipv4{ttl = ?INIT_VAL},#udp{}],
    NewPacket3 = [#ether{},#mpls_tag{stack =
					[#mpls_stack_entry{ttl = ?NEW_VAL},
                     #mpls_stack_entry{ttl = ?NEW_VAL}]},
                  #ipv4{ttl = ?INIT_VAL},#udp{}],
    check_action(Action, Packet3, NewPacket3).

%%------------------------------------------------------------------------------

check_action(Action, Packet, NewPacket) ->
	Pkt = pkt:encapsulate(Packet),
	Pkt2 = linc_max_fast_actions:apply_list([Action], Pkt, #blaze{}),
	?assertEqual(pkt:encapsulate(NewPacket), Pkt2).

check_action_error(Action, Packet, Error) ->
	Pkt = pkt:encapsulate(Packet),
    ?assertEqual(Error, linc_max_fast_actions:apply_list([Action], Pkt, #blaze{})).

%%EOF
