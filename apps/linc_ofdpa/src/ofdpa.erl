%%
%% LINCX - OF-DPA integration layer
%%
-module(ofdpa).

-export([enum_to_integer/2,integer_to_enum/2]).

%% generated using 'genera exports'

-export([ofdpaFlowEntryInit/1,
         ofdpaFlowAdd/1,
         ofdpaFlowModify/1,
         ofdpaFlowDelete/1,
         ofdpaFlowNextGet/1,
         ofdpaFlowStatsGet/1,
         ofdpaFlowByCookieGet/1,
         ofdpaFlowByCookieDelete/1,
         ofdpaGroupTypeGet/1,
         ofdpaGroupVlanGet/1,
         ofdpaGroupPortIdGet/1,
         ofdpaGroupIndexShortGet/1,
         ofdpaGroupIndexGet/1,
         ofdpaGroupTypeSet/2,
         ofdpaGroupVlanSet/2,
         ofdpaGroupOverlayTunnelIdSet/2,
         ofdpaGroupOverlaySubTypeSet/2,
         ofdpaGroupOverlayIndexSet/2,
         ofdpaGroupPortIdSet/2,
         ofdpaGroupIndexShortSet/2,
         ofdpaGroupIndexSet/2,
         ofdpaGroupDecode/2,
         ofdpaGroupEntryInit/1,
         ofdpaGroupAdd/1,
         ofdpaGroupDelete/1,
         ofdpaGroupNextGet/1,
         ofdpaGroupTypeNextGet/2,
         ofdpaGroupStatsGet/1,
         ofdpaGroupBucketEntryInit/1,
         ofdpaGroupBucketEntryAdd/1,
         ofdpaGroupBucketEntryDelete/2,
         ofdpaGroupBucketsDeleteAll/1,
         ofdpaGroupBucketEntryGet/2,
         ofdpaGroupBucketEntryFirstGet/1,
         ofdpaGroupBucketEntryNextGet/2,
         ofdpaGroupBucketEntryModify/1,
         ofdpaGroupTableInfoGet/1,
         ofdpaPortTypeGet/1,
         ofdpaPortTypeSet/2,
         ofdpaPortIndexGet/1,
         ofdpaPortIndexSet/2,
         ofdpaPortNextGet/1,
         ofdpaPortMacGet/1,
         ofdpaPortNameGet/1,
         ofdpaPortStateGet/1,
         ofdpaPortConfigSet/2,
         ofdpaPortConfigGet/1,
         ofdpaPortMaxSpeedGet/1,
         ofdpaPortCurrSpeedGet/1,
         ofdpaPortFeatureGet/1,
         ofdpaPortAdvertiseFeatureSet/2,
         ofdpaPortStatsClear/1,
         ofdpaPortStatsGet/1,
         ofdpaPktSend/4,
         ofdpaMaxPktSizeGet/0,
         ofdpaPktReceive/2,
         ofdpaEventReceive/1,
         ofdpaPortEventNextGet/1,
         ofdpaFlowEventNextGet/1,
         ofdpaFlowTableInfoGet/1,
         ofdpaNumQueuesGet/1,
         ofdpaQueueStatsGet/2,
         ofdpaQueueStatsClear/2,
         ofdpaQueueRateSet/4,
         ofdpaQueueRateGet/2]).

-include("ofdpa.hrl").

%%
%%  4: len (without len field)
%%	2: #0xca11
%%	2: function
%%	4: cookie
%%	-: args
%%

call([{enum,error_t}|Returns], What, Args) ->
	case gen_server:call(ofdpa_link, {call,What,encode_args(Args)}) of
	{ok,<<0:32/little>>} ->
		ok;
	{ok,<<0:32/little,RetBlob/binary>>} ->
		list_to_tuple([ok|decode_args(RetBlob, Returns)]);
	{ok,<<Code:32/signed-little,_/binary>>} ->
		{error,integer_to_enum(error_t, Code)};
	{error,_} =Error ->
		Error
	end;
call(Returns, What, Args) ->
	case gen_server:call(ofdpa_link, {call,What,encode_args(Args)}) of
	{ok,<<>>} ->
		ok;
	{ok,RetBlob} ->
		list_to_tuple([ok|decode_args(RetBlob, Returns)]);
	{error,_} =Error ->
		Error
	end.

encode_args(Args) ->
	encode_args(Args, []).

encode_args([], Acc) ->
	list_to_binary(lists:reverse(Acc));
encode_args([{uint8_t,N}|Args], Acc) ->
	encode_args(Args, [<<N>>|Acc]);
encode_args([{uint16_t,N}|Args], Acc) ->
	encode_args(Args, [<<N:16/little>>|Acc]);
encode_args([{uint32_t,N}|Args], Acc) ->
	encode_args(Args, [<<N:32/little>>|Acc]);
encode_args([{uint64_t,N}|Args], Acc) ->
	encode_args(Args, [<<N:64/little>>|Acc]);
encode_args([B|Args], Acc) when is_binary(B) ->
	encode_args(Args, [<<(size(B)):32/little,B/binary>>|Acc]).

decode_args(Blob, Rs) ->
	decode_args(Blob, Rs, []).

decode_args(_, [], Acc) ->
	lists:reverse(Acc);
decode_args(<<N:32/little,Blob/binary>>, [{enum,E}|Rs], Acc) ->
	decode_args(Blob, Rs, [integer_to_enum(E, N)|Acc]);
decode_args(<<N,Blob/binary>>, [uint8_t|Rs], Acc) ->
	decode_args(Blob, Rs, [N|Acc]);
decode_args(<<N:16/little,Blob/binary>>, [uint16_t|Rs], Acc) ->
	decode_args(Blob, Rs, [N|Acc]);
decode_args(<<N:32/little,Blob/binary>>, [uint32_t|Rs], Acc) ->
	decode_args(Blob, Rs, [N|Acc]);
decode_args(<<N:64/little,Blob/binary>>, [uint64_t|Rs], Acc) ->
	decode_args(Blob, Rs, [N|Acc]);
decode_args(<<Sz:32/little,Bin:(Sz)/binary,Blob/binary>>, [{struct,S}|Rs], Acc) ->
	decode_args(Blob, Rs, [binary_to_struct(S, Bin)|Acc]);
decode_args(<<Mac:6/binary,Blob/binary>>, [ofdpaMacAddr_t|Rs], Acc) ->
	decode_args(Blob, Rs, [Mac|Acc]);
decode_args(<<Sz:32/little,Str:(Sz)/binary,Blob/binary>>, [string|Rs], Acc) ->
	decode_args(Blob, Rs, [Str|Acc]).

%% enum_to_integer/2 and integer_to_enum/2 generated using 'genera enums'

enum_to_integer(error_t, e_none) ->
    0;
enum_to_integer(error_t, e_rpc) ->
    -20;
enum_to_integer(error_t, e_internal) ->
    -21;
enum_to_integer(error_t, e_param) ->
    -22;
enum_to_integer(error_t, e_error) ->
    -23;
enum_to_integer(error_t, e_full) ->
    -24;
enum_to_integer(error_t, e_exists) ->
    -25;
enum_to_integer(error_t, e_timeout) ->
    -26;
enum_to_integer(error_t, e_fail) ->
    -27;
enum_to_integer(error_t, e_disabled) ->
    -28;
enum_to_integer(error_t, e_unavail) ->
    -29;
enum_to_integer(error_t, e_not_found) ->
    -30;
enum_to_integer(error_t, e_empty) ->
    -31;
enum_to_integer(flow_table_id_t, flow_table_id_none) ->
    0; %% table not set
enum_to_integer(flow_table_id_t, flow_table_id_ingress_port) ->
    0;
enum_to_integer(flow_table_id_t, flow_table_id_vlan) ->
    10;
enum_to_integer(flow_table_id_t, flow_table_id_termination_mac) ->
    20;
enum_to_integer(flow_table_id_t, flow_table_id_unicast_routing) ->
    30;
enum_to_integer(flow_table_id_t, flow_table_id_multicast_routing) ->
    40;
enum_to_integer(flow_table_id_t, flow_table_id_bridging) ->
    50;
enum_to_integer(flow_table_id_t, flow_table_id_acl_policy) ->
    60;
enum_to_integer(group_entry_type_t, group_entry_type_l2_interface) ->
    0;
enum_to_integer(group_entry_type_t, group_entry_type_l2_rewrite) ->
    1;
enum_to_integer(group_entry_type_t, group_entry_type_l3_unicast) ->
    2;
enum_to_integer(group_entry_type_t, group_entry_type_l2_multicast) ->
    3;
enum_to_integer(group_entry_type_t, group_entry_type_l2_flood) ->
    4;
enum_to_integer(group_entry_type_t, group_entry_type_l3_interface) ->
    5;
enum_to_integer(group_entry_type_t, group_entry_type_l3_multicast) ->
    6;
enum_to_integer(group_entry_type_t, group_entry_type_l3_ecmp) ->
    7;
enum_to_integer(group_entry_type_t, group_entry_type_l2_overlay) ->
    8;
enum_to_integer(group_entry_type_t, group_entry_type_last) ->
    9;
enum_to_integer(l2_overlay_subtype_t, l2_overlay_flood_unicast_tunnel) ->
    0;
enum_to_integer(l2_overlay_subtype_t, l2_overlay_flood_multicast_tunnel) ->
    1;
enum_to_integer(l2_overlay_subtype_t, l2_overlay_multicast_unicast_tunnel) ->
    2;
enum_to_integer(l2_overlay_subtype_t, l2_overlay_multicast_multicast_tunnel) ->
    3;
enum_to_integer(port_config_t, port_config_normal) ->
	0;
enum_to_integer(port_config_t, port_config_down) ->
    1;
enum_to_integer(port_state_t, port_state_link_up) ->
    0;
enum_to_integer(port_state_t, port_state_link_down) ->
    1.

integer_to_enum(error_t, 0) ->
    e_none;
integer_to_enum(error_t, -20) ->
    e_rpc;
integer_to_enum(error_t, -21) ->
    e_internal;
integer_to_enum(error_t, -22) ->
    e_param;
integer_to_enum(error_t, -23) ->
    e_error;
integer_to_enum(error_t, -24) ->
    e_full;
integer_to_enum(error_t, -25) ->
    e_exists;
integer_to_enum(error_t, -26) ->
    e_timeout;
integer_to_enum(error_t, -27) ->
    e_fail;
integer_to_enum(error_t, -28) ->
    e_disabled;
integer_to_enum(error_t, -29) ->
    e_unavail;
integer_to_enum(error_t, -30) ->
    e_not_found;
integer_to_enum(error_t, -31) ->
    e_empty;
integer_to_enum(flow_table_id_t, 0) ->
    flow_table_id_ingress_port;
integer_to_enum(flow_table_id_t, 10) ->
    flow_table_id_vlan;
integer_to_enum(flow_table_id_t, 20) ->
    flow_table_id_termination_mac;
integer_to_enum(flow_table_id_t, 30) ->
    flow_table_id_unicast_routing;
integer_to_enum(flow_table_id_t, 40) ->
    flow_table_id_multicast_routing;
integer_to_enum(flow_table_id_t, 50) ->
    flow_table_id_bridging;
integer_to_enum(flow_table_id_t, 60) ->
    flow_table_id_acl_policy;
integer_to_enum(group_entry_type_t, 0) ->
    group_entry_type_l2_interface;
integer_to_enum(group_entry_type_t, 1) ->
    group_entry_type_l2_rewrite;
integer_to_enum(group_entry_type_t, 2) ->
    group_entry_type_l3_unicast;
integer_to_enum(group_entry_type_t, 3) ->
    group_entry_type_l2_multicast;
integer_to_enum(group_entry_type_t, 4) ->
    group_entry_type_l2_flood;
integer_to_enum(group_entry_type_t, 5) ->
    group_entry_type_l3_interface;
integer_to_enum(group_entry_type_t, 6) ->
    group_entry_type_l3_multicast;
integer_to_enum(group_entry_type_t, 7) ->
    group_entry_type_l3_ecmp;
integer_to_enum(group_entry_type_t, 8) ->
    group_entry_type_l2_overlay;
integer_to_enum(group_entry_type_t, 9) ->
    group_entry_type_last;
integer_to_enum(l2_overlay_subtype_t, 0) ->
    l2_overlay_flood_unicast_tunnel;
integer_to_enum(l2_overlay_subtype_t, 1) ->
    l2_overlay_flood_multicast_tunnel;
integer_to_enum(l2_overlay_subtype_t, 2) ->
    l2_overlay_multicast_unicast_tunnel;
integer_to_enum(l2_overlay_subtype_t, 3) ->
    l2_overlay_multicast_multicast_tunnel;
integer_to_enum(port_config_t, 0) ->
	port_config_normal;
integer_to_enum(port_config_t, 1) ->
    port_config_down;
integer_to_enum(port_state_t, 0) ->
	port_state_link_up;
integer_to_enum(port_state_t, 1) ->
    port_state_link_down;

%% added manually

integer_to_enum(port_feature_t, 1) -> port_feat_10mb_hd;
integer_to_enum(port_feature_t, 2) -> port_feat_10mb_fd;
integer_to_enum(port_feature_t, 4) -> port_feat_100mb_hd;
integer_to_enum(port_feature_t, 8) -> port_feat_100mb_fd;
integer_to_enum(port_feature_t, 16) -> port_feat_1gb_hd;
integer_to_enum(port_feature_t, 32) -> port_feat_1gb_fd;
integer_to_enum(port_feature_t, 64) -> port_feat_10gb_fd;
integer_to_enum(port_feature_t, 128) -> port_feat_40gb_fd;
integer_to_enum(port_feature_t, 256) -> port_feat_100gb_fd;
integer_to_enum(port_feature_t, 512) -> port_feat_1tb_fd;
integer_to_enum(port_feature_t, 1024) -> port_feat_other;
integer_to_enum(port_feature_t, 2048) -> port_feat_cooper;
integer_to_enum(port_feature_t, 4096) -> port_feat_fiber;
integer_to_enum(port_feature_t, 8192) -> port_feat_autoneg;
integer_to_enum(port_feature_t, 16384) -> port_feat_pause;
integer_to_enum(port_feature_t, 32768) -> port_feat_pause_asym;

integer_to_enum(port_feature_t, Bits) ->
	lists:foldl(fun(M, Fs)  when Bits band M =/= 0 ->
		[integer_to_enum(port_feature_t, M)|Fs];
	(_, Fs) ->
		Fs
	end, [], [1 bsl N || N <- lists:seq(0, 15)]).

%% manually

pad_binary(Bin, N) when size(Bin) =:= N ->
	Bin;
pad_binary(Bin, N) ->
	Pad = list_to_binary(lists:duplicate(N -size(Bin), 0)),
	<<Bin/binary,Pad/binary>>.

%% struct_to_binary/1 and binary_to_struct/2 generated using 'genera structs'

struct_to_binary(#ingress_port_flow_match{inPort = InPort,
                                          inPortMask = InPortMask}) ->
    <<InPort:32/little,InPortMask:32/little>>;
struct_to_binary(#ingress_port_flow_entry{match_criteria =
                                              Match_criteria,
                                          gotoTableId = GotoTableId}) ->
	MatchBin = struct_to_binary(Match_criteria),
	T = enum_to_integer(flow_table_id_t, GotoTableId),
    <<MatchBin:8/binary,T:32/little>>;
struct_to_binary(#vlan_flow_match{inPort = InPort,
                                  vlanId = VlanId,
                                  vlanIdMask = VlanIdMask}) ->
    <<InPort:32/little,VlanId:16/little,VlanIdMask:16/little>>;
struct_to_binary(#vlan_flow_entry{match_criteria = Match_criteria,
                                  gotoTableId = GotoTableId,
                                  newVlanId = NewVlanId}) ->
	MatchBin = struct_to_binary(Match_criteria),
	T = enum_to_integer(flow_table_id_t, GotoTableId),
    <<MatchBin:8/binary,T:32/little,NewVlanId:16/little>>;
struct_to_binary(#termination_mac_flow_match{inPort = InPort,
                                             inPortMask = InPortMask,
                                             etherType = EtherType,
                                             destMac = DestMac,
                                             destMacMask = DestMacMask,
                                             vlanId = VlanId,
                                             vlanIdMask = VlanIdMask}) ->
    <<InPort:32/little,
      InPortMask:32/little,
      EtherType:16/little,
      DestMac:6/binary,
      DestMacMask:6/binary,
      VlanId:16/little,
      VlanIdMask:16/little>>;
struct_to_binary(#termination_mac_flow_entry{match_criteria =
                                                 Match_criteria,
                                             gotoTableId = GotoTableId,
                                             outputPort = OutputPort}) ->
	MatchBin = struct_to_binary(Match_criteria),
	T = enum_to_integer(flow_table_id_t, GotoTableId),
    <<MatchBin:26/binary,
      T:32/little,
      0:16,
      OutputPort:32/little>>;
struct_to_binary(#bridging_flow_match{vlanId = VlanId,
                                      tunnelId = TunnelId,
                                      destMac = DestMac,
                                      destMacMask = DestMacMask}) ->
    <<VlanId:16/little,
      TunnelId:16/little,
      DestMac:6/binary,
      DestMacMask:6/binary>>;
struct_to_binary(#bridging_flow_entry{match_criteria = Match_criteria,
                                      gotoTableId = GotoTableId,
                                      groupID = GroupID,
                                      tunnelLogicalPort =
                                          TunnelLogicalPort,
                                      outputPort = OutputPort}) ->
	MatchBin = struct_to_binary(Match_criteria),
	T = enum_to_integer(flow_table_id_t, GotoTableId),
    <<MatchBin:16/binary,
      T:32/little,
      GroupID:32/little,
      TunnelLogicalPort:32/little,
      OutputPort:32/little>>;
struct_to_binary(#unicast_routing_flow_match{etherType = EtherType,
                                             dstIp4 = DstIp4,
                                             dstIp4Mask = DstIp4Mask,
                                             dstIp6 = DstIp6,
                                             dstIp6Mask = DstIp6Mask}) ->
    <<EtherType:16/little,
      DstIp4:4/binary,
      DstIp4Mask:4/binary,
      DstIp6:16/binary,
      DstIp6Mask:16/binary>>;
struct_to_binary(#unicast_routing_flow_entry{match_criteria =
                                                 Match_criteria,
                                             gotoTableId = GotoTableId,
                                             groupID = GroupID}) ->
	MatchBin = struct_to_binary(Match_criteria),
	T = enum_to_integer(flow_table_id_t, GotoTableId),
    <<MatchBin:42/binary,
      T:32/little,
      0:16,
      GroupID:32/little>>;
struct_to_binary(#multicast_routing_flow_match{etherType = EtherType,
                                               vlanId = VlanId,
                                               srcIp4 = SrcIp4,
                                               srcIp4Mask = SrcIp4Mask,
                                               dstIp4 = DstIp4,
                                               srcIp6 = SrcIp6,
                                               srcIp6Mask = SrcIp6Mask,
                                               dstIp6 = DstIp6}) ->
    <<EtherType:16/little,
      VlanId:16/little,
      SrcIp4:4/binary,
      SrcIp4Mask:4/binary,
      DstIp4:4/binary,
      SrcIp6:16/binary,
      SrcIp6Mask:16/binary,
      DstIp6:16/binary>>;
struct_to_binary(#multicast_routing_flow_entry{match_criteria =
                                                   Match_criteria,
                                               gotoTableId = GotoTableId,
                                               groupID = GroupID}) ->
	MatchBin = struct_to_binary(Match_criteria),
	T = enum_to_integer(flow_table_id_t, GotoTableId),
    <<MatchBin:64/binary,T:32/little,GroupID:32/little>>;
struct_to_binary(#policy_acl_flow_match{inPort = InPort,
                                        inPortMask = InPortMask,
                                        srcMac = SrcMac,
                                        srcMacMask = SrcMacMask,
                                        destMac = DestMac,
                                        destMacMask = DestMacMask,
                                        etherType = EtherType,
                                        vlanId = VlanId,
                                        vlanIdMask = VlanIdMask,
                                        vlanPcp = VlanPcp,
                                        vlanPcpMask = VlanPcpMask,
                                        tunnelId = TunnelId,
                                        sourceIp4 = SourceIp4,
                                        sourceIp4Mask = SourceIp4Mask,
                                        destIp4 = DestIp4,
                                        destIp4Mask = DestIp4Mask,
                                        sourceIp6 = SourceIp6,
                                        sourceIp6Mask = SourceIp6Mask,
                                        destIp6 = DestIp6,
                                        destIp6Mask = DestIp6Mask,
                                        ipv4ArpSpa = Ipv4ArpSpa,
                                        ipv4ArpSpaMask = Ipv4ArpSpaMask,
                                        ipProto = IpProto,
                                        ipProtoMask = IpProtoMask,
                                        dscp = Dscp,
                                        dscpMask = DscpMask,
                                        ecn = Ecn,
                                        ecnMask = EcnMask,
                                        srcL4Port = SrcL4Port,
                                        srcL4PortMask = SrcL4PortMask,
                                        destL4Port = DestL4Port,
                                        destL4PortMask = DestL4PortMask,
                                        icmpType = IcmpType,
                                        icmpTypeMask = IcmpTypeMask,
                                        icmpCode = IcmpCode,
                                        icmpCodeMask = IcmpCodeMask,
                                        ipv6FlowLabel = Ipv6FlowLabel,
                                        ipv6FlowLabelMask =
                                            Ipv6FlowLabelMask}) ->
    <<InPort:32/little,
      InPortMask:32/little,
      SrcMac:6/binary,
      SrcMacMask:6/binary,
      DestMac:6/binary,
      DestMacMask:6/binary,
      EtherType:16/little,
      VlanId:16/little,
      VlanIdMask:16/little,
      VlanPcp:16/little,
      VlanPcpMask:16/little,
      0:16,
      TunnelId:32/little,
      SourceIp4:4/binary,
      SourceIp4Mask:4/binary,
      DestIp4:4/binary,
      DestIp4Mask:4/binary,
      SourceIp6:16/binary,
      SourceIp6Mask:16/binary,
      DestIp6:16/binary,
      DestIp6Mask:16/binary,
      Ipv4ArpSpa:32/little,
      Ipv4ArpSpaMask:32/little,
      IpProto:16/little,
      IpProtoMask:16/little,
      Dscp:16/little,
      DscpMask:16/little,
      Ecn:16/little,
      EcnMask:16/little,
      SrcL4Port:32/little,
      SrcL4PortMask:32/little,
      DestL4Port:32/little,
      DestL4PortMask:32/little,
      IcmpType:8/little,
      IcmpTypeMask:8/little,
      IcmpCode:8/little,
      IcmpCodeMask:8/little,
      Ipv6FlowLabel:32/little,
      Ipv6FlowLabelMask:32/little>>;
struct_to_binary(#policy_acl_flow_entry{match_criteria = Match_criteria,
                                        groupID = GroupID,
                                        queueIDAction = QueueIDAction,
                                        queueID = QueueID,
                                        vlanPcpAction = VlanPcpAction,
                                        vlanPcp = VlanPcp,
                                        dscpAction = DscpAction,
                                        dscp = Dscp,
                                        outputTunnelPort =
                                            OutputTunnelPort,
                                        outputPort = OutputPort,
                                        clearActions = ClearActions}) ->
	MatchBin = struct_to_binary(Match_criteria),
    <<MatchBin:176/binary,
      GroupID:32/little,
      QueueIDAction:8/little,
      QueueID:8/little,
      VlanPcpAction:8/little,
      VlanPcp:8/little,
      DscpAction:8/little,
      Dscp:8/little,
      0:16,
      OutputTunnelPort:32/little,
      OutputPort:32/little,
      ClearActions:32/little>>;
struct_to_binary(#ofdpa_flow_entry{tableId = TableId,
                             priority = Priority,
                             flowData = FlowData,
                             hard_time = Hard_time,
                             idle_time = Idle_time,
                             cookie = Cookie}) ->
	DataBin = pad_binary(struct_to_binary(FlowData), 200),
	T = enum_to_integer(flow_table_id_t, TableId),
    <<T:32/little,
      Priority:32/little,
      DataBin:200/binary,
      Hard_time:32/little,
      Idle_time:32/little,
      Cookie:8/binary>>;
struct_to_binary(#flow_entry_stats{durationSec = DurationSec,
                                   receivedPackets = ReceivedPackets,
                                   receivedBytes = ReceivedBytes}) ->
    <<DurationSec:32/little,
      0:32,
      ReceivedPackets:64/little,
      ReceivedBytes:64/little>>;
struct_to_binary(#group_entry{groupId = GroupId}) ->
    <<GroupId:32/little>>;
struct_to_binary(#group_entry_stats{refCount = RefCount,
                                    duration = Duration,
                                    bucketCount = BucketCount}) ->
    <<RefCount:32/little,Duration:32/little,BucketCount:32/little>>;
struct_to_binary(#l_2_interface_group_bucket_data{outputPort =
                                                      OutputPort,
                                                  popVlanTag =
                                                      PopVlanTag}) ->
    <<OutputPort:32/little,PopVlanTag:32/little>>;
struct_to_binary(#l_3_interface_group_bucket_data{vlanId = VlanId,
                                                  srcMac = SrcMac}) ->
    <<VlanId:32/little,SrcMac:6/binary>>;
struct_to_binary(#l_3_unicast_group_bucket_data{srcMac = SrcMac,
                                                dstMac = DstMac,
                                                vlanId = VlanId}) ->
    <<SrcMac:6/binary,DstMac:6/binary,VlanId:32/little>>;
struct_to_binary(#l_2_overlay_group_bucket_data{outputPort = OutputPort}) ->
    <<OutputPort:32/little>>;
struct_to_binary(#l_2_rewrite_group_bucket_data{srcMac = SrcMac,
                                                dstMac = DstMac,
                                                vlanId = VlanId}) ->
    <<SrcMac:6/binary,DstMac:6/binary,VlanId:32/little>>;
struct_to_binary(#group_bucket_entry{groupId = GroupId,
                                     bucketIndex = BucketIndex,
                                     referenceGroupId = ReferenceGroupId,
                                     bucketData = BucketData}) ->
	DataBin = pad_binary(struct_to_binary(BucketData), 16),
    <<GroupId:32/little,
      BucketIndex:32/little,
      ReferenceGroupId:32/little,
      DataBin:16/binary>>;
struct_to_binary(#group_table_info{numGroupEntries = NumGroupEntries,
                                   maxGroupEntries = MaxGroupEntries,
                                   maxBucketEntries = MaxBucketEntries}) ->
    <<NumGroupEntries:32/little,
      MaxGroupEntries:32/little,
      MaxBucketEntries:32/little>>;
struct_to_binary(#port_feature{curr = Curr,
                               advertised = Advertised,
                               supported = Supported,
                               peer = Peer}) ->
	C = enum_to_integer(port_feature_t, Curr),
	A = enum_to_integer(port_feature_t, Advertised),
	S = enum_to_integer(port_feature_t, Supported),
	P = enum_to_integer(port_feature_t, Peer),
    <<C:32/little,
      A:32/little,
      S:32/little,
      P:32/little>>;
struct_to_binary(#port_stats{rx_packets = Rx_packets,
                             tx_packets = Tx_packets,
                             rx_bytes = Rx_bytes,
                             tx_bytes = Tx_bytes,
                             rx_errors = Rx_errors,
                             tx_errors = Tx_errors,
                             rx_drops = Rx_drops,
                             tx_drops = Tx_drops,
                             rx_frame_err = Rx_frame_err,
                             rx_over_err = Rx_over_err,
                             rx_crc_err = Rx_crc_err,
                             collisions = Collisions,
                             duration_seconds = Duration_seconds}) ->
    <<Rx_packets:64/little,
      Tx_packets:64/little,
      Rx_bytes:64/little,
      Tx_bytes:64/little,
      Rx_errors:64/little,
      Tx_errors:64/little,
      Rx_drops:64/little,
      Tx_drops:64/little,
      Rx_frame_err:64/little,
      Rx_over_err:64/little,
      Rx_crc_err:64/little,
      Collisions:64/little,
      Duration_seconds:32/little>>;
struct_to_binary(#packet{reason = _Reason,
                         tableId = _TableId,
                         inPortNum = _InPortNum,
                         pktData = _PktData}) ->
    implement_manually;
struct_to_binary(#port_event{eventMask = EventMask,
                             portNum = PortNum,
                             state = State}) ->
	M = enum_to_integer(port_event_mask_t, EventMask),
	S = enum_to_integer(port_state_t, State),
    <<M:32/little,PortNum:32/little,S:32/little>>;
struct_to_binary(#flow_event{eventMask = EventMask,
                             flowMatch = FlowMatch}) ->
	M = enum_to_integer(port_event_mask_t, EventMask),
	MatchBin = struct_to_binary(FlowMatch),
    <<M:32/little,MatchBin:224/binary>>;
struct_to_binary(#flow_table_info{numEntries = NumEntries,
                                  maxEntries = MaxEntries}) ->
    <<NumEntries:32/little,MaxEntries:32/little>>;
struct_to_binary(#port_queue_stats{txBytes = TxBytes,
                                   txPkts = TxPkts,
                                   duration_seconds = Duration_seconds}) ->
    <<TxBytes:64/little,TxPkts:64/little,Duration_seconds:32/little>>.

binary_to_struct(flow_entry,<<TableId:32/little,
							  Priority:32/little,
							  DataBin:200/binary,
							  HardTime:32/little,
							  IdleTime:32/little,
							  Cookie:64/little>>) ->
	T = integer_to_enum(flow_table_id_t, TableId),
	#ofdpa_flow_entry{tableId = T,
				priority = Priority,
				flowData = binary_to_struct(table_to_struct(T), DataBin),
				hard_time = HardTime,
				idle_time = IdleTime,
				cookie = Cookie};

binary_to_struct(ingress_port_flow_match,
                 <<InPort:32/little,InPortMask:32/little>>) ->
    #ingress_port_flow_match{inPort = InPort,inPortMask = InPortMask};
binary_to_struct(ingress_port_flow_entry,
                 <<MatchBin:8/binary,T:32/little,_/binary>>) ->
	Match_criteria = binary_to_struct(ingress_port_flow_match, MatchBin),
	GotoTableId = integer_to_enum(flow_table_id_t, T),
    #ingress_port_flow_entry{match_criteria = Match_criteria,
                             gotoTableId = GotoTableId};
binary_to_struct(vlan_flow_match,
                 <<InPort:32/little,
                   VlanId:16/little,
                   VlanIdMask:16/little>>) ->
    #vlan_flow_match{inPort = InPort,
                     vlanId = VlanId,
                     vlanIdMask = VlanIdMask};
binary_to_struct(vlan_flow_entry,
                 <<MatchBin:8/binary,
                   T:32/little,
                   NewVlanId:16/little,_/binary>>) ->
	Match_criteria = binary_to_struct(vlan_flow_match, MatchBin),
	GotoTableId = integer_to_enum(flow_table_id_t, T),
    #vlan_flow_entry{match_criteria = Match_criteria,
                     gotoTableId = GotoTableId,
                     newVlanId = NewVlanId};
binary_to_struct(termination_mac_flow_match,
                 <<InPort:32/little,
                   InPortMask:32/little,
                   EtherType:16/little,
                   DestMac:6/binary,
                   DestMacMask:6/binary,
                   VlanId:16/little,
                   VlanIdMask:16/little>>) ->
    #termination_mac_flow_match{inPort = InPort,
                                inPortMask = InPortMask,
                                etherType = EtherType,
                                destMac = DestMac,
                                destMacMask = DestMacMask,
                                vlanId = VlanId,
                                vlanIdMask = VlanIdMask};
binary_to_struct(termination_mac_flow_entry,
                 <<MatchBin:16/binary,
                   T:32/little,
                   _:16,
                   OutputPort:32/little,_/binary>>) ->
	Match_criteria = binary_to_struct(termination_mac_flow_match, MatchBin),
	GotoTableId = integer_to_enum(flow_table_id_t, T),
    #termination_mac_flow_entry{match_criteria = Match_criteria,
                                gotoTableId = GotoTableId,
                                outputPort = OutputPort};
binary_to_struct(bridging_flow_match,
                 <<VlanId:16/little,
                   TunnelId:16/little,
                   DestMac:6/binary,
                   DestMacMask:6/binary>>) ->
    #bridging_flow_match{vlanId = VlanId,
                         tunnelId = TunnelId,
                         destMac = DestMac,
                         destMacMask = DestMacMask};
binary_to_struct(bridging_flow_entry,
                 <<MatchBin:16/binary,
                   T:32/little,
                   GroupID:32/little,
                   TunnelLogicalPort:32/little,
                   OutputPort:32/little,_/binary>>) ->
	Match_criteria = binary_to_struct(bridging_flow_match, MatchBin),
	GotoTableId = integer_to_enum(flow_table_id_t, T),
    #bridging_flow_entry{match_criteria = Match_criteria,
                         gotoTableId = GotoTableId,
                         groupID = GroupID,
                         tunnelLogicalPort = TunnelLogicalPort,
                         outputPort = OutputPort};
binary_to_struct(unicast_routing_flow_match,
                 <<EtherType:16/little,
                   DstIp4:4/binary,
                   DstIp4Mask:4/binary,
                   DstIp6:16/binary,
                   DstIp6Mask:16/binary>>) ->
    #unicast_routing_flow_match{etherType = EtherType,
                                dstIp4 = DstIp4,
                                dstIp4Mask = DstIp4Mask,
                                dstIp6 = DstIp6,
                                dstIp6Mask = DstIp6Mask};
binary_to_struct(unicast_routing_flow_entry,
                 <<MatchBin:42/binary,
                   T:32/little,
                   _:16,
                   GroupID:32/little,_/binary>>) ->
	Match_criteria = binary_to_struct(unicast_routing_flow_match, MatchBin),
	GotoTableId = integer_to_enum(flow_table_id_t, T),
    #unicast_routing_flow_entry{match_criteria = Match_criteria,
                                gotoTableId = GotoTableId,
                                groupID = GroupID};
binary_to_struct(multicast_routing_flow_match,
                 <<EtherType:16/little,
                   VlanId:16/little,
                   SrcIp4:4/binary,
                   SrcIp4Mask:4/binary,
                   DstIp4:4/binary,
                   SrcIp6:16/binary,
                   SrcIp6Mask:16/binary,
                   DstIp6:16/binary>>) ->
    #multicast_routing_flow_match{etherType = EtherType,
                                  vlanId = VlanId,
                                  srcIp4 = SrcIp4,
                                  srcIp4Mask = SrcIp4Mask,
                                  dstIp4 = DstIp4,
                                  srcIp6 = SrcIp6,
                                  srcIp6Mask = SrcIp6Mask,
                                  dstIp6 = DstIp6};
binary_to_struct(multicast_routing_flow_entry,
                 <<MatchBin:64/binary,
                   T:32/little,
                   GroupID:32/little,_/binary>>) ->
	Match_criteria = binary_to_struct(multicast_routing_flow_match, MatchBin),
	GotoTableId = integer_to_enum(flow_table_id_t, T),
    #multicast_routing_flow_entry{match_criteria = Match_criteria,
                                  gotoTableId = GotoTableId,
                                  groupID = GroupID};
binary_to_struct(policy_acl_flow_match,
                 <<InPort:32/little,
                   InPortMask:32/little,
                   SrcMac:6/binary,
                   SrcMacMask:6/binary,
                   DestMac:6/binary,
                   DestMacMask:6/binary,
                   EtherType:16/little,
                   VlanId:16/little,
                   VlanIdMask:16/little,
                   VlanPcp:16/little,
                   VlanPcpMask:16/little,
                   _:16,
                   TunnelId:32/little,
                   SourceIp4:4/binary,
                   SourceIp4Mask:4/binary,
                   DestIp4:4/binary,
                   DestIp4Mask:4/binary,
                   SourceIp6:16/binary,
                   SourceIp6Mask:16/binary,
                   DestIp6:16/binary,
                   DestIp6Mask:16/binary,
                   Ipv4ArpSpa:32/little,
                   Ipv4ArpSpaMask:32/little,
                   IpProto:16/little,
                   IpProtoMask:16/little,
                   Dscp:16/little,
                   DscpMask:16/little,
                   Ecn:16/little,
                   EcnMask:16/little,
                   SrcL4Port:32/little,
                   SrcL4PortMask:32/little,
                   DestL4Port:32/little,
                   DestL4PortMask:32/little,
                   IcmpType:8/little,
                   IcmpTypeMask:8/little,
                   IcmpCode:8/little,
                   IcmpCodeMask:8/little,
                   Ipv6FlowLabel:32/little,
                   Ipv6FlowLabelMask:32/little>>) ->
    #policy_acl_flow_match{inPort = InPort,
                           inPortMask = InPortMask,
                           srcMac = SrcMac,
                           srcMacMask = SrcMacMask,
                           destMac = DestMac,
                           destMacMask = DestMacMask,
                           etherType = EtherType,
                           vlanId = VlanId,
                           vlanIdMask = VlanIdMask,
                           vlanPcp = VlanPcp,
                           vlanPcpMask = VlanPcpMask,
                           tunnelId = TunnelId,
                           sourceIp4 = SourceIp4,
                           sourceIp4Mask = SourceIp4Mask,
                           destIp4 = DestIp4,
                           destIp4Mask = DestIp4Mask,
                           sourceIp6 = SourceIp6,
                           sourceIp6Mask = SourceIp6Mask,
                           destIp6 = DestIp6,
                           destIp6Mask = DestIp6Mask,
                           ipv4ArpSpa = Ipv4ArpSpa,
                           ipv4ArpSpaMask = Ipv4ArpSpaMask,
                           ipProto = IpProto,
                           ipProtoMask = IpProtoMask,
                           dscp = Dscp,
                           dscpMask = DscpMask,
                           ecn = Ecn,
                           ecnMask = EcnMask,
                           srcL4Port = SrcL4Port,
                           srcL4PortMask = SrcL4PortMask,
                           destL4Port = DestL4Port,
                           destL4PortMask = DestL4PortMask,
                           icmpType = IcmpType,
                           icmpTypeMask = IcmpTypeMask,
                           icmpCode = IcmpCode,
                           icmpCodeMask = IcmpCodeMask,
                           ipv6FlowLabel = Ipv6FlowLabel,
                           ipv6FlowLabelMask = Ipv6FlowLabelMask};
binary_to_struct(policy_acl_flow_entry,
                 <<MatchBin:176/binary,
                   GroupID:32/little,
                   QueueIDAction:8/little,
                   QueueID:8/little,
                   VlanPcpAction:8/little,
                   VlanPcp:8/little,
                   DscpAction:8/little,
                   Dscp:8/little,
                   _:16,
                   OutputTunnelPort:32/little,
                   OutputPort:32/little,
                   ClearActions:32/little,_/binary>>) ->
	Match_criteria = binary_to_struct(flow_match, MatchBin),
    #policy_acl_flow_entry{match_criteria = Match_criteria,
                           groupID = GroupID,
                           queueIDAction = QueueIDAction,
                           queueID = QueueID,
                           vlanPcpAction = VlanPcpAction,
                           vlanPcp = VlanPcp,
                           dscpAction = DscpAction,
                           dscp = Dscp,
                           outputTunnelPort = OutputTunnelPort,
                           outputPort = OutputPort,
                           clearActions = ClearActions};
binary_to_struct(flow_entry_stats,
                 <<DurationSec:32/little,
                   _:32,
                   ReceivedPackets:64/little,
                   ReceivedBytes:64/little>>) ->
    #flow_entry_stats{durationSec = DurationSec,
                      receivedPackets = ReceivedPackets,
                      receivedBytes = ReceivedBytes};
binary_to_struct(group_entry, <<GroupId:32/little>>) ->
    #group_entry{groupId = GroupId};
binary_to_struct(group_entry_stats,
                 <<RefCount:32/little,
                   Duration:32/little,
                   BucketCount:32/little>>) ->
    #group_entry_stats{refCount = RefCount,
                       duration = Duration,
                       bucketCount = BucketCount};
binary_to_struct(l_2_interface_group_bucket_data,
                 <<OutputPort:32/little,PopVlanTag:32/little,_binary>>) ->
    #l_2_interface_group_bucket_data{outputPort = OutputPort,
                                     popVlanTag = PopVlanTag};
binary_to_struct(l_3_interface_group_bucket_data,
                 <<VlanId:32/little,SrcMac:6/binary,_/binary>>) ->
    #l_3_interface_group_bucket_data{vlanId = VlanId,srcMac = SrcMac};
binary_to_struct(l_3_unicast_group_bucket_data,
                 <<SrcMac:6/binary,DstMac:6/binary,VlanId:32/little,_binary>>) ->
    #l_3_unicast_group_bucket_data{srcMac = SrcMac,
                                   dstMac = DstMac,
                                   vlanId = VlanId};
binary_to_struct(l_2_overlay_group_bucket_data,
                 <<OutputPort:32/little,_/binary>>) ->
    #l_2_overlay_group_bucket_data{outputPort = OutputPort};
binary_to_struct(l_2_rewrite_group_bucket_data,
                 <<SrcMac:6/binary,DstMac:6/binary,VlanId:32/little,_/binary>>) ->
    #l_2_rewrite_group_bucket_data{srcMac = SrcMac,
                                   dstMac = DstMac,
                                   vlanId = VlanId};
binary_to_struct(group_bucket_entry,
				 <<GroupType:32/little,		%% added manually
                   GroupId:32/little,		%% C struct starts here
                   BucketIndex:32/little,
                   ReferenceGroupId:32/little,
                   DataBin:16/binary>>) ->
	T = integer_to_enum(group_entry_type_t, GroupType),
	BucketData = binary_to_struct(bucket_type(T), DataBin),
    #group_bucket_entry{groupId = GroupId,
                        bucketIndex = BucketIndex,
                        referenceGroupId = ReferenceGroupId,
                        bucketData = BucketData};
binary_to_struct(group_table_info,
                 <<NumGroupEntries:32/little,
                   MaxGroupEntries:32/little,
                   MaxBucketEntries:32/little>>) ->
    #group_table_info{numGroupEntries = NumGroupEntries,
                      maxGroupEntries = MaxGroupEntries,
                      maxBucketEntries = MaxBucketEntries};
binary_to_struct(port_feature,
                 <<C:32/little,
                   A:32/little,
                   S:32/little,
                   P:32/little>>) ->
	Curr = integer_to_enum(port_feature_t, C),
	Advertised = integer_to_enum(port_feature_t, A),
	Supported = integer_to_enum(port_feature_t, S),
	Peer = integer_to_enum(port_feature_t, P),
    #port_feature{curr = Curr,
                  advertised = Advertised,
                  supported = Supported,
                  peer = Peer};
binary_to_struct(port_stats,
                 <<Rx_packets:64/little,
                   Tx_packets:64/little,
                   Rx_bytes:64/little,
                   Tx_bytes:64/little,
                   Rx_errors:64/little,
                   Tx_errors:64/little,
                   Rx_drops:64/little,
                   Tx_drops:64/little,
                   Rx_frame_err:64/little,
                   Rx_over_err:64/little,
                   Rx_crc_err:64/little,
                   Collisions:64/little,
                   Duration_seconds:32/little,_/binary>>) ->
    #port_stats{rx_packets = Rx_packets,
                tx_packets = Tx_packets,
                rx_bytes = Rx_bytes,
                tx_bytes = Tx_bytes,
                rx_errors = Rx_errors,
                tx_errors = Tx_errors,
                rx_drops = Rx_drops,
                tx_drops = Tx_drops,
                rx_frame_err = Rx_frame_err,
                rx_over_err = Rx_over_err,
                rx_crc_err = Rx_crc_err,
                collisions = Collisions,
                duration_seconds = Duration_seconds};
binary_to_struct(packet, _) ->
    implement_manually;
binary_to_struct(port_event,
                 <<M:32/little,PortNum:32/little,S:32/little>>) ->
	EventMask = integer_to_enum(port_event_mask_t, M),
	State = integer_to_enum(port_state_t, S),
    #port_event{eventMask = EventMask,portNum = PortNum,state = State};
binary_to_struct(flow_event,
                 <<M:32/little,MatchBin:224/binary>>) ->
	EventMask = integer_to_enum(flow_event_mask_t, M),
	FlowMatch = binary_to_struct(flow_entry, MatchBin),
    #flow_event{eventMask = EventMask,flowMatch = FlowMatch};
binary_to_struct(flow_table_info,
                 <<NumEntries:32/little,MaxEntries:32/little>>) ->
    #flow_table_info{numEntries = NumEntries,maxEntries = MaxEntries};
binary_to_struct(port_queue_stats,
                 <<TxBytes:64/little,
                   TxPkts:64/little,
                   Duration_seconds:32/little,_/binary>>) ->
    #port_queue_stats{txBytes = TxBytes,
                      txPkts = TxPkts,
                      duration_seconds = Duration_seconds}.

%% added manually

table_to_struct(flow_table_id_ingress_port) -> ingress_port_flow_entry;
table_to_struct(flow_table_id_vlan) -> vlan_flow_entry;
table_to_struct(flow_table_id_termination_mac) -> termination_mac_flow_entry;
table_to_struct(flow_table_id_unicast_routing) -> unicast_routing_flow_entry;
table_to_struct(flow_table_id_multicast_routing) -> multicast_routing_flow_entry;
table_to_struct(flow_table_id_bridging) -> bridging_flow_entry;
table_to_struct(flow_table_id_acl_policy) -> acl_policy_flow_entry.

bucket_type(group_entry_type_l2_interface) -> l_2_interface_group_bucket_data;
bucket_type(group_entry_type_l3_interface) -> l_3_interface_group_bucket_data;
bucket_type(group_entry_type_l3_unicast) -> l_3_unicast_group_bucket_data;
bucket_type(group_entry_type_l2_overlay) -> l_2_overlay_group_bucket_data;
bucket_type(group_entry_type_l2_rewrite) -> l_2_rewrite_group_bucket_data.

%% generated using 'genera stubs'

ofdpaFlowEntryInit(TableId) ->
    call([{enum,error_t},{struct,flow_entry}],
         100,
         [{uint32_t,enum_to_integer(flow_table_id_t, TableId)}]).

ofdpaFlowAdd(Flow) ->
    call([{enum,error_t}], 101, [struct_to_binary(Flow)]).

ofdpaFlowModify(Flow) ->
    call([{enum,error_t}], 102, [struct_to_binary(Flow)]).

ofdpaFlowDelete(Flow) ->
    call([{enum,error_t}], 103, [struct_to_binary(Flow)]).

ofdpaFlowNextGet(Flow) ->
    call([{enum,error_t},{struct,flow_entry}],
         104,
         [struct_to_binary(Flow)]).

ofdpaFlowStatsGet(Flow) ->
    call([{enum,error_t},{struct,flow_entry_stats}],
         105,
         [struct_to_binary(Flow)]).

ofdpaFlowByCookieGet(Cookie) ->
    call([{enum,error_t},{struct,flow_entry},{struct,flow_entry_stats}],
         106,
         [{uint64_t,Cookie}]).

ofdpaFlowByCookieDelete(Cookie) ->
    call([{enum,error_t}], 107, [{uint64_t,Cookie}]).

ofdpaGroupTypeGet(GroupId) ->
    call([{enum,error_t},uint32_t],
         108,
         [{uint32_t,GroupId}]).

ofdpaGroupVlanGet(GroupId) ->
    call([{enum,error_t},uint32_t],
         109,
         [{uint32_t,GroupId}]).

ofdpaGroupPortIdGet(GroupId) ->
    call([{enum,error_t},uint32_t],
         110,
         [{uint32_t,GroupId}]).

ofdpaGroupIndexShortGet(GroupId) ->
    call([{enum,error_t},uint32_t],
         111,
         [{uint32_t,GroupId}]).

ofdpaGroupIndexGet(GroupId) ->
    call([{enum,error_t},uint32_t],
         112,
         [{uint32_t,GroupId}]).

ofdpaGroupTypeSet(GroupId, Type) ->
    call([{enum,error_t},uint32_t],
         113,
         [{uint32_t,GroupId},{uint32_t,Type}]).

ofdpaGroupVlanSet(GroupId, VlanId) ->
    call([{enum,error_t},uint32_t],
         114,
         [{uint32_t,GroupId},{uint32_t,VlanId}]).

ofdpaGroupOverlayTunnelIdSet(GroupId, TunnelId) ->
    call([{enum,error_t},uint32_t],
         115,
         [{uint32_t,GroupId},{uint32_t,TunnelId}]).

ofdpaGroupOverlaySubTypeSet(GroupId, SubType) ->
    call([{enum,error_t},uint32_t],
         116,
         [{uint32_t,GroupId},
          {uint32_t,enum_to_integer(l2_overlay_subtype_t, SubType)}]).

ofdpaGroupOverlayIndexSet(GroupId, Index) ->
    call([{enum,error_t},uint32_t],
         117,
         [{uint32_t,GroupId},{uint32_t,Index}]).

ofdpaGroupPortIdSet(GroupId, PortId) ->
    call([{enum,error_t},uint32_t],
         118,
         [{uint32_t,GroupId},{uint32_t,PortId}]).

ofdpaGroupIndexShortSet(GroupId, Index) ->
    call([{enum,error_t},uint32_t],
         119,
         [{uint32_t,GroupId},{uint32_t,Index}]).

ofdpaGroupIndexSet(GroupId, Index) ->
    call([{enum,error_t},uint32_t],
         120,
         [{uint32_t,GroupId},{uint32_t,Index}]).

ofdpaGroupDecode(GroupId, BufSize) ->
    call([{enum,error_t},string],
         121,
         [{uint32_t,GroupId},{uint32_t,BufSize}]).

ofdpaGroupEntryInit(GroupType) ->
    call([{enum,error_t},{struct,group_entry}],
         122,
         [{uint32_t,enum_to_integer(group_entry_type_t, GroupType)}]).

ofdpaGroupAdd(Group) ->
    call([{enum,error_t}], 123, [struct_to_binary(Group)]).

ofdpaGroupDelete(GroupId) ->
    call([{enum,error_t}], 124, [{uint32_t,GroupId}]).

ofdpaGroupNextGet(GroupId) ->
    call([{enum,error_t},{struct,group_entry}],
         125,
         [{uint32_t,GroupId}]).

ofdpaGroupTypeNextGet(GroupId, GroupType) ->
    call([{enum,error_t},{struct,group_entry}],
         126,
         [{uint32_t,GroupId},
          {uint32_t,enum_to_integer(group_entry_type_t, GroupType)}]).

ofdpaGroupStatsGet(GroupId) ->
    call([{enum,error_t},{struct,group_entry_stats}],
         127,
         [{uint32_t,GroupId}]).

ofdpaGroupBucketEntryInit(GroupType) ->
    call([{enum,error_t},{struct,group_bucket_entry}],
         128,
         [{uint32_t,enum_to_integer(group_entry_type_t, GroupType)}]).

ofdpaGroupBucketEntryAdd(Bucket) ->
    call([{enum,error_t}], 129, [struct_to_binary(Bucket)]).

ofdpaGroupBucketEntryDelete(GroupId, BucketIndex) ->
    call([{enum,error_t}],
         130,
         [{uint32_t,GroupId},{uint32_t,BucketIndex}]).

ofdpaGroupBucketsDeleteAll(GroupId) ->
    call([{enum,error_t}], 131, [{uint32_t,GroupId}]).

ofdpaGroupBucketEntryGet(GroupId, BucketIndex) ->
    call([{enum,error_t},{struct,group_bucket_entry}],
         132,
         [{uint32_t,GroupId},
          {uint32_t,BucketIndex}]).

ofdpaGroupBucketEntryFirstGet(GroupId) ->
    call([{enum,error_t},{struct,group_bucket_entry}],
         133,
         [{uint32_t,GroupId}]).

ofdpaGroupBucketEntryNextGet(GroupId, BucketIndex) ->
    call([{enum,error_t},{struct,group_bucket_entry}],
         134,
         [{uint32_t,GroupId},
          {uint32_t,BucketIndex}]).

ofdpaGroupBucketEntryModify(Bucket) ->
    call([{enum,error_t}], 135, [struct_to_binary(Bucket)]).

ofdpaGroupTableInfoGet(GroupType) ->
    call([{enum,error_t},{struct,group_table_info}],
         136,
         [{uint32_t,enum_to_integer(group_entry_type_t, GroupType)}]).

ofdpaPortTypeGet(PortNum) ->
    call([uint32_t], 137, [{uint32_t,PortNum}]).

ofdpaPortTypeSet(PortNum, Type) ->
    call([uint32_t], 138, [{uint32_t,PortNum},{uint32_t,Type}]).

ofdpaPortIndexGet(PortNum) ->
    call([uint32_t], 139, [{uint32_t,PortNum}]).

ofdpaPortIndexSet(PortNum, Index) ->
    call([uint32_t], 140, [{uint32_t,PortNum},{uint32_t,Index}]).

ofdpaPortNextGet(PortNum) ->
    call([{enum,error_t},uint32_t],
         141,
         [{uint32_t,PortNum}]).

ofdpaPortMacGet(PortNum) ->
    call([{enum,error_t},ofdpaMacAddr_t],
         142,
         [{uint32_t,PortNum}]).

ofdpaPortNameGet(PortNum) ->
    call([{enum,error_t},string],
         143,
         [{uint32_t,PortNum}]).

ofdpaPortStateGet(PortNum) ->
    call([{enum,error_t},{enum,port_state_t}],
         144,
         [{uint32_t,PortNum}]).

ofdpaPortConfigSet(PortNum, Config) ->
    call([{enum,error_t}],
         145,
         [{uint32_t,PortNum},
          {uint32_t,enum_to_integer(port_config_t, Config)}]).

ofdpaPortConfigGet(PortNum) ->
    call([{enum,error_t},{enum,port_config_t}],
         146,
         [{uint32_t,PortNum}]).

ofdpaPortMaxSpeedGet(PortNum) ->
    call([{enum,error_t},uint32_t],
         147,
         [{uint32_t,PortNum}]).

ofdpaPortCurrSpeedGet(PortNum) ->
    call([{enum,error_t},uint32_t],
         148,
         [{uint32_t,PortNum}]).

ofdpaPortFeatureGet(PortNum) ->
    call([{enum,error_t},{struct,port_feature}],
         149,
         [{uint32_t,PortNum}]).

ofdpaPortAdvertiseFeatureSet(PortNum, Advertise) ->
    call([{enum,error_t}],
         150,
         [{uint32_t,PortNum},{uint32_t,Advertise}]).

ofdpaPortStatsClear(PortNum) ->
    call([{enum,error_t}], 151, [{uint32_t,PortNum}]).

ofdpaPortStatsGet(PortNum) ->
    call([{enum,error_t},{struct,port_stats}],
         152,
         [{uint32_t,PortNum}]).

ofdpaPktSend(Pkt, Flags, OutPortNum, InPortNum) ->
    call([{enum,error_t}],
         153,
         [Pkt,
          {uint32_t,Flags},
          {uint32_t,OutPortNum},
          {uint32_t,InPortNum}]).

ofdpaMaxPktSizeGet() ->
    call([{enum,error_t},uint32_t], 154, []).

ofdpaPktReceive(Timeout, Pkt) ->
    call([{enum,error_t},timeval,{struct,packet}],
         155,
         [{timeval,Timeout},struct_to_binary(Pkt)]).

ofdpaEventReceive(Timeout) ->
    call([{enum,error_t},timeval], 156, [{timeval,Timeout}]).

ofdpaPortEventNextGet(EventData) ->
    call([{enum,error_t},{struct,port_event}],
         157,
         [struct_to_binary(EventData)]).

ofdpaFlowEventNextGet(EventData) ->
    call([{enum,error_t},{struct,flow_event}],
         158,
         [struct_to_binary(EventData)]).

ofdpaFlowTableInfoGet(TableId) ->
    call([{enum,error_t},{struct,flow_table_info}],
         159,
         [{uint32_t,enum_to_integer(flow_table_id_t, TableId)}]).

ofdpaNumQueuesGet(PortNum) ->
    call([{enum,error_t},uint32_t],
         160,
         [{uint32_t,PortNum}]).

ofdpaQueueStatsGet(PortNum, QueueId) ->
    call([{enum,error_t},{struct,port_queue_stats}],
         161,
         [{uint32_t,PortNum},{uint32_t,QueueId}]).

ofdpaQueueStatsClear(PortNum, QueueId) ->
    call([{enum,error_t}], 162, [{uint32_t,PortNum},{uint32_t,QueueId}]).

ofdpaQueueRateSet(PortNum, QueueId, MinRate, MaxRate) ->
    call([{enum,error_t}],
         163,
         [{uint32_t,PortNum},
          {uint32_t,QueueId},
          {uint32_t,MinRate},
          {uint32_t,MaxRate}]).

ofdpaQueueRateGet(PortNum, QueueId) ->
    call([{enum,error_t},uint32_t,uint32_t],
         164,
         [{uint32_t,PortNum},
          {uint32_t,QueueId}]).

%%EOF
