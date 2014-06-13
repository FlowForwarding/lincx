%%
%% LINCX - OF-DPA integration layer
%%
-module(ofdpa).

-export([enum_to_integer/2,integer_to_enum/2]).

%% generated using 'genera exports'

-export([ofdpaFlowEntryInit/2,
         ofdpaFlowAdd/1,
         ofdpaFlowModify/1,
         ofdpaFlowDelete/1,
         ofdpaFlowNextGet/2,
         ofdpaFlowStatsGet/2,
         ofdpaFlowByCookieGet/3,
         ofdpaFlowByCookieDelete/1,
         ofdpaGroupTypeGet/2,
         ofdpaGroupVlanGet/2,
         ofdpaGroupPortIdGet/2,
         ofdpaGroupIndexShortGet/2,
         ofdpaGroupIndexGet/2,
         ofdpaGroupTypeSet/2,
         ofdpaGroupVlanSet/2,
         ofdpaGroupOverlayTunnelIdSet/2,
         ofdpaGroupOverlaySubTypeSet/2,
         ofdpaGroupOverlayIndexSet/2,
         ofdpaGroupPortIdSet/2,
         ofdpaGroupIndexShortSet/2,
         ofdpaGroupIndexSet/2,
         ofdpaGroupDecode/3,
         ofdpaGroupEntryInit/2,
         ofdpaGroupAdd/1,
         ofdpaGroupDelete/1,
         ofdpaGroupNextGet/2,
         ofdpaGroupTypeNextGet/3,
         ofdpaGroupStatsGet/2,
         ofdpaGroupBucketEntryInit/2,
         ofdpaGroupBucketEntryAdd/1,
         ofdpaGroupBucketEntryDelete/2,
         ofdpaGroupBucketsDeleteAll/1,
         ofdpaGroupBucketEntryGet/3,
         ofdpaGroupBucketEntryFirstGet/2,
         ofdpaGroupBucketEntryNextGet/3,
         ofdpaGroupBucketEntryModify/1,
         ofdpaGroupTableInfoGet/2,
         ofdpaPortTypeGet/2,
         ofdpaPortTypeSet/2,
         ofdpaPortIndexGet/2,
         ofdpaPortIndexSet/2,
         ofdpaPortNextGet/2,
         ofdpaPortMacGet/2,
         ofdpaPortNameGet/2,
         ofdpaPortStateGet/2,
         ofdpaPortConfigSet/2,
         ofdpaPortConfigGet/2,
         ofdpaPortMaxSpeedGet/2,
         ofdpaPortCurrSpeedGet/2,
         ofdpaPortFeatureGet/2,
         ofdpaPortAdvertiseFeatureSet/2,
         ofdpaPortStatsClear/1,
         ofdpaPortStatsGet/2,
         ofdpaPktSend/4,
         ofdpaMaxPktSizeGet/1,
         ofdpaPktReceive/2,
         ofdpaEventReceive/1,
         ofdpaPortEventNextGet/1,
         ofdpaFlowEventNextGet/1,
         ofdpaFlowTableInfoGet/2,
         ofdpaNumQueuesGet/2,
         ofdpaQueueStatsGet/3,
         ofdpaQueueStatsClear/2,
         ofdpaQueueRateSet/4,
         ofdpaQueueRateGet/4]).

-include("ofdpa.hrl").

%%
%%  4: len (without len field)
%%	2: #0xca11
%%	2: function
%%	4: cookie
%%	-: args
%%

call(Returns, What, Args) ->
	case gen_server:call(ofdpa_link, {call,What,encode_args(Args)}) of
	{ok,RetBlob} ->
		{ok,decode_args(RetBlob, Returns)};
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
	decode_args(Blob, Rs, [binary_to_struct(S, Bin)|Acc]).

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
enum_to_integer(port_config_t, port_config_down) ->
    1;
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
integer_to_enum(port_config_t, 1) ->
    port_config_down;
integer_to_enum(port_state_t, 1) ->
    port_state_link_down.

%% struct_to_binary/1 and binary_to_struct/2 generated using 'genera struct'

struct_to_binary(#ingress_port_flow_match{inPort = InPort,
                                          inPortMask = InPortMask}) ->
    <<InPort:32/little,InPortMask:32/little>>;
struct_to_binary(#ingress_port_flow_entry{match_criteria =
                                              Match_criteria,
                                          gotoTableId = GotoTableId}) ->
    <<Match_criteria:64/binary,GotoTableId:32/binary>>;
struct_to_binary(#vlan_flow_match{inPort = InPort,
                                  vlanId = VlanId,
                                  vlanIdMask = VlanIdMask}) ->
    <<InPort:32/little,VlanId:16/little,VlanIdMask:16/little>>;
struct_to_binary(#vlan_flow_entry{match_criteria = Match_criteria,
                                  gotoTableId = GotoTableId,
                                  newVlanId = NewVlanId}) ->
    <<Match_criteria:64/binary,GotoTableId:32/binary,NewVlanId:16/little>>;
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
      DestMac:48/binary,
      DestMacMask:48/binary,
      VlanId:16/little,
      VlanIdMask:16/little>>;
struct_to_binary(#termination_mac_flow_entry{match_criteria =
                                                 Match_criteria,
                                             gotoTableId = GotoTableId,
                                             outputPort = OutputPort}) ->
    <<Match_criteria:208/binary,
      GotoTableId:32/binary,
      0:16,
      OutputPort:32/little>>;
struct_to_binary(#bridging_flow_match{vlanId = VlanId,
                                      tunnelId = TunnelId,
                                      destMac = DestMac,
                                      destMacMask = DestMacMask}) ->
    <<VlanId:16/little,
      TunnelId:16/little,
      DestMac:48/binary,
      DestMacMask:48/binary>>;
struct_to_binary(#bridging_flow_entry{match_criteria = Match_criteria,
                                      gotoTableId = GotoTableId,
                                      groupID = GroupID,
                                      tunnelLogicalPort =
                                          TunnelLogicalPort,
                                      outputPort = OutputPort}) ->
    <<Match_criteria:128/binary,
      GotoTableId:32/binary,
      GroupID:32/little,
      TunnelLogicalPort:32/little,
      OutputPort:32/little>>;
struct_to_binary(#unicast_routing_flow_match{etherType = EtherType,
                                             dstIp4 = DstIp4,
                                             dstIp4Mask = DstIp4Mask,
                                             dstIp6 = DstIp6,
                                             dstIp6Mask = DstIp6Mask}) ->
    <<EtherType:16/little,
      DstIp4:32/binary,
      DstIp4Mask:32/binary,
      DstIp6:128/binary,
      DstIp6Mask:128/binary>>;
struct_to_binary(#unicast_routing_flow_entry{match_criteria =
                                                 Match_criteria,
                                             gotoTableId = GotoTableId,
                                             groupID = GroupID}) ->
    <<Match_criteria:336/binary,
      GotoTableId:32/binary,
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
      SrcIp4:32/binary,
      SrcIp4Mask:32/binary,
      DstIp4:32/binary,
      SrcIp6:128/binary,
      SrcIp6Mask:128/binary,
      DstIp6:128/binary>>;
struct_to_binary(#multicast_routing_flow_entry{match_criteria =
                                                   Match_criteria,
                                               gotoTableId = GotoTableId,
                                               groupID = GroupID}) ->
    <<Match_criteria:512/binary,GotoTableId:32/binary,GroupID:32/little>>;
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
      SrcMac:48/binary,
      SrcMacMask:48/binary,
      DestMac:48/binary,
      DestMacMask:48/binary,
      EtherType:16/little,
      VlanId:16/little,
      VlanIdMask:16/little,
      VlanPcp:16/little,
      VlanPcpMask:16/little,
      0:16,
      TunnelId:32/little,
      SourceIp4:32/binary,
      SourceIp4Mask:32/binary,
      DestIp4:32/binary,
      DestIp4Mask:32/binary,
      SourceIp6:128/binary,
      SourceIp6Mask:128/binary,
      DestIp6:128/binary,
      DestIp6Mask:128/binary,
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
    <<Match_criteria:1408/binary,
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
struct_to_binary(#flow_entry{tableId = TableId,
                             priority = Priority,
                             flowData = FlowData,
                             hard_time = Hard_time,
                             idle_time = Idle_time,
                             cookie = Cookie}) ->
    <<TableId:32/binary,
      Priority:32/little,
      FlowData:1600/binary,
      Hard_time:32/little,
      Idle_time:32/little,
      Cookie:64/little>>;
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
    <<VlanId:32/little,SrcMac:48/binary>>;
struct_to_binary(#l_3_unicast_group_bucket_data{srcMac = SrcMac,
                                                dstMac = DstMac,
                                                vlanId = VlanId}) ->
    <<SrcMac:48/binary,DstMac:48/binary,VlanId:32/little>>;
struct_to_binary(#l_2_overlay_group_bucket_data{outputPort = OutputPort}) ->
    <<OutputPort:32/little>>;
struct_to_binary(#l_2_rewrite_group_bucket_data{srcMac = SrcMac,
                                                dstMac = DstMac,
                                                vlanId = VlanId}) ->
    <<SrcMac:48/binary,DstMac:48/binary,VlanId:32/little>>;
struct_to_binary(#group_bucket_entry{groupId = GroupId,
                                     bucketIndex = BucketIndex,
                                     referenceGroupId = ReferenceGroupId,
                                     bucketData = BucketData}) ->
    <<GroupId:32/little,
      BucketIndex:32/little,
      ReferenceGroupId:32/little,
      BucketData:128/binary>>;
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
    <<Curr:32/binary,
      Advertised:32/binary,
      Supported:32/binary,
      Peer:32/binary>>;
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
    <<EventMask:32/binary,PortNum:32/little,State:32/binary>>;
struct_to_binary(#flow_event{eventMask = EventMask,
                             flowMatch = FlowMatch}) ->
    <<EventMask:32/binary,FlowMatch:1792/binary>>;
struct_to_binary(#flow_table_info{numEntries = NumEntries,
                                  maxEntries = MaxEntries}) ->
    <<NumEntries:32/little,MaxEntries:32/little>>;
struct_to_binary(#port_queue_stats{txBytes = TxBytes,
                                   txPkts = TxPkts,
                                   duration_seconds = Duration_seconds}) ->
    <<TxBytes:64/little,TxPkts:64/little,Duration_seconds:32/little>>.

binary_to_struct(ingress_port_flow_match,
                 <<InPort:32/little,InPortMask:32/little>>) ->
    #ingress_port_flow_match{inPort = InPort,inPortMask = InPortMask};
binary_to_struct(ingress_port_flow_entry,
                 <<Match_criteria:64/binary,GotoTableId:32/binary>>) ->
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
                 <<Match_criteria:64/binary,
                   GotoTableId:32/binary,
                   NewVlanId:16/little>>) ->
    #vlan_flow_entry{match_criteria = Match_criteria,
                     gotoTableId = GotoTableId,
                     newVlanId = NewVlanId};
binary_to_struct(termination_mac_flow_match,
                 <<InPort:32/little,
                   InPortMask:32/little,
                   EtherType:16/little,
                   DestMac:48/binary,
                   DestMacMask:48/binary,
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
                 <<Match_criteria:208/binary,
                   GotoTableId:32/binary,
                   _:16,
                   OutputPort:32/little>>) ->
    #termination_mac_flow_entry{match_criteria = Match_criteria,
                                gotoTableId = GotoTableId,
                                outputPort = OutputPort};
binary_to_struct(bridging_flow_match,
                 <<VlanId:16/little,
                   TunnelId:16/little,
                   DestMac:48/binary,
                   DestMacMask:48/binary>>) ->
    #bridging_flow_match{vlanId = VlanId,
                         tunnelId = TunnelId,
                         destMac = DestMac,
                         destMacMask = DestMacMask};
binary_to_struct(bridging_flow_entry,
                 <<Match_criteria:128/binary,
                   GotoTableId:32/binary,
                   GroupID:32/little,
                   TunnelLogicalPort:32/little,
                   OutputPort:32/little>>) ->
    #bridging_flow_entry{match_criteria = Match_criteria,
                         gotoTableId = GotoTableId,
                         groupID = GroupID,
                         tunnelLogicalPort = TunnelLogicalPort,
                         outputPort = OutputPort};
binary_to_struct(unicast_routing_flow_match,
                 <<EtherType:16/little,
                   DstIp4:32/binary,
                   DstIp4Mask:32/binary,
                   DstIp6:128/binary,
                   DstIp6Mask:128/binary>>) ->
    #unicast_routing_flow_match{etherType = EtherType,
                                dstIp4 = DstIp4,
                                dstIp4Mask = DstIp4Mask,
                                dstIp6 = DstIp6,
                                dstIp6Mask = DstIp6Mask};
binary_to_struct(unicast_routing_flow_entry,
                 <<Match_criteria:336/binary,
                   GotoTableId:32/binary,
                   _:16,
                   GroupID:32/little>>) ->
    #unicast_routing_flow_entry{match_criteria = Match_criteria,
                                gotoTableId = GotoTableId,
                                groupID = GroupID};
binary_to_struct(multicast_routing_flow_match,
                 <<EtherType:16/little,
                   VlanId:16/little,
                   SrcIp4:32/binary,
                   SrcIp4Mask:32/binary,
                   DstIp4:32/binary,
                   SrcIp6:128/binary,
                   SrcIp6Mask:128/binary,
                   DstIp6:128/binary>>) ->
    #multicast_routing_flow_match{etherType = EtherType,
                                  vlanId = VlanId,
                                  srcIp4 = SrcIp4,
                                  srcIp4Mask = SrcIp4Mask,
                                  dstIp4 = DstIp4,
                                  srcIp6 = SrcIp6,
                                  srcIp6Mask = SrcIp6Mask,
                                  dstIp6 = DstIp6};
binary_to_struct(multicast_routing_flow_entry,
                 <<Match_criteria:512/binary,
                   GotoTableId:32/binary,
                   GroupID:32/little>>) ->
    #multicast_routing_flow_entry{match_criteria = Match_criteria,
                                  gotoTableId = GotoTableId,
                                  groupID = GroupID};
binary_to_struct(policy_acl_flow_match,
                 <<InPort:32/little,
                   InPortMask:32/little,
                   SrcMac:48/binary,
                   SrcMacMask:48/binary,
                   DestMac:48/binary,
                   DestMacMask:48/binary,
                   EtherType:16/little,
                   VlanId:16/little,
                   VlanIdMask:16/little,
                   VlanPcp:16/little,
                   VlanPcpMask:16/little,
                   _:16,
                   TunnelId:32/little,
                   SourceIp4:32/binary,
                   SourceIp4Mask:32/binary,
                   DestIp4:32/binary,
                   DestIp4Mask:32/binary,
                   SourceIp6:128/binary,
                   SourceIp6Mask:128/binary,
                   DestIp6:128/binary,
                   DestIp6Mask:128/binary,
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
                 <<Match_criteria:1408/binary,
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
                   ClearActions:32/little>>) ->
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
binary_to_struct(flow_entry,
                 <<TableId:32/binary,
                   Priority:32/little,
                   FlowData:1600/binary,
                   Hard_time:32/little,
                   Idle_time:32/little,
                   Cookie:64/little>>) ->
    #flow_entry{tableId = TableId,
                priority = Priority,
                flowData = FlowData,
                hard_time = Hard_time,
                idle_time = Idle_time,
                cookie = Cookie};
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
                 <<OutputPort:32/little,PopVlanTag:32/little>>) ->
    #l_2_interface_group_bucket_data{outputPort = OutputPort,
                                     popVlanTag = PopVlanTag};
binary_to_struct(l_3_interface_group_bucket_data,
                 <<VlanId:32/little,SrcMac:48/binary>>) ->
    #l_3_interface_group_bucket_data{vlanId = VlanId,srcMac = SrcMac};
binary_to_struct(l_3_unicast_group_bucket_data,
                 <<SrcMac:48/binary,DstMac:48/binary,VlanId:32/little>>) ->
    #l_3_unicast_group_bucket_data{srcMac = SrcMac,
                                   dstMac = DstMac,
                                   vlanId = VlanId};
binary_to_struct(l_2_overlay_group_bucket_data,
                 <<OutputPort:32/little>>) ->
    #l_2_overlay_group_bucket_data{outputPort = OutputPort};
binary_to_struct(l_2_rewrite_group_bucket_data,
                 <<SrcMac:48/binary,DstMac:48/binary,VlanId:32/little>>) ->
    #l_2_rewrite_group_bucket_data{srcMac = SrcMac,
                                   dstMac = DstMac,
                                   vlanId = VlanId};
binary_to_struct(group_bucket_entry,
                 <<GroupId:32/little,
                   BucketIndex:32/little,
                   ReferenceGroupId:32/little,
                   BucketData:128/binary>>) ->
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
                 <<Curr:32/binary,
                   Advertised:32/binary,
                   Supported:32/binary,
                   Peer:32/binary>>) ->
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
                   Duration_seconds:32/little>>) ->
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
                 <<EventMask:32/binary,PortNum:32/little,State:32/binary>>) ->
    #port_event{eventMask = EventMask,portNum = PortNum,state = State};
binary_to_struct(flow_event,
                 <<EventMask:32/binary,FlowMatch:1792/binary>>) ->
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

%% generated using 'genera stubs'

ofdpaFlowEntryInit(TableId, Flow) ->
    call([{enum,error_t},{struct,flow_entry}],
         100,
         [{uint32_t,enum_to_integer(flow_table_id_t, TableId)},
          struct_to_binary(Flow)]).

ofdpaFlowAdd(Flow) ->
    call([{enum,error_t}], 101, [struct_to_binary(Flow)]).

ofdpaFlowModify(Flow) ->
    call([{enum,error_t}], 102, [struct_to_binary(Flow)]).

ofdpaFlowDelete(Flow) ->
    call([{enum,error_t}], 103, [struct_to_binary(Flow)]).

ofdpaFlowNextGet(Flow, NextFlow) ->
    call([{enum,error_t},{struct,flow_entry}],
         104,
         [struct_to_binary(Flow),struct_to_binary(NextFlow)]).

ofdpaFlowStatsGet(Flow, FlowStats) ->
    call([{enum,error_t},{struct,flow_entry_stats}],
         105,
         [struct_to_binary(Flow),struct_to_binary(FlowStats)]).

ofdpaFlowByCookieGet(Cookie, Flow, FlowStats) ->
    call([{enum,error_t},{struct,flow_entry},{struct,flow_entry_stats}],
         106,
         [{uint64_t,Cookie},
          struct_to_binary(Flow),
          struct_to_binary(FlowStats)]).

ofdpaFlowByCookieDelete(Cookie) ->
    call([{enum,error_t}], 107, [{uint64_t,Cookie}]).

ofdpaGroupTypeGet(GroupId, Type) ->
    call([{enum,error_t},uint32_t],
         108,
         [{uint32_t,GroupId},{uint32_t,Type}]).

ofdpaGroupVlanGet(GroupId, VlanId) ->
    call([{enum,error_t},uint32_t],
         109,
         [{uint32_t,GroupId},{uint32_t,VlanId}]).

ofdpaGroupPortIdGet(GroupId, PortId) ->
    call([{enum,error_t},uint32_t],
         110,
         [{uint32_t,GroupId},{uint32_t,PortId}]).

ofdpaGroupIndexShortGet(GroupId, Index) ->
    call([{enum,error_t},uint32_t],
         111,
         [{uint32_t,GroupId},{uint32_t,Index}]).

ofdpaGroupIndexGet(GroupId, Index) ->
    call([{enum,error_t},uint32_t],
         112,
         [{uint32_t,GroupId},{uint32_t,Index}]).

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

ofdpaGroupDecode(GroupId, OutBuf, BufSize) ->
    call([{enum,error_t},char],
         121,
         [{uint32_t,GroupId},{char,OutBuf},{int,BufSize}]).

ofdpaGroupEntryInit(GroupType, Group) ->
    call([{enum,error_t},{struct,group_entry}],
         122,
         [{uint32_t,enum_to_integer(group_entry_type_t, GroupType)},
          struct_to_binary(Group)]).

ofdpaGroupAdd(Group) ->
    call([{enum,error_t}], 123, [struct_to_binary(Group)]).

ofdpaGroupDelete(GroupId) ->
    call([{enum,error_t}], 124, [{uint32_t,GroupId}]).

ofdpaGroupNextGet(GroupId, NextGroup) ->
    call([{enum,error_t},{struct,group_entry}],
         125,
         [{uint32_t,GroupId},struct_to_binary(NextGroup)]).

ofdpaGroupTypeNextGet(GroupId, GroupType, NextGroup) ->
    call([{enum,error_t},{struct,group_entry}],
         126,
         [{uint32_t,GroupId},
          {uint32_t,enum_to_integer(group_entry_type_t, GroupType)},
          struct_to_binary(NextGroup)]).

ofdpaGroupStatsGet(GroupId, GroupStats) ->
    call([{enum,error_t},{struct,group_entry_stats}],
         127,
         [{uint32_t,GroupId},struct_to_binary(GroupStats)]).

ofdpaGroupBucketEntryInit(GroupType, Bucket) ->
    call([{enum,error_t},{struct,group_bucket_entry}],
         128,
         [{uint32_t,enum_to_integer(group_entry_type_t, GroupType)},
          struct_to_binary(Bucket)]).

ofdpaGroupBucketEntryAdd(Bucket) ->
    call([{enum,error_t}], 129, [struct_to_binary(Bucket)]).

ofdpaGroupBucketEntryDelete(GroupId, BucketIndex) ->
    call([{enum,error_t}],
         130,
         [{uint32_t,GroupId},{uint32_t,BucketIndex}]).

ofdpaGroupBucketsDeleteAll(GroupId) ->
    call([{enum,error_t}], 131, [{uint32_t,GroupId}]).

ofdpaGroupBucketEntryGet(GroupId, BucketIndex, GroupBucket) ->
    call([{enum,error_t},{struct,group_bucket_entry}],
         132,
         [{uint32_t,GroupId},
          {uint32_t,BucketIndex},
          struct_to_binary(GroupBucket)]).

ofdpaGroupBucketEntryFirstGet(GroupId, FirstGroupBucket) ->
    call([{enum,error_t},{struct,group_bucket_entry}],
         133,
         [{uint32_t,GroupId},struct_to_binary(FirstGroupBucket)]).

ofdpaGroupBucketEntryNextGet(GroupId, BucketIndex, NextBucketEntry) ->
    call([{enum,error_t},{struct,group_bucket_entry}],
         134,
         [{uint32_t,GroupId},
          {uint32_t,BucketIndex},
          struct_to_binary(NextBucketEntry)]).

ofdpaGroupBucketEntryModify(Bucket) ->
    call([{enum,error_t}], 135, [struct_to_binary(Bucket)]).

ofdpaGroupTableInfoGet(GroupType, Info) ->
    call([{enum,error_t},{struct,group_table_info}],
         136,
         [{uint32_t,enum_to_integer(group_entry_type_t, GroupType)},
          struct_to_binary(Info)]).

ofdpaPortTypeGet(PortNum, Type) ->
    call([uint32_t], 137, [{uint32_t,PortNum},{uint32_t,Type}]).

ofdpaPortTypeSet(PortNum, Type) ->
    call([uint32_t], 138, [{uint32_t,PortNum},{uint32_t,Type}]).

ofdpaPortIndexGet(PortNum, Index) ->
    call([uint32_t], 139, [{uint32_t,PortNum},{uint32_t,Index}]).

ofdpaPortIndexSet(PortNum, Index) ->
    call([uint32_t], 140, [{uint32_t,PortNum},{uint32_t,Index}]).

ofdpaPortNextGet(PortNum, NextPortNum) ->
    call([{enum,error_t},uint32_t],
         141,
         [{uint32_t,PortNum},{uint32_t,NextPortNum}]).

ofdpaPortMacGet(PortNum, Mac) ->
    call([{enum,error_t},ofdpaMacAddr_t],
         142,
         [{uint32_t,PortNum},{ofdpaMacAddr_t,Mac}]).

ofdpaPortNameGet(PortNum, Name) ->
    call([{enum,error_t},ofdpa_buffdesc],
         143,
         [{uint32_t,PortNum},{ofdpa_buffdesc,Name}]).

ofdpaPortStateGet(PortNum, State) ->
    call([{enum,error_t},{enum,port_state_t}],
         144,
         [{uint32_t,PortNum},
          {uint32_t,enum_to_integer(port_state_t, State)}]).

ofdpaPortConfigSet(PortNum, Config) ->
    call([{enum,error_t}],
         145,
         [{uint32_t,PortNum},
          {uint32_t,enum_to_integer(port_config_t, Config)}]).

ofdpaPortConfigGet(PortNum, Config) ->
    call([{enum,error_t},{enum,port_config_t}],
         146,
         [{uint32_t,PortNum},
          {uint32_t,enum_to_integer(port_config_t, Config)}]).

ofdpaPortMaxSpeedGet(PortNum, MaxSpeed) ->
    call([{enum,error_t},uint32_t],
         147,
         [{uint32_t,PortNum},{uint32_t,MaxSpeed}]).

ofdpaPortCurrSpeedGet(PortNum, CurrSpeed) ->
    call([{enum,error_t},uint32_t],
         148,
         [{uint32_t,PortNum},{uint32_t,CurrSpeed}]).

ofdpaPortFeatureGet(PortNum, Feature) ->
    call([{enum,error_t},{struct,port_feature}],
         149,
         [{uint32_t,PortNum},struct_to_binary(Feature)]).

ofdpaPortAdvertiseFeatureSet(PortNum, Advertise) ->
    call([{enum,error_t}],
         150,
         [{uint32_t,PortNum},{uint32_t,Advertise}]).

ofdpaPortStatsClear(PortNum) ->
    call([{enum,error_t}], 151, [{uint32_t,PortNum}]).

ofdpaPortStatsGet(PortNum, Stats) ->
    call([{enum,error_t},{struct,port_stats}],
         152,
         [{uint32_t,PortNum},struct_to_binary(Stats)]).

ofdpaPktSend(Pkt, Flags, OutPortNum, InPortNum) ->
    call([{enum,error_t},ofdpa_buffdesc],
         153,
         [{ofdpa_buffdesc,Pkt},
          {uint32_t,Flags},
          {uint32_t,OutPortNum},
          {uint32_t,InPortNum}]).

ofdpaMaxPktSizeGet(PktSize) ->
    call([{enum,error_t},uint32_t], 154, [{uint32_t,PktSize}]).

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

ofdpaFlowTableInfoGet(TableId, Info) ->
    call([{enum,error_t},{struct,flow_table_info}],
         159,
         [{uint32_t,enum_to_integer(flow_table_id_t, TableId)},
          struct_to_binary(Info)]).

ofdpaNumQueuesGet(PortNum, NumQueues) ->
    call([{enum,error_t},uint32_t],
         160,
         [{uint32_t,PortNum},{uint32_t,NumQueues}]).

ofdpaQueueStatsGet(PortNum, QueueId, Stats) ->
    call([{enum,error_t},{struct,port_queue_stats}],
         161,
         [{uint32_t,PortNum},{uint32_t,QueueId},struct_to_binary(Stats)]).

ofdpaQueueStatsClear(PortNum, QueueId) ->
    call([{enum,error_t}], 162, [{uint32_t,PortNum},{uint32_t,QueueId}]).

ofdpaQueueRateSet(PortNum, QueueId, MinRate, MaxRate) ->
    call([{enum,error_t}],
         163,
         [{uint32_t,PortNum},
          {uint32_t,QueueId},
          {uint32_t,MinRate},
          {uint32_t,MaxRate}]).

ofdpaQueueRateGet(PortNum, QueueId, MinRate, MaxRate) ->
    call([{enum,error_t},uint32_t,uint32_t],
         164,
         [{uint32_t,PortNum},
          {uint32_t,QueueId},
          {uint32_t,MinRate},
          {uint32_t,MaxRate}]).

