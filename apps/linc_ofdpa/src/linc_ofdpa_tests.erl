-module(linc_ofdpa_tests).

-include("ofdpa.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(OFDPA_HOST, "192.168.0.1").
-define(OFDPA_PORT, 5005).

ensure_link() ->
	case whereis(ofdpa_link) of
	undefined ->
		ofdpa_link:start_link(?OFDPA_HOST, ?OFDPA_PORT);
	_ ->
		ok
	end.

flow_test_() ->
	[{setup,
	  fun() -> ensure_link() end,
	  fun(_) -> ok end,
	  [
		{"ofdpaFlowEntryInit", fun flow_entry_init/0},
		{"ofdpaFlowAdd", fun flow_add/0},
		{"ofdpaFlowModify", fun flow_modify/0},
		{"ofdpaFlowDelete", fun flow_delete/0},
		{"ofdpaFlowNextGet", fun flow_next_get/0},
		{"ofdpaFlowStatsGet", fun flow_stats_get/0},
		{"ofdpaFlowByCookieGet", fun flow_by_cookie_get/0},
		{"ofdpaFlowByCookieDelete", fun flow_by_cookie_delete/0}
	  ]}].

group_test_() ->
	[{setup,
	  fun() -> ensure_link() end,
	  fun(_) -> ok end,
	  [
		{"ofdpaGroupTypeGet", fun group_type_get/0},
		{"ofdpaGroupVlanGet", fun group_vlan_get/0},
		{"ofdpaGroupPortIdGet", fun group_port_id_get/0},
		{"ofdpaGroupIndexShortGet", fun group_index_short_get/0},
		{"ofdpaGroupIndexGet", fun group_index_get/0},
		{"ofdpaGroupTypeSet", fun group_type_set/0},
		{"ofdpaGroupVlanSet", fun group_vlan_set/0},
		{"ofdpaGroupOverlayTunnelIdSet", fun group_overlay_tunnel_id_set/0},
		{"ofdpaGroupOverlaySubTypeSet", fun group_overlay_sub_type_set/0},
		{"ofdpaGroupOverlayIndexSet", fun group_overlay_index_set/0},
		{"ofdpaGroupPortIdSet", fun group_port_id_set/0},
		{"ofdpaGroupIndexShortSet", fun group_index_short_set/0},
		{"ofdpaGroupIndexSet", fun group_index_set/0},
		{"ofdpaGroupDecode", fun group_decode/0},
		{"ofdpaGroupEntryInit", fun group_entry_init/0},
		{"ofdpaGroupAdd", fun group_add/0},
		{"ofdpaGroupDelete", fun group_delete/0},
		{"ofdpaGroupNextGet", fun group_next_get/0},
		{"ofdpaGroupTypeNextGet", fun group_type_next_get/0},
		{"ofdpaGroupStatsGet", fun group_stats_get/0},
		{"ofdpaGroupBucketEntryInit", fun group_bucket_entry_init/0},
		{"ofdpaGroupBucketEntryAdd", fun group_bucket_entry_add/0},
		{"ofdpaGroupBucketEntryDelete", fun group_bucket_entry_delete/0},
		{"ofdpaGroupBucketsDeleteAll", fun group_buckets_delete_all/0},
		{"ofdpaGroupBucketEntryGet", fun group_bucket_entry_get/0},
		{"ofdpaGroupBucketEntryFirstGet", fun group_bucket_entry_first_get/0},
		{"ofdpaGroupBucketEntryNextGet", fun group_bucket_entry_next_get/0},
		{"ofdpaGroupBucketEntryModify", fun group_bucket_entry_modify/0},
		{"ofdpaGroupTableInfoGet", fun group_table_info_get/0}
	  ]}].

port_test_() ->
	[{setup,
	  fun() -> ensure_link() end,
	  fun(_) -> ok end,
	  [
		{"ofdpaPortTypeGet", fun port_type_get/0},
		{"ofdpaPortTypeSet", fun port_type_set/0},
		{"ofdpaPortIndexGet", fun port_index_get/0},
		{"ofdpaPortIndexSet", fun port_index_set/0},
		{"ofdpaPortNextGet", fun port_next_get/0},
		{"ofdpaPortMacGet", fun port_mac_get/0},
		{"ofdpaPortNameGet", fun port_name_get/0},
		{"ofdpaPortStateGet", fun port_state_get/0},
		{"ofdpaPortConfigSet", fun port_config_set/0},
		{"ofdpaPortConfigGet", fun port_config_get/0},
		{"ofdpaPortMaxSpeedGet", fun port_max_speed_get/0},
		{"ofdpaPortCurrSpeedGet", fun port_curr_speed_get/0},
		{"ofdpaPortFeatureGet", fun port_feature_get/0},
		{"ofdpaPortAdvertiseFeatureSet", fun port_advertise_feature_set/0},
		{"ofdpaPortStatsClear", fun port_stats_clear/0},
		{"ofdpaPortStatsGet", fun port_stats_get/0}
	  ]}].

pkt_test_() ->
	[{setup,
	  fun() -> ensure_link() end,
	  fun(_) -> ok end,
	  [
		{"ofdpaPktSend", fun pkt_send/0},
		{"ofdpaMaxPktSizeGet", fun max_pkt_size_get/0}
	  ]}].

table_test_() ->
	[{setup,
	  fun() -> ensure_link() end,
	  fun(_) -> ok end,
	  [
		{"ofdpaFlowTableInfoGet", fun flow_table_info_get/0}
	  ]}].

queue_test_() ->
	[{setup,
	  fun() -> ensure_link() end,
	  fun(_) -> ok end,
	  [
		{"ofdpaNumQueuesGet", fun num_queues_get/0},
		{"ofdpaQueueStatsGet", fun queue_stats_get/0},
		{"ofdpaQueueStatsClear", fun queue_stats_clear/0},
		{"ofdpaQueueRateSet", fun queue_rate_set/0},
		{"ofdpaQueueRateGet", fun queue_rate_get/0}
	  ]}].

%% OFDPA_ERROR_t ofdpaFlowEntryInit(OFDPA_FLOW_TABLE_ID_t tableId, ofdpaFlowEntry_t *flow);
flow_entry_init() ->
	{ok,E,F} = ofdpa:ofdpaFlowEntryInit(flow_table_id_vlan),
	?assertEqual(E, e_none),
	?assertEqual(F#flow_entry.tableId, flow_table_id_vlan),
	?assertEqual(F#flow_entry.priority, 1),
	?assertEqual(F#flow_entry.hard_time, 2),
	?assertEqual(F#flow_entry.idle_time, 3),
	?assertEqual(F#flow_entry.cookie, 4).

%% OFDPA_ERROR_t ofdpaFlowAdd(ofdpaFlowEntry_t *flow [in]);
flow_add() ->
	M = #vlan_flow_match{inPort = 7,
						 vlanId = 30,
						 vlanIdMask = 0},
	V = #vlan_flow_entry{gotoTableId = flow_table_id_termination_mac,
						  match_criteria = M,
						  newVlanId = 4},
	F = #flow_entry{tableId = flow_table_id_vlan,
					priority = 11,
					flowData = V,
					hard_time = 12,
					idle_time = 13,
					cookie = 14},
	{ok,E} = ofdpa:ofdpaFlowAdd(F),
	?assertEqual(E, e_none).

%% OFDPA_ERROR_t ofdpaFlowModify(ofdpaFlowEntry_t *flow [in]);
flow_modify() ->
	M = #ingress_port_flow_match{inPort = 7,inPortMask = 15},
	V = #ingress_port_flow_entry{gotoTableId = flow_table_id_termination_mac,
						  match_criteria = M},
	F = #flow_entry{tableId = flow_table_id_ingress_port,
					priority = 11,
					flowData = V,
					hard_time = 12,
					idle_time = 13,
					cookie = 14},
	{ok,E} = ofdpa:ofdpaFlowModify(F),
	?assertEqual(E, e_none).

%% OFDPA_ERROR_t ofdpaFlowDelete(ofdpaFlowEntry_t *flow [in]);
flow_delete() ->
	M = #termination_mac_flow_match{inPort = 7,
				inPortMask = 15,
				etherType = 16#800,
				destMac = <<1,2,3,4,5,6>>,
				destMacMask = <<255,255,255,255,255,255>>,
				vlanId = 40,
				vlanIdMask = 255},
	V = #termination_mac_flow_entry{gotoTableId = flow_table_id_vlan,
						  match_criteria = M,
						  outputPort = 0},
	F = #flow_entry{tableId = flow_table_id_termination_mac,
					priority = 41,
					flowData = V,
					hard_time = 42,
					idle_time = 43,
					cookie = 44},
	{ok,E} = ofdpa:ofdpaFlowDelete(F),
	?assertEqual(E, e_none).

%% OFDPA_ERROR_t ofdpaFlowNextGet(ofdpaFlowEntry_t *flow [in], ofdpaFlowEntry_t *nextFlow);
flow_next_get() ->
	M = #vlan_flow_match{inPort = 7,
						 vlanId = 30,
						 vlanIdMask = 0},
	V = #vlan_flow_entry{gotoTableId = flow_table_id_termination_mac,
						  match_criteria = M,
						  newVlanId = 4},
	F = #flow_entry{tableId = flow_table_id_vlan,
					priority = 11,
					flowData = V,
					hard_time = 12,
					idle_time = 13,
					cookie = 14},
	{ok,E,X} = ofdpa:ofdpaFlowNextGet(F),
	?assertEqual(E, e_none),
	?assertEqual(X#flow_entry.tableId, flow_table_id_bridging),
	?assertEqual(X#flow_entry.priority, 133),
	?assertEqual(X#flow_entry.hard_time, 7),
	?assertEqual(X#flow_entry.idle_time, 6),
	?assertEqual(X#flow_entry.cookie, 101).

%% OFDPA_ERROR_t ofdpaFlowStatsGet(ofdpaFlowEntry_t *flow [in], ofdpaFlowEntryStats_t *flowStats);
flow_stats_get() ->
	M = #vlan_flow_match{inPort = 7,
						 vlanId = 30,
						 vlanIdMask = 0},
	V = #vlan_flow_entry{gotoTableId = flow_table_id_termination_mac,
						  match_criteria = M,
						  newVlanId = 4},
	F = #flow_entry{tableId = flow_table_id_vlan,
					priority = 11,
					flowData = V,
					hard_time = 12,
					idle_time = 13,
					cookie = 14},
	{ok,E,X} = ofdpa:ofdpaFlowStatsGet(F),
	?assertEqual(E, e_none),
	?assertEqual(X#flow_entry_stats.durationSec,  100),
	?assertEqual(X#flow_entry_stats.receivedPackets, 200),
	?assertEqual(X#flow_entry_stats.receivedBytes, 300).

%% OFDPA_ERROR_t ofdpaFlowByCookieGet(uint64_t cookie, ofdpaFlowEntry_t *flow, ofdpaFlowEntryStats_t *flowStats);
flow_by_cookie_get() ->
	{ok,E,X,Y} = ofdpa:ofdpaFlowByCookieGet(1),
	?assertEqual(E, e_none),
	?assertEqual(X#flow_entry.tableId, flow_table_id_bridging),
	?assertEqual(X#flow_entry.priority, 1),
	?assertEqual(X#flow_entry.hard_time, 2),
	?assertEqual(X#flow_entry.idle_time, 3),
	?assertEqual(X#flow_entry.cookie, 4),
	?assertEqual(Y#flow_entry_stats.durationSec, 10),
	?assertEqual(Y#flow_entry_stats.receivedPackets, 20),
	?assertEqual(Y#flow_entry_stats.receivedBytes, 30).

%% OFDPA_ERROR_t ofdpaFlowByCookieDelete(uint64_t cookie);
flow_by_cookie_delete() ->
	{ok,E} = ofdpa:ofdpaFlowByCookieDelete(1),
	?assertEqual(E, e_none).

%% OFDPA_ERROR_t ofdpaGroupTypeGet(uint32_t groupId, uint32_t *type);
group_type_get() ->
	{ok,E,X} = ofdpa:ofdpaGroupTypeGet(1),
	?assertEqual(E, e_none),
	?assertEqual(X, 8).

%% OFDPA_ERROR_t ofdpaGroupVlanGet(uint32_t groupId, uint32_t *vlanId);
group_vlan_get() ->
	{ok,E,X} = ofdpa:ofdpaGroupVlanGet(1),
	?assertEqual(E, e_none),
	?assertEqual(X, 3).

%% OFDPA_ERROR_t ofdpaGroupPortIdGet(uint32_t groupId, uint32_t *portId);
group_port_id_get() ->
	{ok,E,X} = ofdpa:ofdpaGroupPortIdGet(1),
	?assertEqual(E, e_none),
	?assertEqual(X, 4).

%% OFDPA_ERROR_t ofdpaGroupIndexShortGet(uint32_t groupId, uint32_t *index);
group_index_short_get() ->
	{ok,E,X} = ofdpa:ofdpaGroupIndexShortGet(1),
	?assertEqual(E, e_none),
	?assertEqual(X, 5).

%% OFDPA_ERROR_t ofdpaGroupIndexGet(uint32_t groupId, uint32_t *index);
group_index_get() ->
	{ok,E,X} = ofdpa:ofdpaGroupIndexGet(1),
	?assertEqual(E, e_none),
	?assertEqual(X, 6).

%% OFDPA_ERROR_t ofdpaGroupTypeSet(uint32_t *groupId [inout], uint32_t type);
group_type_set() ->
	{ok,E,X} = ofdpa:ofdpaGroupTypeSet(199, 7),
	?assertEqual(E, e_none),
	?assertEqual(X, 200).

%% OFDPA_ERROR_t ofdpaGroupVlanSet(uint32_t *groupId [inout], uint32_t vlanId);
group_vlan_set() ->
	{ok,E,X} = ofdpa:ofdpaGroupVlanSet(299, 7),
	?assertEqual(E, e_none),
	?assertEqual(X, 300).

%% OFDPA_ERROR_t ofdpaGroupOverlayTunnelIdSet(uint32_t *groupId [inout], uint32_t tunnelId);
group_overlay_tunnel_id_set() ->
	{ok,E,X} = ofdpa:ofdpaGroupOverlayTunnelIdSet(399, 7),
	?assertEqual(E, e_none),
	?assertEqual(X, 400).

%% OFDPA_ERROR_t ofdpaGroupOverlaySubTypeSet(uint32_t *groupId [inout], OFDPA_L2_OVERLAY_SUBTYPE_t subType);
group_overlay_sub_type_set() ->
	{ok,E,X} = ofdpa:ofdpaGroupOverlaySubTypeSet(499,
						 l2_overlay_flood_multicast_tunnel),
	?assertEqual(E, e_none),
	?assertEqual(X, 500).

%% OFDPA_ERROR_t ofdpaGroupOverlayIndexSet(uint32_t *groupId [inout], uint32_t index);
group_overlay_index_set() ->
	{ok,E,X} = ofdpa:ofdpaGroupOverlayIndexSet(599, 7),
	?assertEqual(E, e_none),
	?assertEqual(X, 600).

%% OFDPA_ERROR_t ofdpaGroupPortIdSet(uint32_t *groupId [inout], uint32_t portId);
group_port_id_set() ->
	{ok,E,X} = ofdpa:ofdpaGroupPortIdSet(699, 7),
	?assertEqual(E, e_none),
	?assertEqual(X, 700).

%% OFDPA_ERROR_t ofdpaGroupIndexShortSet(uint32_t *groupId [inout], uint32_t index);
group_index_short_set() ->
	{ok,E,X} = ofdpa:ofdpaGroupIndexShortSet(799, 7),
	?assertEqual(E, e_none),
	?assertEqual(X, 800).

%% OFDPA_ERROR_t ofdpaGroupIndexSet(uint32_t *groupId [inout], uint32_t index);
group_index_set() ->
	{ok,E,X} = ofdpa:ofdpaGroupIndexSet(899, 7),
	?assertEqual(E, e_none),
	?assertEqual(X, 900).

%% OFDPA_ERROR_t ofdpaGroupDecode(uint32_t groupId, char *outBuf, int bufSize); %% special
group_decode() ->
	{ok,E,S} = ofdpa:ofdpaGroupDecode(7, 1024),
	?assertEqual(E, e_none),
	?assertEqual(S, <<"Hey there">>).

%% OFDPA_ERROR_t ofdpaGroupEntryInit(OFDPA_GROUP_ENTRY_TYPE_t groupType, ofdpaGroupEntry_t *group);
group_entry_init() ->
	{ok,E,G} = ofdpa:ofdpaGroupEntryInit(group_entry_type_l3_unicast),
	?assertEqual(E, e_none),
	?assertEqual(G#group_entry.groupId, 111).

%% OFDPA_ERROR_t ofdpaGroupAdd(ofdpaGroupEntry_t *group [in]);
group_add() ->
	{ok,E} = ofdpa:ofdpaGroupAdd(#group_entry{groupId = 13}),
	?assertEqual(E, e_none).

%% OFDPA_ERROR_t ofdpaGroupDelete(uint32_t groupId);
group_delete() ->
	{ok,E} = ofdpa:ofdpaGroupDelete(14),
	?assertEqual(E, e_none).

%% OFDPA_ERROR_t ofdpaGroupNextGet(uint32_t groupId, ofdpaGroupEntry_t *nextGroup);
group_next_get() ->
	{ok,E,G} = ofdpa:ofdpaGroupNextGet(9),
	?assertEqual(E, e_none),
	?assertEqual(G#group_entry.groupId, 222).

%% OFDPA_ERROR_t ofdpaGroupTypeNextGet(uint32_t groupId,
%%                                     OFDPA_GROUP_ENTRY_TYPE_t groupType,
%%                                     ofdpaGroupEntry_t *nextGroup);
group_type_next_get() ->
	{ok,E,G} = ofdpa:ofdpaGroupTypeNextGet(9, group_entry_type_l2_multicast),
	?assertEqual(E, e_none),
	?assertEqual(G#group_entry.groupId, 333).

%% OFDPA_ERROR_t ofdpaGroupStatsGet(uint32_t groupId, ofdpaGroupEntryStats_t *groupStats);
group_stats_get() ->
	{ok,E,S} = ofdpa:ofdpaGroupStatsGet(9),
	?assertEqual(E, e_none),
	?assertEqual(S#group_entry_stats.refCount, 1),
	?assertEqual(S#group_entry_stats.duration, 2),
	?assertEqual(S#group_entry_stats.bucketCount, 3).

%% OFDPA_ERROR_t ofdpaGroupBucketEntryInit(OFDPA_GROUP_ENTRY_TYPE_t groupType, ofdpaGroupBucketEntry_t *bucket);
group_bucket_entry_init() ->
	{ok,E,B} = ofdpa:ofdpaGroupBucketEntryInit(group_entry_type_l2_overlay),
	D = B#group_bucket_entry.bucketData,
	?assertEqual(E, e_none),
	?assertEqual(B#group_bucket_entry.groupId, 7),
	?assertEqual(B#group_bucket_entry.bucketIndex, 8),
	?assertEqual(B#group_bucket_entry.referenceGroupId, 9),
	?assertEqual(D#l_2_overlay_group_bucket_data.outputPort, 10).

%% OFDPA_ERROR_t ofdpaGroupBucketEntryAdd(ofdpaGroupBucketEntry_t *bucket [in]);
group_bucket_entry_add() ->
	D = #l_2_overlay_group_bucket_data{outputPort = 20},
	B = #group_bucket_entry{groupId =1,
							bucketIndex =2,
							referenceGroupId =3,
							bucketData =D},
	{ok,E} = ofdpa:ofdpaGroupBucketEntryAdd(B),
	?assertEqual(E, e_none).

%% OFDPA_ERROR_t ofdpaGroupBucketEntryDelete(uint32_t groupId, uint32_t bucketIndex);
group_bucket_entry_delete() ->
	{ok,E} = ofdpa:ofdpaGroupBucketEntryDelete(1, 2),
	?assertEqual(E, e_none).

%% OFDPA_ERROR_t ofdpaGroupBucketsDeleteAll(uint32_t groupId);
group_buckets_delete_all() ->
	{ok,E} = ofdpa:ofdpaGroupBucketsDeleteAll(1),
	?assertEqual(E, e_none).

%% OFDPA_ERROR_t ofdpaGroupBucketEntryGet(uint32_t groupId, uint32_t bucketIndex,
%%                                        ofdpaGroupBucketEntry_t *groupBucket);
group_bucket_entry_get() ->
	{ok,E,B} = ofdpa:ofdpaGroupBucketEntryGet(1, 2),
	D = B#group_bucket_entry.bucketData,
	?assertEqual(E, e_none),
	?assertEqual(B#group_bucket_entry.groupId, 17),
	?assertEqual(B#group_bucket_entry.bucketIndex, 18),
	?assertEqual(B#group_bucket_entry.referenceGroupId, 19),
	?assertEqual(D#l_2_overlay_group_bucket_data.outputPort, 110).

%% OFDPA_ERROR_t ofdpaGroupBucketEntryFirstGet(uint32_t groupId,
%%                                             ofdpaGroupBucketEntry_t *firstGroupBucket);
group_bucket_entry_first_get() ->
	{ok,E,B} = ofdpa:ofdpaGroupBucketEntryFirstGet(1),
	D = B#group_bucket_entry.bucketData,
	?assertEqual(E, e_none),
	?assertEqual(B#group_bucket_entry.groupId, 27),
	?assertEqual(B#group_bucket_entry.bucketIndex, 28),
	?assertEqual(B#group_bucket_entry.referenceGroupId, 29),
	?assertEqual(D#l_2_overlay_group_bucket_data.outputPort, 210).

%% OFDPA_ERROR_t ofdpaGroupBucketEntryNextGet(uint32_t groupId, uint32_t bucketIndex,
%%                                            ofdpaGroupBucketEntry_t *nextBucketEntry);
group_bucket_entry_next_get() ->
	{ok,E,B} = ofdpa:ofdpaGroupBucketEntryNextGet(1, 2),
	D = B#group_bucket_entry.bucketData,
	?assertEqual(E, e_none),
	?assertEqual(B#group_bucket_entry.groupId, 37),
	?assertEqual(B#group_bucket_entry.bucketIndex, 38),
	?assertEqual(B#group_bucket_entry.referenceGroupId, 39),
	?assertEqual(D#l_2_overlay_group_bucket_data.outputPort, 310).
	
%% OFDPA_ERROR_t ofdpaGroupBucketEntryModify(ofdpaGroupBucketEntry_t *bucket [in]);
group_bucket_entry_modify() ->
	D = #l_2_overlay_group_bucket_data{outputPort = 420},
	B = #group_bucket_entry{groupId =41,
							bucketIndex =42,
							referenceGroupId =43,
							bucketData =D},
	{ok,E} = ofdpa:ofdpaGroupBucketEntryModify(B),
	?assertEqual(E, e_none).

%% OFDPA_ERROR_t ofdpaGroupTableInfoGet(OFDPA_GROUP_ENTRY_TYPE_t groupType, ofdpaGroupTableInfo_t *info);
group_table_info_get() ->
	{ok,E,I} = ofdpa:ofdpaGroupTableInfoGet(group_entry_type_l2_overlay),
	?assertEqual(E, e_none),
	?assertEqual(I#group_table_info.numGroupEntries, 10),
	?assertEqual(I#group_table_info.maxGroupEntries, 20),
	?assertEqual(I#group_table_info.maxBucketEntries, 30).

%% void ofdpaPortTypeGet(uint32_t portNum, uint32_t *type);
port_type_get() ->
	{ok,T} = ofdpa:ofdpaPortTypeGet(1),
	?assertEqual(T, 100).

%% void ofdpaPortTypeSet(uint32_t *portNum [inout], uint32_t type);
port_type_set() ->
	{ok,P} = ofdpa:ofdpaPortTypeSet(1, 2),
	?assertEqual(P, 3).

%% void ofdpaPortIndexGet(uint32_t portNum, uint32_t *index);
port_index_get() ->
	{ok,I} = ofdpa:ofdpaPortIndexGet(1),
	?assertEqual(I, 7).

%% void ofdpaPortIndexSet(uint32_t *portNum [inout], uint32_t index);
port_index_set() ->
	{ok,P} = ofdpa:ofdpaPortIndexSet(1, 2),
	?assertEqual(P, 8).

%% OFDPA_ERROR_t ofdpaPortNextGet(uint32_t portNum, uint32_t *nextPortNum);
port_next_get() ->
	{ok,E,P} = ofdpa:ofdpaPortNextGet(1),
	?assertEqual(E, e_none),
	?assertEqual(P, 9).

%% OFDPA_ERROR_t ofdpaPortMacGet(uint32_t portNum, ofdpaMacAddr_t *mac);
port_mac_get() ->
	{ok,E,M} = ofdpa:ofdpaPortMacGet(1),
	?assertEqual(E, e_none),
	?assertEqual(M, <<1,2,3,4,5,6>>).

%% OFDPA_ERROR_t ofdpaPortNameGet(uint32_t portNum, ofdpa_buffdesc *name);	%% special
port_name_get() ->
	{ok,E,N} = ofdpa:ofdpaPortNameGet(1),
	?assertEqual(E, e_none),
	?assertEqual(N, <<"Port-1">>).

%% OFDPA_ERROR_t ofdpaPortStateGet(uint32_t  portNum, OFDPA_PORT_STATE_t  *state);
port_state_get() ->
	{ok,E,S} = ofdpa:ofdpaPortStateGet(1),
	?assertEqual(E, e_none),
	?assertEqual(S, port_state_link_down).

%% OFDPA_ERROR_t ofdpaPortConfigSet(uint32_t portNum, OFDPA_PORT_CONFIG_t config);
port_config_set() ->
	{ok,E} = ofdpa:ofdpaPortConfigSet(1, port_config_down),
	?assertEqual(E, e_none).

%% OFDPA_ERROR_t ofdpaPortConfigGet(uint32_t portNum, OFDPA_PORT_CONFIG_t  *config);
port_config_get() ->
	{ok,E,C} = ofdpa:ofdpaPortConfigGet(1),
	?assertEqual(E, e_none),
	?assertEqual(C, port_config_down).

%% OFDPA_ERROR_t ofdpaPortMaxSpeedGet(uint32_t portNum, uint32_t  *maxSpeed);
port_max_speed_get() ->
	{ok,E,S} = ofdpa:ofdpaPortMaxSpeedGet(1),
	?assertEqual(E, e_none),
	?assertEqual(S, 1001).

%% OFDPA_ERROR_t ofdpaPortCurrSpeedGet(uint32_t portNum, uint32_t  *currSpeed);
port_curr_speed_get() ->
	{ok,E,S} = ofdpa:ofdpaPortCurrSpeedGet(1),
	?assertEqual(E, e_none),
	?assertEqual(S, 999).

%% OFDPA_ERROR_t ofdpaPortFeatureGet(uint32_t portNum, ofdpaPortFeature_t *feature);
port_feature_get() ->
	{ok,E,F} = ofdpa:ofdpaPortFeatureGet(1),
	?assertEqual(E, e_none),
	?assertEqual(F#port_feature.curr, port_feat_10mb_hd), %% 1
	?assertEqual(F#port_feature.advertised, port_feat_10mb_fd), %% 2
	?assertEqual(F#port_feature.supported, [port_feat_10mb_fd,port_feat_10mb_hd]), %% 3
	?assertEqual(F#port_feature.peer, [port_feat_100mb_fd,port_feat_100mb_hd,port_feat_10mb_hd]). %% 13

%% OFDPA_ERROR_t ofdpaPortAdvertiseFeatureSet(uint32_t portNum, uint32_t advertise);
port_advertise_feature_set() ->
	{ok,E} = ofdpa:ofdpaPortAdvertiseFeatureSet(1, 12),
	?assertEqual(E, e_none).

%% OFDPA_ERROR_t ofdpaPortStatsClear(uint32_t portNum);
port_stats_clear() ->
	{ok,E} = ofdpa:ofdpaPortStatsClear(1),
	?assertEqual(E, e_none).

%% OFDPA_ERROR_t ofdpaPortStatsGet(uint32_t portNum, ofdpaPortStats_t *stats);
port_stats_get() ->
	{ok,E,S} = ofdpa:ofdpaPortStatsGet(1),
	?assertEqual(E, e_none),
	?assertEqual(S#port_stats.rx_packets, 1),
	?assertEqual(S#port_stats.tx_packets, 2),
	?assertEqual(S#port_stats.rx_bytes, 3),
	?assertEqual(S#port_stats.tx_bytes, 4),
	?assertEqual(S#port_stats.rx_errors, 5),
	?assertEqual(S#port_stats.tx_errors, 6),
	?assertEqual(S#port_stats.rx_drops, 7),
	?assertEqual(S#port_stats.tx_drops, 8),
	?assertEqual(S#port_stats.rx_frame_err, 9),
	?assertEqual(S#port_stats.rx_over_err, 10),
	?assertEqual(S#port_stats.rx_crc_err, 11),
	?assertEqual(S#port_stats.collisions, 12),
	?assertEqual(S#port_stats.duration_seconds, 13).

%% OFDPA_ERROR_t ofdpaPktSend(ofdpa_buffdesc *pkt, uint32_t flags, uint32_t outPortNum, uint32_t inPortNum); %% special
pkt_send() ->
	{ok,E} = ofdpa:ofdpaPktSend(<<1,2,3>>, 0, 1, 2),
	?assertEqual(E, e_none).

%% OFDPA_ERROR_t ofdpaMaxPktSizeGet(uint32_t *pktSize);
max_pkt_size_get() ->
	{ok,E,S} = ofdpa:ofdpaMaxPktSizeGet(),
	?assertEqual(E, e_none),
	?assertEqual(S, 1500).
 
%% OFDPA_ERROR_t ofdpaFlowTableInfoGet(OFDPA_FLOW_TABLE_ID_t tableId, ofdpaFlowTableInfo_t *info);
flow_table_info_get() ->
	{ok,E,I} = ofdpa:ofdpaFlowTableInfoGet(flow_table_id_vlan),
	?assertEqual(E, e_none),
	?assertEqual(I#flow_table_info.numEntries, 10),
	?assertEqual(I#flow_table_info.maxEntries, 20).

%% OFDPA_ERROR_t ofdpaNumQueuesGet(uint32_t portNum, uint32_t *numQueues);
num_queues_get() ->
	{ok,E,N} = ofdpa:ofdpaNumQueuesGet(1),
	?assertEqual(E, e_none),
	?assertEqual(N, 137).

%% OFDPA_ERROR_t ofdpaQueueStatsGet(uint32_t portNum, uint32_t queueId, ofdpaPortQueueStats_t *stats);
queue_stats_get() ->
	{ok,E,S} = ofdpa:ofdpaQueueStatsGet(1, 2),
	?assertEqual(E, e_none),
	?assertEqual(S#port_queue_stats.txBytes, 1234),
	?assertEqual(S#port_queue_stats.txPkts, 5678),
	?assertEqual(S#port_queue_stats.duration_seconds, 987654321).

%% OFDPA_ERROR_t ofdpaQueueStatsClear(uint32_t portNum, uint32_t queueId);
queue_stats_clear() ->
	{ok,E} = ofdpa:ofdpaQueueStatsClear(1, 2),
	?assertEqual(E, e_none).

%% OFDPA_ERROR_t ofdpaQueueRateSet(uint32_t portNum, uint32_t queueId, uint32_t minRate, uint32_t maxRate);
queue_rate_set() ->
	{ok,E} = ofdpa:ofdpaQueueRateSet(1, 2, 10, 20),
	?assertEqual(E, e_none).

%% OFDPA_ERROR_t ofdpaQueueRateGet(uint32_t portNum, uint32_t queueId, uint32_t *minRate, uint32_t *maxRate);
queue_rate_get() ->
	{ok,E,R1,R2} = ofdpa:ofdpaQueueRateGet(1, 2),
	?assertEqual(E, e_none),
	?assertEqual(R1, 100),
	?assertEqual(R2, 200).

%%EOF
