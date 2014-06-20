//
// Selected functions of OF-DPA API
//

#include "ofdpa_datatypes.h"
#include "ofdpa_api.h"

#include "string.h"

// Flow entries
OFDPA_ERROR_t ofdpaFlowEntryInit(OFDPA_FLOW_TABLE_ID_t tableId, ofdpaFlowEntry_t *flow)
{
	flow->tableId = tableId;
	flow->priority = 1;
	flow->flowData.vlanFlowEntry.gotoTableId = 0;
	flow->hard_time = 2;
	flow->idle_time = 3;
	flow->cookie = 4;
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaFlowAdd(ofdpaFlowEntry_t *flow)
{
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaFlowModify(ofdpaFlowEntry_t *flow)
{
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaFlowDelete(ofdpaFlowEntry_t *flow)
{
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaFlowNextGet(ofdpaFlowEntry_t *flow, ofdpaFlowEntry_t *nextFlow)
{
	nextFlow->tableId = OFDPA_FLOW_TABLE_ID_BRIDGING;
	nextFlow->priority = 133;
	nextFlow->hard_time = 7;
	nextFlow->idle_time = 6;
	nextFlow->cookie = 101;
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaFlowStatsGet(ofdpaFlowEntry_t *flow, ofdpaFlowEntryStats_t *flowStats)
{
	flowStats->durationSec = 100;
	flowStats->receivedPackets = 200;
	flowStats->receivedBytes = 300;
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaFlowByCookieGet(uint64_t cookie, ofdpaFlowEntry_t *flow, ofdpaFlowEntryStats_t *flowStats)
{
	flow->tableId = OFDPA_FLOW_TABLE_ID_BRIDGING;
	flow->priority = 1;
	flow->hard_time = 2;
	flow->idle_time = 3;
	flow->cookie = 4;
	flowStats->durationSec = 10;
	flowStats->receivedPackets = 20;
	flowStats->receivedBytes = 30;
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaFlowByCookieDelete(uint64_t cookie)
{
	return OFDPA_E_NONE;
}


// Groups
OFDPA_ERROR_t ofdpaGroupTypeGet(uint32_t groupId, uint32_t *type)
{
	*type = 8;	// l2Overlay
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupVlanGet(uint32_t groupId, uint32_t *vlanId)
{
	*vlanId = 3;
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupPortIdGet(uint32_t groupId, uint32_t *portId)
{
	*portId = 4;
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupIndexShortGet(uint32_t groupId, uint32_t *index)
{
	*index = 5;
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupIndexGet(uint32_t groupId, uint32_t *index)
{
	*index = 6;
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupTypeSet(uint32_t *groupId, uint32_t type)
{
	*groupId = 200;
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupVlanSet(uint32_t *groupId, uint32_t vlanId)
{
	*groupId = 300;
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupOverlayTunnelIdSet(uint32_t *groupId, uint32_t tunnelId)
{
	*groupId = 400;
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupOverlaySubTypeSet(uint32_t *groupId, OFDPA_L2_OVERLAY_SUBTYPE_t subType)
{
	*groupId = 500;
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupOverlayIndexSet(uint32_t *groupId, uint32_t index)
{
	*groupId = 600;
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupPortIdSet(uint32_t *groupId, uint32_t portId)
{
	*groupId = 700;
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupIndexShortSet(uint32_t *groupId, uint32_t index)
{
	*groupId = 800;
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupIndexSet(uint32_t *groupId, uint32_t index)
{
	*groupId = 900;
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupDecode(uint32_t groupId, char *outBuf, int bufSize)
{
	strcpy(outBuf, "Hey there"); // bufSize disregarded
	return OFDPA_E_NONE;
}
 
OFDPA_ERROR_t ofdpaGroupEntryInit(OFDPA_GROUP_ENTRY_TYPE_t groupType, ofdpaGroupEntry_t *group)
{
	group->groupId = 111;
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupAdd(ofdpaGroupEntry_t *group)
{
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupDelete(uint32_t groupId)
{
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupNextGet(uint32_t groupId, ofdpaGroupEntry_t *nextGroup)
{
	nextGroup->groupId = 222;
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupTypeNextGet(uint32_t groupId,
                                    OFDPA_GROUP_ENTRY_TYPE_t groupType,
                                    ofdpaGroupEntry_t *nextGroup)
{
	nextGroup->groupId = 333;
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupStatsGet(uint32_t groupId, ofdpaGroupEntryStats_t *groupStats)
{
	groupStats->refCount = 1;
	groupStats->duration = 2;
	groupStats->bucketCount = 3;
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupBucketEntryInit(OFDPA_GROUP_ENTRY_TYPE_t groupType, ofdpaGroupBucketEntry_t *bucket)
{
	bucket->groupId = 7;
	bucket->bucketIndex = 8;
	bucket->referenceGroupId = 9;
	bucket->bucketData.l2Overlay.outputPort = 10;
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupBucketEntryAdd(ofdpaGroupBucketEntry_t *bucket)
{
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupBucketEntryDelete(uint32_t groupId, uint32_t bucketIndex)
{
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupBucketsDeleteAll(uint32_t groupId)
{
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupBucketEntryGet(uint32_t groupId, uint32_t bucketIndex,
                                       ofdpaGroupBucketEntry_t *groupBucket)
{
	groupBucket->groupId = 17;
	groupBucket->bucketIndex = 18;
	groupBucket->referenceGroupId = 19;
	groupBucket->bucketData.l2Overlay.outputPort = 110;
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupBucketEntryFirstGet(uint32_t groupId,
                                            ofdpaGroupBucketEntry_t *firstGroupBucket)
{
	firstGroupBucket->groupId = 27;
	firstGroupBucket->bucketIndex = 28;
	firstGroupBucket->referenceGroupId = 29;
	firstGroupBucket->bucketData.l2Overlay.outputPort = 210;
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupBucketEntryNextGet(uint32_t groupId, uint32_t bucketIndex,
                                           ofdpaGroupBucketEntry_t *nextBucketEntry)
{
	nextBucketEntry->groupId = 37;
	nextBucketEntry->bucketIndex = 38;
	nextBucketEntry->referenceGroupId = 39;
	nextBucketEntry->bucketData.l2Overlay.outputPort = 310;
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupBucketEntryModify(ofdpaGroupBucketEntry_t *bucket)
{
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupTableInfoGet(OFDPA_GROUP_ENTRY_TYPE_t groupType, ofdpaGroupTableInfo_t *info)
{
	info->numGroupEntries = 10;
	info->maxGroupEntries = 20;
	info->maxBucketEntries = 30;
	return OFDPA_E_NONE;
}

// Ports
void ofdpaPortTypeGet(uint32_t portNum, uint32_t *type)
{
	*type = 100;
}

void ofdpaPortTypeSet(uint32_t *portNum, uint32_t type)
{
	*portNum = 3;
}

void ofdpaPortIndexGet(uint32_t portNum, uint32_t *index)
{
	*index = 7;
}

void ofdpaPortIndexSet(uint32_t *portNum, uint32_t index)
{
	*portNum = 8;
}

OFDPA_ERROR_t ofdpaPortNextGet(uint32_t portNum, uint32_t *nextPortNum)
{
	*nextPortNum = 9;
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaPortMacGet(uint32_t portNum, ofdpaMacAddr_t *mac)
{
	mac->addr[0] = 1;
	mac->addr[1] = 2;
	mac->addr[2] = 3;
	mac->addr[3] = 4;
	mac->addr[4] = 5;
	mac->addr[5] = 6;
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaPortNameGet(uint32_t portNum, ofdpa_buffdesc *name)
{
	strcpy(name->pstart, "Port-1");
	name->size = 7;
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaPortStateGet(uint32_t  portNum, OFDPA_PORT_STATE_t  *state)
{
	*state = OFDPA_PORT_STATE_LINK_DOWN;
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaPortConfigSet(uint32_t portNum, OFDPA_PORT_CONFIG_t config)
{
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaPortConfigGet(uint32_t portNum, OFDPA_PORT_CONFIG_t  *config)
{
	*config = OFDPA_PORT_CONFIG_DOWN;
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaPortMaxSpeedGet(uint32_t portNum, uint32_t  *maxSpeed)
{
	*maxSpeed = 1001;
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaPortCurrSpeedGet(uint32_t portNum, uint32_t  *currSpeed)
{
	*currSpeed = 999;
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaPortFeatureGet(uint32_t portNum, ofdpaPortFeature_t *feature)
{
	feature->curr = 1;
	feature->advertised = 2;
	feature->supported = 3;
	feature->peer = 13;
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaPortAdvertiseFeatureSet(uint32_t portNum, uint32_t advertise)
{
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaPortStatsClear(uint32_t portNum)
{
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaPortStatsGet(uint32_t portNum, ofdpaPortStats_t *stats)
{
	stats->rx_packets = 1;
	stats->tx_packets = 2;
	stats->rx_bytes = 3;
	stats->tx_bytes = 4;
	stats->rx_errors = 5;
	stats->tx_errors = 6;
	stats->rx_drops = 7;
	stats->tx_drops = 8;
	stats->rx_frame_err = 9;
	stats->rx_over_err = 10;
	stats->rx_crc_err = 11;
	stats->collisions = 12;
	stats->duration_seconds = 13;
	return OFDPA_E_NONE;
}

// Packet-out
OFDPA_ERROR_t ofdpaPktSend(ofdpa_buffdesc *pkt, uint32_t flags, uint32_t outPortNum, uint32_t inPortNum)
{
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaMaxPktSizeGet(uint32_t *pktSize)
{
	*pktSize = 1500;
	return OFDPA_E_NONE;
}

// Tables
OFDPA_ERROR_t ofdpaFlowTableInfoGet(OFDPA_FLOW_TABLE_ID_t tableId, ofdpaFlowTableInfo_t *info)
{
	info->numEntries = 10;
	info->maxEntries = 20;
	return OFDPA_E_NONE;
}

// Queues
OFDPA_ERROR_t ofdpaNumQueuesGet(uint32_t portNum, uint32_t *numQueues)
{
	*numQueues = 137;
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaQueueStatsGet(uint32_t portNum, uint32_t queueId, ofdpaPortQueueStats_t *stats)
{
	stats->txBytes = 1234;
	stats->txPkts = 5678;
	stats->duration_seconds = 987654321;

	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaQueueStatsClear(uint32_t portNum, uint32_t queueId)
{
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaQueueRateSet(uint32_t portNum, uint32_t queueId, uint32_t minRate, uint32_t maxRate)
{
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaQueueRateGet(uint32_t portNum, uint32_t queueId, uint32_t *minRate, uint32_t *maxRate)
{
	*minRate = 100;
	*maxRate = 200;
	return OFDPA_E_NONE;
}

// Asynchronous events

OFDPA_ERROR_t ofdpaFlowEventNextGet(ofdpaFlowEvent_t *eventData)
{
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaPortEventNextGet(ofdpaPortEvent_t *eventData)
{
	return OFDPA_E_NONE;
}

int ofdpaClientEventSockFdGet()
{
	//TODO
	return -1;
}

int ofdpaClientPktSockFdGet()
{
	//TODO
	return -1;
}

OFDPA_ERROR_t ofdpaPktReceive(struct timeval *timeout, ofdpaPacket_t *pkt)
{
	//TODO
	return OFDPA_E_NONE;
}

//EOF
