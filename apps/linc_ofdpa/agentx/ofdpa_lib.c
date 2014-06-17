//
// Selected functions of OF-DPA API
//

#include "ofdpa_datatypes.h"
#include "ofdpa_api.h"

// Flow entries
OFDPA_ERROR_t ofdpaFlowEntryInit(OFDPA_FLOW_TABLE_ID_t tableId, ofdpaFlowEntry_t *flow)
{
	flow->tableId = tableId;
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
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupVlanGet(uint32_t groupId, uint32_t *vlanId)
{
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupPortIdGet(uint32_t groupId, uint32_t *portId)
{
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupIndexShortGet(uint32_t groupId, uint32_t *index)
{
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupIndexGet(uint32_t groupId, uint32_t *index)
{
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupTypeSet(uint32_t *groupId, uint32_t type)
{
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupVlanSet(uint32_t *groupId, uint32_t vlanId)
{
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupOverlayTunnelIdSet(uint32_t *groupId, uint32_t tunnelId)
{
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupOverlaySubTypeSet(uint32_t *groupId, OFDPA_L2_OVERLAY_SUBTYPE_t subType)
{
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupOverlayIndexSet(uint32_t *groupId, uint32_t index)
{
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupPortIdSet(uint32_t *groupId, uint32_t portId)
{
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupIndexShortSet(uint32_t *groupId, uint32_t index)
{
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupIndexSet(uint32_t *groupId, uint32_t index)
{
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupDecode(uint32_t groupId, char *outBuf, int bufSize)
{
	//TODO
	return OFDPA_E_NONE;
}
 // special
OFDPA_ERROR_t ofdpaGroupEntryInit(OFDPA_GROUP_ENTRY_TYPE_t groupType, ofdpaGroupEntry_t *group)
{
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupAdd(ofdpaGroupEntry_t *group)
{
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupDelete(uint32_t groupId)
{
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupNextGet(uint32_t groupId, ofdpaGroupEntry_t *nextGroup)
{
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupTypeNextGet(uint32_t groupId,
                                    OFDPA_GROUP_ENTRY_TYPE_t groupType,
                                    ofdpaGroupEntry_t *nextGroup)
{
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupStatsGet(uint32_t groupId, ofdpaGroupEntryStats_t *groupStats)
{
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupBucketEntryInit(OFDPA_GROUP_ENTRY_TYPE_t groupType, ofdpaGroupBucketEntry_t *bucket)
{
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupBucketEntryAdd(ofdpaGroupBucketEntry_t *bucket)
{
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupBucketEntryDelete(uint32_t groupId, uint32_t bucketIndex)
{
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupBucketsDeleteAll(uint32_t groupId)
{
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupBucketEntryGet(uint32_t groupId, uint32_t bucketIndex,
                                       ofdpaGroupBucketEntry_t *groupBucket)
{
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupBucketEntryFirstGet(uint32_t groupId,
                                            ofdpaGroupBucketEntry_t *firstGroupBucket)
{
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupBucketEntryNextGet(uint32_t groupId, uint32_t bucketIndex,
                                           ofdpaGroupBucketEntry_t *nextBucketEntry)
{
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupBucketEntryModify(ofdpaGroupBucketEntry_t *bucket)
{
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaGroupTableInfoGet(OFDPA_GROUP_ENTRY_TYPE_t groupType, ofdpaGroupTableInfo_t *info)
{
	//TODO
	return OFDPA_E_NONE;
}


// Ports
void ofdpaPortTypeGet(uint32_t portNum, uint32_t *type)
{
	//TODO
}

void ofdpaPortTypeSet(uint32_t *portNum, uint32_t type)
{
	//TODO
}

void ofdpaPortIndexGet(uint32_t portNum, uint32_t *index)
{
	//TODO
}

void ofdpaPortIndexSet(uint32_t *portNum, uint32_t index)
{
	//TODO
}

OFDPA_ERROR_t ofdpaPortNextGet(uint32_t portNum, uint32_t *nextPortNum)
{
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaPortMacGet(uint32_t portNum, ofdpaMacAddr_t *mac)
{
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaPortNameGet(uint32_t portNum, ofdpa_buffdesc *name)
{
	//TODO
	return OFDPA_E_NONE;
}
	// special
OFDPA_ERROR_t ofdpaPortStateGet(uint32_t  portNum, OFDPA_PORT_STATE_t  *state)
{
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaPortConfigSet(uint32_t portNum, OFDPA_PORT_CONFIG_t config)
{
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaPortConfigGet(uint32_t portNum, OFDPA_PORT_CONFIG_t  *config)
{
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaPortMaxSpeedGet(uint32_t portNum, uint32_t  *maxSpeed)
{
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaPortCurrSpeedGet(uint32_t portNum, uint32_t  *currSpeed)
{
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaPortFeatureGet(uint32_t portNum, ofdpaPortFeature_t *feature)
{
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaPortAdvertiseFeatureSet(uint32_t portNum, uint32_t advertise)
{
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaPortStatsClear(uint32_t portNum)
{
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaPortStatsGet(uint32_t portNum, ofdpaPortStats_t *stats)
{
	//TODO
	return OFDPA_E_NONE;
}


// Packet-out
OFDPA_ERROR_t ofdpaPktSend(ofdpa_buffdesc *pkt, uint32_t flags, uint32_t outPortNum, uint32_t inPortNum)
{
	//TODO
	return OFDPA_E_NONE;
}
 // special
OFDPA_ERROR_t ofdpaMaxPktSizeGet(uint32_t *pktSize)
{
	//TODO
	return OFDPA_E_NONE;
}

 // special
OFDPA_ERROR_t ofdpaEventReceive(struct timeval *timeout)
{
	//TODO
	return OFDPA_E_NONE;
}
 // special
OFDPA_ERROR_t ofdpaPortEventNextGet(ofdpaPortEvent_t *eventData)
{
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaFlowEventNextGet(ofdpaFlowEvent_t *eventData)
{
	//TODO
	return OFDPA_E_NONE;
}


// Tables
OFDPA_ERROR_t ofdpaFlowTableInfoGet(OFDPA_FLOW_TABLE_ID_t tableId, ofdpaFlowTableInfo_t *info)
{
	//TODO
	return OFDPA_E_NONE;
}


// Queues
OFDPA_ERROR_t ofdpaNumQueuesGet(uint32_t portNum, uint32_t *numQueues)
{
	//TODO
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
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaQueueRateSet(uint32_t portNum, uint32_t queueId, uint32_t minRate, uint32_t maxRate)
{
	//TODO
	return OFDPA_E_NONE;
}

OFDPA_ERROR_t ofdpaQueueRateGet(uint32_t portNum, uint32_t queueId, uint32_t *minRate, uint32_t *maxRate)
{
	//TODO
	return OFDPA_E_NONE;
}

// Asynchronous events

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
