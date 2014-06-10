```
// enums
OFDPA_ERROR_t
OFDPA_FLOW_TABLE_ID_t
OFDPA_L2_OVERLAY_SUBTYPE_t
OFDPA_GROUP_ENTRY_TYPE_t
OFDPA_PORT_STATE_t
OFDPA_PORT_CONFIG_t
OFDPA_PORT_FEATURE_t
OFDPA_PACKET_IN_REASON_t
OFDPA_PORT_EVENT_MASK_t
OFDPA_FLOW_EVENT_MASK_t

// structs
ofdpaFlowEntry_t // +

ofdpaIngressPortFlowEntry_t // +
ofdpaVlanFlowEntry_t // +
ofdpaTerminationMacFlowEntry_t // +
ofdpaBridgingFlowEntry_t // +
ofdpaUnicastRoutingFlowEntry_t // +
ofdpaMulticastRoutingFlowEntry_t // +
ofdpaPolicyAclFlowEntry_t // +

ofdpaIngressPortFlowMatch_t // +
ofdpaVlanFlowMatch_t // +
ofdpaTerminationMacFlowMatch_t // +
ofdpaBridgingFlowMatch_t // +
ofdpaUnicastRoutingFlowMatch_t // +
ofdpaMulticastRoutingFlowMatch_t // +
ofdpaPolicyAclFlowMatch_t // +

ofdpaMacAddr_t // +

ofdpaFlowEntryStats_t // +

ofdpaGroupEntry_t // +
ofdpaGroupEntryStats_t // +
ofdpaGroupBucketEntry_t // +

ofdpaL2InterfaceGroupBucketData_t // +
ofdpaL3InterfaceGroupBucketData_t // +
ofdpaL3UnicastGroupBucketData_t // +
ofdpaL2RewriteGroupBucketData_t // +
ofdpaL2OverlayGroupBucketData_t // +

ofdpaGroupTableInfo_t // +

ofdpa_buffdesc // +

ofdpaPortFeature_t // +
ofdpaPortStats_t // +

ofdpaPacket_t // + embedded ofdpa_buffdesc
ofdpaPortEvent_t // +

ofdpaFlowTableInfo_t // +

ofdpaPortQueueStats_t // +

