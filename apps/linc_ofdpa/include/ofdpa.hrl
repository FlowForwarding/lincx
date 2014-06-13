%%
%%
%%

%% record definitions generated using 'genera records'

-record(ingress_port_flow_match,{inPort,inPortMask}).

-record(ingress_port_flow_entry,{match_criteria,gotoTableId}).

-record(vlan_flow_match,{inPort,vlanId,vlanIdMask}).

-record(vlan_flow_entry,{match_criteria,gotoTableId,newVlanId}).

-record(termination_mac_flow_match,{inPort,
                                    inPortMask,
                                    etherType,
                                    destMac,
                                    destMacMask,
                                    vlanId,
                                    vlanIdMask}).

-record(termination_mac_flow_entry,{match_criteria,
                                    gotoTableId,
                                    outputPort}).

-record(bridging_flow_match,{vlanId,tunnelId,destMac,destMacMask}).

-record(bridging_flow_entry,{match_criteria,
                             gotoTableId,
                             groupID,
                             tunnelLogicalPort,
                             outputPort}).

-record(unicast_routing_flow_match,{etherType,
                                    dstIp4,
                                    dstIp4Mask,
                                    dstIp6,
                                    dstIp6Mask}).

-record(unicast_routing_flow_entry,{match_criteria,
                                    gotoTableId,
                                    groupID}).

-record(multicast_routing_flow_match,{etherType,
                                      vlanId,
                                      srcIp4,
                                      srcIp4Mask,
                                      dstIp4,
                                      srcIp6,
                                      srcIp6Mask,
                                      dstIp6}).

-record(multicast_routing_flow_entry,{match_criteria,
                                      gotoTableId,
                                      groupID}).

-record(policy_acl_flow_match,{inPort,
                               inPortMask,
                               srcMac,
                               srcMacMask,
                               destMac,
                               destMacMask,
                               etherType,
                               vlanId,
                               vlanIdMask,
                               vlanPcp,
                               vlanPcpMask,
                               tunnelId,
                               sourceIp4,
                               sourceIp4Mask,
                               destIp4,
                               destIp4Mask,
                               sourceIp6,
                               sourceIp6Mask,
                               destIp6,
                               destIp6Mask,
                               ipv4ArpSpa,
                               ipv4ArpSpaMask,
                               ipProto,
                               ipProtoMask,
                               dscp,
                               dscpMask,
                               ecn,
                               ecnMask,
                               srcL4Port,
                               srcL4PortMask,
                               destL4Port,
                               destL4PortMask,
                               icmpType,
                               icmpTypeMask,
                               icmpCode,
                               icmpCodeMask,
                               ipv6FlowLabel,
                               ipv6FlowLabelMask}).

-record(policy_acl_flow_entry,{match_criteria,
                               groupID,
                               queueIDAction,
                               queueID,
                               vlanPcpAction,
                               vlanPcp,
                               dscpAction,
                               dscp,
                               outputTunnelPort,
                               outputPort,
                               clearActions}).

-record(flow_entry,{tableId,
                    priority,
                    flowData,
                    hard_time,
                    idle_time,
                    cookie}).

-record(flow_entry_stats,{durationSec,receivedPackets,receivedBytes}).

-record(group_entry,{groupId}).

-record(group_entry_stats,{refCount,duration,bucketCount}).

-record(l_2_interface_group_bucket_data,{outputPort,popVlanTag}).

-record(l_3_interface_group_bucket_data,{vlanId,srcMac}).

-record(l_3_unicast_group_bucket_data,{srcMac,dstMac,vlanId}).

-record(l_2_overlay_group_bucket_data,{outputPort}).

-record(l_2_rewrite_group_bucket_data,{srcMac,dstMac,vlanId}).

-record(group_bucket_entry,{groupId,
                            bucketIndex,
                            referenceGroupId,
                            bucketData}).

-record(group_table_info,{numGroupEntries,
                          maxGroupEntries,
                          maxBucketEntries}).

-record(port_feature,{curr,advertised,supported,peer}).

-record(port_stats,{rx_packets,
                    tx_packets,
                    rx_bytes,
                    tx_bytes,
                    rx_errors,
                    tx_errors,
                    rx_drops,
                    tx_drops,
                    rx_frame_err,
                    rx_over_err,
                    rx_crc_err,
                    collisions,
                    duration_seconds}).

-record(packet,{reason,tableId,inPortNum,pktData}).

-record(port_event,{eventMask,portNum,state}).

-record(flow_event,{eventMask,flowMatch}).

-record(flow_table_info,{numEntries,maxEntries}).

-record(port_queue_stats,{txBytes =0,txPkts =0,duration_seconds =0}).

%EOF
