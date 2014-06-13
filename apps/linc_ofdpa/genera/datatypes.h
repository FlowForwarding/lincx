/*********************************************************************
*
* (C) Copyright Broadcom Corporation 2003-2014
*
*  Licensed under the Apache License, Version 2.0 (the "License");
*  you may not use this file except in compliance with the License.
*  You may obtain a copy of the License at
*
*      http://www.apache.org/licenses/LICENSE-2.0
*
*  Unless required by applicable law or agreed to in writing, software
*  distributed under the License is distributed on an "AS IS" BASIS,
*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
*  See the License for the specific language governing permissions and
*  limitations under the License.
*
**********************************************************************
*
* @filename     ofdpa_datatypes.h
*
* @purpose      OF-DPA datatypes header
*
* @component    OF-DPA
*
* @comments     none
*
* @create       27 June 2013
*
* @end
*
**********************************************************************/
#ifndef INCLUDE_OFDPA_DATATYPES_H
#define INCLUDE_OFDPA_DATATYPES_H

#include <stdint.h>
#include <sys/time.h>
#include <netinet/in.h>

/** OFDPA uses these enumerators to indicate the error codes. */
typedef enum
{
  /** Success. */
  OFDPA_E_NONE = 0,
  /** Error in RPC. */
  OFDPA_E_RPC                  = -20,
  /** Internal error. */
  OFDPA_E_INTERNAL             = -21,
  /** Invalid parameter. */
  OFDPA_E_PARAM                = -22,
  /** Parameter constraint violated. */
  OFDPA_E_ERROR                = -23,
  /** Maximum count is already reached or table full. */
  OFDPA_E_FULL                 = -24,
  /** Already exists. */
  OFDPA_E_EXISTS               = -25,
  /** Operation Timeout. */
  OFDPA_E_TIMEOUT              = -26,
  /** Operation Fail. */
  OFDPA_E_FAIL                 = -27,
  /** Disabled. */
  OFDPA_E_DISABLED             = -28,
  /** Parameter/feature is not supported. */
  OFDPA_E_UNAVAIL              = -29,
  /** Parameter not found. */
  OFDPA_E_NOT_FOUND            = -30,
  /** Nothing to report or table is empty. */
  OFDPA_E_EMPTY                = -31,
} OFDPA_ERROR_t;

/** MAC address length */
#define OFDPA_MAC_ADDR_LEN 6

/** Check if MAC address is NULL */
#define OFDPA_MAC_ADDR_IS_NULL(mac) \
   ((mac[0] == 0) && (mac[1] == 0) && (mac[2] == 0) && \
    (mac[3] == 0) && (mac[4] == 0) && (mac[5] == 0))

/** Open Flow Controller port */
#define OFDPA_PORT_CONTROLLER     0xfffffffdu

/** VLAN Id Field Mask */
#define OFDPA_VID_FIELD_MASK 0x0000
/** VLAN Id Exact Mask */
#define OFDPA_VID_EXACT_MASK 0x0fff

/** VLAN Id None */
#define OFDPA_VID_NONE    0x0000
/** VLAN Id present */
#define OFDPA_VID_PRESENT 0x1000

typedef enum
{
  /** Port type Physical */
  OFDPA_PORT_TYPE_PHYSICAL = 0,
  /** Port type Logical Tunnel */
  OFDPA_PORT_TYPE_LOGICAL_TUNNEL = 1
} OFDPA_PORT_TYPE_t;

///** MAC address */
//typedef struct ofdpaMacAddr_s
//{
//  /** MAC address */
//  uint8_t     addr[OFDPA_MAC_ADDR_LEN];
//} ofdpaMacAddr_t;

/** Intrinsic buffer descriptor.
 *
 * NOTE: This is intended to be used as a basic data type that can be
 *       passed directly between functions -- keep it small and simple.
 *       To use this as an IN/OUT or OUT value, pass a pointer to the
 *       element so that the called routine can update the 'size' field
 *       with the actual content length being output.
 *
 * NOTE: When setting the 'size' field to indicate the amount of data
 *       contained in the buffer, the conventional usage is to account
 *       for all bytes including any string termination characters
 *       (e.g. strlen()+1).
 */
//typedef struct
//{
//  /** total buffer size (IN) / content length (OUT) */
//  uint32_t                size;
//  /** ptr to buffer starting location */
//  char                   *pstart;
//} ofdpa_buffdesc;

/*
 * Debugging and Logging definitions
 */

/**  enable or disable for config or status parameter. */
typedef enum
{
  /** disable */
  OFDPA_DISABLE,

  /** enable */
  OFDPA_ENABLE,

} OFDPA_CONTROL_t;

/** Component enumerator */
typedef enum
{
  /** First Component */
  OFDPA_COMPONENT_FIRST = 1,
    /** API Component */
  OFDPA_COMPONENT_API   = OFDPA_COMPONENT_FIRST,
    /** Mapping Component */
  OFDPA_COMPONENT_MAPPING,
    /** RPC Component */
  OFDPA_COMPONENT_RPC,
    /** OFDB Component */
  OFDPA_COMPONENT_OFDB,
    /** Datapath Component */
  OFDPA_COMPONENT_DATAPATH,

  /** Must be last */
  OFDPA_COMPONENT_MAX
} ofdpaComponentIds_t;

/** Debug verbosity enumerator */
typedef enum
{
  /** Logging Always */
  OFDPA_DEBUG_ALWAYS = 0,
  /** Basic Logging */
  OFDPA_DEBUG_BASIC,
  /** Verbose Logging */
  OFDPA_DEBUG_VERBOSE,
  /** Very Verbose Logging */
  OFDPA_DEBUG_VERY_VERBOSE,
  /** Too Verbose Logging */
  OFDPA_DEBUG_TOO_VERBOSE,

  /** Must be last */
  OFDPA_DEBUG_MAX
} ofdpaDebugLevels_t;

/** Exact Mask */
#define OFDPA_INPORT_EXACT_MASK  0xffffffffu
/** Field Mask */
#define OFDPA_INPORT_FIELD_MASK  0x00000000u
/** Number Mask */
#define OFDPA_INPORT_INDEX_MASK 0x0000ffffu
/** Type Mask */
#define OFDPA_INPORT_TYPE_MASK   0xffff0000u

/** IPv4 Address Exact Mask */
#define OFDPA_IPV4_ADDR_EXACT_MASK  0xffffffffu
/** IPv4 Address Field Mask */
#define OFDPA_IPV4_ADDR_FIELD_MASK  0x00000000u

/* Flow Tables.
*/

/** Flow Table Id enumerator */
typedef enum
{
  OFDPA_FLOW_TABLE_ID_INGRESS_PORT      =  0,  /**< Ingress Port Table */
  OFDPA_FLOW_TABLE_ID_VLAN              = 10,  /**< VLAN Table */
  OFDPA_FLOW_TABLE_ID_TERMINATION_MAC   = 20,  /**< Termination MAC Table */
  OFDPA_FLOW_TABLE_ID_UNICAST_ROUTING   = 30,  /**< Unicast Routing Table */
  OFDPA_FLOW_TABLE_ID_MULTICAST_ROUTING = 40,  /**< Multicast Routing Table */
  OFDPA_FLOW_TABLE_ID_BRIDGING          = 50,  /**< Bridging Table */
  OFDPA_FLOW_TABLE_ID_ACL_POLICY        = 60,  /**< ACL Table */

} OFDPA_FLOW_TABLE_ID_t;

/** Source MAC Lookup Table */
#define OFDPA_FLOW_TABLE_ID_SA_LOOKUP 254

/** Ingress Port Flow Table Match */
typedef struct ofdpaIngressPortFlowMatch_s
{
  /** OpenFlow Ingress Port number */
  uint32_t    inPort;
  /** Mask must be equal to OFDPA_INPORT_TYPE_MASK */
  uint32_t    inPortMask;
} ofdpaIngressPortFlowMatch_t;

/** Ingress Port Flow Table Entry */
typedef struct ofdpaIngressPortFlowEntry_s
{
  /** Match Criteria  */
  ofdpaIngressPortFlowMatch_t  match_criteria;

  /* flow instructions  */
  /** Goto-Table instruction */
  /** must be OFDPA_FLOW_TABLE_ID_TERMINATION_MAC or 0 to drop */
  OFDPA_FLOW_TABLE_ID_t gotoTableId;

} ofdpaIngressPortFlowEntry_t;

/*------------------------------------------------------------------------------------*/

/** VLAN Flow Table Match */
typedef struct ofdpaVlanFlowMatch_s
{
  /** OpenFlow Ingress Port number */
  uint32_t    inPort;

  uint16_t    vlanId;
  uint16_t    vlanIdMask;

} ofdpaVlanFlowMatch_t;

/** VLAN Flow Table Entry */
typedef struct ofdpaVlanFlowEntry_s
{
  /** Match Criteria  */
  ofdpaVlanFlowMatch_t  match_criteria;

  /* flow instructions  */

  /** Goto-Table instruction */
  /** must be OFDPA_FLOW_TABLE_ID_TERMINATION_MAC or 0 to drop */
  OFDPA_FLOW_TABLE_ID_t gotoTableId;

  /** Write Actions Instruction */
  /** New VLAN Id */
  uint16_t    newVlanId;

} ofdpaVlanFlowEntry_t;

/*------------------------------------------------------------------------------------*/
/** Termination MAC Flow Table Match */
typedef struct ofdpaTerminationMacFlowMatch_s
{
  /** OpenFlow Ingress Port number */
  uint32_t    inPort;

  /** Field maskable only */
  uint32_t    inPortMask;

  /** Must be either 0x0800 or 0x86dd*/
  uint16_t    etherType;

  /** Destination MAC */
  ofdpaMacAddr_t destMac;

  /** destMacMask is only used for multicast destMac entries (mask requirements apply) */
  ofdpaMacAddr_t destMacMask;

  /** VLAN Id */
  uint16_t    vlanId;
  /** VLAN Id Mask. Field maskable only */
  uint16_t    vlanIdMask;

} ofdpaTerminationMacFlowMatch_t;

/** Termination MAC Flow Table Entry */
typedef struct ofdpaTerminationMacFlowEntry_s
{
  /** Match Criteria  */
  ofdpaTerminationMacFlowMatch_t  match_criteria;

  /* flow instructions  */

  /** Goto-Table instruction */
  /** only acceptable values are OFDPA_FLOW_TABLE_ID_ROUTING,
      OFDPA_FLOW_TABLE_ID_MULTICAST_ROUTING */
  OFDPA_FLOW_TABLE_ID_t gotoTableId;

  /** Apply-Action(s) instruction */
  /* If specified, must be CONTROLLER, set to 0 otherwise */

  uint32_t    outputPort;

} ofdpaTerminationMacFlowEntry_t;

/*------------------------------------------------------------------------------------*/

/** Bridging Flow Table Match */
typedef struct ofdpaBridgingFlowMatch_s
{
  uint16_t    vlanId;
  uint16_t    tunnelId;

  /** Destination MAC */
  ofdpaMacAddr_t destMac;
  /** Field maskable, DLF rule if masked */
  ofdpaMacAddr_t destMacMask;

} ofdpaBridgingFlowMatch_t;

/** Bridging Flow Table Entry */
typedef struct ofdpaBridgingFlowEntry_s
{
  /** Match Criteria  */
  ofdpaBridgingFlowMatch_t  match_criteria;

  /* flow instructions  */
  /** Goto-Table instruction */
  /** must be OFDPA_FLOW_TABLE_ID_ACL_POLICY,
      setting to 0 indicates drop action */

  OFDPA_FLOW_TABLE_ID_t gotoTableId;

  /** Write-Action(s) instruction */
  /** data for GROUP action must be a L2 Interface, L2 Multicast, L2 Flood,
   *  or L2 Overlay group entry as appropriate */
  uint32_t    groupID;
  /** Unicast Tenant Bridging flows specify a tunnel logical port ID */
  uint32_t    tunnelLogicalPort;

  /** Apply-Action(s) instruction */
  /** data for OUTPUT action, restricted to CONTROLLER, set to 0 otherwise */

  uint32_t    outputPort;

} ofdpaBridgingFlowEntry_t;

/*------------------------------------------------------------------------------------*/

/** Unicast Routing Flow Table Match */
typedef struct ofdpaUnicastRoutingFlowMatch_s
{
  /** Ethertype. Must be 0x0800 or 0x86dd */
  uint16_t    etherType;

  /** Destination IPv4 address stored in host byte order. Must be unicast address */
  in_addr_t    dstIp4;

  /** IP mask stored in host byte order. Must be prefix mask  */
  in_addr_t    dstIp4Mask;

  /** Destination IPv6 address. Must be unicast address */
  struct in6_addr   dstIp6;

  /** IP Mask. Must be prefix mask  */
  struct in6_addr   dstIp6Mask;

} ofdpaUnicastRoutingFlowMatch_t;

/** Unicast Routing Flow Table Entry */
typedef struct ofdpaUnicastRoutingFlowEntry_s
{
  /** Match Criteria  */
  ofdpaUnicastRoutingFlowMatch_t  match_criteria;

  /* flow instructions  */
  /** Goto-Table instruction */
  /** Setting this to 0 indicates drop action; otherwise must be OFDPA_FLOW_TABLE_ID_ACL_POLICY */

  OFDPA_FLOW_TABLE_ID_t gotoTableId;

  /** Write-Action(s) instruction */
  /** Data for GROUP action, must be an L3 Unicast group entry */

  uint32_t    groupID;

} ofdpaUnicastRoutingFlowEntry_t;

/*------------------------------------------------------------------------------------*/

/** Multicast Routing Flow Table Match */
typedef struct ofdpaMulticastRoutingFlowMatch_s
{
  /** Ethertype. Must be 0x0800 or 0x86dd */
  uint16_t    etherType;

  /** VLAN Id */
  uint16_t    vlanId;

  /** Source IPv4 address stored in host byte order. Optional, can contain IPv4 address, must be completely masked if not used */
  in_addr_t  srcIp4;

  /** IP Mask stored in host byte order */
  in_addr_t  srcIp4Mask;

  /** Destination IPv4 address stored in host byte order. Must be multicast address */
  in_addr_t  dstIp4;

  /** Source IPv6 Address. Optional. Can contain IPv6 address, must be completely masked if not used */
  struct in6_addr  srcIp6;

  /** IP Mask */
  struct in6_addr  srcIp6Mask;

  /** Destination IPv6 Address. Must be multicast address */
  struct in6_addr  dstIp6;

} ofdpaMulticastRoutingFlowMatch_t;

/** Multicast Routing Flow Table Entry */
typedef struct ofdpaMulticastRoutingFlowEntry_s
{
  /** Match Criteria  */
  ofdpaMulticastRoutingFlowMatch_t  match_criteria;

  /* flow instructions  */
  /** Goto-Table instruction */
  /** Setting this to 0 indicates drop action; otherwise must be OFDPA_FLOW_TABLE_ID_ACL_POLICY */

  OFDPA_FLOW_TABLE_ID_t gotoTableId;

  /** Write-Action(s) instruction */
  /** Data for GROUP action. Must be an L3 Multicast group entry */
  uint32_t    groupID;

} ofdpaMulticastRoutingFlowEntry_t;

/*------------------------------------------------------------------------------------*/

/** ACL Flow Table Match */
typedef struct ofdpaPolicyAclFlowMatch_s
{
  /** OpenFlow Ingress Port number */
  uint32_t    inPort;

  /** Field maskable only */
  uint32_t    inPortMask;

  /** Source MAC */
  ofdpaMacAddr_t srcMac;

  /** Source MAC Mask */
  ofdpaMacAddr_t srcMacMask;

  /** Destination MAC */
  ofdpaMacAddr_t destMac;

  /** Destination MAC Mask */
  ofdpaMacAddr_t destMacMask;

  /** etherType dictates what other fields are eligible for matching;
  if etherType == 0x86dd, only IPv6 L3 fields are matched */
  uint16_t    etherType;

  /** VLAN ID */
  uint16_t    vlanId;

  /** VLAN ID Mask */
  uint16_t    vlanIdMask;

  /** VLAN Priority Code Point. 3 bits used for VLAN PCP */
  uint16_t    vlanPcp;

  /** VLAN PCP Mask */
  uint16_t    vlanPcpMask;

  /** Tenant Identifier */
  uint32_t    tunnelId;

  /** Source v4 IP stored in host byte order */
  in_addr_t   sourceIp4;

  /** Source v4 IP mask stored in host byte order */
  in_addr_t   sourceIp4Mask;

  /** Destination v4 IP stored in host byte order */
  in_addr_t   destIp4;

  /** Destination v4 IP mask stored in host byte order */
  in_addr_t   destIp4Mask;

  /** Source v6 IP */
  struct in6_addr  sourceIp6;

  /** Source v6 IP Mask */
  struct in6_addr  sourceIp6Mask;

  /** Destination v4 IP */
  struct in6_addr  destIp6;

  /** Destination v4 IP Mask */
  struct in6_addr  destIp6Mask;

  /** Source IPv4 address in the ARP payload. Only used if etherType == 0x0806 */
  uint32_t    ipv4ArpSpa;

  /** Mask */
  uint32_t    ipv4ArpSpaMask;

  /** IP protocol. Only used if etherType == 0x0800 */
  uint16_t    ipProto;

  /** IP protocol Mask */
  uint16_t    ipProtoMask;

  /** DSCP */
  uint16_t    dscp;

  /** DSCP Mask */
  uint16_t    dscpMask;

  /** ECN bits of IP header. Part of the IPv4 ToS field or the IPv6 Trac Class field. */
  uint16_t    ecn;

  /** ECN Mask */
  uint16_t    ecnMask;

  /** Source L4 Port. Only used for TCP, UDP, or SCTP */
  uint32_t    srcL4Port;

  /** Mask */
  uint32_t    srcL4PortMask;

  /** Destination L4 Port. Only used for TCP, UDP, or SCTP */
  uint32_t    destL4Port;

  /** Mask */
  uint32_t    destL4PortMask;

  /** ICMP Type. Only used if IP_PROTO is 1 */
  uint8_t     icmpType;

  /** Mask */
  uint8_t     icmpTypeMask;

  /** ICMP Code */
  uint8_t     icmpCode;

  /** Mask */
  uint8_t     icmpCodeMask;

  /** IPv6 Flow label. Only used if etherType == 0x86dd */
  uint32_t    ipv6FlowLabel;

  /** Mask */
  uint32_t    ipv6FlowLabelMask;

} ofdpaPolicyAclFlowMatch_t;

/* NOTE: ACL Flow Table entries are indexed by the priority value.
 *       This indexing establishes the rule precedence in the TCAM.
 */

/** Policy ACL Flow Table Entry */
typedef struct ofdpaPolicyAclFlowEntry_s
{
  /** match criteria */
  ofdpaPolicyAclFlowMatch_t match_criteria;

  /* flow instructions  */

  /** Write-Action(s) instruction */

  /** data for GROUP action -- ID == 0 means no group write action */
  uint32_t    groupID;

  /** If non-zero, write the queue ID */
  uint8_t     queueIDAction;

  /** data for SET_QUEUE action */
  uint8_t     queueID;

  /** If non-zero, write the VLAN priority */
  uint8_t     vlanPcpAction;

  /** data for SET_VLAN_PCP action */
  uint8_t     vlanPcp;

  /** If non-zero, write the DSCP */
  uint8_t     dscpAction;

  /** data for SET_IP_DSCP action */
  uint8_t     dscp;

  /**  data for OUTPUT write-action, restricted to valid tunnel logical port, set to 0 otherwise */
  uint32_t    outputTunnelPort;

  /** Apply-Action(s) instruction */

  /**  data for OUTPUT apply-action, restricted to CONTROLLER, set to 0 otherwise */
  uint32_t    outputPort;

  /** Clear-Action(s) instruction */
  /** if 1 packets matching flow are dropped (all other instructions ignored) */
  uint32_t    clearActions;

} ofdpaPolicyAclFlowEntry_t;


/** This structure is used to add a new flow or modify an existing flow.
*/
typedef struct ofdpaFlowEntry_s
{
  /** Flow Table Id */
  OFDPA_FLOW_TABLE_ID_t      tableId;

  /** Flow priority */
  uint32_t                   priority;

  union
  {
    /** Ingress Port Table Flow Entry */
    ofdpaIngressPortFlowEntry_t      ingressPortFlowEntry;

    /** VLAN Port Table Flow Entry */
    ofdpaVlanFlowEntry_t             vlanFlowEntry;

    /** Termination MAC Table Flow Entry */
    ofdpaTerminationMacFlowEntry_t   terminationMacFlowEntry;

    /** Bridging Table Flow Entry */
    ofdpaBridgingFlowEntry_t         bridgingFlowEntry;

    /** Unicast Routing Table Flow Entry */
    ofdpaUnicastRoutingFlowEntry_t   unicastRoutingFlowEntry;

    /** Multiicast Routing Table Flow Entry */
    ofdpaMulticastRoutingFlowEntry_t multicastRoutingFlowEntry;

    /** ACL Table Flow Entry */
    ofdpaPolicyAclFlowEntry_t        policyAclFlowEntry;
  } flowData;

  /** Hard timeout for the flow entry */
  uint32_t   hard_time;

  /** Idle timeout for the flow entry */
  uint32_t   idle_time;

  /** cookie */
  uint64_t   cookie;

} ofdpaFlowEntry_t;

/** Flow Stats information. */
typedef struct ofdpaFlowEntryStats_s
{
  /** flow duration */
  uint32_t  durationSec;

  /** not all flow tables support these counters; for flow tables that do not support
     the counters, 0 is returned */
  uint64_t  receivedPackets;

  /** not all flow tables support these counters; for flow tables that do not support
     the counters, 0 is returned */
  uint64_t  receivedBytes;

} ofdpaFlowEntryStats_t;

/*------------------------------------------------------------------------------------*/
/* group table APIs */

/** Group Type Enumerator */
typedef enum
{
  /** Group type L2 Interface */
  OFDPA_GROUP_ENTRY_TYPE_L2_INTERFACE = 0,
  /** Group type L2 Rewrite */
  OFDPA_GROUP_ENTRY_TYPE_L2_REWRITE   = 1,
  /** Group type L3 Unicast */
  OFDPA_GROUP_ENTRY_TYPE_L3_UNICAST   = 2,
  /** Group type L2 Multicast */
  OFDPA_GROUP_ENTRY_TYPE_L2_MULTICAST = 3,
  /** Group type L2 Flood */
  OFDPA_GROUP_ENTRY_TYPE_L2_FLOOD     = 4,
  /** Group type L3 Interface */
  OFDPA_GROUP_ENTRY_TYPE_L3_INTERFACE = 5,
  /** Group type L3 Multicast */
  OFDPA_GROUP_ENTRY_TYPE_L3_MULTICAST = 6,
  /** Group type L3 ECMP */
  OFDPA_GROUP_ENTRY_TYPE_L3_ECMP      = 7,
  /** Group type L2 Overlay */
  OFDPA_GROUP_ENTRY_TYPE_L2_OVERLAY   = 8,

  /** Must be last */
  OFDPA_GROUP_ENTRY_TYPE_LAST
} OFDPA_GROUP_ENTRY_TYPE_t;

/** L2 Overlay Group Sub-type Enumerator */
typedef enum
{
  /** send unknown multicast traffic via unicast tunnels   */
  OFDPA_L2_OVERLAY_FLOOD_UNICAST_TUNNEL       = 0,  
  /** send unknown multicast traffic via multicast tunnels */
  OFDPA_L2_OVERLAY_FLOOD_MULTICAST_TUNNEL     = 1,  
  /** send known multicast traffic via unicast tunnels   */
  OFDPA_L2_OVERLAY_MULTICAST_UNICAST_TUNNEL   = 2,  
  /** send known multicast traffic via multicast tunnels */
  OFDPA_L2_OVERLAY_MULTICAST_MULTICAST_TUNNEL = 3   

} OFDPA_L2_OVERLAY_SUBTYPE_t;

/**
   The Group Table contains one entry for each Group.  The table is indexed
   by the groupId which identifies the group entry.  Data is encoded into the groupId to specify the OF-DPA
   group entry type and information required by OF-DPA to configure the datapath.

   The groupId encoding method is:

   L2 Interface type:
        (MSB to LSB) 4 bits encode the Group Table Entry type | 12 bits of VLAN ID | 16 bits of port identifier

   L2 Multicast, L2 Flood and L3 Multicast types:
        (MSB to LSB) 4 bits encode the Group Table Entry type | 12 bits of VLAN ID | 16 bits of index

   L2 Rewrite, L3 Unicast, L3 Interface and L3 ECMP types:
        (MSB to LSB) 4 bits encode the Group Table Entry type | 28 bits of index

   L2 Overlay type
        (MSB to LSB) 4 bits encode the Group Table Entry type | 16 bits of tunnel ID | 2 bits of sub-type | 10 bits of index

*/

/** Group Table Entry */
typedef struct ofdpaGroupEntry_s
{
  /** Group Id */
  uint32_t    groupId;

} ofdpaGroupEntry_t;

/** Group Table Entry Statistics */
typedef struct ofdpaGroupEntryStats_s
{
  /** Group reference count */
  uint32_t                  refCount;

  /** time in secounds since the Group was added */
  uint32_t                  duration;

  /** number of buckets in the Group */
  uint32_t                  bucketCount;
} ofdpaGroupEntryStats_t;

/*
   Group Table entries contain one or more Action Buckets depending on their type.
   The Group Bucket Table stores these references.  It is indexed by groupId and referenceGroupId.  The presence
   of an entry in this table creates a referral by the Group Table entry specified in groupId to the Group Table
   entry specified in referenceGroupId.  Restrictions on the number of references and the allowable type of the
   referenced Group Table entries varies by entry type.
*/

/** L2 Interface Group Bucket */
typedef struct ofdpaL2InterfaceGroupBucketData_s
{
  /** bucket action set */
  /** controller responsible for assuring data in outputPort
      and the port identifier data encoded in the groupId are equal */

  uint32_t       outputPort;

  /** controller responsible for assuring vlan is identified in groupId */
  /** flag indicating if outer VLAN tag should be stripped
  (0 - do not strip VLAN tag, 1 - strip VLAN tag) */

  uint32_t       popVlanTag;

} ofdpaL2InterfaceGroupBucketData_t;

/** L3 Interface Group Bucket */
typedef struct ofdpaL3InterfaceGroupBucketData_s
{
  /** bucket action set */
  /** data for Set-Field action; vlanId data must match
      the VLAN ID encoded in the groupId */
  uint32_t    vlanId;
  ofdpaMacAddr_t     srcMac;

} ofdpaL3InterfaceGroupBucketData_t;

/** L3 Unicast Group Bucket */
typedef struct ofdpaL3UnicastGroupBucketData_s
{
  /** bucket action set */

  /** data for Set-Field action */
  ofdpaMacAddr_t     srcMac;
  ofdpaMacAddr_t     dstMac;
  uint32_t    vlanId;

} ofdpaL3UnicastGroupBucketData_t;

typedef struct
{
  /* bucket action set */
  uint32_t       outputPort;  /* controller responsible for assuring data in outputPort
                                 and the port identifier data encoded in the groupId are equal */
                              /* only Access and Tunnel Endpoint Logical ports are accepted */
} ofdpaL2OverlayGroupBucketData_t;


/** L2 Rewrite Group Bucket */
typedef struct ofdpaL2RewriteGroupBucketData_s
{
  /** bucket action set */

  /** data for Set-Field action */
  ofdpaMacAddr_t     srcMac;
  ofdpaMacAddr_t     dstMac;
  uint32_t           vlanId;

} ofdpaL2RewriteGroupBucketData_t;

/** Group Bucket Table Entry */
typedef struct ofdpaGroupBucketEntry_s
{
  /** Group Id */
  uint32_t    groupId;

  /** Bucket Index */
  uint32_t    bucketIndex;

  /** bucket action set */
  /** References a chained group entry, must be zero for L2 Interface Group entries */

  uint32_t    referenceGroupId;

  union
  {
    /** L2 Interface */
    ofdpaL2InterfaceGroupBucketData_t  l2Interface;
    /** L3 Interface */
    ofdpaL3InterfaceGroupBucketData_t  l3Interface;
    /** L3 Unicast */
    ofdpaL3UnicastGroupBucketData_t    l3Unicast;
    /** L2 Rewrite */
    ofdpaL2RewriteGroupBucketData_t    l2Rewrite;
    /** L2 Overlay */
    ofdpaL2OverlayGroupBucketData_t    l2Overlay;
  } bucketData;

} ofdpaGroupBucketEntry_t;

/** Group table information */
typedef struct ofdpaGroupTableInfo_s
{
  /** Current number of group entries of a given type */
  uint32_t     numGroupEntries;

  /** Max number of group entries of a given type */
  uint32_t     maxGroupEntries;

  /** Max number of group bucket entries per group for a given group type */
  uint32_t     maxBucketEntries;

} ofdpaGroupTableInfo_t;


/*------------------------------------------------------------------------------------*/
/* Port Table */

/** Port name maximum string length */
#define OFDPA_PORT_NAME_STRING_SIZE 16

/** Flags to indicate behavior of the physical port. These flags are
* used in ofp_port to describe the current configuration. They are
* used in the ofp_port_mod message to configure the port's behavior.
*/
typedef enum
{
  /** Port is administratively down. */
  OFDPA_PORT_CONFIG_DOWN = 1 << 0,
} OFDPA_PORT_CONFIG_t;

/** Current state of the physical port. These are not configurable from
* the controller.
*/
typedef enum
{
  /** No physical link present. */
  OFDPA_PORT_STATE_LINK_DOWN = 1 << 0,
} OFDPA_PORT_STATE_t;

/** Features of ports available in a datapath. */
typedef enum
{
  /** 10 Mb half-duplex rate support. */
  OFDPA_PORT_FEAT_10MB_HD = 1 << 0,

  /** 10 Mb full-duplex rate support. */
  OFDPA_PORT_FEAT_10MB_FD = 1 << 1,

  /** 100 Mb half-duplex rate support. */
  OFDPA_PORT_FEAT_100MB_HD = 1 << 2,

  /** 100 Mb full-duplex rate support. */
  OFDPA_PORT_FEAT_100MB_FD = 1 << 3,

  /** 1 Gb half-duplex rate support. */
  OFDPA_PORT_FEAT_1GB_HD = 1 << 4,

  /** 1 Gb full-duplex rate support. */
  OFDPA_PORT_FEAT_1GB_FD = 1 << 5,

  /** 10 Gb full-duplex rate support. */
  OFDPA_PORT_FEAT_10GB_FD = 1 << 6,

  /** 40 Gb full-duplex rate support. */
  OFDPA_PORT_FEAT_40GB_FD = 1 << 7,

  /** 100 Gb full-duplex rate support. */
  OFDPA_PORT_FEAT_100GB_FD = 1 << 8,

  /** 1 Tb full-duplex rate support. */
  OFDPA_PORT_FEAT_1TB_FD = 1 << 9,

  /** Other rate, not in the list. */
  OFDPA_PORT_FEAT_OTHER = 1 << 10,

  /** Copper medium. */
  OFDPA_PORT_FEAT_COPPER = 1 << 11,

  /** Fiber medium. */
  OFDPA_PORT_FEAT_FIBER = 1 << 12,

  /** Auto-negotiation. */
  OFDPA_PORT_FEAT_AUTONEG = 1 << 13,

  /** Pause. */
  OFDPA_PORT_FEAT_PAUSE = 1 << 14,

  /** Asymmetric pause. */
  OFDPA_PORT_FEAT_PAUSE_ASYM = 1 << 15
} OFDPA_PORT_FEATURE_t;

/** Bitmaps of OFDPA_PORT_FEAT_* that describe features. All bits zeroed if
 * unsupported or unavailable. */
typedef struct ofdpaPortFeature_s
{
  /** Current features. */
  OFDPA_PORT_FEATURE_t curr;

  /** Features being advertised by the port. */
  OFDPA_PORT_FEATURE_t advertised;

  /** Features supported by the port. */
  OFDPA_PORT_FEATURE_t supported;

  /** Features advertised by peer. */
  OFDPA_PORT_FEATURE_t peer;
} ofdpaPortFeature_t;

/** Port Statistics */
typedef struct ofdpaPortStats_s
{
  /** Received Packets */
  uint64_t rx_packets;

  /** Transmitted Packets */
  uint64_t tx_packets;

  /** Received Bytes */
  uint64_t rx_bytes;

  /** Transmitted Bytes */
  uint64_t tx_bytes;

  /** Received Errors */
  uint64_t rx_errors;

  /** Transmitted Errors */
  uint64_t tx_errors;

  /** Received Packets Dropped */
  uint64_t rx_drops;

  /** Transmit Packets Dropped */
  uint64_t tx_drops;

  /** Received Frame Errors */
  uint64_t rx_frame_err;

  /** Received Frame Overrun Errors */
  uint64_t rx_over_err;

  /** Received Packets with CRC Errors */
  uint64_t rx_crc_err;

  /** Transmit collisions */
  uint64_t collisions;

  /** Time port has been alive in seconds */
  uint32_t duration_seconds;
} ofdpaPortStats_t;

/** Flags field in ofdpaPktSend() */
#define OFDPA_PKT_LOOKUP  1

/** Packet In reason codes  */
typedef enum
{
  /** No Match */
  OFDPA_PACKET_IN_REASON_NO_MATCH = 0,
  /** Action */
  OFDPA_PACKET_IN_REASON_ACTION,
  /** Invalid TTL */
  OFDPA_PACKET_IN_REASON_INVALID_TTL

} OFDPA_PACKET_IN_REASON_t;

/** Packet  */
typedef struct ofdpaPacket_s
{
  /** Reason */
  OFDPA_PACKET_IN_REASON_t  reason;

  /** Flow Table Id */
  OFDPA_FLOW_TABLE_ID_t     tableId;

  /** Input port */
  uint32_t                  inPortNum;

  /** Packet */
  ofdpa_buffdesc            pktData;
} ofdpaPacket_t;

/* Asynchronous Control Events */

/** Port event type */
typedef enum
{
  /** Port created */
  OFDPA_EVENT_PORT_CREATE = 1 << 0,

  /** Port deleted */
  OFDPA_EVENT_PORT_DELETE = 1 << 1,

  /** Port link state has changed */
  OFDPA_EVENT_PORT_STATE = 1 << 2,

} OFDPA_PORT_EVENT_MASK_t;

/** Port events */
typedef struct ofdpaPortEvent_s
{
  /** Event mask indication the event type */
  OFDPA_PORT_EVENT_MASK_t eventMask;

  /** Port number associated with the port event */
  uint32_t                portNum;

  /** Port Link state */
  OFDPA_PORT_STATE_t      state;
} ofdpaPortEvent_t;

/** Flow event type */
typedef enum
{
  /** Flow idle timeout event */
  OFDPA_FLOW_EVENT_IDLE_TIMEOUT = 1 << 0,

  /** Flow hard timeout event */
  OFDPA_FLOW_EVENT_HARD_TIMEOUT = 1 << 1

} OFDPA_FLOW_EVENT_MASK_t;

/** Flow events */
typedef struct ofdpaFlowEvent_s
{
  /** events that have occurred for this flow */
  OFDPA_FLOW_EVENT_MASK_t   eventMask;

  /** Flow match criteria */
  ofdpaFlowEntry_t flowMatch;

} ofdpaFlowEvent_t;

/*------------------------------------------------------------------------------------*/
/* Tunnel Logical Port APIs */

typedef enum
{
  /** Tunnel Port type Endpoint */
  OFDPA_TUNNEL_PORT_TYPE_ENDPOINT = 1,
  /** Tunnel Port type Access */
  OFDPA_TUNNEL_PORT_TYPE_ACCESS

} OFDPA_TUNNEL_PORT_TYPE_t;

typedef enum
{
  /** Tunnel Port protocol VxLAN */
  OFDPA_TUNNEL_PROTO_VXLAN = 1,
  /** Tunnel Port protocol NVGRE */
  OFDPA_TUNNEL_PROTO_NVGRE

} OFDPA_TUNNEL_PROTO_t;

typedef struct
{
  /** the data for terminatorUdpDstPort and useEntropy value must be the same for all VXLAN Tunnel Endpoint entries */
  uint16_t terminatorUdpDstPort;
  uint16_t initiatorUdpDstPort;

  uint16_t udpSrcPortIfNoEntropy;
  uint16_t useEntropy;

} ofdpaVxlanProtoInfo_t;

typedef struct
{
  uint16_t useEntropyInKey;

} ofdpaNvgreProtoInfo_t;

typedef struct
{
  uint32_t            physicalPortNum;
  uint16_t            vlanId;
  uint16_t            etag;
  uint16_t            untagged;
  uint16_t            useEtag;

} ofdpaAccessPortConfig_t;

typedef struct
{
  in_addr_t         remoteEndpoint;
  in_addr_t         localEndpoint;
  uint32_t          ttl;
  uint32_t          ecmp;

  uint32_t          nextHopId;

  union
  {
    ofdpaVxlanProtoInfo_t vxlan;
    ofdpaNvgreProtoInfo_t nvgre;
  } protocolInfo;

} ofdpaEndpointConfig_t;

typedef struct
{
  OFDPA_TUNNEL_PORT_TYPE_t  type;
  OFDPA_TUNNEL_PROTO_t      tunnelProtocol;

  union
  {
    ofdpaAccessPortConfig_t      access;
    ofdpaEndpointConfig_t        endpoint;
  } configData;
} ofdpaTunnelPortConfig_t;

typedef struct
{
  uint32_t refCount;
  uint32_t tenantCount;

} ofdpaTunnelPortStatus_t;

typedef struct
{
  uint32_t refCount;

} ofdpaTunnelPortTenantStatus_t;

typedef struct
{
  OFDPA_TUNNEL_PROTO_t  protocol;
  uint32_t              virtualNetworkId;

  in_addr_t             mcastIp;
  uint32_t              mcastNextHopId;

} ofdpaTunnelTenantConfig_t;

typedef struct
{
  uint32_t refCount;

} ofdpaTunnelTenantStatus_t;

typedef struct
{
  OFDPA_TUNNEL_PROTO_t  protocol;
  ofdpaMacAddr_t        srcAddr;
  ofdpaMacAddr_t        dstAddr;
  uint32_t              physicalPortNum;
  uint16_t              vlanId;

} ofdpaTunnelNextHopConfig_t;

typedef struct
{
  uint32_t refCount;

} ofdpaTunnelNextHopStatus_t;

typedef struct
{
  OFDPA_TUNNEL_PROTO_t protocol;

} ofdpaTunnelEcmpNextHopGroupConfig_t;

typedef struct
{
  uint32_t             refCount;
  uint32_t             memberCount;

} ofdpaTunnelEcmpNextHopGroupStatus_t;

/*------------------------------------------------------------------------------------*/
/* Table APIs */

/** Table name maximum string length */
#define OFDPA_TABLE_NAME_LEN 32

/** Flow table information */
typedef struct ofdpaFlowTableInfo_s
{
  /** Current number of entries in the table */
  uint32_t     numEntries;

  /** Max number of entries in the table */
  uint32_t     maxEntries;

  /* Not supporting any features etc as they will be well documented in programmers
     guide
  */
} ofdpaFlowTableInfo_t;

/*------------------------------------------------------------------------------------*/
/* Queue APIs */

/** Queue Stats */
typedef struct ofdpaPortQueueStats_s
{
  /** Transmitted bytes */
  uint64_t   txBytes;

  /** Transmitted packets */
  uint64_t   txPkts;

  /** Time queue has been alive in seconds. => Time since port is up */
  uint32_t   duration_seconds;

} ofdpaPortQueueStats_t;

/*------------------------------------------------------------------------------------*/
/* Vendor Extension APIs */

/** Source MAC Learning Mode */
typedef struct ofdpaSrcMacLearnModeCfg_s
{
  /**destPortNum - must be either LOCAL or CONTROLLER.
  Currently only controller interface is supported */
  uint32_t destPortNum;
}
ofdpaSrcMacLearnModeCfg_t;

#endif /* INCLUDE_OFDPA_DATATYPES_H */
