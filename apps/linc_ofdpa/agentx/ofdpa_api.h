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
* @filename     ofdpa_api.h
*
* @purpose      OF-DPA API header
*
* @component    OF-DPA
*
* @comments     none
*
* @create       03/22/2013
*
* @end
*
**********************************************************************/
#ifndef INCLUDE_OFDPA_API_H
#define INCLUDE_OFDPA_API_H

#include <stdint.h>
#include <sys/time.h>
#include <netinet/in.h>
#include "ofdpa_datatypes.h"

/*
 * All clients must register first.
 */
/*********************************************************************
* @purpose  Initialize OF-DPA client or user application. For OF-DPA Standalone
*           Application mode, sets up the RPC communication channel for the
*           client. For the statically linked User Application, initializes
*           the system.
*
* @param    clientName    @b{(input)} client name
*
* @returns  OFDPA_E_NONE  success
* @returns  OFDPA_E_PARAM error in parameter passed
* @returns  OFDPA_E_FAIL  failure
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaClientInitialize(char *clientName);

/*
 * Debugging and Logging definitions
 */

/*********************************************************************
* @purpose  Generate a log message using printf formatting
*
* @param    priority    @b{(input)} message priority (syslog values)
* @param    fmt         @b{(input)} format string
* @param    parms       @b{(input)} optional parameters for format string
*
* @returns  number of characters in log message
* @returns  value less than zero if error
*
* @end
*********************************************************************/
int ofdpaCltLogPrintf(int priority, char *fmt, ...);

/*********************************************************************
* @purpose  Generate a log message using a preformatted buffer
*
* @param    priority    @b{(input)} message priority (syslog values)
* @param    message     @b{(input)} buffer containing message
*
* @returns  number of characters in log message
* @returns  value less than zero if error
*
* @end
*********************************************************************/
int ofdpaCltLogBuf(int priority, ofdpa_buffdesc message);

/*********************************************************************
* @purpose  Generate a debug message using printf formatting
*
* @param    functionName @b{(input)} name of function generating message
* @param    component    @b{(input)} ID of component generating message
* @param    verbosity    @b{(input)} message severity
* @param    format       @b{(input)} format string
* @param    parms        @b{(input)} optional parameters for format string
*
* @returns  number of characters in debug message
* @returns  value less than zero if error
*
* @end
*********************************************************************/
int ofdpaCltDebugPrintf(const char *functionName, ofdpaComponentIds_t component, ofdpaDebugLevels_t verbosity, const char *format, ...);

/*********************************************************************
* @purpose  Generate a debug message using a preformatted buffer
*
* @param    functionName @b{(input)} name of function generating message
* @param    component    @b{(input)} ID of component generating message
* @param    verbosity    @b{(input)} message severity
* @param    message      @b{(input)} buffer containing message
*
* @returns  number of characters in debug message
* @returns  value less than zero if error
*
* @end
*********************************************************************/
int ofdpaCltDebugBuf(ofdpa_buffdesc functionName, ofdpaComponentIds_t component, ofdpaDebugLevels_t verbosity, ofdpa_buffdesc message);

/* Flow Tables.
*/

/*********************************************************************
 * @purpose  Initialize Flow entry structure.
 *
 * @param    tableId         @b{(input)}  Flow Table ID
 * @param    flow            @b{(inout)}  Flow entry structure
 *
 * @returns  OFDPA_E_NONE     if flow entry structure is initialized successfully.
 * @returns  OFDPA_E_PARAM    if an input parameter is invalid.
 *
 * @end
 *********************************************************************/
OFDPA_ERROR_t ofdpaFlowEntryInit(OFDPA_FLOW_TABLE_ID_t tableId, ofdpaFlowEntry_t *flow);

/*********************************************************************
* @purpose  Add a new entry to a flow table.
*
* @param    flow    @b{(input)} structure containing flow entry parameters
*
* @returns  OFDPA_E_NONE  flow entry added successfully
* @returns  OFDPA_E_PARAM error in flow entry parameters passed to function
* @returns  OFDPA_E_ERROR flow entry validity check failed
* @returns  OFDPA_E_FAIL  failure occurred during flow entry installation
* @returns  OFDPA_E_FULL  maximum number of flow entries already exist in table
* @returns  OFDPA_E_EXISTS an entry that overlaps the new entry already
*                          exists in the table
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaFlowAdd(ofdpaFlowEntry_t *flow);

/*********************************************************************
* @purpose  Modify an existing entry in a flow table.
*
* @param    flow    @b{(input)} structure containing flow entry parameters
*
* @returns  OFDPA_E_NONE  flow entry added successfully
* @returns  OFDPA_E_PARAM error in flow entry parameters passed to function
* @returns  OFDPA_E_ERROR flow entry validity check failed
* @returns  OFDPA_E_FAIL  failure occurred during flow entry installation
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaFlowModify(ofdpaFlowEntry_t *flow);

/*********************************************************************
* @purpose Delete an entry from a flow table.
*
* @param    flow    @b{(input)} structure containing flow entry parameters
*
* @returns  OFDPA_E_NONE  flow entry added successfully
* @returns  OFDPA_E_PARAM error in flow entry parameters passed to function
* @returns  OFDPA_E_FAIL  failure occurred during flow entry installation
* @returns  OFDPA_E_NOT_FOUND no matching flow entry found
* @returns  OFDPA_E_ERROR flow deletion validation failed
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaFlowDelete(ofdpaFlowEntry_t *flow);

/*********************************************************************
* @purpose Retrieve the next flow entry in the specified flow table.
*          This API allows the client to walk the entries in a flow table.
*          In order to get the first entry in a flow table, the client
*          calls with the flow table identifier set and all other
*          parameters set to zero.  Table entries are walked in the
*          order the table is indexed.  If there are no entries in the
*          table after the entry specified, a return code indicates the
*          end of the flow table has been reached
*
* @param    flow    @b{(input)} structure containing flow entry parameters
* @param    nextFlow @b{(output)} structure containing the next flow table
*                                 entry, if any
*
* @returns  OFDPA_E_NONE  the next flow entry was found and returned
* @returns  OFDPA_E_PARAM error in flow entry parameters passed to function
* @returns  OFDPA_E_NOT_FOUND  no next flow entry found
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaFlowNextGet(ofdpaFlowEntry_t *flow, ofdpaFlowEntry_t *nextFlow);

/*********************************************************************
* @purpose Returns statistics for the flow entry specified.
*
* @param    flow     @b{(input)} structure containing flow entry parameters
* @param    flowStats @b{(output)} structure to store flow entry statistics
*
* @returns  OFDPA_E_NONE  flow entry was found and statistics returned
* @returns  OFDPA_E_PARAM error in flow entry parameters passed to function
* @returns  OFDPA_E_NOT_FOUND  no matching flow entry found
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaFlowStatsGet(ofdpaFlowEntry_t *flow, ofdpaFlowEntryStats_t *flowStats);

/*********************************************************************
* @purpose Returns statistics for the flow entry with the specified cookie value.
*
* @param    cookie @b{(input)} cookie for the flow entry
* @param    flow    @b{(output)} structure to store flow entry parameters
* @param    flowflowStats @b{(output)} structure to store flow entry statistics
*
* @returns  OFDPA_E_NONE  flow entry was found and statistics returned
* @returns  OFDPA_E_PARAM error in flow entry parameters passed to function
* @returns  OFDPA_E_NOT_FOUND  no flow entry with matching cookie found
* @returns  OFDPA_E_FAIL the operation failed to complete
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaFlowByCookieGet(uint64_t cookie, ofdpaFlowEntry_t *flow, ofdpaFlowEntryStats_t *flowStats);

/*********************************************************************
* @purpose  Delete the flow entry with the specified cookie.
*
* @param    cookie @b{(input)} cookie for the flow entry
*
* @returns  OFDPA_E_NONE  flow entry deleted successfully
* @returns  OFDPA_E_FAIL  error while deleting flow entry, including
*                         failure to find matching flow entry
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaFlowByCookieDelete(uint64_t cookie);

/*------------------------------------------------------------------------------------*/
/* group table APIs */

/*
   The Group Table contains one entry for each Group.  The table is indexed
   by the groupId which identifies the group entry.  Data is encoded into the groupId to specify the OF-DPA
   group entry type and information required by OF-DPA to configure the datapath.

   The groupId encoding method is:

   L2 Interface, L2 Rewrite types:
        (MSB to LSB) 4 bits encode the Group Table Entry type |12 bits of VLAN ID | 16 bits of port identifier

   L2 Multicast, L2 Flood, L3 Multicast, and L3 Interface types:
        (MSB to LSB) 4 bits encode the Group Table Entry type |12 bits of VLAN ID | 16 bits of index

   L3 Unicast and L3 ECMP types:
        (MSB to LSB) 4 bits encode the Group Table Entry type | 28 bits of index

*/

/* APIs for getting/setting group IDs -- set APIs must set the type first. */

/*********************************************************************
* @purpose  Get the Group Type encoded in the Group ID.
*
* @param    groupId    @b{(input)} group id
* @param    type       @b{(output)} group type
*
* @returns  OFDPA_E_NONE  Group Id returned successfully
*
* @note     Applicable for all group types
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaGroupTypeGet(uint32_t groupId, uint32_t *type);

/*********************************************************************
* @purpose  Get the VLAN ID encoded in the Group Id.
*
* @param    groupId    @b{(input)} group id
* @param    vlanId     @b{(output)} VLAN Id
*
* @returns  OFDPA_E_NONE  VLAN Id returned successfully
* @returns  OFDPA_E_UNAVAIL  VLAN Id not available
*
* @note     Applicable for L2 Interface, L2 Multicast, L2 Flood,
*           and L3 Multicast group types only
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaGroupVlanGet(uint32_t groupId, uint32_t *vlanId);

/*********************************************************************
* @purpose  Get the Port ID encoded in the Group Id.
*
* @param    groupId    @b{(input)} group id
* @param    portId     @b{(output)} Port Id
*
* @returns  OFDPA_E_NONE  Port Id returned successfully
* @returns  OFDPA_E_UNAVAIL  Port Id not available
*
* @note     Applicable for L2 Interface group types only
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaGroupPortIdGet(uint32_t groupId, uint32_t *portId);

/*********************************************************************
* @purpose  Get the Short Index encoded in the Group Id.
*
* @param    groupId    @b{(input)} group id
* @param    index     @b{(output)} Index
*
* @returns  OFDPA_E_NONE  Index returned successfully
* @returns  OFDPA_E_UNAVAIL  Index not available
*
* @note     Applicable for L2 Multicastl L2 Flood and L3 Multicast group types only
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaGroupIndexShortGet(uint32_t groupId, uint32_t *index);

/*********************************************************************
* @purpose  Get the Index encoded in the Group Id.
*
* @param    groupId    @b{(input)} group id
* @param    index      @b{(output)} Index
*
* @returns  OFDPA_E_NONE  Index returned successfully
* @returns  OFDPA_E_UNAVAIL  Index not available
*
* @note     Applicable Group Types L2 Rewrite, L3 Unicast, L3 Interface, L3 ECMP
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaGroupIndexGet(uint32_t groupId, uint32_t *index);

/*********************************************************************
* @purpose  Encode the Group Type in the Group Id.
*
* @param    groupId    @b{(inout)} group id
* @param    type       @b{(input)} group type
*
* @returns  OFDPA_E_NONE  Encoded successfully
*
* @note     Applicable for all group types
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaGroupTypeSet(uint32_t *groupId, uint32_t type);

/*********************************************************************
* @purpose  Encode the VLAN Id in the Group Id.
*
* @param    groupId    @b{(inout)} group id
* @param    vlanId     @b{(input)} VLAN Id
*
* @returns  OFDPA_E_NONE  Encoded successfully
* @returns  OFDPA_E_UNAVAIL  VLAN Id field not available in Group Id
*
* @note     Applicable Group Types L2 Interface, L2 Multicast, L2 Flood
*           and L3 Multicast
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaGroupVlanSet(uint32_t *groupId, uint32_t vlanId);

/*********************************************************************
* @purpose  Encode the tunnel Id in the Group Id.
*
* @param    groupId    @b{(inout)} group id
* @param    tunnelId   @b{(input)} tunnel Id
*
* @returns  OFDPA_E_NONE  Encoded successfully
* @returns  OFDPA_E_UNAVAIL  tunnel Id field not available in Group Id
*
* @note     Applicable Group Types L2 Overlay
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaGroupOverlayTunnelIdSet(uint32_t *groupId, uint32_t tunnelId);

/*********************************************************************
* @purpose  Encode the Overlay Group Sub-type in the Group Id.
*
* @param    groupId    @b{(inout)} group id
* @param    subType    @b{(input)} sub-type
*
* @returns  OFDPA_E_NONE  Encoded successfully
* @returns  OFDPA_E_UNAVAIL  Sub-type field not available in Group Id
*
* @note     Applicable Group Types L2 Overlay
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaGroupOverlaySubTypeSet(uint32_t *groupId, OFDPA_L2_OVERLAY_SUBTYPE_t subType);

/*********************************************************************
* @purpose  Encode the Overlay Group index in the Group Id.
*
* @param    groupId    @b{(inout)} group id
* @param    index      @b{(input)} index
*
* @returns  OFDPA_E_NONE  Encoded successfully
* @returns  OFDPA_E_UNAVAIL  Sub-type field not available in Group Id
*
* @note     Applicable Group Types L2 Overlay
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaGroupOverlayIndexSet(uint32_t *groupId, uint32_t index);

/*********************************************************************
* @purpose  Encode the Port Id in the Group Id.
*
* @param    groupId    @b{(inout)} group id
* @param    portId     @b{(input)} Port Id
*
* @returns  OFDPA_E_NONE  Encoded successfully
* @returns  OFDPA_E_UNAVAIL  Port Id field not available in Group Id
*
* @note     Applicable Group Types L2 Interface only
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaGroupPortIdSet(uint32_t *groupId, uint32_t portId);

/*********************************************************************
* @purpose  Encode the Short Index in the Group Id.
*
* @param    groupId    @b{(inout)} group id
* @param    index      @b{(input)} Index
*
* @returns  OFDPA_E_NONE  Encoded successfully
* @returns  OFDPA_E_UNAVAIL  Short Index field not available in Group Id
*
* @note     Applicable for L2 Multicastl L2 Flood and L3 Multicast group types only
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaGroupIndexShortSet(uint32_t *groupId, uint32_t index);

/*********************************************************************
* @purpose  Encode the Index in the Group Id.
*
* @param    groupId    @b{(inout)} group id
* @param    index      @b{(input)} Index
*
* @returns  OFDPA_E_NONE  Encoded successfully
* @returns  OFDPA_E_UNAVAIL  Index field not available in Group Id
*
* @note     Applicable Group Types L2 Rewrite, L3 Unicast, L3 Interface, L3 ECMP
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaGroupIndexSet(uint32_t *groupId, uint32_t index);

/*********************************************************************
* @purpose  Decode Group Id.
*
* @param    groupId    @b{(input)} group id
* @param    outBuf     @b{(output)} group id decoded
* @param    bufSize    @b{(input)} size of outBuf
*
* @returns  OFDPA_E_NONE  Group Id Decoded successfully
* @returns  OFDPA_E_FULL  bufSize not sufficient to hold the decoded Group Id
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaGroupDecode(uint32_t groupId, char *outBuf, int bufSize);

/*********************************************************************
* @purpose  Add Group.
*
* @param    group    @b{(input)} group entry
*
* @returns  OFDPA_E_NONE  Group added successfully
* @returns  OFDPA_E_PARAM error in parameters passed to function
* @returns  OFDPA_E_FULL  Group table full
* @returns  OFDPA_E_EXISTS  Group entry exists
* @returns  OFDPA_E_NOT_FOUND  port encoded in Group type L2
                               Interface not found
* @returns  OFDPA_E_INTERNAL  internal error
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaGroupAdd(ofdpaGroupEntry_t *group);

/*********************************************************************
* @purpose  Delete Group.
*
* @param    groupId    @b{(input)} group id
*
* @returns  OFDPA_E_NONE  Group deleted successfully
* @returns  OFDPA_E_PARAM error in parameters passed to function
* @returns  OFDPA_E_NOT_FOUND  group entry not found
* @returns  OFDPA_E_FAIL  failure; other errors
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaGroupDelete(uint32_t groupId);

/*********************************************************************
* @purpose  Get the next Group entry.
*
* @param    groupId    @b{(input)} group id
* @param    nextGroup  @b{(output)} next group entry
*
* @returns  OFDPA_E_NONE  next group entry returned successfully
* @returns  OFDPA_E_PARAM error in parameters passed to function
* @returns  OFDPA_E_FAIL  next group does not exist
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaGroupNextGet(uint32_t groupId, ofdpaGroupEntry_t *nextGroup);

/*********************************************************************
* @purpose  Get the next Group entry of a given type.
*
* @param    groupId    @b{(input)} group id
* @param    groupType  @b{(input)} group type
* @param    nextGroup  @b{(output)} next group entry
*
* @returns  OFDPA_E_NONE  next group entry returned successfully
* @returns  OFDPA_E_PARAM error in parameters passed to function
* @returns  OFDPA_E_FAIL  next group does not exist
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaGroupTypeNextGet(uint32_t groupId,
                                    OFDPA_GROUP_ENTRY_TYPE_t groupType,
                                    ofdpaGroupEntry_t *nextGroup);

/*********************************************************************
* @purpose  Get Group statistics.
*
* @param    groupId    @b{(input)} group id
* @param    groupStats @b{(output)} group statistics
*
* @returns  OFDPA_E_NONE  group statistics returned successfully
* @returns  OFDPA_E_PARAM error in parameters passed to function
* @returns  OFDPA_E_NOT_FOUND  group not found
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaGroupStatsGet(uint32_t groupId, ofdpaGroupEntryStats_t *groupStats);

/*
   Group Table entries contain one or more Action Buckets depending on their type.
   The Group Bucket Table stores these references.  It is indexed by groupId and referenceGroupId.  The presence
   of an entry in this table creates a referral by the Group Table entry specified in groupId to the Group Table
   entry specified in referenceGroupId.  Restrictions on the number of references and the allowable type of the
   referenced Group Table entries varies by entry type.
*/

/*********************************************************************
* @purpose  Add a Group Bucket entry.
*
* @param    bucket    @b{(input)} group bucket entry
*
* @returns  OFDPA_E_NONE        group bucket entry added successfully
* @returns  OFDPA_E_PARAM       error in parameters passed to function
* @returns  OFDPA_E_FULL        group bucket table full
* @returns  OFDPA_E_NOT_FOUND   group not found
* @returns  OFDPA_E_EXISTS      group bucket entry exists
* @returns  OFDPA_E_INTERNAL    internal errors or failures
* @returns  OFDPA_E_FAIL        internal errors or failures
*
* @notes For group table entries allowed to have multiple buckets,
*        this is called more than once
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaGroupBucketEntryAdd(ofdpaGroupBucketEntry_t *bucket);

/*********************************************************************
* @purpose  Delete a Group Bucket entry.
*
* @param    groupId      @b{(input)} group id
* @param    bucketIndex  @b{(input)} group bucket index
*
* @returns  OFDPA_E_NONE        group bucket entry deleted successfully
* @returns  OFDPA_E_PARAM       error in parameters passed to function
* @returns  OFDPA_E_NOT_FOUND   group bucket entry not found
* @returns  OFDPA_E_EXISTS      group reference exists
* @returns  OFDPA_E_INTERNAL    internal errors or failures
* @returns  OFDPA_E_FAIL        internal errors or failures
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaGroupBucketEntryDelete(uint32_t groupId, uint32_t bucketIndex);

/*********************************************************************
* @purpose  Delete all Buckets from a Group.
*
* @param    groupId      @b{(input)} group id
*
* @returns  OFDPA_E_NONE        group bucket entry deleted successfully
* @returns  OFDPA_E_PARAM       error in parameters passed to function
* @returns  OFDPA_E_EXISTS      group reference exists
* @returns  OFDPA_E_INTERNAL    internal errors or failures
* @returns  OFDPA_E_FAIL        internal errors or failures
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaGroupBucketsDeleteAll(uint32_t groupId);

/*********************************************************************
* @purpose  Get the Group Bucket entry.
*
* @param    groupId      @b{(input)} group id
* @param    bucketIndex  @b{(input)} bucket index
* @param    groupBucket  @b{(output)} group bucket entry
*
* @returns  OFDPA_E_NONE        group bucket entry returned successfully
* @returns  OFDPA_E_PARAM       error in parameters passed to function
* @returns  OFDPA_E_FAIL        group bucket entry does not exist
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaGroupBucketEntryGet(uint32_t groupId, uint32_t bucketIndex,
                                       ofdpaGroupBucketEntry_t *groupBucket);

/*********************************************************************
* @purpose  Get the first Group Bucket entry.
*
* @param    groupId      @b{(input)} group id
* @param    firstGroupBucket  @b{(output)} first group bucket entry
*
* @returns  OFDPA_E_NONE        group bucket entry returned successfully
* @returns  OFDPA_E_PARAM       error in parameters passed to function
* @returns  OFDPA_E_FAIL        group bucket entry does not exist
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaGroupBucketEntryFirstGet(uint32_t groupId,
                                            ofdpaGroupBucketEntry_t *firstGroupBucket);

/*********************************************************************
* @purpose  Get the next Group Bucket entry.
*
* @param    groupId      @b{(input)} group id
* @param    bucketIndex  @b{(input)} bucket index
* @param    nextBucketEntry  @b{(output)} next group bucket entry
*
* @returns  OFDPA_E_NONE        next group bucket entry returned successfully
* @returns  OFDPA_E_PARAM       error in parameters passed to function
* @returns  OFDPA_E_FAIL        next group bucket entry does not exist
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaGroupBucketEntryNextGet(uint32_t groupId, uint32_t bucketIndex,
                                           ofdpaGroupBucketEntry_t *nextBucketEntry);

/*********************************************************************
* @purpose  Modify Group Bucket entry.
*
* @param    bucket      @b{(input)} group bucket entry to be modified
*
* @returns  OFDPA_E_NONE        group bucket entry modified successfully
* @returns  OFDPA_E_PARAM       error in parameters passed to function
* @returns  OFDPA_E_NOT_FOUND   group bucket entry not found
* @returns  OFDPA_E_INTERNAL    internal errors or failures
* @returns  OFDPA_E_FAIL        internal errors or failures
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaGroupBucketEntryModify(ofdpaGroupBucketEntry_t *bucket);

/*********************************************************************
* @purpose  Get Group table info.
*
* @param    groupType      @b{(input)} group type
* @param    info           @b{(input)} info
*
* @returns  OFDPA_E_NONE        group info returned successfully
* @returns  OFDPA_E_PARAM       error in parameters passed to function
* @returns  OFDPA_E_NOT_FOUND   group type not found
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaGroupTableInfoGet(OFDPA_GROUP_ENTRY_TYPE_t groupType, ofdpaGroupTableInfo_t *info);

/*********************************************************************
* @purpose  Get the port type encoded in a port number value.
*
* @param    portNum    @b{(input)} port number value
* @param    type       @b{(output)} port type
*
* @returns  nothing
*
* @end
*********************************************************************/
void ofdpaPortTypeGet(uint32_t portNum, uint32_t *type);

/*********************************************************************
* @purpose  Encode the port type in a port number value
*
* @param    portNum    @b{(inout)} port number storage
* @param    type       @b{(input)} port type
*
* @returns  nothing
*
* @end
*********************************************************************/
void ofdpaPortTypeSet(uint32_t *portNum, uint32_t type);

/*********************************************************************
* @purpose  Get the port index encoded in a port number value.
*
* @param    portNum     @b{(input)} port number value
* @param    index       @b{(output)} port index
*
* @returns  nothing
*
* @end
*********************************************************************/
void ofdpaPortIndexGet(uint32_t portNum, uint32_t *index);

/*********************************************************************
* @purpose  Encode the port index in a port number
*
* @param    portNum     @b{(inout)} port number storage
* @param    index       @b{(input)} port index
*
* @returns  nothing
*
* @end
*********************************************************************/
void ofdpaPortIndexSet(uint32_t *portNum, uint32_t index);

/*********************************************************************
* @purpose  Get the next port from the port table
*
* @param    portNum @b{(input)} current port
* @param    nextPortNum @b{(output)} next port
*
* @returns  OFDPA_E_NONE  next port returned successfully
* @returns  OFDPA_E_PARAM error in parameters passed to function
* @returns  OFDPA_E_FAIL  no more ports to be returned
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaPortNextGet(uint32_t portNum, uint32_t *nextPortNum);

/*********************************************************************
* @purpose  Get the MAC address of the given port
*
* @param    portNum @b{(input)} current port
* @param    nextPortNum @b{(output)} next port
*
* @returns  OFDPA_E_NONE  MAC address returned successfully
* @returns  OFDPA_E_PARAM error in parameters passed to function
* @returns  OFDPA_E_NOT_FOUND  port not found
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaPortMacGet(uint32_t portNum, ofdpaMacAddr_t *mac);

/*********************************************************************
* @purpose  Get the port name
*
* @param    portNum @b{(input)} port number
* @param    name @b{(output)} port name
*
* @returns  OFDPA_E_NONE  port name returned successfully
* @returns  OFDPA_E_PARAM error in parameters passed to function
* @returns  OFDPA_E_NOT_FOUND  port not found
*
* @note     Initialize the ofdpa_buffdesc with the name buffer and size
*           appropriately. The size of the name buffer (including '\0')
*           should be at least equal to OFDPA_MAX_PORT_NAME_STRING_SIZE
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaPortNameGet(uint32_t portNum, ofdpa_buffdesc *name);

/*********************************************************************
* @purpose  Get the port link state
*
* @param    portNum @b{(input)} port number
* @param    state @b{(output)} port link state
*
* @returns  OFDPA_E_NONE  port link state returned successfully
* @returns  OFDPA_E_PARAM error in parameters passed to function
* @returns  OFDPA_E_NOT_FOUND  port not found
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaPortStateGet(uint32_t  portNum, OFDPA_PORT_STATE_t  *state);

/*********************************************************************
* @purpose  Set the port administrative state
*
* @param    portNum @b{(input)} port number
* @param    state @b{(input)} port administrative state
*
* @returns  OFDPA_E_NONE  port admin state set successfully
* @returns  OFDPA_E_PARAM error in parameters passed to function
* @returns  OFDPA_E_NOT_FOUND  port not found
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaPortConfigSet(uint32_t portNum, OFDPA_PORT_CONFIG_t config);

/*********************************************************************
* @purpose  Get the port administrative state
*
* @param    portNum @b{(input)} port number
* @param    state @b{(output)} port administrative state
*
* @returns  OFDPA_E_NONE  port admin state returned successfully
* @returns  OFDPA_E_PARAM error in parameters passed to function
* @returns  OFDPA_E_NOT_FOUND  port not found
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaPortConfigGet(uint32_t portNum, OFDPA_PORT_CONFIG_t  *config);

/*********************************************************************
* @purpose  Get the port maximum speed
*
* @param    portNum @b{(input)} port number
* @param    maxSpeed @b{(output)} port maximum speed
*
* @returns  OFDPA_E_NONE  port max speed returned successfully
* @returns  OFDPA_E_PARAM error in parameters passed to function
* @returns  OFDPA_E_NOT_FOUND  port not found
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaPortMaxSpeedGet(uint32_t portNum, uint32_t  *maxSpeed);

/*********************************************************************
* @purpose  Get the port current speed
*
* @param    portNum @b{(input)} port number
* @param    currSpeed @b{(output)} port current speed
*
* @returns  OFDPA_E_NONE  port current speed returned successfully
* @returns  OFDPA_E_PARAM error in parameters passed to function
* @returns  OFDPA_E_NOT_FOUND  port not found
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaPortCurrSpeedGet(uint32_t portNum, uint32_t  *currSpeed);

/*********************************************************************
* @purpose  Get the port features
*
* @param    portNum @b{(input)} port number
* @param    feature @b{(output)} port features
*
* @returns  OFDPA_E_NONE  port features returned successfully
* @returns  OFDPA_E_PARAM error in parameters passed to function
* @returns  OFDPA_E_NOT_FOUND  port not found
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaPortFeatureGet(uint32_t portNum, ofdpaPortFeature_t *feature);

/*********************************************************************
* @purpose  Set the port features that can be advertized durng Auto-Negotiation
*
* @param    portNum @b{(input)} port number
* @param    feature @b{(input)} port features
*
* @returns  OFDPA_E_NONE  port features set successfully
* @returns  OFDPA_E_PARAM error in parameters passed to function
* @returns  OFDPA_E_NOT_FOUND  port not found
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaPortAdvertiseFeatureSet(uint32_t portNum, uint32_t advertise);

/*********************************************************************
* @purpose  Clear port statistics
*
* @param    portNum @b{(input)} port number
*
* @returns  OFDPA_E_NONE  port statistics cleared successfully
* @returns  OFDPA_E_PARAM error in parameters passed to function
* @returns  OFDPA_E_NOT_FOUND  port not found
* @returns  OFDPA_E_INTERNAL  internal error
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaPortStatsClear(uint32_t portNum);

/*********************************************************************
* @purpose  Get port statistics
*
* @param    portNum @b{(input)} port number
* @param    stats @b{(output)} port statistics
*
* @returns  OFDPA_E_NONE  port statistics returned successfully
* @returns  OFDPA_E_PARAM error in parameters passed to function
* @returns  OFDPA_E_NOT_FOUND  port not found
* @returns  OFDPA_E_UNAVAIL   counter not available
* @returns  OFDPA_E_INTERNAL  internal error
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaPortStatsGet(uint32_t portNum, ofdpaPortStats_t *stats);

/*********************************************************************
* @purpose  Create a tunnel logical port.
*
* @param    portNum  @b{(input)} unique identifier for the port within the switch
* @param    name     @b{(input)} human readable name for the interface
* @param    config   @b{(input)} structure containing tunnel logical port parameters
*
* @returns  OFDPA_E_NONE  logical port successfully created
* @returns  OFDPA_E_PARAM error in parameters passed to function
* @returns  OFDPA_E_ERROR internal error
* @returns  OFDPA_E_FAIL  failure occurred during logical port creation
* @returns  OFDPA_E_FULL  maximum number of logical ports already created
* @returns  OFDPA_E_EXISTS an entry with the specified portnum already created
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaTunnelPortCreate(uint32_t portNum, ofdpa_buffdesc *name, ofdpaTunnelPortConfig_t *config);

/*********************************************************************
* @purpose  Delete a tunnel logical port.
*
* @param    portNum  @b{(input)} unique identifier for the port within the switch
*
* @returns  OFDPA_E_NONE  logical port successfully deleted
* @returns  OFDPA_E_PARAM error in parameters passed to function
* @returns  OFDPA_E_NOT_FOUND logical port with identifier not configured
* @returns  OFDPA_E_FAIL  failure occurred during logical port deletion
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaTunnelPortDelete(uint32_t portNum);

/*********************************************************************
* @purpose  Retrieve tunnel logical port configuration and/or status.
*
* @param    portNum  @b{(input)} unique identifier for the port within the switch
* @param    config   @b{(output)} structure containing tunnel logical port configuration (optional)
* @param    status   @b{(output)} structure containing tunnel logical port status (optional)
*
* @returns  OFDPA_E_NONE  logical port data successfully retrieved
* @returns  OFDPA_E_PARAM error in parameters passed to function
* @returns  OFDPA_E_NOT_FOUND logical port with identifier not configured
*
* @notes If the caller does not require the data in the configuration or
*        status structure, either or both parameters may be set to NULL.
*        An example of this type of invocation is if the caller is checking
*        on presence of a logical port, but has no need for the configuration or
*        status data.
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaTunnelPortGet(uint32_t portNum,
                                 ofdpaTunnelPortConfig_t *config,
                                 ofdpaTunnelPortStatus_t *status);

/*********************************************************************
* @purpose  Return the tunnel logical port identifier, if any, for the
*           logical port after the one specified in the logical port table.
*
* @param    portNum  @b{(input)} unique identifier for the port within the switch
* @param    nextPortNum  @b{(output)} unique identifier for the next logical port within the switch
*
* @returns  OFDPA_E_NONE  next logical port identifer found
* @returns  OFDPA_E_FAIL  no next logical port identifer found
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaTunnelPortNextGet(uint32_t portNum, uint32_t *nextPortNum);

/*********************************************************************
* @purpose  Adds a tenant reference to a logical port tunnel entry. This
*           is done by adding any entry to the tunnel port tenant table.
*           The index into this table is the portNum, tunnelId tuple.
*
* @param    portNum   @b{(input)} unique identifier for the port within the switch
* @param    tunnelId  @b{(input)} tenant entry identifier
*
* @returns  OFDPA_E_NONE  logical port to tenant association successfully added
* @returns  OFDPA_E_PARAM error in parameters passed to function
* @returns  OFDPA_E_ERROR configuration error, includes logical port or tenant
*                         entry not found or other constraint violation
* @returns  OFDPA_E_FAIL  failure occurred during logical port association with tenant
* @returns  OFDPA_E_FULL  maximum number of associations already created
* @returns  OFDPA_E_EXISTS an entry with the specified portnum and tunnelId already added
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaTunnelPortTenantAdd(uint32_t portNum, uint32_t tunnelId);

/*********************************************************************
* @purpose  Deletes a tenant reference to a logical port tunnel entry.
*
* @param    portNum   @b{(input)} unique identifier for the port within the switch
* @param    tunnelId  @b{(input)} tenant entry identifier
*
* @returns  OFDPA_E_NONE  logical port to tenant association successfully deleted
* @returns  OFDPA_E_FAIL  failure occurred deleting logical port association with tenant,
*                         including the entry not being found
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaTunnelPortTenantDelete(uint32_t portNum, uint32_t tunnelId);

/*********************************************************************
* @purpose  Test whether a tenant reference to a logical port exists for
*           the port and tenant combination.
*           Optionally retrieve status of entry.
*
* @param    portNum   @b{(input)} unique identifier for the port within the switch
* @param    tunnelId  @b{(input)} tenant entry identifier
* @param    status    @b{(output)} structure containing tunnel logical port/tenant status (optional)
*
* @returns  OFDPA_E_NONE  logical port to tenant association successfully deleted
* @returns  OFDPA_E_NOT_FOUND logical port to tenant association is not configured
*
* @notes If the caller does not require the data in
*        status structure, this parameter may be set to NULL.
*        An example of this type of invocation is if the caller is checking
*        on presence of an entry for the logical port and tenant, but has no need for the
*        status data.
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaTunnelPortTenantGet(uint32_t portNum, uint32_t tunnelId, ofdpaTunnelPortTenantStatus_t *status);

/*********************************************************************
* @purpose  Gets the next logical port tunnel entry after the port and tenant identified.
*           Iteration stops after returning the tunnelId for the last tenant
*           associated with the logical port.
*
* @param    portNum       @b{(input)} unique identifier for the port within the switch
* @param    tunnelId      @b{(input)} tenant entry identifier
* @param    nextTunnelId  @b{(output)} next tenant entry identifier, if any
*
* @returns  OFDPA_E_NONE       next logical port tunnel identifer found
* @returns  OFDPA_E_PARAM      error in parameters passed to function
* @returns  OFDPA_E_NOT_FOUND  no next logical port tunnel identifer found
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaTunnelPortTenantNextGet(uint32_t portNum, uint32_t tunnelId, uint32_t *nextTunnelId);

/*********************************************************************
* @purpose  Create a tunnel tenant entry.
*
* @param    tunnelId  @b{(input)} unique identifier for the tenant
* @param    config   @b{(input)} structure containing tenant parameters
*
* @returns  OFDPA_E_NONE  tenant entry successfully created
* @returns  OFDPA_E_PARAM error in parameters passed to function
* @returns  OFDPA_E_ERROR configuration error including specifying a
*                         multicast next hop entry reference that does not exist
* @returns  OFDPA_E_FAIL  failure occurred during tenant creation
* @returns  OFDPA_E_FULL  maximum number of tenants already created
* @returns  OFDPA_E_EXISTS an entry with the specified tunnelId already created
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaTunnelTenantCreate(uint32_t tunnelId, ofdpaTunnelTenantConfig_t *config);

/*********************************************************************
* @purpose  Delete a tunnel tenant entry.
*
* @param    tunnelId  @b{(input)} unique identifier for the tenant
*
* @returns  OFDPA_E_NONE  tenant entry successfully deleted
* @returns  OFDPA_E_PARAM error in parameters passed to function
* @returns  OFDPA_E_NOT_FOUND tenant entry with identifier not configured
* @returns  OFDPA_E_FAIL  failure occurred during tenant deletion,
*                         reasons include attempting to delete a tenant entry
*                         that is being referenced
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaTunnelTenantDelete(uint32_t tunnelId);

/*********************************************************************
* @purpose  Retrieve tunnel tenant configuration and/or status.
*
* @param    tunnelId  @b{(input)} unique identifier for the tenant
* @param    config   @b{(output)} structure containing tunnel tenant configuration (optional)
* @param    status   @b{(output)} structure containing tunnel tenant status (optional)
*
* @returns  OFDPA_E_NONE  data successfully retrieved
* @returns  OFDPA_E_PARAM error in parameters passed to function
* @returns  OFDPA_E_NOT_FOUND entry with identifier not configured
*
* @notes If the caller does not require the data in the configuration or
*        status structure, either or both parameters may be set to NULL.
*        An example of this type of invocation is if the caller is checking
*        on presence of the entry, but has no need for the configuration or
*        status data.
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaTunnelTenantGet(uint32_t tunnelId,
                                   ofdpaTunnelTenantConfig_t *config,
                                   ofdpaTunnelTenantStatus_t *status);

/*********************************************************************
* @purpose  Return the tunnel identifier, if any, for the
*           entry after the one specified.
*
* @param    tunnelId  @b{(input)} unique identifier for the tenant
* @param    nextTunnelId  @b{(output)} unique identifier for the next tenant entry
*
* @returns  OFDPA_E_NONE  next identifer found
* @returns  OFDPA_E_FAIL  no next identifer found
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaTunnelTenantNextGet(uint32_t tunnelId, uint32_t *nextTunnelId);

/*********************************************************************
* @purpose  Create a tunnel next hop entry.
*
* @param    nextHopId  @b{(input)} unique identifier for the entry
* @param    config   @b{(input)} structure containing configuration parameters
*
* @returns  OFDPA_E_NONE  entry successfully created
* @returns  OFDPA_E_PARAM error in parameters passed to function
* @returns  OFDPA_E_ERROR configuration error
* @returns  OFDPA_E_FAIL  failure occurred during entry creation
* @returns  OFDPA_E_FULL  maximum number of entries already created
* @returns  OFDPA_E_EXISTS an entry with the specified identifier already created
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaTunnelNextHopCreate(uint32_t nextHopId, ofdpaTunnelNextHopConfig_t *config);

/*********************************************************************
* @purpose  Delete a tunnel next hop entry.
*
* @param    nextHopId  @b{(input)} unique identifier for the entry
*
* @returns  OFDPA_E_NONE  entry successfully deleted
* @returns  OFDPA_E_PARAM error in parameters passed to function
* @returns  OFDPA_E_NOT_FOUND entry with identifier not configured
* @returns  OFDPA_E_FAIL  failure occurred during deletion,
*                         reasons include attempting to delete an entry
*                         that is being referenced
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaTunnelNextHopDelete(uint32_t nextHopId);

/*********************************************************************
* @purpose  Modify a tunnel next hop entry. This allows updating the configuration
*           data of a next hop while it is referenced by other configuration.
*           A referenced entry cannot be deleted but it can be modified.
*
* @param    nextHopId  @b{(input)} unique identifier for an existing entry
* @param    config   @b{(input)} structure containing new configuration parameters
*
* @returns  OFDPA_E_NONE  entry successfully modified
* @returns  OFDPA_E_PARAM error in parameters passed to function
* @returns  OFDPA_E_ERROR configuration error, including attempting to modify next hop
*                         in a manner not allowed.  See notes.
* @returns  OFDPA_E_FAIL  failure occurred during entry modification
* @returns  OFDPA_E_NOT_FOUND  entry with identifier not configured
*
* @notes Modification cannot change the destination MAC address from unicast to multicast or
*        multicast to unicast.  Modification cannot change the protocol of the next hop entry.
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaTunnelNextHopModify(uint32_t nextHopId, ofdpaTunnelNextHopConfig_t *config);

/*********************************************************************
* @purpose  Retrieve tunnel next hop configuration and/or status.
*
* @param    tunnelId  @b{(input)} unique identifier for the entry
* @param    config   @b{(output)} structure containing tunnel next hop configuration (optional)
* @param    status   @b{(output)} structure containing tunnel next hop status (optional)
*
* @returns  OFDPA_E_NONE  data successfully retrieved
* @returns  OFDPA_E_PARAM error in parameters passed to function
* @returns  OFDPA_E_NOT_FOUND entry with identifier not configured
*
* @notes If the caller does not require the data in the configuration or
*        status structure, either or both parameters may be set to NULL.
*        An example of this type of invocation is if the caller is checking
*        on presence of the entry, but has no need for the configuration or
*        status data.
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaTunnelNextHopGet(uint32_t nextHopId,
                                    ofdpaTunnelNextHopConfig_t *config,
                                    ofdpaTunnelNextHopStatus_t *status);

/*********************************************************************
* @purpose  Return the tunnel next hop identifier, if any, for the
*           entry after the one specified.
*
* @param    nextHopId  @b{(input)} unique identifier for the next hop
* @param    nextNextHopId  @b{(output)} unique identifier for the next next hop entry
*
* @returns  OFDPA_E_NONE  next identifer found
* @returns  OFDPA_E_FAIL  no next identifer found
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaTunnelNextHopNextGet(uint32_t nextHopId, uint32_t *nextNextHopId);

/*********************************************************************
* @purpose  Create a tunnel ECMP next hop group entry.
*
* @param    ecmpNextHopGroupId  @b{(input)} unique identifier for the entry
* @param    config   @b{(input)} structure containing configuration parameters
*
* @returns  OFDPA_E_NONE  entry successfully created
* @returns  OFDPA_E_PARAM error in parameters passed to function
* @returns  OFDPA_E_ERROR configuration error
* @returns  OFDPA_E_FAIL  failure occurred during entry creation
* @returns  OFDPA_E_FULL  maximum number of entries already created
* @returns  OFDPA_E_EXISTS an entry with the specified identifier already created
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaTunnelEcmpNextHopGroupCreate(uint32_t ecmpNextHopGroupId, ofdpaTunnelEcmpNextHopGroupConfig_t *config);

/*********************************************************************
* @purpose  Delete a tunnel ECMP next hop group entry.
*
* @param    ecmpNextHopGroupId  @b{(input)} unique identifier for the entry
*
* @returns  OFDPA_E_NONE  entry successfully deleted
* @returns  OFDPA_E_PARAM error in parameters passed to function
* @returns  OFDPA_E_NOT_FOUND entry with identifier not configured
* @returns  OFDPA_E_FAIL  failure occurred during deletion,
*                         reasons include attempting to delete an entry
*                         that is being referenced
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaTunnelEcmpNextHopGroupDelete(uint32_t ecmpNextHopGroupId);

/*********************************************************************
* @purpose  Retrieve tunnel ECMP next hop group configuration and/or status.
*
* @param    ecmpNextHopGroupId  @b{(input)} unique identifier for the entry
* @param    config   @b{(output)} structure containing ECMP tunnel next hop configuration (optional)
* @param    status   @b{(output)} structure containing ECMP tunnel next hop status (optional)
*
* @returns  OFDPA_E_NONE  data successfully retrieved
* @returns  OFDPA_E_PARAM error in parameters passed to function
* @returns  OFDPA_E_NOT_FOUND entry with identifier not configured
*
* @notes If the caller does not require the data in the configuration or
*        status structure, either or both parameters may be set to NULL.
*        An example of this type of invocation is if the caller is checking
*        on presence of the entry, but has no need for the configuration or
*        status data.
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaTunnelEcmpNextHopGroupGet(uint32_t ecmpNextHopGroupId,
                                             ofdpaTunnelEcmpNextHopGroupConfig_t *config,
                                             ofdpaTunnelEcmpNextHopGroupStatus_t *status);

/*********************************************************************
* @purpose  Return the ECMP tunnel next hop group identifier, if any, for the
*           entry after the one specified.
*
* @param    ecmpNextHopGroupId  @b{(input)} unique identifier for the entry
* @param    nextEcmpNextHopGroupId  @b{(output)} unique identifier for the next entry
*
* @returns  OFDPA_E_NONE  next identifer found
* @returns  OFDPA_E_FAIL  no next identifer found
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaTunnelEcmpNextHopGroupNextGet(uint32_t ecmpNextHopGroupId, uint32_t *nextEcmpNextHopGroupId);

/*********************************************************************
* @purpose  Return the number of ECMP next hop group members supported by the
*           switch.
*
* @param    maxMemberCount  @b{(output)} maximum members supported in an ECMP next hop group
*
* @returns  OFDPA_E_NONE  max count returned
* @returns  OFDPA_E_PARAM NULL pointer passed
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaTunnelEcmpNextHopGroupMaxMembersGet(uint32_t *maxMemberCount);

/*********************************************************************
* @purpose  Adds a next hop member to an ECMP next hop group entry. This
*           is done by adding any entry to the ECMP next hop group member table.
*           The index into this table is the ecmpNextHopGroupId, nextHopId tuple.
*
* @param    ecmpNextHopGroupId   @b{(input)} ECMP next hop group entry identifier
* @param    nextHopId  @b{(input)} next hop entry identifier
*
* @returns  OFDPA_E_NONE  member next hop successfully added
* @returns  OFDPA_E_PARAM error in parameters passed to function
* @returns  OFDPA_E_ERROR configuration error, includes ECMP tunnel next hop or
*                         next hop entry not found or other constraint violation
* @returns  OFDPA_E_FAIL  failure occurred while adding next hop to ECMP next hop group
* @returns  OFDPA_E_FULL  maximum number of member next hops already added
* @returns  OFDPA_E_EXISTS an entry with the specified identifier combination already added
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaTunnelEcmpNextHopGroupMemberAdd(uint32_t ecmpNextHopGroupId, uint32_t nextHopId);

/*********************************************************************
* @purpose  Adds a next hop member to an ECMP next hop group entry. This
*           is done by adding any entry to the ECMP next hop group member table.
*           The index into this table is the ecmpNextHopGroupId, nextHopId tuple.
*
* @param    ecmpNextHopGroupId   @b{(input)} ECMP next hop group entry identifier
* @param    nextHopId  @b{(input)} next hop entry identifier
*
* @returns  OFDPA_E_NONE  next hop member successfully deleted
* @returns  OFDPA_E_NOT_FOUND next hop member is not configured in ECMP next hop group
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaTunnelEcmpNextHopGroupMemberDelete(uint32_t ecmpNextHopGroupId, uint32_t nextHopId);

/*********************************************************************
* @purpose  Test whether a next hop is a member in the specified ECMP next hop group.
*
* @param    ecmpNextHopGroupId   @b{(input)} ECMP next hop group entry identifier
* @param    nextHopId  @b{(input)} next hop entry identifier
*
* @returns  OFDPA_E_NONE  next hop is a member of ECMP next hop group
* @returns  OFDPA_E_NOT_FOUND next hop is not a member of ECMP next hop group
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaTunnelEcmpNextHopGroupMemberGet(uint32_t ecmpNextHopListGroupId, uint32_t nextHopId);

/*********************************************************************
* @purpose  Gets the next member next hop in the ECMP next hop group.
*
* @param    ecmpNextHopGroupId   @b{(input)} ECMP next hop group entry identifier
* @param    nextHopId  @b{(input)} next hop entry identifier
* @param    nextNextHopId  @b{(output)} next next hop entry identifier, if any
*
* @returns  OFDPA_E_NONE       next entry found
* @returns  OFDPA_E_PARAM      error in parameters passed to function
* @returns  OFDPA_E_NOT_FOUND  no next entry found
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaTunnelEcmpNextHopGroupMemberNextGet(uint32_t ecmpNextHopListGroupId, uint32_t nextHopId, uint32_t *nextNextHopId);

/*********************************************************************
* @purpose  Packet out API. Sends a packet out a switch port
*
* @param    pkt @b{(input)} packet
* @param    flags @b{(input)}
* OFDPA_PACKET_LOOKUP_TX: Indicates that packet must go through a hardware lookup
*                          starting from the first table in the pipeline. The srcIfNum
*                          is used as the ingress port in table lookups.
*
*                          If this flag is not set, then the packet is sent unmodified
*                          out of the outIfNum
*
* @param    outPortNum @b{(output)} Output port. Can be physical or logical or reserved
* @param    inPortNum @b{(input)} Input port. Indicates the interface on which packet was
*                                 originally received.
*
* @returns  OFDPA_E_NONE if packet is transmitted successfully
* @returns  OFDPA_E_PARAM if an input parameter is bad
* @returns  OFDPA_E_NOT_FOUND if the input or output port parameter is bad
* @returns  OFDPA_E_INTERNAL if transmit DMA buffer has not been allocated
* @returns  Any other return code if there is a failure
*
* @notes    DPA will not do any buffering of packets and it is expected to be done
*           at the agent if required
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaPktSend(ofdpa_buffdesc *pkt, uint32_t flags, uint32_t outPortNum, uint32_t inPortNum);

/*********************************************************************
 * @purpose  Report the size in bytes of the largest packet that can be received.
 *
 * @param    pktSize    @b{(output)}  The maximum packet size, in bytes.
 *
 * @returns  OFDPA_E_NONE
 *
 * @notes    A client can use the returned value to size a packet receive buffer.
 *
 * @end
 * ********************************************************************/
OFDPA_ERROR_t ofdpaMaxPktSizeGet(uint32_t *pktSize);

// Events are handled on the agentx side

///*------------------------------------------------------------------------------------*/
///* Event APIs */
//
///*
//   Asynchronous events: Have classified them into two broad category:
//   Packet In : Packet received from the hardware to be sent to the controller
//   Control messages: Events like Port creation, deletion, link state, Flow age and Error
//
//   The packet In events can have a high frequency, so we want to ensure that there is
//   separate control for polling of these two event types to give flexibility to the agent
//*/
//
///*********************************************************************
// * @purpose  Get OF-DPA Client's event socket fd
// *
// * @param    none
// *
// * @returns  event socket fd
// *
// * @end
// *********************************************************************/
//int ofdpaClientEventSockFdGet(void);
//
///*********************************************************************
// * @purpose  Get OF-DPA Client's packet socket fd
// *
// * @param    none
// *
// * @returns  packet socket fd
// *
// * @end
// *********************************************************************/
//int ofdpaClientPktSockFdGet(void);
//
///*********************************************************************
// * @purpose  The client calls this function to retrieve a single packet that the hardware
// *           has punted to the CPU.
// *
// * @param    timeout  @b{(input)}  If NULL, the function blocks until a packet is received.
// *                                 If the timeout value is zero, the function returns immediately,
// *                                 whether or not a packet is available. The return code
// *                                 indicates whether a packet has been received. If the timeout
// *                                 value is non-zero, the function blocks up to this length of
// *                                 time if a packet is not immediately available. Again, the
// *                                 return code indicates whether a packet was received.
// * @param    pkt      @b{(output)} A packet structure allocated by the caller and used to
// *                                 return a received packet. The packet structure includes some
// *                                 metadata to indicate properties like why the packet came to
// *                                 the CPU and the ingress port. On input, the caller must set
// *                                 pkt->pktData.size to the size in bytes of the buffer allocated
// *                                 to hold the received packet. On output, pkt->pktData.size
// *                                 specifies the length of the packet in bytes. pkt->pktData.pstart
// *                                 on input must point to a buffer large enough to hold the largest
// *                                 possible received packet (OFDPA_MAX_PKT_LEN). This function copies
// *                                 the packet into this buffer, starting with the Ethernet header
// *                                 (the destination MAC address). The trailing Ethernet CRC is not
// *                                 included.
// *
// * @returns  OFDPA_E_NONE     if a packet is returned
// * @returns  OFDPA_E_TIMEOUT  if no packet is available within the specified timeout
// * @returns  OFDPA_E_PARAM    if an input parameter is invalid (e.g., pkt is NULL)
// * @returns  OFDPA_E_FAIL     for any other failure
// *
// * @notes    This function runs in the client's own context and is not an RPC API.
// *
// * @end
// * ********************************************************************/
////OFDPA_ERROR_t ofdpaPktReceive(struct timeval *timeout, ofdpaPacket_t *pkt);
//
//
///*********************************************************************
//* @purpose  Receive an event.
//*
//* @param    timeout    @b{(input)} time to wait for the event
//*
//* @returns  OFDPA_E_NONE        event received successfully
//* @returns  OFDPA_E_FAIL        failure in socket creation, timeout configuration
//*                               or event reception
//* @returns  OFDPA_E_TIMEOUT     no event waiting to be received
//*
//* @note
//*
//* @end
//*********************************************************************/
//OFDPA_ERROR_t ofdpaEventReceive(struct timeval *timeout);
//
///*********************************************************************
//* @purpose  Get the next port event.
//*
//* @param    eventData    @b{(inout)} event data
//*
//* @returns  OFDPA_E_NONE        port event data returned successfully
//* @returns  OFDPA_E_PARAM       error in parameters passed to function
//* @returns  OFDPA_E_NOT_FOUND   next port event not found
//*
//* @note     populate the port number in eventData to get the
//*           event for the next port
//*
//* @end
//*********************************************************************/
//OFDPA_ERROR_t ofdpaPortEventNextGet(ofdpaPortEvent_t *eventData);
//
///*********************************************************************
//* @purpose  Get the next flow event.
//*
//* @param    eventData    @b{(inout)} event data
//*
//* @returns  OFDPA_E_NONE        flow event data returned successfully
//* @returns  OFDPA_E_PARAM       error in parameters passed to function
//* @returns  OFDPA_E_NOT_FOUND   next port event not found
//*
//* @note     populate the table id in eventData to get the
//*           events for the flow table
//*
//* @end
//*********************************************************************/
//OFDPA_ERROR_t ofdpaFlowEventNextGet(ofdpaFlowEvent_t *eventData);

/*------------------------------------------------------------------------------------*/
/* Table APIs */

/* Get information for a given table Id */

/*********************************************************************
* @purpose  Get Flow Table Info.
*
* @param    tableId    @b{(input)} flow table Id
* @param    info       @b{(output)} flow table info
*
* @returns  OFDPA_E_NONE        table info returned successfully
* @returns  OFDPA_E_PARAM       error in parameters passed to function
* @returns  OFDPA_E_NOT_FOUND   table id not found
*
*
* @end
*********************************************************************/
OFDPA_ERROR_t ofdpaFlowTableInfoGet(OFDPA_FLOW_TABLE_ID_t tableId, ofdpaFlowTableInfo_t *info);

/*------------------------------------------------------------------------------------*/
/* Queue APIs */

/*********************************************************************
 * @purpose  The client calls this function to get the number of COS
 *           queues on a port.
 *
 * @param    portNum      @b{(input)}  Port number
 * @param    numQueues    @b{(output)} Number of Queues on a port
 *
 * @returns  OFDPA_E_NONE     if number of queues on a port is returned.
 * @returns  OFDPA_E_PARAM    if an input parameter is invalid.
 * @returns  OFDPA_E_NOT_FOUND the port does not exist.
 *
 * @notes
 *
 * @end
 *********************************************************************/
OFDPA_ERROR_t ofdpaNumQueuesGet(uint32_t portNum, uint32_t *numQueues);

/*********************************************************************
 * @purpose  The client calls this function to get the statistics of a
 *           queue on a port.
 *
 * @param    portNum      @b{(input)}  Port number
 * @param    queueId      @b{(input)}  Queue ID
 * @param    stats        @b{(output)} Queue Statistics on a given port
 *
 * @returns  OFDPA_E_NONE     if the port queue statistics are returned.
 * @returns  OFDPA_E_PARAM    if an input parameter is invalid.
 * @returns  OFDPA_E_NOT_FOUND the port does not exist.
 *
 * @notes
 *
 * @end
 *********************************************************************/
OFDPA_ERROR_t ofdpaQueueStatsGet(uint32_t portNum, uint32_t queueId, ofdpaPortQueueStats_t *stats);

/*********************************************************************
 * @purpose  The client calls this function to clear all the statistics of a
 *           queue on a port.
 *
 * @param    portNum      @b{(input)}  Port number
 * @param    queueId      @b{(input)}  Queue ID
 *
 * @returns  OFDPA_E_NONE     if the port queue statistics are cleared.
 * @returns  OFDPA_E_PARAM    if an input parameter is invalid.
 * @returns  OFDPA_E_NOT_FOUND the port does not exist.
 *
 * @notes
 *
 * @end
 *********************************************************************/
OFDPA_ERROR_t ofdpaQueueStatsClear(uint32_t portNum, uint32_t queueId);

/*********************************************************************
 * @purpose  The client calls this function to set minimum and maximum
 *           bandwidth on a queue on a given port.
 *
 * @param    portNum      @b{(input)}  Port number
 * @param    queueId      @b{(input)}  Queue ID
 * @param    minRate      @b{(input)}  Minimum bandwith
 * @param    maxRate      @b{(input)}  Maximum bandwidth
 *
 * @returns  OFDPA_E_NONE     if minimum and maximum bandwidths of a queue
 *                            are set successfully on a given port.
 * @returns  OFDPA_E_PARAM    if an input parameter is invalid.
 * @returns  OFDPA_E_NOT_FOUND the port does not exist.
 *
 * @notes
 *
 * @end
 *********************************************************************/
OFDPA_ERROR_t ofdpaQueueRateSet(uint32_t portNum, uint32_t queueId, uint32_t minRate, uint32_t maxRate);

/*********************************************************************
 * @purpose  The client calls this function to get minimum and maximum
 *           bandwidth on a queue on a given port.
 *
 * @param    portNum      @b{(input)}  Port number
 * @param    queueId      @b{(input)}  Queue ID
 * @param    minRate      @b{(output)} Minimum bandwith
 * @param    maxRate      @b{(output)} Maximum bandwidth
 *
 * @returns  OFDPA_E_NONE     if minimum and maximum bandwidths of a queue
 *                            are returned successfully on a given port.
 * @returns  OFDPA_E_PARAM    if an input parameter is invalid
 * @returns  OFDPA_E_NOT_FOUND the port does not exist.
 *
 * @notes
 *
 * @end
 *********************************************************************/
OFDPA_ERROR_t ofdpaQueueRateGet(uint32_t portNum, uint32_t queueId, uint32_t *minRate, uint32_t *maxRate);

/*------------------------------------------------------------------------------------*/
/* Vendor Extension APIs */

/*********************************************************************
 * @purpose  The client calls this function to set the Source Mac Learning
 *           Mode with other config parameters.
 *
 * @param    mode                    @b{(input)}  Source MAC Learning Mode
 * @param    srcMacLearnModeCfg      @b{(input)}  Config structure holding other parameters
 *                                                of Source MAC Learning
 *
 * @returns  OFDPA_E_NONE     if Source MAC Learning Mode is set successfully.
 * @returns  OFDPA_E_UNAVAIL  if any of the learning methods (Local/Controller managed)
 *                            is unsupported.
 * @returns  OFDPA_E_PARAM    if an input parameter is invalid.
 *
 * @notes    Enable mode: Copies the packets with unknown source address to CPU.
 * @notes    Disable mode: Switches the packets with unknown source address.
 *
 * @end
 *********************************************************************/
OFDPA_ERROR_t ofdpaSourceMacLearningSet(OFDPA_CONTROL_t mode, ofdpaSrcMacLearnModeCfg_t *srcMacLearnModeCfg);

/*********************************************************************
 * @purpose  The client calls this function to get the Source Mac Learning
 *           Mode with other config parameters.
 *
 * @param    mode                    @b{(output)}  Source MAC Learning Mode
 * @param    srcMacLearnModeCfg      @b{(output)}  Config structure holding other parameters
 *                                                 of Source MAC Learning
 *
 * @returns  OFDPA_E_NONE     if Source MAC Learning Mode is returned successfully
 *                            on controller port.
 * @returns  OFDPA_E_PARAM    if an input parameter is invalid.
 *
 *
 * @end
 *********************************************************************/
OFDPA_ERROR_t ofdpaSourceMacLearningGet(OFDPA_CONTROL_t *mode, ofdpaSrcMacLearnModeCfg_t *srcMacLearnModeCfg);

/*********************************************************************
 * @purpose  Initialize Flow entry structure.
 *
 * @param    groupType       @b{(input)}  Group Type
 * @param    group           @b{(inout)}  Group entry structure
 *
 * @returns  OFDPA_E_NONE     if group entry structure is initialized successfully.
 * @returns  OFDPA_E_PARAM    if an input parameter is invalid.
 *
 * @end
 *********************************************************************/
OFDPA_ERROR_t ofdpaGroupEntryInit(OFDPA_GROUP_ENTRY_TYPE_t groupType, ofdpaGroupEntry_t *group);

/*********************************************************************
 * @purpose  Initialize Flow entry structure.
 *
 * @param    groupType       @b{(input)}  Group Type
 * @param    bucket          @b{(inout)}  Group bucket entry structure
 *
 * @returns  OFDPA_E_NONE     if group bucket entry structure is initialized successfully.
 * @returns  OFDPA_E_PARAM    if an input parameter is invalid.
 *
 * @end
 *********************************************************************/
OFDPA_ERROR_t ofdpaGroupBucketEntryInit(OFDPA_GROUP_ENTRY_TYPE_t groupType, ofdpaGroupBucketEntry_t *bucket);


/* Development utility APIs */
/*********************************************************************
 * @purpose  Set the Debug Verbosity
 *
 * @param    lvl    @b{(input)}  verbosity level (ofdpaDebugLevels_t)
 *
 * @returns  0     success
 *
 * @end
 *********************************************************************/
int ofdpaDebugLvl(int lvl);

/*********************************************************************
 * @purpose  Get the Debug Verbosity
 *
 * @param    none
 *
 * @returns  lvl     verbosity level (ofdpaDebugLevels_t)
 *
 * @end
 *********************************************************************/
int ofdpaDebugLvlGet(void);

/*********************************************************************
 * @purpose  Get the Component Name
 *
 * @param    component    @b{(input)}  component number
 * @param    name         @b{(output)} component name
 *
 * @returns  0     success
 * @returns  1     failure to get component
 *
 * @end
 *********************************************************************/
int ofdpaComponentNameGet(int component, ofdpa_buffdesc *name);

/*********************************************************************
 * @purpose  Enable/Disable the Component for debugging
 *
 * @param    component    @b{(input)} component number (ofdpaComponentIds_t)
 * @param    enable      @b{(input)} debug mode (ofdpaDebugLevels_t)
 *
 * @returns  0     success
 * @returns  1     failure
 *
 * @end
 *********************************************************************/
int ofdpaDebugComponentSet(int component, int enable);

/*********************************************************************
 * @purpose  Get Component debugging mode
 *
 * @param    component    @b{(input)} component number (ofdpaComponentIds_t)
 *
 * @returns  debug mode
 *
 * @end
 *********************************************************************/
int ofdpaDebugComponentGet(int component);

/*********************************************************************
 * @purpose  Execute a BCM command
 *
 * @param    buffer    @b{(input)} BCM command
 *
 * @returns  0     success
 * @returns  <0    failure
 *
 * @end
 *********************************************************************/
int ofdpaBcmCommand(ofdpa_buffdesc buffer);

#endif /* INCLUDE_OFDPA_API_H */
