//
//
//

#include <stdlib.h>
#include <assert.h>

#include <apr-1/apr_general.h>
#include <apr-1/apr_network_io.h>

#include "ofdpa_datatypes.h"
#include "ofdpa_api.h"

#include "agentx.h"
#include "getput.h"

#define AGENTX_PORT		5005

#define TAG_CALL	0xca11
#define	TAG_RETURN	0xbac6

apr_status_t handle_link(int *done, apr_socket_t *sock, apr_pool_t *cont);
apr_status_t receive(apr_socket_t *sock, int len, uint8_t *buf);
apr_status_t invoke(uint16_t what, uint32_t cookie,
		uint8_t *arg_buf, uint32_t arg_len, uint8_t *ret_buf, uint32_t *ret_len);
void error_exit(apr_status_t rs);

int main(int argc, char *argv[])
{
	apr_status_t rs;
	apr_pool_t *p;
	apr_socket_t *listener;
	apr_sockaddr_t *sa;

	//printf("sizeof(ofdpaPortQueueStats_t) = %d\n", sizeof(ofdpaPortQueueStats_t));

	apr_initialize();
	rs = apr_pool_create(&p, 0);
	if (rs == 0)
		rs = apr_socket_create(&listener, APR_INET, SOCK_STREAM, APR_PROTO_TCP, p);
	if (rs == 0)
		rs = apr_sockaddr_info_get(&sa, 0, APR_UNSPEC, AGENTX_PORT, 0, p);
	if (rs == 0)
		rs = apr_socket_bind(listener, sa);
	if (rs == 0)
		rs = apr_socket_listen(listener, 8);
	if (rs != 0)
		error_exit(rs);

	printf("Agentx listens on %d\n", AGENTX_PORT);

	int done = 0;
	while (!done)
	{
		apr_pool_t *cont;
		rs = apr_pool_create(&cont, p);
		apr_socket_t *sock;
		if (rs == 0)
			rs = apr_socket_accept(&sock, listener, cont);
		if (rs == 0)
			rs = handle_link(&done, sock, cont);
		if (rs != 0)
			error_exit(rs);

		apr_socket_close(sock);
		apr_pool_destroy(cont);
	}

	apr_pool_destroy(p);
	apr_terminate();
}

//
//  4: len without len field)
//	2: #0xca11
//	2: function
//	4: cookie
//	-: args
//

apr_status_t handle_link(int *done, apr_socket_t *sock, apr_pool_t *cont)
{
	apr_status_t rs;

	printf("linc_ofdpa connection accepted\n");

	while (1)
	{
		uint32_t len;
		uint8_t buf[4096];
		rs = receive(sock, 4, buf);
		if (rs == 0)
		{
			len = GET32(buf);
			assert(len <= sizeof(buf));
			rs = receive(sock, len, buf);
		}
		if (rs != 0)
			return rs;

		assert(len >= 2 +2 +4);
		uint16_t tag = GET16(buf);
		uint16_t what = GET16(buf +2);
		uint32_t cookie = GET32(buf +2 +2);
		uint8_t *arg_buf = buf +2 +2 +4;
		uint32_t arg_len = len -2 -2 -4;

		assert(tag == TAG_CALL);
		assert(what <= API_CALL_MAX);

		uint8_t reply[12];

		uint8_t ret_buf[4096];
		uint32_t ret_len = sizeof(ret_buf);
		rs = invoke(what, cookie, arg_buf, arg_len, ret_buf, &ret_len);
		if (rs == 0)
		{
			uint32_t rep_len = 2 +2 +4 +ret_len;
			PUT32(reply, rep_len);
			PUT16(reply +4, TAG_RETURN);
			PUT16(reply +4 +2, what);
			PUT32(reply +4 +2 +2, cookie);

			rs = transmit(sock, 12, reply);
		}
		if (rs == 0)
			rs = transmit(sock, ret_len, ret_buf);
		if (rs != 0)
			return rs;
	}
}

void error_exit(apr_status_t rs)
{
	char buf[4096];
	apr_strerror(rs, buf, sizeof(buf));
	printf("Error: %s\n", buf);
	exit(1);
}

apr_status_t receive(apr_socket_t *sock, int len, uint8_t *buf)
{
	apr_status_t rs;
	int nrecv = 0;
	while (nrecv < len)
	{
		apr_size_t n = len -nrecv;
		rs = apr_socket_recv(sock, buf +nrecv, &n);
		if (rs != 0)
			return rs;
		nrecv += n;
	}
	
	return 0;
}

apr_status_t transmit(apr_socket_t *sock, int len, uint8_t *buf)
{
	apr_status_t rs;
	int nsent = 0;
	while (nsent < len)
	{
		apr_size_t n = len -nsent;
		rs = apr_socket_send(sock, buf +nsent, &n);
		if (rs != 0)
			return rs;
		nsent += n;
	}

	return 0;
}

// OFDPA_ERROR_t ofdpaQueueStatsGet(uint32_t portNum, uint32_t queueId, ofdpaPortQueueStats_t *stats);

apr_status_t invoke(uint16_t what, uint32_t cookie,
		uint8_t *arg_buf, uint32_t arg_len, uint8_t *ret_buf, uint32_t *ret_len)
{
	int off = 0;
	int roff = 0;
	int sz;
	OFDPA_ERROR_t err;

	printf("Invoking function [%d], cookie [%x] arg_len [%d]\n", what, cookie, arg_len);

	switch (what)
	{
	case FLOW_ENTRY_INIT:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		PUT32(ret_buf +roff, sizeof(ofdpaFlowEntry_t));
		roff += 4;
		assert(roff +sizeof(ofdpaFlowEntry_t) <= *ret_len);
		ofdpaFlowEntry_t *flow = (ofdpaFlowEntry_t *)(ret_buf +roff);
		roff += sizeof(ofdpaFlowEntry_t);

		assert(off +4 <= arg_len);
		OFDPA_FLOW_TABLE_ID_t tableId = (OFDPA_FLOW_TABLE_ID_t)GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaFlowEntryInit(tableId, flow);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case FLOW_ADD:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;


		assert(off +4 <= arg_len);
		sz = GET32(arg_buf +off);
		off += 4;
		assert(off +sz <= arg_len);
		ofdpaFlowEntry_t *flow = (ofdpaFlowEntry_t *)(arg_buf +off);
		off += sz;

		OFDPA_ERROR_t err = ofdpaFlowAdd(flow);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case FLOW_MODIFY:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;


		assert(off +4 <= arg_len);
		sz = GET32(arg_buf +off);
		off += 4;
		assert(off +sz <= arg_len);
		ofdpaFlowEntry_t *flow = (ofdpaFlowEntry_t *)(arg_buf +off);
		off += sz;

		OFDPA_ERROR_t err = ofdpaFlowModify(flow);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case FLOW_DELETE:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;


		assert(off +4 <= arg_len);
		sz = GET32(arg_buf +off);
		off += 4;
		assert(off +sz <= arg_len);
		ofdpaFlowEntry_t *flow = (ofdpaFlowEntry_t *)(arg_buf +off);
		off += sz;

		OFDPA_ERROR_t err = ofdpaFlowDelete(flow);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case FLOW_NEXT_GET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		PUT32(ret_buf +roff, sizeof(ofdpaFlowEntry_t));
		roff += 4;
		assert(roff +sizeof(ofdpaFlowEntry_t) <= *ret_len);
		ofdpaFlowEntry_t *nextFlow = (ofdpaFlowEntry_t *)(ret_buf +roff);
		roff += sizeof(ofdpaFlowEntry_t);

		assert(off +4 <= arg_len);
		sz = GET32(arg_buf +off);
		off += 4;
		assert(off +sz <= arg_len);
		ofdpaFlowEntry_t *flow = (ofdpaFlowEntry_t *)(arg_buf +off);
		off += sz;

		OFDPA_ERROR_t err = ofdpaFlowNextGet(flow, nextFlow);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case FLOW_STATS_GET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		PUT32(ret_buf +roff, sizeof(ofdpaFlowEntryStats_t));
		roff += 4;
		assert(roff +sizeof(ofdpaFlowEntryStats_t) <= *ret_len);
		ofdpaFlowEntryStats_t *flowStats = (ofdpaFlowEntryStats_t *)(ret_buf +roff);
		roff += sizeof(ofdpaFlowEntryStats_t);

		assert(off +4 <= arg_len);
		sz = GET32(arg_buf +off);
		off += 4;
		assert(off +sz <= arg_len);
		ofdpaFlowEntry_t *flow = (ofdpaFlowEntry_t *)(arg_buf +off);
		off += sz;

		OFDPA_ERROR_t err = ofdpaFlowStatsGet(flow, flowStats);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case FLOW_BY_COOKIE_GET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		PUT32(ret_buf +roff, sizeof(ofdpaFlowEntry_t));
		roff += 4;
		assert(roff +sizeof(ofdpaFlowEntry_t) <= *ret_len);
		ofdpaFlowEntry_t *flow = (ofdpaFlowEntry_t *)(ret_buf +roff);
		roff += sizeof(ofdpaFlowEntry_t);
		assert(roff +4 <= *ret_len);
		PUT32(ret_buf +roff, sizeof(ofdpaFlowEntryStats_t));
		roff += 4;
		assert(roff +sizeof(ofdpaFlowEntryStats_t) <= *ret_len);
		ofdpaFlowEntryStats_t *flowStats = (ofdpaFlowEntryStats_t *)(ret_buf +roff);
		roff += sizeof(ofdpaFlowEntryStats_t);

		assert(off +8 <= arg_len);
		uint64_t cookie = GET64(arg_buf +off);
		off += 8;

		OFDPA_ERROR_t err = ofdpaFlowByCookieGet(cookie, flow, flowStats);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case FLOW_BY_COOKIE_DELETE:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;


		assert(off +8 <= arg_len);
		uint64_t cookie = GET64(arg_buf +off);
		off += 8;

		OFDPA_ERROR_t err = ofdpaFlowByCookieDelete(cookie);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case GROUP_TYPE_GET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		uint32_t *type = (uint32_t *)(ret_buf +roff);
		roff += 4;

		assert(off +4 <= arg_len);
		uint32_t groupId = GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaGroupTypeGet(groupId, type);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case GROUP_VLAN_GET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		uint32_t *vlanId = (uint32_t *)(ret_buf +roff);
		roff += 4;

		assert(off +4 <= arg_len);
		uint32_t groupId = GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaGroupVlanGet(groupId, vlanId);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case GROUP_PORT_ID_GET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		uint32_t *portId = (uint32_t *)(ret_buf +roff);
		roff += 4;

		assert(off +4 <= arg_len);
		uint32_t groupId = GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaGroupPortIdGet(groupId, portId);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case GROUP_INDEX_SHORT_GET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		uint32_t *index = (uint32_t *)(ret_buf +roff);
		roff += 4;

		assert(off +4 <= arg_len);
		uint32_t groupId = GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaGroupIndexShortGet(groupId, index);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case GROUP_INDEX_GET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		uint32_t *index = (uint32_t *)(ret_buf +roff);
		roff += 4;

		assert(off +4 <= arg_len);
		uint32_t groupId = GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaGroupIndexGet(groupId, index);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case GROUP_TYPE_SET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		uint32_t *groupId = (uint32_t *)(ret_buf +roff);
		roff += 4;

		assert(off +4 <= arg_len);
		*groupId = GET32(arg_buf +off);
		off += 4;
		assert(off +4 <= arg_len);
		uint32_t type = GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaGroupTypeSet(groupId, type);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case GROUP_VLAN_SET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		uint32_t *groupId = (uint32_t *)(ret_buf +roff);
		roff += 4;

		assert(off +4 <= arg_len);
		*groupId = GET32(arg_buf +off);
		off += 4;
		assert(off +4 <= arg_len);
		uint32_t vlanId = GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaGroupVlanSet(groupId, vlanId);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case GROUP_OVERLAY_TUNNEL_ID_SET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		uint32_t *groupId = (uint32_t *)(ret_buf +roff);
		roff += 4;

		assert(off +4 <= arg_len);
		*groupId = GET32(arg_buf +off);
		off += 4;
		assert(off +4 <= arg_len);
		uint32_t tunnelId = GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaGroupOverlayTunnelIdSet(groupId, tunnelId);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case GROUP_OVERLAY_SUB_TYPE_SET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		uint32_t *groupId = (uint32_t *)(ret_buf +roff);
		roff += 4;

		assert(off +4 <= arg_len);
		*groupId = GET32(arg_buf +off);
		off += 4;
		assert(off +4 <= arg_len);
		OFDPA_L2_OVERLAY_SUBTYPE_t subType = (OFDPA_L2_OVERLAY_SUBTYPE_t)GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaGroupOverlaySubTypeSet(groupId, subType);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case GROUP_OVERLAY_INDEX_SET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		uint32_t *groupId = (uint32_t *)(ret_buf +roff);
		roff += 4;

		assert(off +4 <= arg_len);
		*groupId = GET32(arg_buf +off);
		off += 4;
		assert(off +4 <= arg_len);
		uint32_t index = GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaGroupOverlayIndexSet(groupId, index);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case GROUP_PORT_ID_SET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		uint32_t *groupId = (uint32_t *)(ret_buf +roff);
		roff += 4;

		assert(off +4 <= arg_len);
		*groupId = GET32(arg_buf +off);
		off += 4;
		assert(off +4 <= arg_len);
		uint32_t portId = GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaGroupPortIdSet(groupId, portId);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case GROUP_INDEX_SHORT_SET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		uint32_t *groupId = (uint32_t *)(ret_buf +roff);
		roff += 4;

		assert(off +4 <= arg_len);
		*groupId = GET32(arg_buf +off);
		off += 4;
		assert(off +4 <= arg_len);
		uint32_t index = GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaGroupIndexShortSet(groupId, index);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case GROUP_INDEX_SET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		uint32_t *groupId = (uint32_t *)(ret_buf +roff);
		roff += 4;

		assert(off +4 <= arg_len);
		*groupId = GET32(arg_buf +off);
		off += 4;
		assert(off +4 <= arg_len);
		uint32_t index = GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaGroupIndexSet(groupId, index);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case GROUP_DECODE:
	{
//		assert(roff +4 <= *ret_len);
//		// make space for OFDPA_ERROR_t
//		roff += 4;
//
//		//TODO: {outBuf,char,out,scalar}
//
//		assert(off +4 <= arg_len);
//		uint32_t groupId = GET32(arg_buf +off);
//		off += 4;
//		//TODO: {bufSize,int,in,scalar}
//
//		OFDPA_ERROR_t err = ofdpaGroupDecode(groupId, outBuf, bufSize);
//		PUT32(ret_buf, err);
//
//		*ret_len = roff;
		break;
	}
	case GROUP_ENTRY_INIT:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		PUT32(ret_buf +roff, sizeof(ofdpaGroupEntry_t));
		roff += 4;
		assert(roff +sizeof(ofdpaGroupEntry_t) <= *ret_len);
		ofdpaGroupEntry_t *group = (ofdpaGroupEntry_t *)(ret_buf +roff);
		roff += sizeof(ofdpaGroupEntry_t);

		assert(off +4 <= arg_len);
		OFDPA_GROUP_ENTRY_TYPE_t groupType = (OFDPA_GROUP_ENTRY_TYPE_t)GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaGroupEntryInit(groupType, group);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case GROUP_ADD:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;


		assert(off +4 <= arg_len);
		sz = GET32(arg_buf +off);
		off += 4;
		assert(off +sz <= arg_len);
		ofdpaGroupEntry_t *group = (ofdpaGroupEntry_t *)(arg_buf +off);
		off += sz;

		OFDPA_ERROR_t err = ofdpaGroupAdd(group);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case GROUP_DELETE:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;


		assert(off +4 <= arg_len);
		uint32_t groupId = GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaGroupDelete(groupId);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case GROUP_NEXT_GET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		PUT32(ret_buf +roff, sizeof(ofdpaGroupEntry_t));
		roff += 4;
		assert(roff +sizeof(ofdpaGroupEntry_t) <= *ret_len);
		ofdpaGroupEntry_t *nextGroup = (ofdpaGroupEntry_t *)(ret_buf +roff);
		roff += sizeof(ofdpaGroupEntry_t);

		assert(off +4 <= arg_len);
		uint32_t groupId = GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaGroupNextGet(groupId, nextGroup);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case GROUP_TYPE_NEXT_GET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		PUT32(ret_buf +roff, sizeof(ofdpaGroupEntry_t));
		roff += 4;
		assert(roff +sizeof(ofdpaGroupEntry_t) <= *ret_len);
		ofdpaGroupEntry_t *nextGroup = (ofdpaGroupEntry_t *)(ret_buf +roff);
		roff += sizeof(ofdpaGroupEntry_t);

		assert(off +4 <= arg_len);
		uint32_t groupId = GET32(arg_buf +off);
		off += 4;
		assert(off +4 <= arg_len);
		OFDPA_GROUP_ENTRY_TYPE_t groupType = (OFDPA_GROUP_ENTRY_TYPE_t)GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaGroupTypeNextGet(groupId, groupType, nextGroup);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case GROUP_STATS_GET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		PUT32(ret_buf +roff, sizeof(ofdpaGroupEntryStats_t));
		roff += 4;
		assert(roff +sizeof(ofdpaGroupEntryStats_t) <= *ret_len);
		ofdpaGroupEntryStats_t *groupStats = (ofdpaGroupEntryStats_t *)(ret_buf +roff);
		roff += sizeof(ofdpaGroupEntryStats_t);

		assert(off +4 <= arg_len);
		uint32_t groupId = GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaGroupStatsGet(groupId, groupStats);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case GROUP_BUCKET_ENTRY_INIT:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		PUT32(ret_buf +roff, sizeof(ofdpaGroupBucketEntry_t));
		roff += 4;
		assert(roff +sizeof(ofdpaGroupBucketEntry_t) <= *ret_len);
		ofdpaGroupBucketEntry_t *bucket = (ofdpaGroupBucketEntry_t *)(ret_buf +roff);
		roff += sizeof(ofdpaGroupBucketEntry_t);

		assert(off +4 <= arg_len);
		OFDPA_GROUP_ENTRY_TYPE_t groupType = (OFDPA_GROUP_ENTRY_TYPE_t)GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaGroupBucketEntryInit(groupType, bucket);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case GROUP_BUCKET_ENTRY_ADD:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;


		assert(off +4 <= arg_len);
		sz = GET32(arg_buf +off);
		off += 4;
		assert(off +sz <= arg_len);
		ofdpaGroupBucketEntry_t *bucket = (ofdpaGroupBucketEntry_t *)(arg_buf +off);
		off += sz;

		OFDPA_ERROR_t err = ofdpaGroupBucketEntryAdd(bucket);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case GROUP_BUCKET_ENTRY_DELETE:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;


		assert(off +4 <= arg_len);
		uint32_t groupId = GET32(arg_buf +off);
		off += 4;
		assert(off +4 <= arg_len);
		uint32_t bucketIndex = GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaGroupBucketEntryDelete(groupId, bucketIndex);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case GROUP_BUCKETS_DELETE_ALL:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;


		assert(off +4 <= arg_len);
		uint32_t groupId = GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaGroupBucketsDeleteAll(groupId);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case GROUP_BUCKET_ENTRY_GET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		PUT32(ret_buf +roff, sizeof(ofdpaGroupBucketEntry_t));
		roff += 4;
		assert(roff +sizeof(ofdpaGroupBucketEntry_t) <= *ret_len);
		ofdpaGroupBucketEntry_t *groupBucket = (ofdpaGroupBucketEntry_t *)(ret_buf +roff);
		roff += sizeof(ofdpaGroupBucketEntry_t);

		assert(off +4 <= arg_len);
		uint32_t groupId = GET32(arg_buf +off);
		off += 4;
		assert(off +4 <= arg_len);
		uint32_t bucketIndex = GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaGroupBucketEntryGet(groupId, bucketIndex, groupBucket);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case GROUP_BUCKET_ENTRY_FIRST_GET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		PUT32(ret_buf +roff, sizeof(ofdpaGroupBucketEntry_t));
		roff += 4;
		assert(roff +sizeof(ofdpaGroupBucketEntry_t) <= *ret_len);
		ofdpaGroupBucketEntry_t *firstGroupBucket = (ofdpaGroupBucketEntry_t *)(ret_buf +roff);
		roff += sizeof(ofdpaGroupBucketEntry_t);

		assert(off +4 <= arg_len);
		uint32_t groupId = GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaGroupBucketEntryFirstGet(groupId, firstGroupBucket);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case GROUP_BUCKET_ENTRY_NEXT_GET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		PUT32(ret_buf +roff, sizeof(ofdpaGroupBucketEntry_t));
		roff += 4;
		assert(roff +sizeof(ofdpaGroupBucketEntry_t) <= *ret_len);
		ofdpaGroupBucketEntry_t *nextBucketEntry = (ofdpaGroupBucketEntry_t *)(ret_buf +roff);
		roff += sizeof(ofdpaGroupBucketEntry_t);

		assert(off +4 <= arg_len);
		uint32_t groupId = GET32(arg_buf +off);
		off += 4;
		assert(off +4 <= arg_len);
		uint32_t bucketIndex = GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaGroupBucketEntryNextGet(groupId, bucketIndex, nextBucketEntry);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case GROUP_BUCKET_ENTRY_MODIFY:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;


		assert(off +4 <= arg_len);
		sz = GET32(arg_buf +off);
		off += 4;
		assert(off +sz <= arg_len);
		ofdpaGroupBucketEntry_t *bucket = (ofdpaGroupBucketEntry_t *)(arg_buf +off);
		off += sz;

		OFDPA_ERROR_t err = ofdpaGroupBucketEntryModify(bucket);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case GROUP_TABLE_INFO_GET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		PUT32(ret_buf +roff, sizeof(ofdpaGroupTableInfo_t));
		roff += 4;
		assert(roff +sizeof(ofdpaGroupTableInfo_t) <= *ret_len);
		ofdpaGroupTableInfo_t *info = (ofdpaGroupTableInfo_t *)(ret_buf +roff);
		roff += sizeof(ofdpaGroupTableInfo_t);

		assert(off +4 <= arg_len);
		OFDPA_GROUP_ENTRY_TYPE_t groupType = (OFDPA_GROUP_ENTRY_TYPE_t)GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaGroupTableInfoGet(groupType, info);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case PORT_TYPE_GET:
	{
		assert(roff +4 <= *ret_len);
		uint32_t *type = (uint32_t *)(ret_buf +roff);
		roff += 4;

		assert(off +4 <= arg_len);
		uint32_t portNum = GET32(arg_buf +off);
		off += 4;

		ofdpaPortTypeGet(portNum, type);

		*ret_len = roff;
		break;
	}
	case PORT_TYPE_SET:
	{
		assert(roff +4 <= *ret_len);
		uint32_t *portNum = (uint32_t *)(ret_buf +roff);
		roff += 4;

		assert(off +4 <= arg_len);
		*portNum = GET32(arg_buf +off);
		off += 4;
		assert(off +4 <= arg_len);
		uint32_t type = GET32(arg_buf +off);
		off += 4;

		ofdpaPortTypeSet(portNum, type);

		*ret_len = roff;
		break;
	}
	case PORT_INDEX_GET:
	{
		assert(roff +4 <= *ret_len);
		uint32_t *index = (uint32_t *)(ret_buf +roff);
		roff += 4;

		assert(off +4 <= arg_len);
		uint32_t portNum = GET32(arg_buf +off);
		off += 4;

		ofdpaPortIndexGet(portNum, index);

		*ret_len = roff;
		break;
	}
	case PORT_INDEX_SET:
	{
		assert(roff +4 <= *ret_len);
		uint32_t *portNum = (uint32_t *)(ret_buf +roff);
		roff += 4;

		assert(off +4 <= arg_len);
		*portNum = GET32(arg_buf +off);
		off += 4;
		assert(off +4 <= arg_len);
		uint32_t index = GET32(arg_buf +off);
		off += 4;

		ofdpaPortIndexSet(portNum, index);

		*ret_len = roff;
		break;
	}
	case PORT_NEXT_GET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		uint32_t *nextPortNum = (uint32_t *)(ret_buf +roff);
		roff += 4;

		assert(off +4 <= arg_len);
		uint32_t portNum = GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaPortNextGet(portNum, nextPortNum);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case PORT_MAC_GET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +6 <= *ret_len);
		ofdpaMacAddr_t *mac = (ofdpaMacAddr_t *)(ret_buf +roff);
		roff += 6;

		assert(off +4 <= arg_len);
		uint32_t portNum = GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaPortMacGet(portNum, mac);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case PORT_NAME_GET:
	{
//		assert(roff +4 <= *ret_len);
//		// make space for OFDPA_ERROR_t
//		roff += 4;
//
//		//TODO: {name,ofdpa_buffdesc,out,scalar}
//
//		assert(off +4 <= arg_len);
//		uint32_t portNum = GET32(arg_buf +off);
//		off += 4;
//
//		OFDPA_ERROR_t err = ofdpaPortNameGet(portNum, name);
//		PUT32(ret_buf, err);
//
//		*ret_len = roff;
		break;
	}
	case PORT_STATE_GET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		OFDPA_PORT_STATE_t *state = (OFDPA_PORT_STATE_t *)(ret_buf +roff);
		roff += 4;

		assert(off +4 <= arg_len);
		uint32_t portNum = GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaPortStateGet(portNum, state);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case PORT_CONFIG_SET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;


		assert(off +4 <= arg_len);
		uint32_t portNum = GET32(arg_buf +off);
		off += 4;
		assert(off +4 <= arg_len);
		OFDPA_PORT_CONFIG_t config = (OFDPA_PORT_CONFIG_t)GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaPortConfigSet(portNum, config);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case PORT_CONFIG_GET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		OFDPA_PORT_CONFIG_t *config = (OFDPA_PORT_CONFIG_t *)(ret_buf +roff);
		roff += 4;

		assert(off +4 <= arg_len);
		uint32_t portNum = GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaPortConfigGet(portNum, config);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case PORT_MAX_SPEED_GET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		uint32_t *maxSpeed = (uint32_t *)(ret_buf +roff);
		roff += 4;

		assert(off +4 <= arg_len);
		uint32_t portNum = GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaPortMaxSpeedGet(portNum, maxSpeed);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case PORT_CURR_SPEED_GET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		uint32_t *currSpeed = (uint32_t *)(ret_buf +roff);
		roff += 4;

		assert(off +4 <= arg_len);
		uint32_t portNum = GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaPortCurrSpeedGet(portNum, currSpeed);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case PORT_FEATURE_GET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		PUT32(ret_buf +roff, sizeof(ofdpaPortFeature_t));
		roff += 4;
		assert(roff +sizeof(ofdpaPortFeature_t) <= *ret_len);
		ofdpaPortFeature_t *feature = (ofdpaPortFeature_t *)(ret_buf +roff);
		roff += sizeof(ofdpaPortFeature_t);

		assert(off +4 <= arg_len);
		uint32_t portNum = GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaPortFeatureGet(portNum, feature);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case PORT_ADVERTISE_FEATURE_SET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;


		assert(off +4 <= arg_len);
		uint32_t portNum = GET32(arg_buf +off);
		off += 4;
		assert(off +4 <= arg_len);
		uint32_t advertise = GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaPortAdvertiseFeatureSet(portNum, advertise);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case PORT_STATS_CLEAR:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;


		assert(off +4 <= arg_len);
		uint32_t portNum = GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaPortStatsClear(portNum);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case PORT_STATS_GET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		PUT32(ret_buf +roff, sizeof(ofdpaPortStats_t));
		roff += 4;
		assert(roff +sizeof(ofdpaPortStats_t) <= *ret_len);
		ofdpaPortStats_t *stats = (ofdpaPortStats_t *)(ret_buf +roff);
		roff += sizeof(ofdpaPortStats_t);

		assert(off +4 <= arg_len);
		uint32_t portNum = GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaPortStatsGet(portNum, stats);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case PKT_SEND:
	{
//		assert(roff +4 <= *ret_len);
//		// make space for OFDPA_ERROR_t
//		roff += 4;
//
//		//TODO: {pkt,ofdpa_buffdesc,out,scalar}
//
//		assert(off +4 <= arg_len);
//		uint32_t flags = GET32(arg_buf +off);
//		off += 4;
//		assert(off +4 <= arg_len);
//		uint32_t outPortNum = GET32(arg_buf +off);
//		off += 4;
//		assert(off +4 <= arg_len);
//		uint32_t inPortNum = GET32(arg_buf +off);
//		off += 4;
//
//		OFDPA_ERROR_t err = ofdpaPktSend(pkt, flags, outPortNum, inPortNum);
//		PUT32(ret_buf, err);
//
//		*ret_len = roff;
		break;
	}
	case MAX_PKT_SIZE_GET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		uint32_t *pktSize = (uint32_t *)(ret_buf +roff);
		roff += 4;


		OFDPA_ERROR_t err = ofdpaMaxPktSizeGet(pktSize);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case PKT_RECEIVE:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +6 <= *ret_len);
		struct timeval *timeout = (struct timeval *)(ret_buf +roff);
		roff += 6;
		assert(roff +4 <= *ret_len);
		PUT32(ret_buf +roff, sizeof(ofdpaPacket_t));
		roff += 4;
		assert(roff +sizeof(ofdpaPacket_t) <= *ret_len);
		ofdpaPacket_t *pkt = (ofdpaPacket_t *)(ret_buf +roff);
		roff += sizeof(ofdpaPacket_t);


		OFDPA_ERROR_t err = ofdpaPktReceive(timeout, pkt);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case EVENT_RECEIVE:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +6 <= *ret_len);
		struct timeval *timeout = (struct timeval *)(ret_buf +roff);
		roff += 6;


		OFDPA_ERROR_t err = ofdpaEventReceive(timeout);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case PORT_EVENT_NEXT_GET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		PUT32(ret_buf +roff, sizeof(ofdpaPortEvent_t));
		roff += 4;
		assert(roff +sizeof(ofdpaPortEvent_t) <= *ret_len);
		ofdpaPortEvent_t *eventData = (ofdpaPortEvent_t *)(ret_buf +roff);
		roff += sizeof(ofdpaPortEvent_t);


		OFDPA_ERROR_t err = ofdpaPortEventNextGet(eventData);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case FLOW_EVENT_NEXT_GET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		PUT32(ret_buf +roff, sizeof(ofdpaFlowEvent_t));
		roff += 4;
		assert(roff +sizeof(ofdpaFlowEvent_t) <= *ret_len);
		ofdpaFlowEvent_t *eventData = (ofdpaFlowEvent_t *)(ret_buf +roff);
		roff += sizeof(ofdpaFlowEvent_t);


		OFDPA_ERROR_t err = ofdpaFlowEventNextGet(eventData);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case FLOW_TABLE_INFO_GET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		PUT32(ret_buf +roff, sizeof(ofdpaFlowTableInfo_t));
		roff += 4;
		assert(roff +sizeof(ofdpaFlowTableInfo_t) <= *ret_len);
		ofdpaFlowTableInfo_t *info = (ofdpaFlowTableInfo_t *)(ret_buf +roff);
		roff += sizeof(ofdpaFlowTableInfo_t);

		assert(off +4 <= arg_len);
		OFDPA_FLOW_TABLE_ID_t tableId = (OFDPA_FLOW_TABLE_ID_t)GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaFlowTableInfoGet(tableId, info);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case NUM_QUEUES_GET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		uint32_t *numQueues = (uint32_t *)(ret_buf +roff);
		roff += 4;

		assert(off +4 <= arg_len);
		uint32_t portNum = GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaNumQueuesGet(portNum, numQueues);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case QUEUE_STATS_GET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		PUT32(ret_buf +roff, sizeof(ofdpaPortQueueStats_t));
		roff += 4;
		assert(roff +sizeof(ofdpaPortQueueStats_t) <= *ret_len);
		ofdpaPortQueueStats_t *stats = (ofdpaPortQueueStats_t *)(ret_buf +roff);
		roff += sizeof(ofdpaPortQueueStats_t);

		assert(off +4 <= arg_len);
		uint32_t portNum = GET32(arg_buf +off);
		off += 4;
		assert(off +4 <= arg_len);
		uint32_t queueId = GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaQueueStatsGet(portNum, queueId, stats);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case QUEUE_STATS_CLEAR:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;


		assert(off +4 <= arg_len);
		uint32_t portNum = GET32(arg_buf +off);
		off += 4;
		assert(off +4 <= arg_len);
		uint32_t queueId = GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaQueueStatsClear(portNum, queueId);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case QUEUE_RATE_SET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;


		assert(off +4 <= arg_len);
		uint32_t portNum = GET32(arg_buf +off);
		off += 4;
		assert(off +4 <= arg_len);
		uint32_t queueId = GET32(arg_buf +off);
		off += 4;
		assert(off +4 <= arg_len);
		uint32_t minRate = GET32(arg_buf +off);
		off += 4;
		assert(off +4 <= arg_len);
		uint32_t maxRate = GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaQueueRateSet(portNum, queueId, minRate, maxRate);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case QUEUE_RATE_GET:
	{
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		uint32_t *minRate = (uint32_t *)(ret_buf +roff);
		roff += 4;
		assert(roff +4 <= *ret_len);
		uint32_t *maxRate = (uint32_t *)(ret_buf +roff);
		roff += 4;

		assert(off +4 <= arg_len);
		uint32_t portNum = GET32(arg_buf +off);
		off += 4;
		assert(off +4 <= arg_len);
		uint32_t queueId = GET32(arg_buf +off);
		off += 4;

		OFDPA_ERROR_t err = ofdpaQueueRateGet(portNum, queueId, minRate, maxRate);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}

	default:
		return APR_BADARG;
	}

	printf("Function invocation complete, ret_len [%d]\n", *ret_len);
	return 0;
}

//EOF
