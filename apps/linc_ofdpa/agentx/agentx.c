//
//
//

#include <stdlib.h>
#include <assert.h>

#include <apr-1/apr_general.h>
#include <apr-1/apr_network_io.h>
#include <apr-1/apr_poll.h>
#include <apr-1/apr_portable.h>

#include "ofdpa_datatypes.h"
#include "ofdpa_api.h"

#include "agentx.h"
#include "getput.h"

#include <string.h>

#define AGENTX_PORT		5005

#define TAG_CALL	0xca11
#define	TAG_RETURN	0xbac6
#define TAG_EVENT	0xeeee
#define TAG_PACKET	0xbead

#define EVENT_FLOW	1
#define	EVENT_PORT	2

#define POLL_TIMEO	5000000

apr_status_t handle_link(int *done, apr_socket_t *sock, apr_pool_t *cont);
apr_status_t receive(apr_socket_t *sock, int len, uint8_t *buf);
apr_status_t invoke(uint16_t what, uint32_t cookie,
		uint8_t *arg_buf, uint32_t arg_len, uint8_t *ret_buf, uint32_t *ret_len);
apr_status_t api_call(apr_socket_t *sock, apr_pool_t *cont);
apr_status_t async_event(apr_socket_t *sock, apr_pool_t *cont);
apr_status_t incoming_pkt(apr_socket_t *sock, apr_pool_t *cont);
void error_exit(apr_status_t rs);

int main(int argc, char *argv[])
{
	apr_status_t rs;
	apr_pool_t *p;
	apr_socket_t *listener;
	apr_sockaddr_t *sa;

	//printf("sizeof(ofdpaFlowEntry_t) = %d\n", sizeof(ofdpaFlowEntry_t));
	//printf("sizeof(ofdpaGroupBucketEntry_t) = %d\n", sizeof(ofdpaGroupBucketEntry_t));

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

	int event_fd = ofdpaClientEventSockFdGet();
	int pkt_fd = ofdpaClientPktSockFdGet();

	apr_socket_t *event_sock;
	apr_socket_t *pkt_sock;

	// convert event and pkt descriptors to apr_socket_t
	apr_os_sock_put(&event_sock, (apr_os_sock_t *)&event_fd, cont);
	apr_os_sock_put(&pkt_sock, (apr_os_sock_t *)&pkt_fd, cont);

	apr_pollfd_t pfd = {
		.desc_type = APR_POLL_SOCKET,
		.reqevents = APR_POLLIN,
	};

	apr_pollset_t *pollset;
	rs = apr_pollset_create(&pollset, 3, cont, 0);
	if (rs == 0)
	{
		pfd.desc.s = sock;
		rs = apr_pollset_add(pollset, &pfd);
	}
	if (rs == 0)
	{
		pfd.desc.s = event_sock;
		//rs = apr_pollset_add(pollset, &pfd);
	}
	if (rs == 0)
	{
		pfd.desc.s = pkt_sock;
		//rs = apr_pollset_add(pollset, &pfd);
	}
	if (rs != 0)
		return rs;

	while (1)
	{
		apr_int32_t n;
		const apr_pollfd_t *pfds;

		rs = apr_pollset_poll(pollset, POLL_TIMEO, &n, &pfds);
		if (rs != 0 && rs != APR_TIMEUP)
			return rs;

		int i;
		for (i = 0; i < n; i++)
		{
			if (pfds[i].desc.s == sock)
				rs = api_call(sock, cont);
			else if (pfds[i].desc.s == event_sock)
				rs = async_event(sock, cont);
			else if (pfds[i].desc.s == pkt_sock)
				rs = incoming_pkt(sock, cont);

			if (rs == APR_EOF)
			{
				*done = 1;
				return 0;
			}
			
			if (rs != 0)
				return rs;
		}
	}
}

apr_status_t api_call(apr_socket_t *sock, apr_pool_t *cont)
{
	apr_status_t rs;

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

	return rs;
}

// events has the same packet layout as API calls

apr_status_t async_event(apr_socket_t *sock, apr_pool_t *cont)
{
	apr_status_t rs;

	uint8_t buf[4096];
	int off = 0;
	
	ofdpaFlowEvent_t fe;
	while (ofdpaFlowEventNextGet(&fe) == OFDPA_E_NONE)
	{
		off = 4 +2 +2 +4;
		PUT32(buf +off, fe.eventMask);
		off += 4;
		memcpy((void *)(buf +off), (void *)&fe.flowMatch, sizeof(ofdpaFlowEntry_t));
		off += sizeof(ofdpaFlowEntry_t);

		uint32_t len = off -4;
		PUT32(buf, len);
		PUT16(buf +4, TAG_EVENT);
		PUT16(buf +4 +2, EVENT_FLOW);
		apr_generate_random_bytes(buf +4 +2 +2, 4);	// cookie

		rs = transmit(sock, buf, len);
		if (rs != 0)
			return rs;
	}

	ofdpaPortEvent_t pe;
	while (ofdpaPortEventNextGet(&pe) == OFDPA_E_NONE)
	{
		off = 4 +2 +2 +4;
		PUT32(buf +off, pe.eventMask);
		off += 4;
		PUT32(buf +off, pe.portNum);
		off += 4;
		PUT32(buf +off, pe.state);
		off += 4;

		uint32_t len = off -4;
		PUT32(buf, len);
		PUT16(buf +4, TAG_EVENT);
		PUT16(buf +4 +2, EVENT_PORT);
		apr_generate_random_bytes(buf +4 +2 +2, 4);	// cookie

		rs = transmit(sock, buf, off);
		if (rs != 0)
			return rs;
	}

	return 0;
}

apr_status_t incoming_pkt(apr_socket_t *sock, apr_pool_t *cont)
{
	apr_status_t rs;

	//  +0: 12-byte protocol 
	// +12: 4-byte packet size
	// +16: packet data

	uint8_t buf[4096];
	struct timeval tv = { 0 };

	while (1)
	{	
		ofdpaPacket_t pkt = {
			.pktData.pstart = buf +12 +4,
			.pktData.size = sizeof(buf) -12 -4,
		};

		if (ofdpaPktReceive(&tv, &pkt) != OFDPA_E_NONE)
			break;

		uint32_t len = 12 +4 +pkt.pktData.size -4;
		PUT32(buf, len);
		PUT16(buf +4, TAG_PACKET);
		PUT16(buf +4 +2, 0);
		apr_generate_random_bytes(buf +4 +2 +2, 4);	// cookie

		// fill-in packet size
		PUT32(buf +4 +2 +2 +4, pkt.pktData.size);

		rs = transmit(sock, buf, len +4);
		if (rs != 0)
			return rs;
	}

	return 0;
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
		assert(off +4 <= arg_len);
		uint32_t groupId = GET32(arg_buf +off);
		off += 4;
		assert(off +4 <= arg_len);
		uint32_t bufSize = GET32(arg_buf +off);
		off += 4;

		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;
		assert(roff +4 <= *ret_len);
		uint32_t *saveSize = (uint32_t *)(ret_buf +roff);
		roff += 4;
		assert(roff +bufSize <= *ret_len);
		char *outBuf = (char *)(ret_buf +roff);
		// roff not updated

		OFDPA_ERROR_t err = ofdpaGroupDecode(groupId, outBuf, bufSize);
		PUT32(ret_buf, err);
		
		uint32_t len = strlen(outBuf);
		PUT32(saveSize, len);
		roff += len;

		*ret_len = roff;
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

		assert(off +4 <= arg_len);
		OFDPA_GROUP_ENTRY_TYPE_t groupType = (OFDPA_GROUP_ENTRY_TYPE_t)GET32(arg_buf +off);
		off += 4;

		assert(roff +4 <= *ret_len);
		PUT32(ret_buf +roff, sizeof(ofdpaGroupBucketEntry_t) +4);
		roff += 4;
		
		// GroupType added to the reply for RPC to make sense of the BucketData
		assert(roff +4 <= *ret_len);
		PUT32(ret_buf +roff, groupType);
		roff += 4;

		assert(roff +sizeof(ofdpaGroupBucketEntry_t) <= *ret_len);
		ofdpaGroupBucketEntry_t *bucket = (ofdpaGroupBucketEntry_t *)(ret_buf +roff);
		roff += sizeof(ofdpaGroupBucketEntry_t);

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
		assert(off +4 <= arg_len);
		uint32_t groupId = GET32(arg_buf +off);
		off += 4;
		assert(off +4 <= arg_len);
		uint32_t bucketIndex = GET32(arg_buf +off);
		off += 4;

		// get group type
		uint32_t type;
		OFDPA_ERROR_t e = ofdpaGroupTypeGet(groupId, &type);
		assert(e == OFDPA_E_NONE);

		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		PUT32(ret_buf +roff, sizeof(ofdpaGroupBucketEntry_t) +4);
		roff += 4;

		assert(roff +4 <= *ret_len);
		PUT32(ret_buf +roff, type);
		roff += 4;

		assert(roff +sizeof(ofdpaGroupBucketEntry_t) <= *ret_len);
		ofdpaGroupBucketEntry_t *groupBucket = (ofdpaGroupBucketEntry_t *)(ret_buf +roff);
		roff += sizeof(ofdpaGroupBucketEntry_t);

		OFDPA_ERROR_t err = ofdpaGroupBucketEntryGet(groupId, bucketIndex, groupBucket);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case GROUP_BUCKET_ENTRY_FIRST_GET:
	{
		assert(off +4 <= arg_len);
		uint32_t groupId = GET32(arg_buf +off);
		off += 4;

		uint32_t type;
		OFDPA_ERROR_t e = ofdpaGroupTypeGet(groupId, &type);
		assert(e == OFDPA_E_NONE);

		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		PUT32(ret_buf +roff, sizeof(ofdpaGroupBucketEntry_t) +4);
		roff += 4;

		assert(roff +4 <= *ret_len);
		PUT32(ret_buf +roff, type);
		roff += 4;

		assert(roff +sizeof(ofdpaGroupBucketEntry_t) <= *ret_len);
		ofdpaGroupBucketEntry_t *firstGroupBucket = (ofdpaGroupBucketEntry_t *)(ret_buf +roff);
		roff += sizeof(ofdpaGroupBucketEntry_t);

		OFDPA_ERROR_t err = ofdpaGroupBucketEntryFirstGet(groupId, firstGroupBucket);
		PUT32(ret_buf, err);

		*ret_len = roff;
		break;
	}
	case GROUP_BUCKET_ENTRY_NEXT_GET:
	{
		assert(off +4 <= arg_len);
		uint32_t groupId = GET32(arg_buf +off);
		off += 4;
		assert(off +4 <= arg_len);
		uint32_t bucketIndex = GET32(arg_buf +off);
		off += 4;

		uint32_t type;
		OFDPA_ERROR_t e = ofdpaGroupTypeGet(groupId, &type);
		assert(e == OFDPA_E_NONE);

		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		PUT32(ret_buf +roff, sizeof(ofdpaGroupBucketEntry_t) +4);
		roff += 4;

		assert(roff +4 <= *ret_len);
		PUT32(ret_buf +roff, type);
		roff += 4;

		assert(roff +sizeof(ofdpaGroupBucketEntry_t) <= *ret_len);
		ofdpaGroupBucketEntry_t *nextBucketEntry = (ofdpaGroupBucketEntry_t *)(ret_buf +roff);
		roff += sizeof(ofdpaGroupBucketEntry_t);

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
		char buf[1024];
		ofdpa_buffdesc name = {
			.pstart = buf,
			.size = sizeof(buf)
		};

		assert(off +4 <= arg_len);
		uint32_t portNum = GET32(arg_buf +off);
		off += 4;

		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		OFDPA_ERROR_t err = ofdpaPortNameGet(portNum, &name);
		PUT32(ret_buf, err);

		uint32_t len = strlen(buf);

		assert(roff +4 <= *ret_len);
		PUT32(ret_buf +roff, len);
		roff += 4;

		assert(roff +len <= *ret_len);
		memcpy(ret_buf +roff, buf, len);
		roff += len;

		*ret_len = roff;
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
		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(off +4 <= arg_len);	
		uint32_t pkt_len = GET32(arg_buf +off);
		off +4;
		assert(off +pkt_len <= arg_len);
		uint8_t *pkt_data = (uint8_t *)(arg_buf +off);
		off += pkt_len;

		assert(off +4 <= arg_len);
		uint32_t flags = GET32(arg_buf +off);
		off += 4;
		assert(off +4 <= arg_len);
		uint32_t outPortNum = GET32(arg_buf +off);
		off += 4;
		assert(off +4 <= arg_len);
		uint32_t inPortNum = GET32(arg_buf +off);
		off += 4;

		ofdpa_buffdesc pkt = {
			.pstart = pkt_data,
			.size = pkt_len
		};

		OFDPA_ERROR_t err = ofdpaPktSend(&pkt, flags, outPortNum, inPortNum);
		PUT32(ret_buf, err);

		*ret_len = roff;
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
