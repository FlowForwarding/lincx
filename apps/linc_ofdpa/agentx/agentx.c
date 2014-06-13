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
	OFDPA_ERROR_t err;

	printf("Invoking function [%d], cookie [%x] arg_len [%d]\n", what, cookie, arg_len);

	switch (what)
	{
	case QUEUE_STATS_GET:
	{
		assert(off +4 <= arg_len);
		uint32_t port_num = GET32(arg_buf +off);
		off += 4;
		assert(off +4 <= arg_len);
		uint32_t queue_id = GET32(arg_buf +off);
		off += 4;

		assert(roff +4 <= *ret_len);
		// make space for OFDPA_ERROR_t
		roff += 4;

		assert(roff +4 <= *ret_len);
		PUT32(ret_buf +roff, sizeof(ofdpaPortQueueStats_t));
		roff += 4;
		assert(roff +sizeof(ofdpaPortQueueStats_t) <= *ret_len);
		ofdpaPortQueueStats_t *stats = (ofdpaPortQueueStats_t *)(ret_buf +roff);
		roff += sizeof(ofdpaPortQueueStats_t);

		OFDPA_ERROR_t err = ofdpaQueueStatsGet(port_num, queue_id, stats);

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
