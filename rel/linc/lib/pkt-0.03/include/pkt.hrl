%%% Macros ---------------------------------------------------------------------

-define(ETHERHDRLEN, 16).
-define(IPV4HDRLEN, 20).
-define(IPV6HDRLEN, 40).
-define(TCPHDRLEN, 20).
-define(UDPHDRLEN, 8).
-define(ICMPHDRLEN, 8).
-define(ICMPV6HDRLEN, 4).
-define(GREHDRLEN, 4).

%% From http://en.wikipedia.org/wiki/EtherType
-define(ETH_P_IP,         16#0800).
-define(ETH_P_ARP,        16#0806).
-define(ETH_P_IPV6,       16#86DD).
-define(ETH_P_802_1Q,     16#8100).
-define(ETH_P_MPLS_UNI,   16#8847).
-define(ETH_P_MPLS_MULTI, 16#8848).
-define(ETH_P_ALL,        16#0300).
-define(ETH_P_PBB_B,      16#88A8).
-define(ETH_P_PBB_I,      16#88E7).

-define(ARPHRD_ETHER, 1).
-define(ARPHRD_IEEE80211, 801).

-define(ARPOP_REQUEST, 1).                  % ARP request
-define(ARPOP_REPLY, 2).                    % ARP reply
-define(ARPOP_RREQUEST, 3).                 % RARP request
-define(ARPOP_RREPLY, 4).                   % RARP reply
-define(ARPOP_InREQUEST, 8).                % InARP request
-define(ARPOP_InREPLY, 9).                  % InARP reply
-define(ARPOP_NAK, 10).                     % (ATM)ARP NAK

%% Datalink types
-define(DLT_NULL, 0).                       % BSD loopback
-define(DLT_EN10MB, 1).                     % Ethernet
-define(DLT_LINUX_SLL, 113).                % Linux cooked sockets fake hdr
-define(DLT_EN3MB, 2).
-define(DLT_AX25, 3).
-define(DLT_PRONET, 4).
-define(DLT_CHAOS, 5).
-define(DLT_IEEE802, 6).
-define(DLT_ARCNET, 7).
-define(DLT_SLIP, 8).
-define(DLT_PPP, 9).
-define(DLT_FDDI, 10).
-define(DLT_ATM_RFC1483, 11).
-define(DLT_RAW, 12).
-define(DLT_SLIP_BSDOS, 15).
-define(DLT_PPP_BSDOS, 16).
-define(DLT_PFSYNC, 18).
-define(DLT_ATM_CLIP, 19).
-define(DLT_PPP_SERIAL, 50).
-define(DLT_C_HDLC, 104).
-define(DLT_CHDLC, ?DLT_C_HDLC).
-define(DLT_IEEE802_11, 105).
-define(DLT_LOOP, 108).
-define(DLT_PFLOG, 117).
-define(DLT_IEEE802_11_RADIO, 127).
-define(DLT_APPLE_IP_OVER_IEEE1394, 138).
-define(DLT_IEEE802_11_RADIO_AVS, 163).

-ifndef(PF_UNSPEC).
-define(PF_UNSPEC,0).                       % Unspecified.
-endif.
-ifndef(PF_LOCAL).
-define(PF_LOCAL, 1).                       % Local to host (pipes and file-domain).
-endif.
-ifndef(PF_UNIX).
-define(PF_UNIX, ?PF_LOCAL).                % POSIX name for PF_LOCAL.
-endif.
-ifndef(PF_INET).
-define(PF_INET, 2).                        % IP protocol family.
-endif.
-ifndef(PF_INET6).
-define(PF_INET6, 10).                      % IP version 6.
-endif.
-ifndef(PF_PACKET).
-define(PF_PACKET, 17).                     % Packet family.
-endif.

-define(IPPROTO_IP, 0).
-define(IPPROTO_ICMP, 1).
-define(IPPROTO_TCP, 6).
-define(IPPROTO_UDP, 17).
-define(IPPROTO_IPV6, 41).
-define(IPPROTO_GRE, 47).
-define(IPPROTO_ICMPV6, 58).
-define(IPPROTO_SCTP, 132).
-define(IPPROTO_RAW, 255).

-define(IPV6_HDR_HOP_BY_HOP, 0).
-define(IPV6_HDR_ROUTING, 43).
-define(IPV6_HDR_FRAGMENT, 44).
-define(IPV6_HDR_DEST_OPTS, 60).
-define(IPV6_HDR_NO_NEXT_HEADER, 59).

-define(ICMP_ECHOREPLY, 0).
-define(ICMP_DEST_UNREACH, 3).
-define(    ICMP_UNREACH_NET, 0).           % bad net
-define(    ICMP_UNREACH_HOST, 1).          % bad host
-define(    ICMP_UNREACH_PROTOCOL, 2).      % bad protocol
-define(    ICMP_UNREACH_PORT, 3).          % bad port
-define(    ICMP_UNREACH_NEEDFRAG, 4).      % IP_DF caused drop
-define(    ICMP_UNREACH_SRCFAIL, 5 ).      % src route failed
-define(ICMP_SOURCE_QUENCH, 4).
-define(ICMP_REDIRECT, 5).
-define(    ICMP_REDIRECT_NET, 0).          % for network
-define(    ICMP_REDIRECT_HOST, 1).         % for host
-define(    ICMP_REDIRECT_TOSNET, 2).       % for tos and net
-define(    ICMP_REDIRECT_TOSHOST, 3).      % for tos and host
-define(ICMP_ECHO, 8).
-define(ICMP_TIME_EXCEEDED, 11).
-define(    ICMP_TIMXCEED_INTRANS, 0).      % ttl==0 in transit
-define(    ICMP_TIMXCEED_REASS, 1).        % ttl==0 in reass
-define(ICMP_PARAMETERPROB, 12).
-define(ICMP_TIMESTAMP, 13).
-define(ICMP_TIMESTAMPREPLY, 14).
-define(ICMP_INFO_REQUEST, 15).
-define(ICMP_INFO_REPLY, 16).
-define(ICMP_ADDRESS, 17).
-define(ICMP_ADDRESSREPLY, 18).

-define(ICMPV6_DEST_UNREACH, 1).
-define(ICMPV6_PACKET_TOO_BIG, 2).
-define(ICMPV6_TIME_EXCEEDED, 3).
-define(ICMPV6_PARAMETER_PROBLEM, 4).
-define(ICMPV6_ECHO_REQUEST, 128).
-define(ICMPV6_ECHO_REPLY, 129).

-define(ICMPV6_NDP_RS, 133).
-define(ICMPV6_NDP_RA, 134).
-define(ICMPV6_NDP_NS, 135).
-define(ICMPV6_NDP_NA, 136).
-define(ICMPV6_NDP_REDIRECT, 137).

-define(NDP_OPT_SLL, 1).
-define(NDP_OPT_TLL, 2).

%%% Records --------------------------------------------------------------------

-record(linux_cooked, {
          packet_type,
          hrd = ?ARPHRD_ETHER,
          ll_len = 0,
          ll_bytes = <<>>,
          pro = ?ETH_P_IP
         }).

-record(null, {
          family = ?PF_INET
         }).

%% If the frame is tagged with a IEEE 802.1Q tag the type fileds is used as
%% a TPID of 802.1Q tag. This means that allowed value for the field is 0x8100.
-record(ether, {
          dhost = <<0,0,0,0,0,0>>,
          shost = <<0,0,0,0,0,0>>,
          type = ?ETH_P_IP,
          crc = 0
         }).

%% According to:
%% https://sites.google.com/site/amitsciscozone/home/vpls/pbb-vpls
%% http://en.wikipedia.org/wiki/IEEE_802.1ah-2008
-record(pbb, {
          %% Backbone
          b_dhost = <<0,0,0,0,0,0>>,     %% Destination Address
          b_shost = <<0,0,0,0,0,0>>,     %% Source Address
          b_type = ?ETH_P_PBB_B,
          b_pcp = 0,                     %% Priority Code Point
          b_dei = 0,                     %% Drop Eligibility Identifier
          b_vid = <<0:12>>,              %% VLAN Identifier
          %% Service Instance
          i_type = ?ETH_P_PBB_I,
          i_pcp = 0,                     %% Priority Code Point
          i_dei = 0,                     %% Drop Eligibility Identifier
          i_uca = 0,                     %% Use Customer Addresses
          i_reserved = 0,
          i_sid = <<0:24>>               %% Service Instance VLAN ID
         }).

%% If the frame is tagged with a IEEE 802.1Q tag the ether_type does NOT refers
%% to the TPID but to the EtherType/Length field that is moved behind the tag.
%% The type field from ether record plays the role of TPID then.
-record(ieee802_1q_tag, {
          pcp = 0,
          cfi = 0,
          vid = <<0:12>>,
          ether_type = ?ETH_P_IP
         }).

-record(mpls_stack_entry, {
          label = <<0:20>>,
          qos = 0,
          pri = 0,
          ecn = 0,
          bottom = 0,
          ttl = 255
         }).

-record(mpls_tag, {
          stack = [#mpls_stack_entry{}] :: [#mpls_stack_entry{}],
          mode = unicast :: unicast | multicast
         }).

-record(arp, {
          hrd = ?ARPHRD_ETHER,
          pro = ?ETH_P_IP,
          hln = 6,
          pln = 4,
          op = ?ARPOP_REPLY,

          sha = <<0,0,0,0,0,0>>,
          sip = <<127,0,0,1>>,

          tha = <<0,0,0,0,0,0>>,
          tip = <<127,0,0,1>>
         }).

-record(ipv4, {
          v = 4, hl = 5, dscp = 0, ecn = 0, len = 20,
          id = 0, df = 0, mf = 0,
          off = 0, ttl = 64, p = ?IPPROTO_TCP, sum = 0,
          saddr = <<127,0,0,1>>, daddr = <<127,0,0,1>>,
          opt = <<>>
         }).

-record(ipv6, {
          v = 6, class = 0, flow = 0,
          len = 40, next = ?IPPROTO_TCP, hop = 0,
          saddr, daddr
         }).

%% TODO: introduce separate headers for various IPv6 options
-record(ipv6_header, {
          type :: atom(),
          next :: integer(),
          content :: binary()
         }).

-record(tcp, {
          sport = 0, dport = 0,
          seqno = 0,
          ackno = 0,
          off = 5, cwr = 0, ece = 0, urg = 0, ack = 0,
          psh = 0, rst = 0, syn = 0, fin = 0, win = 0,
          sum = 0, urp = 0,
          opt = <<>>
         }).

-record(udp, {
          sport = 0, dport = 0, ulen = 8, sum = 0
         }).

-record(icmp, {
          type = ?ICMP_ECHO, code = 0, checksum = 0,
          id = 0, sequence = 0,
          gateway = <<127,0,0,1>>,
          un = <<0:32>>,
          mtu = 0,
          pointer = 0,
          ts_orig = 0, ts_recv = 0, ts_tx = 0
         }).

-record(icmpv6, {
          type = ?ICMPV6_ECHO_REQUEST, code = 0, checksum = 0
         }).

-record(ndp_ns, { %% neighbor solicitation
          tgt_addr = <<0:128>>,
          sll :: undefined | binary()
         }).

-record(ndp_na, { %% neighbor advertisement
          src_addr = <<0:128>>,
          r = 0, s = 0, o = 0,
          tll :: undefined | binary()
         }).

-record(sctp, {
          sport = 0, dport = 0, vtag = 0, sum = 0,
          chunks = []
         }).
-record(sctp_chunk, {
          type = 0, flags = 0, len = 0, payload = 0
         }).
-record(sctp_chunk_data, {
          tsn = 0, sid = 0, ssn = 0, ppi = 0, data
         }).

%% RFC 2784 - Generic Routing Encapsulation (GRE)
-record(gre, {
          c = 0, res0 = 0, ver = 0,
          type = ?ETH_P_IP,
          chksum = <<>>, res1 = <<>>
         }).
