%% Copyright (c) 2009-2010, Michael Santos <michael.santos@gmail.com>
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%%
%% Redistributions of source code must retain the above copyright
%% notice, this list of conditions and the following disclaimer.
%%
%% Redistributions in binary form must reproduce the above copyright
%% notice, this list of conditions and the following disclaimer in the
%% documentation and/or other materials provided with the distribution.
%%
%% Neither the name of the author nor the names of its contributors
%% may be used to endorse or promote products derived from this software
%% without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%% COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.
-module(pkt).

-include("pkt.hrl").

-export([
         encapsulate/1,
         decapsulate/1,
         decapsulate_pbb/1,
         decapsulate_dlt/2
        ]).

-export([
         checksum/1,
         makesum/1,
         valid/1,
         ether/1,
         ether_type/1,
         link_type/1,
         arp/1,
         null/1,
         linux_cooked/1,
         icmp/1,
         icmpv6/1,
         ipv4/1,
         ipv6/1,
         ipv6_header/2,
         proto/1,
         tcp/1,
         udp/1,
         dlt/1
        ]).

%%% Types ----------------------------------------------------------------------

-type ether_type() :: ipv4 | ipv6 | arp | unsupported.
-type proto() :: tcp | udp | sctp | icmp | icmpv6 | raw | unsupported.
-type header() :: #linux_cooked{} |
                  #null{} |
                  #pbb{} |
                  #ether{} |
                  #arp{} |
                  #ieee802_1q_tag{} |
                  #mpls_tag{} |
                  #ipv4{} |
                  #ipv6{} |
                  #tcp{} |
                  #udp{} |
                  #icmp{} |
                  #icmpv6{} |
                  #sctp{} |
                  #ndp_ns{} |
                  #ndp_na{} |
                  {unsupported, binary()} |
                  {truncated, binary()}.
%% Packet should be a list of headers with
%% optional binary payload as a last element.
-type packet() :: [header() | binary()].

-export_type([
              packet/0
             ]).

%%% Encapsulate ----------------------------------------------------------------

-spec encapsulate(packet()) -> binary().
encapsulate(Packet) ->
    encapsulate(lists:reverse(Packet), <<>>).

-spec encapsulate(packet(), binary()) -> binary().
encapsulate([], Binary) ->
    Binary;
encapsulate([Payload | Packet], <<>>) when is_binary(Payload) ->
    encapsulate(Packet, << Payload/binary >>);
encapsulate([#tcp{} = TCP | Packet], Binary) ->
    {ok, IP} = find_ip(Packet),
    TCPBinary = tcp(TCP#tcp{sum = makesum([IP, TCP, Binary])}),
    encapsulate(tcp, Packet, << TCPBinary/binary, Binary/binary >>);
encapsulate([#udp{} = UDP0 | Packet], Binary) ->
    {ok, IP} = find_ip(Packet),
    UDP = UDP0#udp{ulen = 8 + byte_size(Binary)},
    UDPBinary = udp(UDP#udp{sum = makesum([IP, UDP, Binary])}),
    encapsulate(udp, Packet, << UDPBinary/binary, Binary/binary >>);
encapsulate([#sctp{} = SCTP | Packet], Binary) ->
    SCTPBinary = sctp(SCTP),
    encapsulate(sctp, Packet, << SCTPBinary/binary, Binary/binary >>);
encapsulate([#icmp{} = ICMP | Packet], Binary) ->
    ICMPNoCsumBinary = icmp(ICMP#icmp{checksum = 0}),
    Checksum = makesum(<<ICMPNoCsumBinary/binary, Binary/binary>>),
    ICMPBinary = icmp(ICMP#icmp{checksum = Checksum}),
    encapsulate(icmp, Packet, << ICMPBinary/binary, Binary/binary >>);
encapsulate([#ndp_ns{} = NDP | Packet], Binary) ->
    encapsulate(ndp_ns, Packet, << (ndp_ns(NDP))/binary, Binary/binary >>);
encapsulate([#ndp_na{} = NDP | Packet], Binary) ->
    encapsulate(ndp_na, Packet, << (ndp_na(NDP))/binary, Binary/binary >>);
encapsulate([#icmpv6{} = ICMP | Packet], Binary) ->
    {ok, IP} = find_ip(Packet),
    ICMPBinary = icmpv6(ICMP#icmpv6{checksum = makesum([IP, ICMP, Binary])}),
    encapsulate(icmpv6, Packet, << ICMPBinary/binary, Binary/binary >>);
encapsulate([#arp{} = ARP | Packet], Binary) ->
    ARPBinary = arp(ARP),
    encapsulate(arp, Packet, << ARPBinary/binary, Binary/binary >>);
encapsulate([{unsupported, Unsupported} | Packet], Binary) ->
    encapsulate(unsupported, Packet, << Unsupported/binary, Binary/binary >>);
encapsulate([{truncated, Truncated} | Packet], Binary) ->
    encapsulate(truncated, Packet, << Truncated/binary, Binary/binary >>);
encapsulate([#ipv6{} | _] = Packet, Binary) ->
    encapsulate(ipv6_hdr_no_next, Packet, Binary).


-spec encapsulate(ether_type() | proto(), packet(), binary()) -> binary().
encapsulate(_, [], Binary) ->
    encapsulate([], Binary);
encapsulate(ICMPPayloadType, [#icmpv6{} = ICMP | Packet], Binary) ->
    encapsulate([fix_icmpv6_type(ICMP, ICMPPayloadType) | Packet], Binary);
encapsulate(Proto, [#ipv4{} = IPv4 | Packet], Binary) ->
    IPv4Binary = ipv4(fill_ipv4_hdr(IPv4, Proto, byte_size(Binary))),
    encapsulate(ipv4, Packet, << IPv4Binary/binary, Binary/binary >>);
encapsulate(Proto, [#ipv6_header{} = IPv6Header | Packet], Binary) ->
    Type = IPv6Header#ipv6_header.type,
    HeaderBinary = ipv6_header(Proto, IPv6Header),
    encapsulate(Type, Packet, << HeaderBinary/binary, Binary/binary >>);
encapsulate(Proto, [#ipv6{} = IPv6 | Packet], Binary) ->
    IPv6Binary = ipv6(fill_ipv6_hdr(IPv6, Proto, byte_size(Binary))),
    encapsulate(ipv6, Packet, << IPv6Binary/binary, Binary/binary >>);
encapsulate(EtherType, [#ieee802_1q_tag{} = VlanTag | Packet], Binary) ->
    TagBinary = ieee802_1q_tag(fill_ether_type(VlanTag, EtherType)),
    encapsulate(ieee802_1q_tag, Packet, << TagBinary/binary, Binary/binary >>);
encapsulate(_UpperProtocol, [#mpls_tag{mode = Mode} = MPLSTag | Packet], Binary) ->
    TagBinary = mpls_tag(MPLSTag),
    encapsulate({mpls_tag, Mode}, Packet, << TagBinary/binary, Binary/binary >>);
encapsulate(EtherType, [#ether{} = Ether | Packet], Binary) ->
    EtherBinary = ether(fill_ether_type(Ether, EtherType)),
    encapsulate(ether, Packet, << EtherBinary/binary, Binary/binary >>);
encapsulate(ether, [#pbb{} = PBB | Packet], Binary) ->
    PBBBinary = pbb(PBB),
    encapsulate(pbb, Packet, << PBBBinary/binary, Binary/binary >>).

%%% Decapsulate ----------------------------------------------------------------

decapsulate_pbb(Data) when is_binary(Data) ->
    decapsulate({pbb, Data}, []).

decapsulate_dlt(Dlt, Data) ->
    decapsulate({link_type(Dlt), Data}, []).

decapsulate({DLT, Data}) when is_integer(DLT) ->
    decapsulate({link_type(DLT), Data}, []);
decapsulate({DLT, Data}) when is_atom(DLT) ->
    decapsulate({DLT, Data}, []);

%% Initial call
decapsulate(Data) when is_binary(Data) ->
    decapsulate({ether, Data}, []).

decapsulate(stop, Packet) ->
    lists:reverse(Packet);

decapsulate({unsupported, Data}, Packet) ->
    decapsulate(stop, [{unsupported, Data}|Packet]);

decapsulate({null, Data}, Packet) when byte_size(Data) >= 16 ->
    {Hdr, Payload} = null(Data),
    decapsulate({family(Hdr#null.family), Payload}, [Hdr|Packet]);
decapsulate({linux_cooked, Data}, Packet) when byte_size(Data) >= 16 ->
    {Hdr, Payload} = linux_cooked(Data),
    decapsulate({ether_type(Hdr#linux_cooked.pro), Payload}, [Hdr|Packet]);

decapsulate({pbb, Data}, Packet) ->
    {Hdr, Payload} = pbb(Data),
    decapsulate({ether, Payload}, [Hdr | Packet]);
decapsulate({ether, Data}, Packet) ->
    {Hdr, Payload} = ether(Data),
    decapsulate({ether_type(Hdr#ether.type), Payload}, [Hdr | Packet]);

decapsulate({ieee802_1q_tag, Data}, Packet) ->
    {Tag, Payload} = ieee802_1q_tag(Data),
    EtherType = ether_type(Tag#ieee802_1q_tag.ether_type),
    decapsulate({EtherType, Payload}, [Tag|Packet]);
decapsulate({{mpls_tag, Mode}, Data}, Packet) ->
    {RawTag, Payload} = mpls_tag(Data),
    UpperProtocol = mpls_payload(Payload),
    Tag = RawTag#mpls_tag{mode = Mode},
    decapsulate({UpperProtocol, Payload}, [Tag|Packet]);

decapsulate({arp, Data}, Packet) when byte_size(Data) >= 28 -> %% IPv4 ARP
    {Hdr, Payload} = arp(Data),
    decapsulate(stop, [Payload, Hdr|Packet]);
decapsulate({ipv4, Data}, Packet) when byte_size(Data) >= ?IPV4HDRLEN ->
    {Hdr, Payload} = ipv4(Data),
    decapsulate({proto(Hdr#ipv4.p), Payload}, [Hdr|Packet]);
decapsulate({ipv6, Data}, Packet) when byte_size(Data) >= ?IPV6HDRLEN ->
    {Hdr, Payload} = ipv6(Data),
    decapsulate({ipv6_proto(Hdr#ipv6.next), Payload}, [Hdr|Packet]);

%% TODO: introduce separate headers for various IPv6 options, parse separately
decapsulate({IPv6Header, Data}, Packet)
  when IPv6Header =:= ipv6_hdr_hop_by_hop;
       IPv6Header =:= ipv6_hdr_routing;
       IPv6Header =:= ipv6_hdr_fragments;
       IPv6Header =:= ipv6_hdr_dest_opts ->
    {Hdr, Payload} = ipv6_header(IPv6Header, Data),
    decapsulate({ipv6_proto(Hdr#ipv6_header.next), Payload}, [Hdr|Packet]);
decapsulate({ipv6_hdr_no_next, Payload}, Packet) ->
    decapsulate(stop, [Payload | Packet]);

%% GRE
decapsulate({gre, Data}, Packet) when byte_size(Data) >= ?GREHDRLEN ->
    {Hdr, Payload} = gre(Data),
    decapsulate({ether_type(Hdr#gre.type), Payload}, [Hdr|Packet]);

decapsulate({tcp, Data}, Packet) when byte_size(Data) >= ?TCPHDRLEN ->
    {Hdr, Payload} = tcp(Data),
    decapsulate(stop, [Payload, Hdr|Packet]);
decapsulate({udp, Data}, Packet) when byte_size(Data) >= ?UDPHDRLEN ->
    {Hdr, Payload} = udp(Data),
    decapsulate(stop, [Payload, Hdr|Packet]);
decapsulate({sctp, Data}, Packet) when byte_size(Data) >= 12 ->
    Hdr = sctp(Data),
    decapsulate(stop, [Hdr|Packet]);
decapsulate({icmp, Data}, Packet) when byte_size(Data) >= ?ICMPHDRLEN ->
    {Hdr, Payload} = icmp(Data),
    decapsulate(stop, [Payload, Hdr|Packet]);
decapsulate({icmpv6, Data}, Packet) when byte_size(Data) >= ?ICMPV6HDRLEN ->
    {Hdr, Payload} = icmpv6(Data),
    decapsulate({icmpv6_payload_type(Hdr#icmpv6.type), Payload}, [Hdr|Packet]);

decapsulate({ndp_ns, Data}, Packet) ->
    decapsulate(stop, [ndp_ns(Data)|Packet]);
decapsulate({ndp_na, Data}, Packet) ->
    decapsulate(stop, [ndp_na(Data)|Packet]);

decapsulate({_, Data}, Packet) ->
    decapsulate(stop, [{truncated, Data}|Packet]).

ether_type(?ETH_P_IP) -> ipv4;
ether_type(?ETH_P_IPV6) -> ipv6;
ether_type(?ETH_P_ARP) -> arp;
ether_type(?ETH_P_802_1Q) -> ieee802_1q_tag;
ether_type(?ETH_P_MPLS_UNI) -> {mpls_tag, unicast};
ether_type(?ETH_P_MPLS_MULTI) -> {mpls_tag, multicast};
ether_type(_) -> unsupported.

ether_type_code(ipv4, _) -> ?ETH_P_IP;
ether_type_code(ipv6, _) -> ?ETH_P_IPV6;
ether_type_code(arp, _) -> ?ETH_P_ARP;
ether_type_code(ieee802_1q_tag, _) -> ?ETH_P_802_1Q;
ether_type_code({mpls_tag, unicast}, _) -> ?ETH_P_MPLS_UNI;
ether_type_code({mpls_tag, multicast}, _) -> ?ETH_P_MPLS_MULTI;
ether_type_code(unsupported, Old) -> Old.

link_type(?DLT_NULL) -> null;
link_type(?DLT_EN10MB) -> ether;
link_type(?DLT_LINUX_SLL) -> linux_cooked;
link_type(_) -> unsupported.

family(?PF_INET) -> ipv4;
family(?PF_INET6) -> ipv6;
family(_) -> unsupported.

ipv6_proto(?IPV6_HDR_HOP_BY_HOP) -> ipv6_hdr_hop_by_hop;
ipv6_proto(?IPV6_HDR_ROUTING) -> ipv6_hdr_routing;
ipv6_proto(?IPV6_HDR_FRAGMENT) -> ipv6_hdr_fragments;
ipv6_proto(?IPV6_HDR_DEST_OPTS) -> ipv6_hdr_dest_opts;
ipv6_proto(?IPV6_HDR_NO_NEXT_HEADER) -> ipv6_hdr_no_next;
ipv6_proto(Proto) -> proto(Proto).

proto(?IPPROTO_IP) -> ip;
proto(?IPPROTO_ICMP) -> icmp;
proto(?IPPROTO_ICMPV6) -> icmpv6;
proto(?IPPROTO_TCP) -> tcp;
proto(?IPPROTO_UDP) -> udp;
proto(?IPPROTO_IPV6) -> ipv6;
proto(?IPPROTO_SCTP) -> sctp;
proto(?IPPROTO_GRE) -> gre;
proto(?IPPROTO_RAW) -> raw;
proto(_) -> unsupported.

ipv6_proto_code(ipv6_hdr_hop_by_hop, _) -> ?IPV6_HDR_HOP_BY_HOP;
ipv6_proto_code(ipv6_hdr_routing, _) -> ?IPV6_HDR_ROUTING;
ipv6_proto_code(ipv6_hdr_fragments, _) -> ?IPV6_HDR_FRAGMENT;
ipv6_proto_code(ipv6_hdr_dest_opts, _) -> ?IPV6_HDR_DEST_OPTS;
ipv6_proto_code(ipv6_hdr_no_next, _) -> ?IPV6_HDR_NO_NEXT_HEADER;
ipv6_proto_code(Proto, Old) -> proto_code(Proto, Old).

proto_code(icmp, _) -> ?IPPROTO_ICMP;
proto_code(icmpv6, _) -> ?IPPROTO_ICMPV6;
proto_code(tcp, _) -> ?IPPROTO_TCP;
proto_code(udp, _) -> ?IPPROTO_UDP;
proto_code(sctp, _) -> ?IPPROTO_SCTP;
proto_code(raw, _) -> ?IPPROTO_RAW;
proto_code(unsupported, Old) -> Old.

icmpv6_payload_type(?ICMPV6_NDP_NS) -> ndp_ns;
icmpv6_payload_type(?ICMPV6_NDP_NA) -> ndp_na;
icmpv6_payload_type(_) -> unsupported.

fill_ether_type(#ieee802_1q_tag{ether_type = OldType} = Tag, EtherType) ->
    Tag#ieee802_1q_tag{ether_type = ether_type_code(EtherType, OldType)};
fill_ether_type(#ether{type = OldType} = Ether, EtherType) ->
    Ether#ether{type = ether_type_code(EtherType, OldType)}.

fill_ipv4_hdr(#ipv4{opt = Opt} = IPv4, Proto, DataLen) ->
    HL = 5 + (bit_size(Opt) + 31) div 32,
    Tmp = IPv4#ipv4{hl = HL,
                    len = 4 * HL + DataLen,
                    p = proto_code(Proto, IPv4#ipv4.p)},
    Tmp#ipv4{sum = pkt:makesum(Tmp)}.

fill_ipv6_hdr(#ipv6{} = IPv6, Proto, Len) ->
    IPv6#ipv6{len = Len,
              next = ipv6_proto_code(Proto, IPv6#ipv6.next)}.

fix_icmpv6_type(#icmpv6{} = ICMP, ndp_ns) ->
    ICMP#icmpv6{type = ?ICMPV6_NDP_NS};
fix_icmpv6_type(#icmpv6{} = ICMP, ndp_na) ->
    ICMP#icmpv6{type = ?ICMPV6_NDP_NA};
fix_icmpv6_type(#icmpv6{} = ICMP, _) ->
    ICMP.

%%
%% BSD loopback
%%
null(<<Family:4/native-unsigned-integer-unit:8, Payload/binary>>) ->
    {#null{
        family = Family
       }, Payload};
null(#null{family = Family}) ->
    <<Family:4/native-unsigned-integer-unit:8>>.

%%
%% Linux cooked capture ("-i any") - DLT_LINUX_SLL
%%
linux_cooked(<<Ptype:16/big, Hrd:16/big, Ll_len:16/big,
               Ll_hdr:8/bytes, Pro:16, Payload/binary>>) ->
    {#linux_cooked{
        packet_type = Ptype, hrd = Hrd,
        ll_len = Ll_len, ll_bytes = Ll_hdr,
        pro = Pro
       }, Payload};
linux_cooked(#linux_cooked{
                packet_type = Ptype, hrd = Hrd,
                ll_len = Ll_len, ll_bytes = Ll_hdr,
                pro = Pro
               }) ->
    <<Ptype:16/big, Hrd:16/big, Ll_len:16/big,
      Ll_hdr:8/bytes, Pro:16>>.

%%
%% Ethernet
%%

%% @doc Regular ethernet or PBB header
ether(<<Dhost:6/bytes, Shost:6/bytes, Type:16, Payload/binary>>) ->
    %% Len = byte_size(Packet) - 4,
    %% <<Payload:Len/bytes, CRC:4/bytes>> = Packet,
    {#ether{
        dhost = Dhost,
        shost = Shost,
        type = Type
       }, Payload};

ether(#ether{
         dhost = Dhost, shost = Shost,
         type = Type
        }) ->
    <<Dhost:6/bytes, Shost:6/bytes, Type:16>>.

%%
%% PBB
%%
pbb(<<Dhost:6/bytes, Shost:6/bytes,
      ?ETH_P_PBB_B:16, BPcp:3, BDei:1, BVid:12/bits,
      ?ETH_P_PBB_I:16, IPcp:3, IDei:1, IUca:1, IRsrv:3, ISid:3/bytes,
      Payload/binary>>) ->
    {#pbb{
        b_dhost = Dhost,
        b_shost = Shost,
        b_type = ?ETH_P_PBB_B,
        b_pcp = BPcp,
        b_dei = BDei,
        b_vid = BVid,
        i_type = ?ETH_P_PBB_I,
        i_pcp = IPcp,
        i_dei = IDei,
        i_uca = IUca,
        i_reserved = IRsrv,
        i_sid = ISid
       }, Payload};

pbb(#pbb{b_dhost = Dhost, b_shost = Shost,
         b_pcp = BPcp, b_dei = BDei, b_vid = BVid,
         i_pcp = IPcp, i_dei = IDei, i_uca = IUca,
         i_reserved = IRsrv, i_sid = ISid}) ->
    <<Dhost:6/bytes, Shost:6/bytes,
      ?ETH_P_PBB_B:16, BPcp:3, BDei:1, BVid:12/bits,
      ?ETH_P_PBB_I:16, IPcp:3, IDei:1, IUca:1, IRsrv:3, ISid:3/bytes>>.

%%
%% MPLS
%%
mpls_tag(#mpls_tag{stack = Stack}) ->
    StackBin = << <<(mpls_stack_entry(SE))/binary>> || SE <- Stack >>,
    <<(set_bottom_bit(StackBin))/binary>>;
mpls_tag(Binary) when is_binary(Binary) ->
    decode_mpls(Binary, []).

mpls_stack_entry(#mpls_stack_entry{label = Label,
                                   qos = QOS,
                                   pri = PRI,
                                   ecn = ECN,
                                   ttl = TTL}) ->
    <<Label/bits, QOS:1, PRI:1, ECN:1, 0:1, TTL:8>>.

set_bottom_bit(MPLSStack) ->
    StartLen = bit_size(MPLSStack) - 9,
    <<Start:StartLen/bits, _:1, TTL:8>> = MPLSStack,
    <<Start:StartLen/bits, 1:1, TTL:8>>.

decode_mpls(<<Label:20/bitstring, QOS:1, PRI:1, ECN:1, Bottom:1, TTL:8,
              Rest/binary>>, Acc) when Bottom =:= 0 ->
    decode_mpls(Rest, [#mpls_stack_entry{label = Label,
                                         qos = QOS,
                                         pri = PRI,
                                         ecn = ECN,
                                         ttl = TTL} | Acc]);
decode_mpls(<<Label:20/bitstring, QOS:1, PRI:1, ECN:1, Bottom:1, TTL:8,
              Payload/binary>>,
            Acc) when Bottom =:= 1->
    StackEntry = #mpls_stack_entry{label = Label,
                                   qos = QOS,
                                   pri = PRI,
                                   ecn = ECN,
                                   ttl = TTL,
                                   bottom = Bottom},
    {#mpls_tag{stack = lists:reverse([StackEntry | Acc])}, Payload}.

%% TODO: Add support for other MPLS payloads.
mpls_payload(<<4:4, _Rest/bitstring>>) ->
    ipv4;
mpls_payload(<<6:4, _Rest/bitstring>>) ->
    ipv6;
mpls_payload(_) ->
    unsupported.

%%
%% 802.1Q
%%
ieee802_1q_tag(#ieee802_1q_tag{pcp = PCP,
                               cfi = CFI,
                               vid = VID,
                               ether_type = EtherType}) ->
    <<PCP:3, CFI:1, VID:12/bits, EtherType:16>>;
ieee802_1q_tag(<<PCP:3, CFI:1, VID:12/bits, EtherType:16, Payload/binary>>) ->
    {#ieee802_1q_tag{pcp = PCP,
                     cfi = CFI,
                     vid = VID,
                     ether_type = EtherType},
     Payload}.

%%
%% ARP
%%
arp(<<Hrd:16, Pro:16,
      Hln:8, Pln:8, Op:16,
      Sha:6/bytes,
      SAddr:32/bits,
      Tha:6/bytes,
      DAddr:32/bits,
      Payload/binary>>
   ) ->
    {#arp{
        hrd = Hrd, pro = Pro,
        hln = Hln, pln = Pln, op = Op,
        sha = Sha,
        sip = SAddr,
        tha = Tha,
        tip = DAddr
       }, Payload};
arp(#arp{
       hrd = Hrd, pro = Pro,
       hln = Hln, pln = Pln, op = Op,
       sha = Sha,
       sip = SAddr,
       tha = Tha,
       tip = DAddr
      }) ->
    <<Hrd:16, Pro:16,
      Hln:8, Pln:8, Op:16,
      Sha:6/bytes,
      SAddr:32/bits,
      Tha:6/bytes,
      DAddr:32/bits>>.


%%
%% IPv4
%%
ipv4(
  <<4:4, HL:4, DSCP:6, ECN:2, Len:16,
    Id:16, 0:1, DF:1, MF:1, %% RFC791 states it's a MUST
    Off:13, TTL:8, P:8, Sum:16,
    SAddr:32/bits, DAddr:32/bits, Rest/binary>>
 ) when HL >= 5 ->
    {Opt, Payload} = options(HL, Rest),
    {#ipv4{
        hl = HL, dscp = DSCP, ecn = ECN, len = Len,
        id = Id, df = DF, mf = MF,
        off = Off, ttl = TTL, p = P, sum = Sum,
        saddr = SAddr,
        daddr = DAddr,
        opt = Opt
       }, Payload};
ipv4(#ipv4{
        hl = HL, dscp = DSCP, ecn = ECN, len = Len,
        id = Id, df = DF, mf = MF,
        off = Off, ttl = TTL, p = P, sum = Sum,
        saddr = SAddr, daddr = DAddr,
        opt = Opt
       }) ->
    <<4:4, HL:4, DSCP:6, ECN:2, Len:16,
      Id:16, 0:1, DF:1, MF:1, %% RFC791 states it's a MUST
      Off:13, TTL:8, P:8, Sum:16,
      SAddr:32/bits, DAddr:32/bits, Opt/binary>>.


%%
%% IPv6
%%
ipv6(
  <<6:4, Class:8, Flow:20,
    Len:16, Next:8, Hop:8,
    SAddr:128/bits, DAddr:128/bits,
    Payload/binary>>
 ) ->
    {#ipv6{
        class = Class, flow = Flow,
        len = Len, next = Next, hop = Hop,
        saddr = SAddr, daddr = DAddr
       }, Payload};
ipv6(#ipv6{
        class = Class, flow = Flow,
        len = Len, next = Next, hop = Hop,
        saddr = SAddr, daddr = DAddr
       }) ->
    <<6:4, Class:8, Flow:20,
      Len:16, Next:8, Hop:8,
      SAddr:128/bits, DAddr:128/bits>>.

ipv6_header(Next, #ipv6_header{next = OldNext,
                               content = Content}) ->
    NextCode = ipv6_proto_code(Next, OldNext),
    HdrLen = byte_size(Content) + 2, % +2 = + NextCode + ExtLen
    HdrLen8 = (HdrLen + 7) div 8, % in 8-octet parts; +7 to round up
    ExtLen = HdrLen8 - 1, %% not including the first 8 octets
    <<NextCode:8,
      ExtLen:8,
      Content/binary>>;
ipv6_header(HeaderType, <<Next:8, ExtLen:8, Tail/binary>>) ->
    ContentLen = 8 * ExtLen + 8 - 2, %% -2 = - Next - ExtLen
    <<Content:ContentLen/binary, Payload/binary>> = Tail,
    {#ipv6_header{type = HeaderType,
                  next = Next,
                  content = Content}, Payload}.

%%
%% GRE
%%
gre(<<0:1,Res0:12,Ver:3,Type:16,Rest/binary>>) ->
    {#gre{c = 0, res0 = Res0, ver = Ver, type = Type
       },Rest
    };
gre(<<1:1,Res0:12,Ver:3,Type:16,Chksum:16,Res1:16,Rest/binary>>) ->
    {#gre{c = 1, res0 = Res0, ver = Ver, type = Type,
    chksum = Chksum, res1 = Res1
       },Rest
    };
gre(#gre{c = 0, res0 = Res0, ver = Ver, type = Type}) ->
    <<0:1,Res0:12,Ver:3,Type:16>>;
gre(#gre{c = 1, res0 = Res0, ver = Ver, type = Type,
    chksum = Chksum, res1 = Res1}) ->
    <<1:1,Res0:12,Ver:3,Type:16,Chksum:16,Res1:16>>.

%%
%% TCP
%%
tcp(
  <<SPort:16, DPort:16,
    SeqNo:32,
    AckNo:32,
    Off:4, 0:4, CWR:1, ECE:1, URG:1, ACK:1,
    PSH:1, RST:1, SYN:1, FIN:1, Win:16,
    Sum:16, Urp:16,
    Rest/binary>>
 ) when Off >= 5 ->
    {Opt, Payload} = options(Off, Rest),
    {#tcp{
        sport = SPort, dport = DPort,
        seqno = SeqNo,
        ackno = AckNo,
        off = Off, cwr = CWR, ece = ECE, urg = URG, ack = ACK,
        psh = PSH, rst = RST, syn = SYN, fin = FIN, win = Win,
        sum = Sum, urp = Urp,
        opt = Opt
       }, Payload};
tcp(#tcp{
       sport = SPort, dport = DPort,
       seqno = SeqNo,
       ackno = AckNo,
       off = Off, cwr = CWR, ece = ECE, urg = URG, ack = ACK,
       psh = PSH, rst = RST, syn = SYN, fin = FIN, win = Win,
       sum = Sum, urp = Urp, opt = Opt
      }) ->
    <<SPort:16, DPort:16,
      SeqNo:32,
      AckNo:32,
      Off:4, 0:4, CWR:1, ECE:1, URG:1, ACK:1,
      PSH:1, RST:1, SYN:1, FIN:1, Win:16,
      Sum:16, Urp:16, Opt/binary >>.

options(Offset, Payload) ->
    N = (Offset-5)*4,
    <<Opt:N/binary, Payload1/binary>> = Payload,
    {Opt, Payload1}.

%%
%% SCTP
%%
sctp(<<SPort:16, DPort:16, VerificationTag:32, CheckSum:32, Payload/binary>>) ->
    #sctp{sport = SPort, dport = DPort, vtag = VerificationTag, sum = CheckSum,
          chunks = sctp_chunks(Payload)};
sctp(#sctp{sport = SPort, dport = DPort, vtag = VerificationTag, sum = CheckSum,
           chunks = Chunks}) ->
    <<SPort:16, DPort:16, VerificationTag:32, CheckSum:32,
      (sctp_chunks(Chunks))/binary>>.

sctp_chunks(Chunks) when is_binary(Chunks) ->
    sctp_chunks(Chunks, []);
sctp_chunks(Chunks) when is_list(Chunks) ->
    << <<(sctp_chunk(Ch))/binary>> || Ch <- Chunks >>.

%% The padding MUST NOT be longer that 3 bytes.
sctp_chunks(Padding, ChunksAcc) when byte_size(Padding) =< 3  ->
    lists:reverse(ChunksAcc);
sctp_chunks(<<_:16, ChunkLength:16, _/binary>> = BinaryChunks, ChunksAcc) ->
    ChunkLengthInBits = ChunkLength*8,
    <<Chunk:ChunkLengthInBits/bits, BinaryChunksRemainder/binary>> = BinaryChunks,
    sctp_chunks(BinaryChunksRemainder, [sctp_chunk(Chunk) | ChunksAcc]).

sctp_chunk(<<Type:8, Flags:8, Length:16, Payload/binary>>) ->
    #sctp_chunk{type = Type, flags = Flags, len = Length,
                payload = sctp_chunk_payload(Type, Payload)};
sctp_chunk(#sctp_chunk{type = Type, flags = Flags, len = Length,
                              payload = Payload}) ->
    BinaryPayload = align_binary_to_4_bytes(sctp_chunk_payload(Type, Payload)),
    %% #sctp_chunk.len does not include 4 bytes of chunk header. It only
    %% indicates the length of the header payload.
    <<Type:8, Flags:8, Length:16, BinaryPayload/binary>>.

sctp_chunk_payload(0, <<Tsn:32, Sid:16, Ssn:16, Ppi:32, Data/binary>>) ->
   #sctp_chunk_data{tsn = Tsn, sid = Sid, ssn = Ssn, ppi = Ppi, data = Data};
sctp_chunk_payload(0, #sctp_chunk_data{tsn = Tsn, sid = Sid, ssn = Ssn,
                                       ppi = Ppi, data = Data}) ->
    <<Tsn:32, Sid:16, Ssn:16, Ppi:32, Data/binary>>;
sctp_chunk_payload(_, Data) ->
    Data.

%%
%% UDP
%%
udp(<<SPort:16, DPort:16, ULen:16, Sum:16, Payload/binary>>) ->
    {#udp{sport = SPort, dport = DPort, ulen = ULen, sum = Sum}, Payload};
udp(#udp{sport = SPort, dport = DPort, ulen = ULen, sum = Sum}) ->
    <<SPort:16, DPort:16, ULen:16, Sum:16>>.


%%
%% ICMP
%%

%% Destination Unreachable Message
icmp(<<?ICMP_DEST_UNREACH:8, Code:8, Checksum:16, Unused:32/bits, Payload/binary>>) ->
    {#icmp{
        type = ?ICMP_DEST_UNREACH, code = Code, checksum = Checksum, un = Unused
       }, Payload};
icmp(#icmp{
        type = ?ICMP_DEST_UNREACH, code = Code, checksum = Checksum, un = Unused
       }) ->
    <<?ICMP_DEST_UNREACH:8, Code:8, Checksum:16, Unused:32/bits>>;

%% Time Exceeded Message
icmp(<<?ICMP_TIME_EXCEEDED:8, Code:8, Checksum:16, Unused:32/bits, Payload/binary>>) ->
    {#icmp{
        type = ?ICMP_TIME_EXCEEDED, code = Code, checksum = Checksum, un = Unused
       }, Payload};
icmp(#icmp{
        type = ?ICMP_TIME_EXCEEDED, code = Code, checksum = Checksum, un = Unused
       }) ->
    <<?ICMP_TIME_EXCEEDED:8, Code:8, Checksum:16, Unused:32/bits>>;

%% Parameter Problem Message
icmp(<<?ICMP_PARAMETERPROB:8, Code:8, Checksum:16, Pointer:8, Unused:24/bits, Payload/binary>>) ->
    {#icmp{
        type = ?ICMP_PARAMETERPROB, code = Code, checksum = Checksum, pointer = Pointer,
        un = Unused
       }, Payload};
icmp(#icmp{
        type = ?ICMP_PARAMETERPROB, code = Code, checksum = Checksum, pointer = Pointer,
        un = Unused
       }) ->
    <<?ICMP_PARAMETERPROB:8, Code:8, Checksum:16, Pointer:8, Unused:24/bits>>;

%% Source Quench Message
icmp(<<?ICMP_SOURCE_QUENCH:8, 0:8, Checksum:16, Unused:32/bits, Payload/binary>>) ->
    {#icmp{
        type = ?ICMP_SOURCE_QUENCH, code = 0, checksum = Checksum, un = Unused
       }, Payload};
icmp(#icmp{
        type = ?ICMP_SOURCE_QUENCH, code = Code, checksum = Checksum, un = Unused
       }) ->
    <<?ICMP_SOURCE_QUENCH:8, Code:8, Checksum:16, Unused:32/bits>>;

%% Redirect Message
icmp(<<?ICMP_REDIRECT:8, Code:8, Checksum:16, DAddr:32/bits, Payload/binary>>) ->
    {#icmp{
        type = ?ICMP_REDIRECT, code = Code, checksum = Checksum, gateway = DAddr
       }, Payload};
icmp(#icmp{
        type = ?ICMP_REDIRECT, code = Code, checksum = Checksum, gateway = DAddr
       }) ->
    <<?ICMP_REDIRECT:8, Code:8, Checksum:16, DAddr:32/bits>>;

%% Echo or Echo Reply Message
icmp(<<Type:8, Code:8, Checksum:16, Id:16, Sequence:16, Payload/binary>>)
  when Type =:= ?ICMP_ECHO; Type =:= ?ICMP_ECHOREPLY ->
    {#icmp{
        type = Type, code = Code, checksum = Checksum, id = Id,
        sequence = Sequence
       }, Payload};
icmp(#icmp{
        type = Type, code = Code, checksum = Checksum, id = Id,
        sequence = Sequence
       })
  when Type =:= ?ICMP_ECHO; Type =:= ?ICMP_ECHOREPLY ->
    <<Type:8, Code:8, Checksum:16, Id:16, Sequence:16>>;

%% Timestamp or Timestamp Reply Message
icmp(<<Type:8, 0:8, Checksum:16, Id:16, Sequence:16, TS_Orig:32, TS_Recv:32, TS_Tx:32>>)
  when Type =:= ?ICMP_TIMESTAMP; Type =:= ?ICMP_TIMESTAMPREPLY ->
    {#icmp{
        type = Type, code = 0, checksum = Checksum, id = Id,
        sequence = Sequence, ts_orig = TS_Orig, ts_recv = TS_Recv, ts_tx = TS_Tx
       }, <<>>};
icmp(#icmp{
        type = Type, code = Code, checksum = Checksum, id = Id,
        sequence = Sequence, ts_orig = TS_Orig, ts_recv = TS_Recv, ts_tx = TS_Tx
       }) when Type =:= ?ICMP_TIMESTAMP; Type =:= ?ICMP_TIMESTAMPREPLY ->
    <<Type:8, Code:8, Checksum:16, Id:16, Sequence:16, TS_Orig:32, TS_Recv:32, TS_Tx:32>>;

%% Information Request or Information Reply Message
icmp(<<Type:8, 0:8, Checksum:16, Id:16, Sequence:16>>)
  when Type =:= ?ICMP_INFO_REQUEST; Type =:= ?ICMP_INFO_REPLY ->
    {#icmp{
        type = Type, code = 0, checksum = Checksum, id = Id,
        sequence = Sequence
       }, <<>>};
icmp(#icmp{
        type = Type, code = Code, checksum = Checksum, id = Id,
        sequence = Sequence
       }) when Type =:= ?ICMP_INFO_REQUEST; Type =:= ?ICMP_INFO_REPLY ->
    <<Type:8, Code:8, Checksum:16, Id:16, Sequence:16>>;

%% Catch/build arbitrary types
icmp(<<Type:8, Code:8, Checksum:16, Un:32, Payload/binary>>) ->
    {#icmp{
        type = Type, code = Code, checksum = Checksum, un = Un
       }, Payload};
icmp(#icmp{type = Type, code = Code, checksum = Checksum, un = Un}) ->
    <<Type:8, Code:8, Checksum:16, Un:32>>.


%%
%% Utility functions
%%

icmpv6(<<Type:8, Code:8, Checksum:16, Body/bits>>) ->
    {#icmpv6{type = Type, code = Code, checksum = Checksum}, Body};
icmpv6(#icmpv6{type = Type, code = Code, checksum = Checksum}) ->
    <<Type:8, Code:8, Checksum:16>>.

%%
%% NDP
%%

ndp_ns(<<_:32, TGTAddr:128/bits, Rest/binary>>) ->
    NDP = #ndp_ns{tgt_addr = TGTAddr},
    case lists:keyfind(?NDP_OPT_SLL, 1, parse_ndp_options(Rest)) of
        {?NDP_OPT_SLL, SLL} ->
            NDP#ndp_ns{sll = SLL};
        false ->
            NDP
    end;
ndp_ns(#ndp_ns{tgt_addr = TGTAddr, sll = SLL}) ->
    <<0:32, TGTAddr/binary, (ndp_addr(?NDP_OPT_SLL, SLL))/binary>>.

ndp_na(<<R:1, S:1, O:1, _:29, SRCAddr:128/bits, Rest/binary>>) ->
    NDP = #ndp_na{r = R, s = S, o = O, src_addr = SRCAddr},
    case lists:keyfind(?NDP_OPT_TLL, 1, parse_ndp_options(Rest)) of
        {?NDP_OPT_TLL, TLL} ->
            NDP#ndp_na{tll = TLL};
        false ->
            NDP
    end;
ndp_na(#ndp_na{src_addr = SRCAddr, r = R, s = S, o = O, tll = TLL}) ->
    <<R:1, S:1, O:1, 0:29, SRCAddr/binary, (ndp_addr(?NDP_OPT_TLL, TLL))/binary>>.

parse_ndp_options(<<>>) ->
    [];
parse_ndp_options(<<_Type:8, 0:8, Rest/binary>>) ->
    parse_ndp_options(Rest); % silently discard option of length 0
parse_ndp_options(<<Type:8, Len:8, Rest/binary>>) ->
    DataLen = 8 * Len - 2,
    <<Data:DataLen/binary, Next/binary>> = Rest,
    [{Type, Data} | parse_ndp_options(Next)].

ndp_addr(_, undefined) ->
    <<>>;
ndp_addr(Type, Value) ->
    Size = (byte_size(Value) + 2 + 7) div 8,
    <<Type:8, Size:8, Value/binary>>.

%%
%% Utility functions
%%

checksum(#ipv4{saddr = SAddr,
               daddr = DAddr},
         #tcp{} = TCPhdr,
         Payload) ->
    TCP = tcp(TCPhdr#tcp{sum = 0}),
    Len = size(TCP) + size(Payload),
    Pad = 8 * (Len rem 2),
    checksum(<<SAddr:32/bits,
               DAddr:32/bits,
               0:8, ?IPPROTO_TCP:8,
               Len:16,
               TCP/binary,
               Payload/bits,
               0:Pad>>);

checksum(#ipv6{saddr = SAddr,
               daddr = DAddr},
         #tcp{} = TCPhdr,
         Payload) ->
    TCP = tcp(TCPhdr#tcp{sum = 0}),
    Len = size(TCP) + size(Payload),
    Pad = 8 * (Len rem 2),
    checksum(<<SAddr:128/bits,
               DAddr:128/bits,
               Len:32,
               0:24, ?IPPROTO_TCP:8,
               TCP/binary,
               Payload/bits,
               0:Pad>>);

checksum(#ipv4{saddr = SAddr,
               daddr = DAddr},
         #udp{ulen = Len} = Hdr,
         Payload) ->
    UDP = udp(Hdr#udp{sum = 0}),
    Pad = bit_size(Payload) rem 16,
    checksum(<<SAddr:32/bits,
               DAddr:32/bits,
               0:8,
               ?IPPROTO_UDP:8,
               Len:16,
               UDP/binary,
               Payload/bits,
               0:Pad>>);

checksum(#ipv6{saddr = SAddr,
               daddr = DAddr},
         #udp{ulen = Len} = Hdr,
         Payload) ->
    UDP = udp(Hdr#udp{sum = 0}),
    Pad = bit_size(Payload) rem 16,
    checksum(<<SAddr:128/bits,
               DAddr:128/bits,
               Len:32,
               0:24,
               ?IPPROTO_UDP:8,
               UDP/binary,
               Payload/bits,
               0:Pad>>);

checksum(#ipv6{saddr = SAddr,
               daddr = DAddr},
         #icmpv6{} = Hdr,
         Payload) ->
    ICMP = icmpv6(Hdr#icmpv6{checksum = 0}),
    Len = size(ICMP) + size(Payload),
    checksum(<<SAddr:128/bits,
               DAddr:128/bits,
               Len:32,
               0:24,
               ?IPPROTO_ICMPV6:8,
               ICMP/binary,
               Payload/bits>>);

checksum(_, _, _) ->
    0.

checksum([IP, TransportLayer, Payload]) ->
    checksum(IP, TransportLayer, Payload);
checksum(#ipv4{} = H) ->
    checksum(ipv4(H#ipv4{sum=0}));

%% this is a hot spot for LINC switch; rewritten to avoid redundancy
%checksum(Hdr) ->
%    lists:foldl(fun compl/2, 0, [ W || <<W:16>> <= Hdr ]).
%
%compl(N) when N =< 16#FFFF -> N;
%compl(N) -> (N band 16#FFFF) + (N bsr 16).
%compl(N,S) -> compl(N+S).

checksum(Bin) ->
	checksum1(Bin, 0).

checksum1(<<>>, Acc) ->
	checksum_fix(Acc);
checksum1(<<W:16,More/binary>>, Acc) ->
    checksum1(More, W + Acc).

checksum_fix(N) when N =< 16#ffff -> N;
checksum_fix(N) -> checksum_fix((N band 16#ffff) + (N bsr 16)).

%% Even better - use a BIF
%checksum(Bin) ->
%	binary:ip_checksum(Bin).

makesum(Hdr) -> 16#FFFF - checksum(Hdr).

valid(16#FFFF) -> true;
valid(_) -> false.

find_ip([#ipv4{} = IP | _]) -> {ok, IP};
find_ip([#ipv6{} = IP | _]) -> {ok, IP};
find_ip([_ | Tail]) -> find_ip(Tail);
find_ip([]) -> {error, no_ip}.

align_binary_to_4_bytes(<<Binary/binary>>) ->
    case byte_size(Binary) rem 4 of
        0 ->
            Binary;
        BinarySizeInBytes ->
            PaddingSizeInBits = (4 - BinarySizeInBytes) * 8,
            <<Binary/binary, 0:PaddingSizeInBits>>
    end.

%%
%% Datalink types
%%
dlt(?DLT_NULL) -> null;
dlt(?DLT_EN10MB) -> en10mb;
dlt(?DLT_EN3MB) -> en3mb;
dlt(?DLT_AX25) -> ax25;
dlt(?DLT_PRONET) -> pronet;
dlt(?DLT_CHAOS) -> chaos;
dlt(?DLT_IEEE802) -> ieee802;
dlt(?DLT_ARCNET) -> arcnet;
dlt(?DLT_SLIP) -> slip;
dlt(?DLT_PPP) -> ppp;
dlt(?DLT_FDDI) -> fddi;
dlt(?DLT_ATM_RFC1483) -> atm_rfc1483;
dlt(?DLT_RAW) -> raw;
dlt(?DLT_SLIP_BSDOS) -> slip_bsdos;
dlt(?DLT_PPP_BSDOS) -> ppp_bsdos;
dlt(?DLT_PFSYNC) -> pfsync;
dlt(?DLT_ATM_CLIP) -> atm_clip;
dlt(?DLT_PPP_SERIAL) -> ppp_serial;
%% dlt(?DLT_C_HDLC) -> c_hdlc;
dlt(?DLT_CHDLC) -> chdlc;
dlt(?DLT_IEEE802_11) -> ieee802_11;
dlt(?DLT_LOOP) -> loop;
dlt(?DLT_LINUX_SLL) -> linux_sll;
dlt(?DLT_PFLOG) -> pflog;
dlt(?DLT_IEEE802_11_RADIO) -> ieee802_11_radio;
dlt(?DLT_APPLE_IP_OVER_IEEE1394) -> apple_ip_over_ieee1394;
dlt(?DLT_IEEE802_11_RADIO_AVS) -> ieee802_11_radio_avs;

dlt(null) -> ?DLT_NULL;
dlt(en10mb) -> ?DLT_EN10MB;
dlt(en3mb) -> ?DLT_EN3MB;
dlt(ax25) -> ?DLT_AX25;
dlt(pronet) -> ?DLT_PRONET;
dlt(chaos) -> ?DLT_CHAOS;
dlt(ieee802) -> ?DLT_IEEE802;
dlt(arcnet) -> ?DLT_ARCNET;
dlt(slip) -> ?DLT_SLIP;
dlt(ppp) -> ?DLT_PPP;
dlt(fddi) -> ?DLT_FDDI;
dlt(atm_rfc1483) -> ?DLT_ATM_RFC1483;
dlt(raw) -> ?DLT_RAW;
dlt(slip_bsdos) -> ?DLT_SLIP_BSDOS;
dlt(ppp_bsdos) -> ?DLT_PPP_BSDOS;
dlt(pfsync) -> ?DLT_PFSYNC;
dlt(atm_clip) -> ?DLT_ATM_CLIP;
dlt(ppp_serial) -> ?DLT_PPP_SERIAL;
dlt(c_hdlc) -> ?DLT_C_HDLC;
dlt(chdlc) -> ?DLT_CHDLC;
dlt(ieee802_11) -> ?DLT_IEEE802_11;
dlt(loop) -> ?DLT_LOOP;
dlt(linux_sll) -> ?DLT_LINUX_SLL;
dlt(pflog) -> ?DLT_PFLOG;
dlt(ieee802_11_radio) -> ?DLT_IEEE802_11_RADIO;
dlt(apple_ip_over_ieee1394) -> ?DLT_APPLE_IP_OVER_IEEE1394;
dlt(ieee802_22_radio_avs) -> ?DLT_IEEE802_11_RADIO_AVS.
