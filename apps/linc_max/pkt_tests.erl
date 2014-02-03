-module(pkt_tests).

-include_lib("eunit/include/eunit.hrl").
-include("pkt.hrl").

-define(TRANSPORT_LAYER_HEADERS, [tcp, udp, sctp, none]).
-define(TRANSPORT_NEXT_HEADERS(UpperProtocol),
        case UpperProtocol of
            none ->
                [icmp, icmpv6, ipv6_no_next];
            _ ->
                [ipv4, ipv6]
        end).

-define(INTERNET_NEXT_HEADERS, [ether, ieee802_11q, mpls]).

-define(MPLS_NEXT_HEADERS, ?INTERNET_NEXT_HEADERS -- [mpls]).
-define(MPLS_LABEL_MAX, 16#FFFFF).
-define(MPLS_ETHER_TYPES,
        [{unicast, ?ETH_P_MPLS_UNI}, {multicast, ?ETH_P_MPLS_MULTI}]).

-define(SCTP_DATA_CHUNK_HEADER_SIZE, 16).

%% Tests generators ------------------------------------------------------------

encapsulate_decapsulate_test_() ->
    {setup,
     fun setup/0,
     fun(_) -> ok end,
     [
      {"Test if the pacekt model is encapsulated.",
       fun encapsulate/0},
      {"Test if the packet is decapsulated to the model",
       fun decapsulate/0},
      {"Test if the initial packet matches after encapsulation and
       decapsulation",
       fun encapsulate_decapsulate/0}
     ]
    }.

%% Tests -----------------------------------------------------------------------

encapsulate() ->
    [?assert(is_binary(pkt:encapsulate(generate_packet_model())))
     || _ <- lists:seq(1, 100)].

decapsulate() ->
    [begin
         BinaryPacket = pkt:encapsulate(generate_packet_model()),
         ?assert(is_list(pkt:decapsulate(BinaryPacket)))
     end || _ <- lists:seq(1, 100)].

encapsulate_decapsulate() ->
    [begin
         PacketModel = generate_packet_model(),
         Decapsulated = pkt:decapsulate(pkt:encapsulate(PacketModel)),
         ?assertEqual(PacketModel, restore_computed_fields(Decapsulated))
     end || _ <- lists:seq(1, 100)].

%% Fixtures -------------------------------------------------------------------

setup() ->
    random:seed(erlang:now()).

%% Helper functions ------------------------------------------------------------

%% @private Generate random payload and randomly add subsequent TCP/IP model
%% headers
generate_packet_model() ->
    add_header(random_list_element(?TRANSPORT_LAYER_HEADERS),
               generate_payload()).

%% @private Add a Transport layer header (there can be no such header
%% in a packet)
add_header(TransportProtocol = tcp, Payload) ->
    add_header({random_list_element(?TRANSPORT_NEXT_HEADERS(TransportProtocol)),
                ?IPPROTO_TCP}, [#tcp{} , Payload]);
add_header(TransportProtocol = udp, Payload) ->
    add_header({random_list_element(?TRANSPORT_NEXT_HEADERS(TransportProtocol)),
                ?IPPROTO_UDP}, [#udp{} , Payload]);
add_header(TransportProtocol = none, Payload) ->
    add_header({random_list_element(?TRANSPORT_NEXT_HEADERS(TransportProtocol)),
                none}, Payload);
add_header(TransportProtocol = sctp, Payload) ->
    add_header({random_list_element(?TRANSPORT_NEXT_HEADERS(TransportProtocol)),
                ?IPPROTO_SCTP},
               [#sctp{chunks = [generate_sctp_data_chunk(Payload)]}]);

%% @ private Add an Internet layer header
add_header({icmp, none}, Payload) ->
    add_header({random_list_element(?INTERNET_NEXT_HEADERS), ?ETH_P_IP},
               [#ipv4{p = ?IPPROTO_ICMP}, #icmp{}, Payload]);
add_header({icmpv6, none}, Payload) ->
    add_header({random_list_element(?INTERNET_NEXT_HEADERS), ?ETH_P_IPV6},
               [#ipv6{next = ?IPPROTO_ICMPV6,
                      hop = 64,
                      saddr = <<0:112, 1:16>>,
                      daddr = <<0:112, 1:16>>},
                #icmpv6{}, {unsupported, Payload}]);
add_header({ipv6_no_next, none}, Payload) ->
    add_header({random_list_element(?INTERNET_NEXT_HEADERS), ?ETH_P_IPV6},
               [#ipv6{next = ?IPV6_HDR_NO_NEXT_HEADER,
                      hop = 64,
                      saddr = <<0:112, 1:16>>,
                      daddr = <<0:112, 1:16>>},
                Payload]);
add_header({ipv4, UpperProtocol}, Payload) ->
    add_header({random_list_element(?INTERNET_NEXT_HEADERS), ?ETH_P_IP},
               [#ipv4{p = UpperProtocol} | Payload]);
add_header({ipv6, UpperProtocol}, Payload) ->
    add_header({random_list_element(?INTERNET_NEXT_HEADERS), ?ETH_P_IPV6},
               [#ipv6{next = UpperProtocol,
                      hop = 64,
                      saddr = <<0:112, 1:16>>,
                      daddr = <<0:112, 1:16>>}
                | Payload]);

%% @ private Add an "2.5 layer" header
%% NOTICE: Maximum 2 MPLS headers will be added
add_header({mpls, EtherType}, [#mpls_tag{stack = Stack} = MPLSTag | Payload]) ->
    StackEntry = #mpls_stack_entry{
      label = <<(random:uniform(?MPLS_LABEL_MAX - 15) + 15):20>>},
    add_header({link, EtherType},
               [MPLSTag#mpls_tag{stack = [StackEntry | Stack]} | Payload]);
add_header({mpls, _UpperProtocol}, Payload) ->
    StackEntry = #mpls_stack_entry{
      label = <<(random:uniform(?MPLS_LABEL_MAX - 15) + 15):20>>, bottom = 1},
    {Mode, EtherType} =  random_list_element(?MPLS_ETHER_TYPES),
    add_header({random_list_element(?MPLS_NEXT_HEADERS), EtherType},
               [#mpls_tag{stack = [StackEntry], mode = Mode} | Payload]);

%% @ private Add a Link layer header
%% TODO: Add support for 802.1ad (http://en.wikipedia.org/wiki/802.1ad)
add_header({ether, EtherType}, Payload) ->
    [#ether{type = EtherType} | Payload];
add_header({ieee802_11q, EtherType}, Payload) ->
    add_header({ether, ?ETH_P_802_1Q},
               [#ieee802_1q_tag{vid = <<(random:uniform(16#FFF)-1):12>>,
                                ether_type = EtherType}
                | Payload]).

%% @private Restores fields that are computed during packet encapsulation to
%% their default values.
restore_computed_fields(Packet) ->
    lists:reverse(restore_computed_fields(Packet, [])).

restore_computed_fields([], Packet) ->
    Packet;
restore_computed_fields([#tcp{} = TcpHeder | Rest], Packet) ->
    restore_computed_fields(Rest, [TcpHeder#tcp{sum = 0} | Packet]);
restore_computed_fields([#udp{} = UdpHeder | Rest], Packet) ->
    restore_computed_fields(Rest, [UdpHeder#udp{sum = 0, ulen = 8} | Packet]);
restore_computed_fields([#icmp{} = IcmpHeader | Rest], Packet) ->
    restore_computed_fields(Rest, [IcmpHeader#icmp{checksum = 0} | Packet]);
restore_computed_fields([#icmpv6{} = Icmpv6Header | Rest], Packet) ->
    restore_computed_fields(Rest, [Icmpv6Header#icmpv6{checksum = 0} | Packet]);
restore_computed_fields([#ipv4{} = IPv4Header | Rest], Packet) ->
    restore_computed_fields(Rest, [IPv4Header#ipv4{len = 20, sum = 0} | Packet]);
restore_computed_fields([#ipv6{} = IPv6Header | Rest], Packet) ->
    restore_computed_fields(Rest, [IPv6Header#ipv6{len = 40} | Packet]);
restore_computed_fields([Header | Rest], Packet) ->
    restore_computed_fields(Rest, [Header | Packet]).

%% @priavet Generate random binary payload.
generate_payload() ->
    generate_payload(random:uniform(1000) - 1).

%% @private Generate random binary payload of the given max lenght in bytes.
generate_payload(0) ->
  <<>>;
generate_payload(MexLength) ->
    << <<(random:uniform(255))>>
       || _ <- lists:seq(1, random:uniform(MexLength) div 8) >>.

%% @private Return random element of a list
random_list_element(List) ->
    lists:nth(random:uniform(length(List)), List).

generate_sctp_data_chunk(Payload) ->
    #sctp_chunk{len = byte_size(Payload) + ?SCTP_DATA_CHUNK_HEADER_SIZE,
                payload = #sctp_chunk_data{data = Payload}}.
