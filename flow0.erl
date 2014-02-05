-module(flow0).
-export([arp/15,icmp/15,icmpv6/17,tcp/15,udp/15,sctp/15,nonext/14]).
arp(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) ->
    miss.
icmp(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) ->
    miss.
icmpv6(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) ->
    miss.
tcp(<<0,1,2,3,_:16/bits,0,16,32,48,64,80,_/binary>> = Packet,
    _ = Actions,
    none = VlanTag,
    2048 = EthType,
    <<_:4/bits,1:1,_:11/bits,3:4,_/bits>> = PbbTag,
    <<17,34,53,_/binary>> = MplsTag,
    <<_:8/bits,
      170,
      _:56/bits,
      6,
      _:16/bits,
      192,
      168,
      _:8/bits,
      5,
      8,
      8,
      8,
      8,
      _/binary>> =
        Ip4Hdr,
    _ = Ip6Hdr,
    _ = Ip6Ext,
    <<48,57,0,80,_/binary>> = TcpHdr,
    1 = InPort,
    _ = InPhyPort,
    <<_:48/bits,2:4,_/bits>> = Metadata,
    _ = TunnelId,
    _ = St) ->
    case linc_max:meter(3, St) of
        ok ->
            flow1:tcp(Packet,
                      begin
                          {Queue,_,_} = Actions,
                          {Queue,2,7}
                      end,
                      VlanTag,
                      EthType,
                      PbbTag,
                      MplsTag,
                      Ip4Hdr,
                      Ip6Hdr,
                      Ip6Ext,
                      TcpHdr,
                      InPort,
                      InPhyPort,
                      linc_max:update_metadata(Metadata, -65284, 15873),
                      TunnelId,
                      St);
        _ ->
            drop
    end;
tcp(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) ->
    miss.
udp(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) ->
    miss.
sctp(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) ->
    miss.
nonext(_, _, _, _, _, _, _, _, _, _, _, _, _, _) ->
    miss.
