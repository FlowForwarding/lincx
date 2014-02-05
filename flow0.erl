-module(flow0).
-export([arp/15,icmp/15,icmpv6/17,tcp/15,udp/15,sctp/15,nonext/14]).
arp(_ = Packet,
    _ = Actions,
    _ = VlanTag,
    _ = EthType,
    _ = PbbTag,
    _ = MplsTag,
    _ = Ip4Hdr,
    _ = Ip6Hdr,
    _ = Ip6Ext,
    _ = ArpMsg,
    1 = InPort,
    _ = InPhyPort,
    _ = Metadata,
    _ = TunnelId,
    _ = St) ->
    case linc_max:meter(12, St) of
        ok ->
            flow1:arp(Packet,
                      {undefined,2,undefined},
                      VlanTag,
                      EthType,
                      PbbTag,
                      MplsTag,
                      Ip4Hdr,
                      Ip6Hdr,
                      Ip6Ext,
                      ArpMsg,
                      InPort,
                      InPhyPort,
                      linc_max:update_metadata(Metadata, -256, 62),
                      TunnelId,
                      St);
        _ ->
            drop
    end;
arp(_, _ = Actions, _, _, _, _, _, _, _, _, 2, _, _, _, _ = St) ->
    case linc_max:meter(12, St) of
        ok ->
            {actions,
             begin
                 {Queue,_,Group} = Actions,
                 {Queue,1,Group}
             end};
        _ ->
            drop
    end.
icmp(_ = Packet,
     _ = Actions,
     _ = VlanTag,
     _ = EthType,
     _ = PbbTag,
     _ = MplsTag,
     _ = Ip4Hdr,
     _ = Ip6Hdr,
     _ = Ip6Ext,
     _ = IcmpMsg,
     1 = InPort,
     _ = InPhyPort,
     _ = Metadata,
     _ = TunnelId,
     _ = St) ->
    case linc_max:meter(12, St) of
        ok ->
            flow1:icmp(Packet,
                       {undefined,2,undefined},
                       VlanTag,
                       EthType,
                       PbbTag,
                       MplsTag,
                       Ip4Hdr,
                       Ip6Hdr,
                       Ip6Ext,
                       IcmpMsg,
                       InPort,
                       InPhyPort,
                       linc_max:update_metadata(Metadata, -256, 62),
                       TunnelId,
                       St);
        _ ->
            drop
    end;
icmp(_, _ = Actions, _, _, _, _, _, _, _, _, 2, _, _, _, _ = St) ->
    case linc_max:meter(12, St) of
        ok ->
            {actions,
             begin
                 {Queue,_,Group} = Actions,
                 {Queue,1,Group}
             end};
        _ ->
            drop
    end.
icmpv6(_ = Packet,
       _ = Actions,
       _ = VlanTag,
       _ = EthType,
       _ = PbbTag,
       _ = MplsTag,
       _ = Ip4Hdr,
       _ = Ip6Hdr,
       _ = Ip6Ext,
       _ = Icmp6Hdr,
       _ = Icmp6OptSll,
       _ = Icmp6OptTll,
       1 = InPort,
       _ = InPhyPort,
       _ = Metadata,
       _ = TunnelId,
       _ = St) ->
    case linc_max:meter(12, St) of
        ok ->
            flow1:icmpv6(Packet,
                         {undefined,2,undefined},
                         VlanTag,
                         EthType,
                         PbbTag,
                         MplsTag,
                         Ip4Hdr,
                         Ip6Hdr,
                         Ip6Ext,
                         Icmp6Hdr,
                         Icmp6OptSll,
                         Icmp6OptTll,
                         InPort,
                         InPhyPort,
                         linc_max:update_metadata(Metadata, -256, 62),
                         TunnelId,
                         St);
        _ ->
            drop
    end;
icmpv6(_, _ = Actions, _, _, _, _, _, _, _, _, _, _, 2, _, _, _, _ = St) ->
    case linc_max:meter(12, St) of
        ok ->
            {actions,
             begin
                 {Queue,_,Group} = Actions,
                 {Queue,1,Group}
             end};
        _ ->
            drop
    end.
tcp(_ = Packet,
    _ = Actions,
    _ = VlanTag,
    _ = EthType,
    _ = PbbTag,
    _ = MplsTag,
    _ = Ip4Hdr,
    _ = Ip6Hdr,
    _ = Ip6Ext,
    _ = TcpHdr,
    1 = InPort,
    _ = InPhyPort,
    _ = Metadata,
    _ = TunnelId,
    _ = St) ->
    case linc_max:meter(12, St) of
        ok ->
            flow1:tcp(Packet,
                      {undefined,2,undefined},
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
                      linc_max:update_metadata(Metadata, -256, 62),
                      TunnelId,
                      St);
        _ ->
            drop
    end;
tcp(_, _ = Actions, _, _, _, _, _, _, _, _, 2, _, _, _, _ = St) ->
    case linc_max:meter(12, St) of
        ok ->
            {actions,
             begin
                 {Queue,_,Group} = Actions,
                 {Queue,1,Group}
             end};
        _ ->
            drop
    end.
udp(_ = Packet,
    _ = Actions,
    _ = VlanTag,
    _ = EthType,
    _ = PbbTag,
    _ = MplsTag,
    _ = Ip4Hdr,
    _ = Ip6Hdr,
    _ = Ip6Ext,
    _ = UdpHdr,
    1 = InPort,
    _ = InPhyPort,
    _ = Metadata,
    _ = TunnelId,
    _ = St) ->
    case linc_max:meter(12, St) of
        ok ->
            flow1:udp(Packet,
                      {undefined,2,undefined},
                      VlanTag,
                      EthType,
                      PbbTag,
                      MplsTag,
                      Ip4Hdr,
                      Ip6Hdr,
                      Ip6Ext,
                      UdpHdr,
                      InPort,
                      InPhyPort,
                      linc_max:update_metadata(Metadata, -256, 62),
                      TunnelId,
                      St);
        _ ->
            drop
    end;
udp(_, _ = Actions, _, _, _, _, _, _, _, _, 2, _, _, _, _ = St) ->
    case linc_max:meter(12, St) of
        ok ->
            {actions,
             begin
                 {Queue,_,Group} = Actions,
                 {Queue,1,Group}
             end};
        _ ->
            drop
    end.
sctp(_ = Packet,
     _ = Actions,
     _ = VlanTag,
     _ = EthType,
     _ = PbbTag,
     _ = MplsTag,
     _ = Ip4Hdr,
     _ = Ip6Hdr,
     _ = Ip6Ext,
     _ = SctpHdr,
     1 = InPort,
     _ = InPhyPort,
     _ = Metadata,
     _ = TunnelId,
     _ = St) ->
    case linc_max:meter(12, St) of
        ok ->
            flow1:sctp(Packet,
                       {undefined,2,undefined},
                       VlanTag,
                       EthType,
                       PbbTag,
                       MplsTag,
                       Ip4Hdr,
                       Ip6Hdr,
                       Ip6Ext,
                       SctpHdr,
                       InPort,
                       InPhyPort,
                       linc_max:update_metadata(Metadata, -256, 62),
                       TunnelId,
                       St);
        _ ->
            drop
    end;
sctp(_, _ = Actions, _, _, _, _, _, _, _, _, 2, _, _, _, _ = St) ->
    case linc_max:meter(12, St) of
        ok ->
            {actions,
             begin
                 {Queue,_,Group} = Actions,
                 {Queue,1,Group}
             end};
        _ ->
            drop
    end.
nonext(_ = Packet,
       _ = Actions,
       _ = VlanTag,
       _ = EthType,
       _ = PbbTag,
       _ = MplsTag,
       _ = Ip4Hdr,
       _ = Ip6Hdr,
       _ = Ip6Ext,
       1 = InPort,
       _ = InPhyPort,
       _ = Metadata,
       _ = TunnelId,
       _ = St) ->
    case linc_max:meter(12, St) of
        ok ->
            flow1:nonext(Packet,
                         {undefined,2,undefined},
                         VlanTag,
                         EthType,
                         PbbTag,
                         MplsTag,
                         Ip4Hdr,
                         Ip6Hdr,
                         Ip6Ext,
                         InPort,
                         InPhyPort,
                         linc_max:update_metadata(Metadata, -256, 62),
                         TunnelId,
                         St);
        _ ->
            drop
    end;
nonext(_, _ = Actions, _, _, _, _, _, _, _, 2, _, _, _, _ = St) ->
    case linc_max:meter(12, St) of
        ok ->
            {actions,
             begin
                 {Queue,_,Group} = Actions,
                 {Queue,1,Group}
             end};
        _ ->
            drop
    end.
