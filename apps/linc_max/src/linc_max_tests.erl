-module(linc_max_tests).
-export([all/0]).

%% mock flow table interface
-export([match/23]).

-include("linc_max.hrl").

match(_ = _Packet,
      _ = VlanTag,
      _ = EthType,
      _ = PbbTag,
      _ = MplsTag,
      _ = Ip4Hdr,
      _ = Ip6Hdr,
      _ = Ip6Ext,
      _ = IpTclass,
      _ = IpProto,
      _ = ArpMsg,
      _ = IcmpMsg,
      _ = Icmp6Hdr,
      _ = Icmp6OptSll,
      _ = Icmp6OptTll,
      _ = TcpHdr,
      _ = UdpHdr,
      _ = SctpHdr,
      _ = _Metadata,
      _ = _InPort,
      _ = _InPhyPort,
      _ = _TunnelId,
      _ = _Actions) ->
	Hdrs = 
		[{vlan_tag,VlanTag},
		 {eth_type,EthType},
		 {pbb_tag,PbbTag},
		 {mpls_tag,MplsTag},
		 {ip4_hdr,Ip4Hdr},
		 {ip6_hdr,Ip6Hdr},
		 {ip6_ext,Ip6Ext},
		 {ip_tclass,IpTclass},
		 {ip_proto,IpProto},
		 {arp_msg,ArpMsg},
		 {icmp_msg,IcmpMsg},
		 {icmp6_hdr,Icmp6Hdr},
		 {icmp6_sll,Icmp6OptSll},
		 {icmp6_tll,Icmp6OptTll},
		 {tcp_hdr,TcpHdr},
		 {udp_hdr,UdpHdr},
		 {sctp_hdr,SctpHdr}],

	lists:filter(fun({_,V}) -> V =/= undefined end, Hdrs).

all() ->
	Frame = <<224,105,149,59,163,24,0,22,182,181,62,198,8,0,69,0,0,54,2,108,64,
          0,53,6,172,243,173,192,82,195,192,168,213,54,0,80,143,166,75,154,
          212,181,116,33,53,92,128,24,0,126,60,199,0,0,1,1,8,10,92,104,96,
          16,22,69,237,136,137,0>>,

	linc_max_preparser:inject(Frame,
			0, {1,10,<<0:64>>}, #actions{}, linc_max_tests).

%%EOF
