# OpenFlow 1.3 conformance

LINCX runs [Ryu suite](https://github.com/osrg/ryu/tree/master/ryu/tests/switch/of13) 
on every commit. 
You can compare it results with [other swithes](http://osrg.github.io/ryu/certification.html).

## Total
OK(934) / ERROR(57)
## Details

### ACTION: 

| Test                           |IPv4   |IPv6   |ARP    |
|--------------------------------|-------|-------|-------|
| **OUTPUT**                     | ✓     | ✓     | ✓     |
| PUSH_VLAN                      | ✓     | ✓     | ✓     |
| PUSH_MPLS                      | ✓     | ✓     | ✓     |
| PUSH_PBB                       | ✓     | ✓     | ✓     |
| PUSH_VLAN (multiple)           | ✓     | ✓     | ✓     |
| POP_VLAN                       | ✓     | ✓     | ✓     |
| COPY_TTL_OUT                   | ✓     | -     |
| COPY_TTL_IN                    | ✓     | -     |
| SET_MPLS_TTL                   | ✓     | ✓     | ✓     |
| DEC_MPLS_TTL                   | ✓     | ✓     | ✓     |
| PUSH_MPLS (multiple)           | ✓     | ✓     | ✓     |
| POP_MPLS                       | ✓     | ✓     | ✓     |
| PUSH_PBB (multiple)            | ✓     | ✓     | ✓     |
| POP_PBB                        | ✓     | ✓     | ✓     |

### ACTION: 

| Test                           |ether  |vlan   |mpls   |pbb    |
|--------------------------------|-------|-------|-------|-------|
| SET_NW_TTL (IPv4)              | ✓     | ✓     | ✓     | ✓     |
| DEC_NW_TTL (IPv4)              | ✓     | ✓     | ✓     | ✓     |
| SET_NW_TTL (IPv6)              | -     | -     | -     | -     |
| DEC_NW_TTL (IPv6)              | -     | -     | -     | -     |

### ACTION: SET_FIELD: 

| Test                           |IPv4   |IPv6   |ARP    |
|--------------------------------|-------|-------|-------|
| ETH_DST                        | ✓     | ✓     | ✓     |
| ETH_SRC                        | ✓     | ✓     | ✓     |
| ETH_TYPE                       | ✓     | ✓     | ✓     |
| TUNNEL_ID                      | ✓     | ✓     | ✓     |
| VLAN_VID                       | ✓     | ✓     | ✓     |
| VLAN_PCP                       | ✓     | ✓     | ✓     |
| MPLS_LABEL                     | ✓     | ✓     | ✓     |
| MPLS_TC                        | ✓     | ✓     | ✓     |
| MPLS_BOS                       | ✓     | ✓     | ✓     |
| PBB_ISID                       | ✓     | ✓     | ✓     |

### ACTION: SET_FIELD: 

| Test                           |ether  |vlan   |mpls   |pbb    |
|--------------------------------|-------|-------|-------|-------|
| IP_DSCP (IPv4)                 | ✓     | ✓     | ✓     | ✓     |
| IP_ECN (IPv4)                  | ✓     | ✓     | ✓     | ✓     |
| IP_PROTO (IPv4)                | -     | -     | -     | -     |
| IPV4_SRC                       | ✓     | ✓     | ✓     | ✓     |
| IPV4_DST                       | ✓     | ✓     | ✓     | ✓     |
| TCP_SRC (IPv4)                 | ✓     | ✓     | ✓     | ✓     |
| TCP_DST (IPv4)                 | ✓     | ✓     | ✓     | ✓     |
| UDP_SRC (IPv4)                 | ✓     | ✓     | ✓     | ✓     |
| UDP_DST (IPv4)                 | ✓     | ✓     | ✓     | ✓     |
| SCTP_SRC (IPv4)                | ✓     | ✓     | ✓     | ✓     |
| SCTP_DST (IPv4)                | ✓     | ✓     | ✓     | ✓     |
| ICMPV4_TYPE                    | ✓     | ✓     | ✓     | ✓     |
| ICMPV4_CODE                    | ✓     | ✓     | ✓     | ✓     |
| IP_DSCP (IPv6)                 | ✓     | ✓     | ✓     | ✓     |
| IP_ECN (IPv6)                  | ✓     | ✓     | ✓     | ✓     |
| IP_PROTO (IPv6)                | -     | -     | -     | -     |
| TCP_SRC (IPv6)                 | ✓     | ✓     | ✓     | ✓     |
| TCP_DST (IPv6)                 | ✓     | ✓     | ✓     | ✓     |
| UDP_SRC (IPv6)                 | ✓     | ✓     | ✓     | ✓     |
| UDP_DST (IPv6)                 | ✓     | ✓     | ✓     | ✓     |
| SCTP_SRC (IPv6)                | ✓     | ✓     | ✓     | ✓     |
| SCTP_DST (IPv6)                | ✓     | ✓     | ✓     | ✓     |
| IPV6_SRC                       | ✓     | ✓     | ✓     | ✓     |
| IPV6_DST                       | ✓     | ✓     | ✓     | ✓     |
| IPV6_FLABEL                    | ✓     | ✓     | ✓     | ✓     |
| ICMPV6_TYPE                    | ✓     | ✓     | ✓     | ✓     |
| ICMPV6_CODE                    | ✓     | ✓     | ✓     | ✓     |
| IPV6_ND_TARGET                 | ✓     | ✓     | ✓     | ✓     |
| IPV6_ND_SLL                    | ✓     | ✓     | ✓     | ✓     |
| IPV6_ND_TLL                    | ✓     | ✓     | ✓     | ✓     |
| ARP_OP                         | ✓     | ✓     | ✓     | ✓     |
| ARP_SPA                        | ✓     | ✓     | ✓     | ✓     |
| ARP_TPA                        | ✓     | ✓     | ✓     | ✓     |
| ARP_SHA                        | ✓     | ✓     | ✓     | ✓     |
| ARP_THA                        | ✓     | ✓     | ✓     | ✓     |

### MATCH: 

| Test                           |IPv4   |IPv6   |ARP    |
|--------------------------------|-------|-------|-------|
| **IN_PORT**                    |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| METADATA                       |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| METADATA (Mask)                |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| **ETH_DST**                    |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| **ETH_DST (Mask)**             |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| **ETH_SRC**                    |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| **ETH_SRC (Mask)**             |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| **ETH_TYPE**                   |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| TUNNEL_ID                      |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| TUNNEL_ID (Mask)               |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| VLAN_VID                       |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| VLAN_VID (Mask)                |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| VLAN_PCP                       |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| MPLS_LABEL                     |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| MPLS_TC                        |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| MPLS_BOS                       |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| PBB_ISID                       |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| PBB_ISID (Mask)                |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |

### MATCH: 

| Test                           |ether  |vlan   |mpls   |pbb    |
|--------------------------------|-------|-------|-------|-------|
| IP_DSCP (IPv4)                 |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| IP_ECN (IPv4)                  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| **IP_PROTO (IPv4)**            |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| **IPV4_SRC**                   |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| **IPV4_SRC (Mask)**            |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| **IPV4_DST**                   |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| **IPV4_DST (Mask)**            |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| **TCP_SRC (IPv4)**             |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| **TCP_DST (IPv4)**             |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| **UDP_SRC (IPv4)**             |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| **UDP_DST (IPv4)**             |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| SCTP_SRC (IPv4)                |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| SCTP_DST (IPv4)                |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| ICMPV4_TYPE                    |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| ICMPV4_CODE                    |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| IP_DSCP (IPv6)                 |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| IP_ECN (IPv6)                  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| **IP_PROTO (IPv6)**            |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| **TCP_SRC (IPv6)**             |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| **TCP_DST (IPv6)**             |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| **UDP_SRC (IPv6)**             |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| **UDP_DST (IPv6)**             |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| SCTP_SRC (IPv6)                |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| SCTP_DST (IPv6)                |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| **IPV6_SRC**                   |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| **IPV6_SRC (Mask)**            |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| **IPV6_DST**                   |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| **IPV6_DST (Mask)**            |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| IPV6_FLABEL                    |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| IPV6_FLABEL (Mask)             |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| ICMPV6_TYPE                    |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| ICMPV6_CODE                    |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| IPV6_ND_TARGET                 |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| IPV6_ND_SLL                    |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| IPV6_ND_TLL                    |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| IPV6_EXTHDR                    |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| IPV6_EXTHDR (Mask)             |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| ARP_OP                         |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| ARP_SPA                        |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| ARP_SPA (Mask)                 |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| ARP_TPA                        |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| ARP_TPA (Mask)                 |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| ARP_SHA                        |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| ARP_SHA (Mask)                 |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| ARP_THA                        |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |
| ARP_THA (Mask)                 |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |  ✓✓✓  |

### METER: 

| Test                           |IPv4   |IPv6   |ARP    |
|--------------------------------|-------|-------|-------|
| DROP_00_KBPS_00_1M             | -     | -     | -     |
| DROP_00_KBPS_01_10M            | -     | -     | -     |
| DROP_00_KBPS_02_100M           | -     | -     | -     |
| DROP_01_PKTPS_00_100           | -     | -     | -     |
| DROP_01_PKTPS_01_1000          | -     | -     | -     |
| DROP_01_PKTPS_02_10000         | -     | -     | -     |
| DSCP_REMARK_00_KBPS_00_1M      | -     | -     | -     |
| DSCP_REMARK_00_KBPS_01_10M     | -     | -     | -     |
| DSCP_REMARK_00_KBPS_02_100M    | -     | -     | -     |
| DSCP_REMARK_01_PKTPS_00_100    | -     | -     | -     |
| DSCP_REMARK_01_PKTPS_01_1000   | -     | -     | -     |
| DSCP_REMARK_01_PKTPS_02_10000  | -     | -     | -     |

### GROUP: 

| Test                           |IPv4   |IPv6   |ARP    |
|--------------------------------|-------|-------|-------|
| **ALL**                        | ✓     | ✓     | -     |
| SELECT_Ether                   | ✓     | ✓     | -     |
| SELECT_IP                      | ✓     | ✓     | ✓     |
| SELECT_Weight_Ether            | ✓     | ✓     | -     |
| SELECT_Weight_IP               | ✓     | ✓     | ✓     |
