
====[ 3/20/2014 11:19 ]=========================================================

The processing delay in LINCX is 1-2us and the ping between test VMs is >200us.

The networking code in Linux takes advantage of multicore. Four VCPUs were
assigned to both vm1 and vm2 domains (used to be one). The ping between vm1 and
vm2 is faster (avg 93us):

root@vm1:/home/mk# ping -q -Q 42 -c 25000 -f 192.168.2.2
PING 192.168.2.2 (192.168.2.2) 56(84) bytes of data.

--- 192.168.2.2 ping statistics ---
25000 packets transmitted, 25000 received, 0% packet loss, time 2529ms
rtt min/avg/max/mdev = 0.085/0.093/0.971/0.023 ms, ipg/ewma 0.101/0.088 ms

The packets per second (pps) rate is the same after the change: 69kpps.

Removing asserts (-DSUPPRESS_ASSERTS) and the latency instrumentation does not
change the pps rate.

vm1 and vm2 interfaces are bridged by br-linc1. There is no LINC switch in
between. The ping delay is actually higher: 221us and pps rate is the same.

Setting txqueuelen to 1000 on vifs of vm1 and vm2 does not make a difference.
vifs and eth0 interfaces inside vms do not report dropped packets.

iperf UDP test with non-existing IP address confirm that the bottleneck is the
ability of the virtual machine to send out packets. It still sends only ~69k
packets per second.

The observed packet rate must be iperf limitation. It is the same in Dom0 or in
VM with unbridged vif. Seek alternatives to iperf.

# netperf

netperf shows better readings in both big and small packets UDP tests:

root@vm1:/home/mk# netperf -t UDP_STREAM -H 192.168.2.2
MIGRATED UDP STREAM TEST from 0.0.0.0 (0.0.0.0) port 0 AF_INET to 192.168.2.2 (192.168.2.2) port 0 AF_INET : demo
Socket  Message  Elapsed      Messages
Size    Size     Time         Okay Errors   Throughput
bytes   bytes    secs            #      #   10^6bits/sec

212992   65507   10.00       82309      0    4313.33
212992           10.00       80732           4230.69

The throughput is 4.2Gbit/s for large UDP packets.

root@vm1:/home/mk# netperf -t UDP_STREAM -H 192.168.2.2 -- -m 64
MIGRATED UDP STREAM TEST from 0.0.0.0 (0.0.0.0) port 0 AF_INET to 192.168.2.2 (192.168.2.2) port 0 AF_INET : demo
Socket  Message  Elapsed      Messages
Size    Size     Time         Okay Errors   Throughput
bytes   bytes    secs            #      #   10^6bits/sec

212992      64   10.00     2669922      0     136.70
212992           10.00     2465775            126.24

The pps rate is 247kpps (254kpps).

The readings for TCP test is the same as iperf (2.8Gbit/s).

# trafgen

trafgen does not seem to give more. The rate is 203kpps.

The experimenting continues using netperf.

When vifs are bridged directly bypassing the LINC switch the rate gets better
(375kpps):

root@vm1:/home/mk# netperf -t UDP_STREAM -H 192.168.2.2 -- -m 64
MIGRATED UDP STREAM TEST from 0.0.0.0 (0.0.0.0) port 0 AF_INET to 192.168.2.2 (192.168.2.2) port 0 AF_INET : demo
Socket  Message  Elapsed      Messages
Size    Size     Time         Okay Errors   Throughput
bytes   bytes    secs            #      #   10^6bits/sec

4194304      64   10.00     7060473      0     361.49
212992           10.00     3752675            192.13

# Rates

Rate, kpps | Interface | Note
-----|-----------|-----
622 | eth0 | vm1 int
297 | vif1018.0 | vm1 ext
289 | vif1028.1 | LINC port 1
260 | vif1028.2 | LINC port 2
260 | vif1019.0 | vm2 ext
248 | eth0 | vm2 int

Most packets are dropped before entering br-linc1. All dropped packets show up
in the statistics for IP packets inside vm1.o

Limiting vm1 and vm2 to a single core does not help with the packet drop.
Pinning lincx to a dedicated core helps a little:

Average:        IFACE   rxpck/s   txpck/s    rxkB/s    txkB/s   rxcmp/s   txcmp/s  rxmcst/s
Average:       xenbr0      0.00      0.00      0.00      0.00      0.00      0.00      0.00
Average:     vif945.0      0.00      0.00      0.00      0.00      0.00      0.00      0.00
Average:    vif1028.1      0.00 309287.60      0.00  32016.10      0.00      0.00      0.00
Average:         eth0      3.20      2.20      0.20      1.36      0.00      0.00      0.00
Average:           lo      0.00      0.00      0.00      0.00      0.00      0.00      0.00
Average:     br-linc0      2.40      2.20      0.12      1.36      0.00      0.00      0.00
Average:     br-linc1 316341.60      0.00  28421.32      0.00      0.00      0.00      0.00
Average:    vif1028.0      0.00      0.00      0.00      0.00      0.00      0.00      0.00
Average:     br-linc2      0.00      0.00      0.00      0.00      0.00      0.00      0.00
Average:    vif1019.0      0.00 258967.40      0.00  26807.17      0.00      0.00      0.00
Average:    vif1018.0 316339.40      0.00  28421.12      0.00      0.00      0.00      0.00
Average:    vif1028.2 258966.80      0.00  23266.55      0.00      0.00      0.00      0.00
Average:     vif946.0      0.00      0.00      0.00      0.00      0.00      0.00      0.00

Stats for directly bridged vifs:

Average:        IFACE   rxpck/s   txpck/s    rxkB/s    txkB/s   rxcmp/s txcmp/s  rxmcst/s
Average:       xenbr0      0.00      0.00      0.00      0.00      0.00 0.00      0.00
Average:     vif945.0      0.00      0.00      0.00      0.00      0.00 0.00      0.00
Average:    vif1028.1      0.00      0.00      0.00      0.00      0.00 0.00      0.00
Average:         eth0      4.20      4.00      0.27      1.47      0.00 0.00      0.00
Average:           lo      0.00      0.00      0.00      0.00      0.00 0.00      0.00
Average:     br-linc0      4.20      3.80      0.21      1.44      0.00 0.00      0.00
Average:     br-linc1 376330.40      0.00  33810.93      0.00      0.00 0.00      0.00
Average:    vif1028.0      0.00      0.00      0.00      0.00      0.00 0.00      0.00
Average:     br-linc2      0.00      0.00      0.00      0.00      0.00 0.00      0.00
Average:    vif1019.0      0.00 376331.60      0.00  38956.20      0.00 0.00      0.00
Average:    vif1018.0 376331.80      0.00  33811.06      0.00      0.00 0.00      0.00
Average:    vif1028.2      0.00      0.00      0.00      0.00      0.00 0.00      0.00
Average:     vif946.0      0.00      0.00      0.00      0.00      0.00 0.00      0.00

It looks like packets are dropped in vm1 because the switch does not capture
them fast enough. The maximum rate is thus ~375kpps.

# Driver parameters

EXT_RX_BUFFERS set to 128 (instead of 256) -- no change:

root@vm1:/home/mk# netperf -t UDP_STREAM -H 192.168.2.2 -- -m 60
MIGRATED UDP STREAM TEST from 0.0.0.0 (0.0.0.0) port 0 AF_INET to 192.168.2.2
(192.168.2.2) port 0 AF_INET : demo
Socket  Message  Elapsed      Messages                
Size    Size     Time         Okay Errors   Throughput
bytes   bytes    secs            #      #   10^6bits/sec

4194304      60   10.00     5483296      0     263.19
212992           10.00     2281722            109.52

Both NR_RX_BUFFERS and EXT_RX_BUFFERS is set to 128 -- no change:

root@vm1:/home/mk# netperf -t UDP_STREAM -H 192.168.2.2 -- -m 60
MIGRATED UDP STREAM TEST from 0.0.0.0 (0.0.0.0) port 0 AF_INET to 192.168.2.2
(192.168.2.2) port 0 AF_INET : demo
Socket  Message  Elapsed      Messages                
Size    Size     Time         Okay Errors   Throughput
bytes   bytes    secs            #      #   10^6bits/sec

4194304      60   10.00     5386016      0     258.52
212992           10.00     2334504            112.05

NR_RX_BUFFERS = EXT_RX_BUFFERS = 32 -- no change:

root@vm1:/home/mk# netperf -t UDP_STREAM -H 192.168.2.2 -- -m 60
MIGRATED UDP STREAM TEST from 0.0.0.0 (0.0.0.0) port 0 AF_INET to 192.168.2.2
(192.168.2.2) port 0 AF_INET : demo
Socket  Message  Elapsed      Messages                
Size    Size     Time         Okay Errors   Throughput
bytes   bytes    secs            #      #   10^6bits/sec

4194304      60   10.00     5194303      0     249.32
212992           10.00     2121578            101.83

NR_RX_BUFFERS = EXT_RX_BUFFERS = 4 -- no change:

root@vm1:/home/mk# netperf -t UDP_STREAM -H 192.168.2.2 -- -m 60
MIGRATED UDP STREAM TEST from 0.0.0.0 (0.0.0.0) port 0 AF_INET to 192.168.2.2
(192.168.2.2) port 0 AF_INET : demo
Socket  Message  Elapsed      Messages
Size    Size     Time         Okay Errors   Throughput
bytes   bytes    secs            #      #   10^6bits/sec

4194304      60   10.00     5581612      0     267.91
212992           10.00     2115262            101.53

This is really strange.

----[31/03/2014 15:15]----------------------------------------------------------

A new instrumentation has been added to the low-level network driver. The
instrumentation API:

ling:experimental(llstat, N).	%% start collecting statistics for interface N
ling:experimental(llstat, stop).
ling:experimental(llstat, []).	%% display the results

The statistics are collected for eth1 and eth2 during the netperf small packet
UDP tests:

eth2:

Duration: 3887.1ms
RX: interrupts: 65205 (0 kicks 0.0%) (freq 16774.7/s period 59.6us)
RX: reqs per int: 0/0.0/0
RX: tx buf freed per int: 0/2.8/256
TX: outputs: 931760 (123337 kicks 13.2) (freq 239705.8/s period 4.2us)
TX: tx buf freed per int: 0/0.8/90
TX: rates: 239.7kpps 195.60Mbps avg pkt size 102.0B
TX: drops: 76131 (freq 19585.6/s period 51.1us)
TX: drop rates: 19.6kpps 15.98Mbps avg pkt size 102.0B

eth1:

Duration: 3584.4ms
RX: interrupts: 378236 (67 kicks 0.0%) (freq 105523.3/s period 9.5us)
RX: reqs per int: 0/2.7/256
RX: tx buf freed per int: 0/0.0/0

A few obvservations:

1. Upon interrupt on average 3 packets are read from the ring -- we may wait for
some time to read more packets per interrupt.

1. The interrupt occur every 9.5us -- this looks like a Xen quantum.

1. TX shows avg packets size of 102B (not 60B or 64B). Why?

netperf TCP_STREAM test (2.8Mbps).

eth1:

Duration: 4575.2ms
RX: interrupts: 48311 (110 kicks 0.2%) (freq 10559.2/s period 94.7us)
RX: reqs per int: 0/23.2/256
RX: tx buf freed per int: 0/7.9/103
TX: outputs: 561142 (145776 kicks 26.0) (freq 122647.6/s period 8.2us)
TX: tx buf freed per int: 0/0.3/59
TX: rates: 122.6kpps 64.82Mbps avg pkt size 66.1B

eth2:

Duration: 3926.3ms
RX: interrupts: 24435 (110 kicks 0.5%) (freq 6223.3/s period 160.7us)
RX: reqs per int: 0/19.5/184
RX: tx buf freed per int: 0/14.5/240
TX: outputs: 949456 (164113 kicks 17.3) (freq 241816.5/s period 4.1us)
TX: tx buf freed per int: 0/0.6/112
TX: rates: 241.8kpps 2928.88Mbps avg pkt size 1514.0B
TX: drops: 425 (freq 108.2/s period 9238.5us)
TX: drop rates: 0.1kpps 1.31Mbps avg pkt size 1514.0B

1. The packet size (1514B) is sound.

1. The driver gets 20 packets per interrupt.

Collecting data for various packet sizes of UDP flood test (port 2 only).

vm1> netperf -t UDP_STREAM -H 192.168.2.2 -- -m 214 # actual packet size is 256B

18> ling:experimental(llstat, []).
Duration: 4550.6ms
RX: interrupts: 15080 (110 kicks 0.7%) (freq 3313.8/s period 301.8us)
RX: reqs per int: 0/0.0/0
RX: tx buf freed per int: 0/19.3/214
TX: outputs: 1279103 (216976 kicks 17.0) (freq 281082.1/s period 3.6us)
TX: tx buf freed per int: 0/0.8/81
TX: rates: 281.1kpps 134.92Mbps avg pkt size 60.0B
TX: drops: 301345 (freq 66220.4/s period 15.1us)
TX: drop rates: 66.2kpps 31.79Mbps avg pkt size 60.0B

----

Duration: 4000.5ms
RX: interrupts: 15451 (110 kicks 0.7%) (freq 3862.3/s period 258.9us)
RX: reqs per int: 0/0.0/0
RX: tx buf freed per int: 0/12.4/143
TX: outputs: 1108760 (265822 kicks 24.0) (freq 277154.9/s period 3.6us)
TX: tx buf freed per int: 0/0.8/95
TX: rates: 277.2kpps 141.90Mbps avg pkt size 64.0B
TX: drops: 126249 (freq 31558.3/s period 31.7us)
TX: drop rates: 31.6kpps 16.16Mbps avg pkt size 64.0B

----

Duration: 3281.9ms
RX: interrupts: 29901 (110 kicks 0.4%) (freq 9110.9/s period 109.8us)
RX: reqs per int: 0/0.0/0
RX: tx buf freed per int: 0/4.4/256
TX: outputs: 847903 (343957 kicks 40.6) (freq 258359.1/s period 3.9us)
TX: tx buf freed per int: 0/0.8/116
TX: rates: 258.4kpps 264.56Mbps avg pkt size 128.0B
TX: drops: 41311 (freq 12587.6/s period 79.4us)
TX: drop rates: 12.6kpps 12.89Mbps avg pkt size 128.0B

----

Duration: 3056.1ms
RX: interrupts: 20072 (110 kicks 0.5%) (freq 6567.7/s period 152.3us)
RX: reqs per int: 0/0.0/0
RX: tx buf freed per int: 0/2.6/256
TX: outputs: 719536 (441826 kicks 61.4) (freq 235438.8/s period 4.2us)
TX: tx buf freed per int: 0/0.9/75
TX: rates: 235.4kpps 482.18Mbps avg pkt size 256.0B
TX: drops: 6557 (freq 2145.5/s period 466.1us)
TX: drop rates: 2.1kpps 4.39Mbps avg pkt size 256.0B

----

Duration: 3624.0ms
RX: interrupts: 23060 (110 kicks 0.5%) (freq 6363.2/s period 157.2us)
RX: reqs per int: 0/0.0/0
RX: tx buf freed per int: 0/4.8/256
TX: outputs: 858915 (556910 kicks 64.8) (freq 237009.6/s period 4.2us)
TX: tx buf freed per int: 0/0.9/100
TX: rates: 237.0kpps 970.79Mbps avg pkt size 512.0B
TX: drops: 45619 (freq 12588.1/s period 79.4us)
TX: drop rates: 12.6kpps 51.56Mbps avg pkt size 512.0B

----

Duration: 4784.7ms
RX: interrupts: 35822 (110 kicks 0.3%) (freq 7486.9/s period 133.6us)
RX: reqs per int: 0/0.0/0
RX: tx buf freed per int: 0/2.3/256
TX: outputs: 1039730 (714772 kicks 68.7) (freq 217305.3/s period 4.6us)
TX: tx buf freed per int: 0/0.9/87
TX: rates: 217.3kpps 1780.17Mbps avg pkt size 1024.0B
TX: drops: 6852 (freq 1432.1/s period 698.3us)
TX: drop rates: 1.4kpps 11.73Mbps avg pkt size 1024.0B

----

Duration: 3511.0ms
RX: interrupts: 40566 (110 kicks 0.3%) (freq 11553.9/s period 86.6us)
RX: reqs per int: 0/0.0/0
RX: tx buf freed per int: 0/2.7/256
TX: outputs: 774499 (828696 kicks 107.0) (freq 220591.5/s period 4.5us)
TX: tx buf freed per int: 0/0.9/117
TX: rates: 220.6kpps 2671.80Mbps avg pkt size 1514.0B
TX: drops: 48577 (freq 13835.6/s period 72.3us)
TX: drop rates: 13.8kpps 167.58Mbps avg pkt size 1514.0B

----

-m 65507

Duration: 4868.6ms
RX: interrupts: 69170 (0 kicks 0.0%) (freq 14207.4/s period 70.4us)
RX: reqs per int: 0/0.0/0
RX: tx buf freed per int: 0/8.5/234
TX: outputs: 1479707 (112263 kicks 7.6) (freq 303928.8/s period 3.3us)
TX: tx buf freed per int: 0/0.6/113
TX: rates: 303.9kpps 3622.66Mbps avg pkt size 1489.9B
TX: drops: 12392 (freq 2545.3/s period 392.9us)
TX: drop rates: 2.5kpps 30.26Mbps avg pkt size 1486.0B

The kick percentage data unreliable (bug). The last test involves heavy packet
fragmentation.

