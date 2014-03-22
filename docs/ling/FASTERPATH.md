
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

This is rearlly strange.

