
This is a journal of performance testing on LINC switch first on Erlang/OTP
platform, then on LING.

----[25/12/13 18:40]------------------------------------------------------------

We took the performance testing setup described in "Cost, Performance &
Flexibility in OpenFlow: Pick Three" paper. The setup is shown on a picture:

![testbed](testbed1.png)

Note: the ping.pcap packet has a checksum field set to 0. This makes it
unsuitable for testing as it gets discarded soon. A new ping.pcap recorded.

The testbed network should function in two modes: 1. vms has Internet
connectivity, ip forwarding is on in Dom0; 2. no ip forwarding, the only way for
vms to talk to each other is through the LINC switch. The mode 1. mostly needed
for installing new packages to Linuxes inside vms. Performance testing uses mode
2.

----[26/12/13 04:33]------------------------------------------------------------

A script called setup-network.sh has been added to switch between modes 1 and 2.

Two flows were added to LINC switch using controllers messages similar to
FlowMod described in the Ping demo document. Each message must have unique xid
and cookie or it is ignored by the switch.

It was double verified that it is the switch that provides the connectivity
between vms. The first run of iperf gives the first performance figure for
LINC/BEAM for the described setup - 140Mbits/s.

