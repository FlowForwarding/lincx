
# Introduction

This tutorial explains how to build and run the LINC switch with the new faster
backend --- linc\_max.

Beware that linc\_max now uses a non-standard Erlang VM called LING. See
erlangonxen.org for details.

# Prerequisites

## Xen

To use linc\_max backend you need a system running Xen. How to setup Xen is
beyond the scope of this tutorial. It may be as easy as running `sudo apt-get
install xen-hypervisor` or it may require many manual steps. Check the
documentation.

## Bridges

You will need to add two Linux bridges to the system named br-linc1 and
br-linc2. The following commands do the trick:

```
sudo brctl addbr br-linc1
sudo brctl addbr br-linc2
```

The standard Xen bridge --- xenbr0 --- must be present too and it must have the
IP address of 192.168.0.1/24:

ip addr add 192.168.0.1/24 dev xenbr0

If you want to use a different IP address for xenbr0 than you should edit
Makefile. Look for a line that starts with 'EXTRA := -ipaddr ...'.

## Test virtual machines

The tutorial assumes that you have two virtual machines running: vm1 and vm2.
Their virtual interfaces must be bridged to br-linc1 and br-linc2 respectively:

```
sudo brctl addif br-linc1 vif100.0		# vm1
sudo brctl addif br-linc2 vif101.0		# vm2
```

IP addresses must be assigned to eth0 interfaces inside the virtual machines.
The rest of the tutorial assumes that these addresses are 192.168.1.2/24 (vm1) and
192.168.2.2/24 (vm2).

## iperf

For the TCP throughput test you will need iperf.

# Step-by-step recipe

## Clone lincx repo

Use `git clone` to create a local copy of the lincx repository.

## Build vmling image

```
./rebar get-deps
./rebar compile
```

Now is the tricky part. You are about to invoke the remote Erlang on Xen build
service (build.erlangonxen.org) to produce the Xen image that contains everything
needed to run the LINC switch. You may want at this point register with the
build service at http://build.erlangonxen.org/register and edit the credentials
in rebar.config under ling\_builder\_opts. Alternatively, you may use the test
account. Then keep the credentials intact.

Run `make me`.

The remote build service takes about 10s to produce the image. The 'LBS: image
saved to vmling' message indicates the success.

## Start LINC

The recipe assumes that the configuration file --- priv/sys.config --- is
unmodified. It sets up LINC with two ports, linc\_max backend, passively waiting
for connections from an OpenFlow controller on port 6634.

Run `sudo ./lincx`.

The lincx script check the prerequisites, then launches a new Xen domain using
vmling image, starts Erlang applications that comprise the LINC switch,
reattach virtual network interfaces to correct bridges, and opens a console to
the lincx domain.

## Run fake OpenFlow controller

The scripts directory contains an of_controller_v4.sh script that acts as a fake
OpenFlow controller that initiates various tests on the LINC switch.

The following command starts a test called simple\_iperf\_test:

```
cd scripts
./of_controller_v4.sh -s simple_iperf_test -r 192.168.0.2:6634
```

You should observe a series of messages both on the lincx console and
of_controller_v4.sh console that indicate that the switch and the controller are
exchanging messages using the OpenFlow protocol. If the exchage is successful,
the last message on the lincx console should read 'Flow table flow_table_0
updated' (repeated twice).

Now the switch will forward all traffic from br-linc1 to br-linc2 and vice
versa.

## Ping vm2 from vm1

You should be able to ping vm2 from vm1 using `ping 192.168.2.2`. A more
elaborate ping command lets you measure the processing delay of the switch. Run:

```
ping -q -Q 42 -n -c 25000 -f 192.168.2.2
```

When the flood ping complete issue the following command in the lincx shell:

ling:experimental(processing_delay, []).

The output of the command should be similar to this:

```
Processing delay statistics:
Packets: 50000
Delay: 1.020us +- 0.003 (95%)
ok
```

If the command complains about out-of-order packets then repeat the ping command
and request the processing delay again.

## Check TCP throughput

Run `iperf -s` in vm2 and then run `iperf -c 192.168.2.2 -i 1` in vm1.

