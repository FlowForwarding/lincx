## LINCX - OpenFlow software switch

[![Build Status](https://api.travis-ci.org/FlowForwarding/lincx.svg)](https://travis-ci.org/FlowForwarding/lincx)

### What is LINCX?

LINCX is a pure OpenFlow software switch written in Erlang. It runs within a
separate domain under Xen hypervisor using LING (erlangonxen.org).

LINCX is a new faster version of [LINC-Switch][oldlinc].

### Features

 * Support for [OpenFlow Protocol 1.2][ofp3] and [OpenFlow Protocol 1.3][ofp4],
 * Support for [OF-Config 1.1.1][ofc11] management protocol,
 * Modular architecture, easily extensible.

### How to use it?

1. Check that you have Erlang/OTP R16B01 installed. Other versions will NOT
work. You may need to build Erlang/OTP R16B01 from [sources](http://www.erlang.org/download_release/19).

1. Clone the LINCX repository:
```
    % git clone https://github.com/FlowForwarding/lincx.git
```
1. Compile the code:
```
    % ./rebar get-deps
	% ./rebar compile
```
1. Create the configuration file:
```
	% cp lincx.config.sample lincx.config
```
1. Edit lincx.config as needed.

1. Build the Xen image for the switch:
```
	% ./railing image
```
1. Boot the lincx Xen domain:
```
	% sudo xl create -c domain_config
```
### How to configure LINCX?

lincx.config is the main configuration file of the LINCX switch. You can use the
file lincx.config.sample as the starting point for you configuration file.

`ipconf` option defines the TCP/IP networking setup. To configure TCP/IP
statically use:
```
	{ipconf, IpAddr, NetMask, Gateway}.
```
To use dhcp change the option to
```
	{ipconf, dhcp}.
```
To add an OpenFlow port use the following option:
```
	{port, PortNo}.
	{port, PortNo, AuxProps}.
```
By default, an OpenFlow port 1 connects to the bridge named br1 in Dom0. The
name of the brdige can be set in auxilliary properties:
```
	{port, 1, [{bridge,br7}]}. %% connects the port to br7
```
To add multiple ports use:
```
	{ports, NumOfPorts}.
	{ports, NumOfPorts, AuxProps}.
```
By default, the ports are numbered from 1 to NumOfPorts and are connected to
brdiges br1, br2, etc. The starting port number and the prefix for bridge names
can be changed using auxilliary properties:
```
	%% adds ports 5,6,7 connected to xenbr5,xenbr6,xenbr7
	{ports, 3, [{start,5},{prefix,xenbr}]}.
```
To add an active connection to a controller use the following option:
```
	{controller,IpAddr,Port}.
```
To listen for connections from controllers:
```
	{listener,IpAddr,Port}.
```
To add a queue use:
```
	{queue, QueueNo, MinRate, MaxRate}.
```
A port can be connected to a queue using a property, e.g.
```
	{queue, 1, 0, 100}.
	{port, 1, [{queue,1}]}.
```
To set the memory size of the lincx domain (in MB, default = 1024):
```
	{memory, Memory}.
```

LINCX supports alternative format -- YAML -- of the configuration file. Rename
the configuration file to lincx.yml to use YAML. See lincx.yml.sample as a
starting point.

### Support

If you have any technical questions, problems or suggestions regarding LINCX
please send them to <linc-dev@flowforwarding.org> mailing list or create an
Issue. Thanks.

 [ovs]: http://openvswitch.org
 [ofp1]: https://www.opennetworking.org/images/stories/downloads/specification/openflow-spec-v1.0.0.pdf
 [ofp2]: https://www.opennetworking.org/images/stories/downloads/specification/openflow-spec-v1.1.0.pdf 
 [ofp3]: https://www.opennetworking.org/images/stories/downloads/specification/openflow-spec-v1.2.pdf 
 [ofp4]: https://www.opennetworking.org/images/stories/downloads/specification/openflow-spec-v1.3.0.pdf 
 [ofc11]: https://www.opennetworking.org/images/stories/downloads/sdn-resources/onf-specifications/openflow-config/of-config-1-1-1.pdf
 [oldlinc]: https://github.com/FlowForwarding/LINC-Switch

