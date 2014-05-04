# LINC - OpenFlow software switch

## What is LINC?

LINC is a pure OpenFlow software switch written in Erlang. It runs within a
separate domain under Xen hypervisor using LING (erlangonxen.org).

### Features

 * Support for [OpenFlow Protocol 1.2][ofp3] and [OpenFlow Protocol 1.3][ofp4],
 * OpenFlow Capable Switch - ability to run multiple logical switches,
 * Support for [OF-Config 1.1.1][ofc11] management protocol,
 * Modular architecture, easily extensible.

## How to use it?

### Prerequisites

* Erlang/OTP R16B01

### LINC

Clone this git repository:

    % git clone <REPO>

Compile everything:

    % ./rebar get-deps
	% ./rebar compile

Create a LING configuration file:

	% cp LINGConfig.mk.sample LINGConfig.mk

Edit LINGConfig.mk as needed.

Create the switch configuration file:

	% cp priv/sys.config.sample priv/sys.config

Edit you priv/sys.config file.

Build the Xen image for the switch:

	% make

Boot the lincx x domain:

	% sudo make boot

For further instructions on how to use LINC check the
"[Ping example](https://github.com/FlowForwarding/LINC-Switch/tree/master/docs/example-ping.md)".

For detailed explanation on how to setup simple LINC testbed check the
"[Testbed setup](https://github.com/FlowForwarding/LINC-Switch/tree/master/docs/testbed-setup.md)".

## Support

If you have any technical questions, problems or suggestions regarding LINC
please send them to <linc-dev@flowforwarding.org> mailing list or create an
Issue. Thanks.

 [ovs]: http://openvswitch.org
 [ofp1]: https://www.opennetworking.org/images/stories/downloads/specification/openflow-spec-v1.0.0.pdf
 [ofp2]: https://www.opennetworking.org/images/stories/downloads/specification/openflow-spec-v1.1.0.pdf 
 [ofp3]: https://www.opennetworking.org/images/stories/downloads/specification/openflow-spec-v1.2.pdf 
 [ofp4]: https://www.opennetworking.org/images/stories/downloads/specification/openflow-spec-v1.3.0.pdf 
 [ofc11]: https://www.opennetworking.org/images/stories/downloads/of-config/of-config-1.1.pdf
 [erlang-src]: http://www.erlang.org/download.html

