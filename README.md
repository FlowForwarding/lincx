# LINCX - OpenFlow software switch

#### Build Status

[![Build Status](https://api.travis-ci.org/cloudozer/lincx.png)](https://travis-ci.org/cloudozer/lincx)

## What is LINCX?

LINCX is a pure OpenFlow software switch written in Erlang. It runs within a
separate domain under Xen hypervisor using LING (erlangonxen.org).

LINCX is a new faster version of [LINC-Switch][oldlinc].

### Features

 * Support for [OpenFlow Protocol 1.2][ofp3] and [OpenFlow Protocol 1.3][ofp4],
 * OpenFlow Capable Switch - ability to run multiple logical switches,
 * Support for [OF-Config 1.1.1][ofc11] management protocol,
 * Modular architecture, easily extensible.

## How to use it?

### Prerequisites

* Erlang/OTP R16B01

### LINCX

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

Boot the lincx Xen domain:

	% sudo make boot

## Support

If you have any technical questions, problems or suggestions regarding LINCX
please send them to <linc-dev@flowforwarding.org> mailing list or create an
Issue. Thanks.

 [ovs]: http://openvswitch.org
 [ofp1]: https://www.opennetworking.org/images/stories/downloads/specification/openflow-spec-v1.0.0.pdf
 [ofp2]: https://www.opennetworking.org/images/stories/downloads/specification/openflow-spec-v1.1.0.pdf 
 [ofp3]: https://www.opennetworking.org/images/stories/downloads/specification/openflow-spec-v1.2.pdf 
 [ofp4]: https://www.opennetworking.org/images/stories/downloads/specification/openflow-spec-v1.3.0.pdf 
 [ofc11]: https://www.opennetworking.org/images/stories/downloads/sdn-resources/onf-specifications/openflow-config/of-config-1-1-1.pdf
 [erlang-src]: http://www.erlang.org/download.html
 [oldlinc]: https://github.com/FlowForwarding/LINC-Switch

