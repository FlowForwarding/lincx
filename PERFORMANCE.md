
This is a journal of performance testing on LINC switch first on Erlang/OTP
platform, then on LING.

----[25/12/13 18:40]------------------------------------------------------------

We took the performance testing setup described in "Cost, Performance &
Flexibility in OpenFlow: Pick Three" paper. The setup is shown on a picture:

![testbed](testbed1.png)

Note: the ping.pcap packet has a checksum field set to 0. This makes it
unsuitable for testing as it gets discarded soon. A new ping.pcap recorded.


