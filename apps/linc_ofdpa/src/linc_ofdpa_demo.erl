-module(linc_ofdpa_demo).
-compile(export_all).

-include("ofdpa.hrl").

l() ->
	ofdpa_link:start_link("192.168.0.1", 5005).

f() ->
	ofdpa:ofdpaQueueStatsGet(1, 2, #port_queue_stats{}).

%%EOF
