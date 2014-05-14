%%
%%
%%

-record(port_info, {
			port_no,
			outlet,
			hw_addr,
			rx_pkt_ref,
			rx_data_ref,
			tx_pkt_ref,
			tx_data_ref}).

-record(blaze, {
			ports		:: [#port_info{}],
			queue_map	:: [{port(),pid()}],
			start_at	:: atom()
	}).

-record(fast_actions, {
			queue,
			output,
			group
		}).

%%EOF
