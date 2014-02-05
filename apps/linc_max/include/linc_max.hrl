
-record(state, {
	in_port =0,
	in_phy_port =0,
	metadata = <<0:64>>,
	tunnel_id = <<0:64>>
}).

-record(actions, {output}).

