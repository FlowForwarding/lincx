-module(linc_ofdpa_demo).
-compile(export_all).

-include("ofdpa.hrl").

t() ->
	{ok,Flow} = ofdpa:ofdpaFlowEntryInit(flow_table_id_vlan),
	io:format("Flow = ~p\n", [Flow]),
	#ofdpa_flow_entry{flowData =FlowData} = Flow,
	io:format("FlowData = ~p\n", [FlowData]),
	#vlan_flow_entry{match_criteria =Match} = FlowData,
	io:format("Match = ~p\n", [Match]),

	Match1 = Match#vlan_flow_match{inPort =1,
		vlanId =16#100a,
		vlanIdMask =16#1fff},
	FlowData1 = FlowData#vlan_flow_entry{match_criteria =Match1},
	Flow1 = Flow#ofdpa_flow_entry{flowData =FlowData1},
	io:format("Flow1 = ~p\n", [Flow1]),
	
	io:format("Adding flow...\n", []),
	X = ofdpa:ofdpaFlowAdd(Flow1),
	io:format("Result = ~p\n", [X]).

%%4> ofdpa:ofdpaGroupEntryInit(group_entry_type_l2_interface).
%%{ok,#group_entry{groupId = 0}}
%%5> G = #group_entry{groupId = 16#a0031}.
%%#group_entry{groupId = 655409}
%%6> ofdpa:ofdpaGroupAdd(G).
%%ok

%%EOF
