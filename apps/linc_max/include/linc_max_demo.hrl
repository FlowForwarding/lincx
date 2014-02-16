%%
%%
%%

%% @author Cloudozer LLP. <info@cloudozer.com>
%% @copyright 2014 FlowForwarding.org
%% @doc Temporary definitions, merge with linc_demo.hrl

-record(instr, {meter,
				apply,
				clear_write,
				metadata,
				goto}).

-record(actions, {
	queue,
	output,
	group
}).

%%EOF
