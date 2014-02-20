%%
%%
%%

-record(blaze, {
		ports		:: {integer(),port(),list()},
		start_at	:: atom()
	}).

-record(fast_actions, {
			queue,
			output,
			group
		}).

%%EOF
