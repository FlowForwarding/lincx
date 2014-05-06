%%
%%
%%

-record(blaze, {
		ports		:: [{integer(),port(),list()}],
		queue_map	:: [{port(),pid()}],
		start_at	:: atom()
	}).

-record(fast_actions, {
			queue,
			output,
			group
		}).

%%EOF
