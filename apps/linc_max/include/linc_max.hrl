
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
