{application, wschat,
	[
		{description, "WebSockets Erlang Chat"},
		{vsn, "1"},
		{modules, []},
		{registered, []},
		{applications, [kernel, stdlib]},
		{mod, {wschat,[]}},
		{env, [{file, "/usr/local/log"}]}
	]
}.