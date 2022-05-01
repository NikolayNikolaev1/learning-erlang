%% Example of a remote spawning module.
-module(dist_demo).
-export([rpc/4, start/1]).

loop() ->
	receive
		{rpc, Pid, M, F, A} ->
			Pid ! {self(), (catch apply(M, F, A))},
			loop()
	end.

rpc(Pid, M, F, A) ->
	Pid ! {rpc, self(), M, F, A},

	receive
		{Pid, Response} -> Response
	end.

start(Node) ->
	spawn(Node, fun() -> loop() end).
