%% Template for a concurrent program.
-module(concurrent_template).
-compile(export_all).

loop(X) ->
	receive
		Any ->
			io:format("Received: ~p~n", [Any]),
			loop(X)
	end.

rpc(Pid, Request) ->
	Pid ! {self(), Request},
	receive
		{Pid, Response} ->
			Response
	end.

start() ->
	spawn(fun() -> loop([]) end).
