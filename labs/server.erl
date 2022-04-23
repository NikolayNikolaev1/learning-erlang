-module(server).
-export([loop/1, start/1]).

start({N, Parent}) ->
	spawn(server, loop, [{N, Parent}]).

loop({0, _Parent}) -> ok;

loop({N, Parent}) ->
	receive
		create_child -> Child = spawn(server, loop, [{N-1, self()}]),
				io:format("N:~p, Child:~p~n", [N, Child]),
				Child ! create_child;
		{do, X} -> M = X * X,
			   io:format("Me:~p, N:~p, M:~p~n", [self(), N, M]),
			   Parent ! {do, M};
		Any -> Any
	end,
	loop({N, Parent}).
