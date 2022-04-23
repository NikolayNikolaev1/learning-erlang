-module(lab1).
-export([loop/0]).

loop() ->
	receive
		X -> io:format("Message: ~p ~n", [X])
	end,
	loop().
