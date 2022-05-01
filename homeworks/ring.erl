-module(ring).
-export([loop/1, start/1]).

loop({1, Root, Children}) ->
	State = {1, Root, Children},

	receive
		create_child ->
			NewState = {1, Root, [Root | Children]};
		{create_subrings, M} ->
			Child = spawn(ring, loop, [{M-1, self, []}]),
			Child ! create_child,
			NewState = {1, Root, [Child | Children]};
		{multiply, Num} when NextChild =/= Root ->
			Result = Num * Num,
			io:format("Result from ~p: ~p~n", [self(), Result]),
			[SubringChild | _] = Children,
			SubringChild ! {multiply, Result},
			NewState = State;
		Any -> Any,
		       NewState = State
	end,

	loop(NewState);

loop({N, Root, Children}) ->
	State = {N, Root, Children},

	receive
		create_child ->
			Child = spawn(ring, loop, [{N-1, Root, []}]),
			io:format("~p. Process ~p created.~n", [N-1, Child]),
			Child ! create_child,
			NewState = {N, Root, [Child | Children]};
		{create_subrings, M} ->
			Child = spawn(ring, loop, [{M-1, self(), []}]),
			Child ! create_child,
			[NextChild | _] = Children,
			NextChild ! {create_subrings, M},
			NewState = {N, Root, [Child | Children]};
		{multiply, Num} ->
			Result = Num * Num,
			io:format("Result from ~p: ~p~n", [self(), Result]),
			[Child ! {multiply, Result} || Child <- Children],
			NewState = State;
		set_root ->
			NewState = {N, self(), Children};
		Any -> Any,
		       NewState = State
	end,

	loop(NewState).

start({N}) ->
	RootPid = spawn(ring, loop, [{N, [], []}]),
	io:format("~p. Process ~p created.~n", [N, RootPid]),
	RootPid ! set_root,
	RootPid ! create_child,
	RootPid.
