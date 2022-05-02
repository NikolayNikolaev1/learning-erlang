%% Module for making a ring of processes with subrings.
-module(ring).
-export([loop/1, start/1]).

%% Handles the last created process of the main and sub rings.
loop({1, Root, Children}) ->
	State = {1, Root, Children},

	receive
		create_child ->
			NewState = {1, Root, [Root | Children]};
		{create_subrings, M} ->
			Child = spawn(ring, loop, [{M-1, self, []}]),
			print(M-1, self()),
			Child ! create_child,
			NewState = {1, Root, [Child | Children]};
		%% Multiply given number by 2 and send message to itself with the result and child pid.
		%% This way prevents an infinite loop of multiplying and sending to root.
		{multiply, Num} ->
			Result = Num * 2,
			io:format("Result from ~p: ~p~n", [self(), Result]),
			[SubringChild | _] = Children,
			self() ! {multiplyto_child, SubringChild, Result},
			NewState = State;
		%% Send result to child if it does not have the same Pid as the Root.
		{multiplyto_child, SubringChild, Result} when SubringChild =/= Root ->
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
			print(N-1, Child),
			Child ! create_child,
			NewState = {N, Root, [Child | Children]};
		%% Function for createing M-1 number of processes with root of the current process.
		%% The final process then becomes the 'Parent' of the root process.
		{create_subrings, M} ->
			Child = spawn(ring, loop, [{M-1, self(), []}]),
			print(M-1, Child),
			Child ! create_child,
			[NextChild | _] = Children,
			NextChild ! {create_subrings, M},
			NewState = {N, Root, [Child | Children]};
		%% Multiply the given input by 2 and sends the result to its main child and sub child.
		{multiply, Num} ->
			Result = Num * 2,
			io:format("Result from ~p: ~p~n", [self(), Result]),
			[Child ! {multiply, Result} || Child <- Children],
			NewState = State;
		%% First created process becomes the Root of the main ring.
		%% That way the last created process can set the root as its 'Child' so the ring can be closed.
		set_root ->
			NewState = {N, self(), Children};
		Any -> Any,
		       NewState = State
	end,

	loop(NewState).

print(N, Pid) ->
	io:format("~p. Process ~p created.~n", [N, Pid]).

start({N}) ->
	RootPid = spawn(ring, loop, [{N, [], []}]),
	print(N, RootPid),
	RootPid ! set_root,
	RootPid ! create_child,
	RootPid.
