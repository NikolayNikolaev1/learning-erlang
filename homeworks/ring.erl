%% Module for making a ring of processes.
-module(ring).
-export([loop/1, start/1]).

loop({Root, 1, Parent, Children}) ->
	State = {Root, 1, Parent,  Children},

	receive
		create_child -> NewState = {Root, 1, Parent, [Root | Children]},
			       Root ! {set_parent, self()};
		show_ring -> io:format("1:[~p, ~p]~n", [Parent, self()]),
			     NewState = State;
		Any ->Any,
		      NewState = State
	end,

	loop(NewState);

loop({Root, PidCount, Parent, Children}) ->
	State = {Root, PidCount, Parent, Children},

	receive
		create_child -> NewChild = spawn(ring, loop, [{Root, PidCount-1, self(), []}]),
			       NewState = {Root, PidCount, Parent, [NewChild | Children]},
			       NewChild ! create_child;
		{set_parent, ParentPid} -> NewState = {Root, PidCount, ParentPid, Children};
		set_root -> NewState = {self(), PidCount, Parent, Children};
		show_ring -> io:format("~p:[~p, ~p]~n", [PidCount, Parent, self()]),
			     [Child ! show_ring || Child <- Children],
			     NewState = State;
		Any -> Any,
		       NewState = State
	end,

	loop(NewState).

start({PidCount}) ->
	RootPid = spawn(ring, loop, [{self(), PidCount, self(), []}]),
	RootPid ! set_root,
	RootPid ! create_child,
	%RootPid ! show_ring,
	RootPid.
