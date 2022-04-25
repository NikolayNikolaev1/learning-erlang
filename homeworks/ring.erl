%% Module for making a ring of processes with subrings.
-module(ring).
-export([loop/1, start/1]).

%% Function for creating a subring.
createSubring(SubringPidCount, SubRootPid) ->
	SubChild = spawn(ring, loop, [{SubRootPid, SubringPidCount, SubRootPid, [], {subring, SubringPidCount, [], []}}]),
	SubChild ! create_child,
	SubChild.

%% Last Pid from main ring.
loop({Root, 1, Parent, Child, {subring, 0, SubParent, SubChild}}) ->
	PidCount = 1,
	SubPidCount = 0,
	Subring = {subring, SubPidCount, SubParent, SubChild},
	State = {Root, PidCount, Parent, Child, Subring},

	receive
		create_child -> NewState = {Root, PidCount, Parent, Root, Subring},
				Root ! {set_parent, self()},
				io:format("Ring Created!~n");
		{create_subrings, SubringPidCount} -> NewSubChild = createSubring(SubringPidCount, self()),
						      NewState = {Root, PidCount, Parent, Child, {subring, SubringPidCount, [], NewSubChild}};
		show_main_ring -> print(PidCount, self(), Child),
			%io:format("~p:[~p, ~p] Subrings: ", [PidCount, Parent, self()]),
				 % SubChild ! show_main_ring,
				  NewState = State;
		Any -> Any,
		       NewState = State
	end,

	loop(NewState);

%% Last Pids from Subrings.
loop({Root, 1, Parent, Child, {subring, SubPidCount, SubParent, SubChild}}) ->
	PidCount = 1,
	State = {Root, PidCount, Parent, Child, {subring, SubPidCount, SubParent, SubChild}},
	
	receive
		create_child -> NewState = {Root, PidCount, Parent, Root, {subring, SubPidCount, SubParent, SubChild}},
				Root ! {set_subring_parent, self()};
		show_subrings -> print(PidCount, self(), Child),
			%io:format("~p:[~p, ~p]~n", [PidCount, Parent, self()]),
				  NewState = State;
		Any -> Any,
		       NewState = State
	end,
	
	loop(NewState);

loop({Root, PidCount, Parent, Child, {subring, SubPidCount, SubParent, SubChild}}) ->
	Subring = {subring, SubPidCount, SubParent, SubChild},
	State = {Root, PidCount, Parent, Child, Subring},

	receive
		create_child -> NewChild = spawn(ring, loop, [{Root, PidCount-1, self(), [], {subring, SubPidCount, [], []}}]),
				NewState = {Root, PidCount, Parent, NewChild, Subring},
				NewChild ! create_child;
		{create_subrings, SubringPidCount} -> NewSubChild = createSubring(SubringPidCount, self()),
						      NewState = {Root, PidCount, Parent, Child, {subring, SubringPidCount, SubParent, NewSubChild}},
						      Child ! {create_subrings, SubringPidCount};
		%% Set_parent is called only by the last created process, so the first created one's parent can be set as that the last's Pid.
		{set_parent, ParentPid} -> NewState = {Root, PidCount, ParentPid, Child, Subring};
		%% Set_root is for changing Root from the main Pid to the first created process Pid.
		%% Later Root will be set as the last created process's child.
		set_root -> NewState = {self(), PidCount, Parent, Child, Subring};
		%% Set_subring_parent is for chainging SubRoot from the...TODO
		{set_subring_parent, SubParentPid} -> NewState = {Root, PidCount, Parent, Child, {subring, SubPidCount, SubParentPid, SubChild}};
		%% Show_ring prints each process Pid and its parent's one.
		show_main_ring -> print(PidCount, self(), Child),
			%io:format("~p:[~p, ~p], ", [PidCount, Parent, self()]),
				  Child ! show_main_ring,
				  NewState = State;
		show_subrings -> print(PidCount, self(), Child),
			%io:format("~p:[~p, ~p], Subrings: ", [PidCount, Parent, self()]),
				 SubChild ! show_main_ring,
				 Child ! show_subrings,
				 NewState = State;
		Any -> Any,
		       NewState = State
	end,

	loop(NewState).

print(PidCount, Pid, Child) -> io:format("~p:[~p, ~p]", [PidCount, Pid, Child]).

start({PidCount}) ->
	RootPid = spawn(ring, loop, [{self(), PidCount, self(), [], {subring, 0, [], []}}]),
	RootPid ! set_root,
	RootPid ! create_child,
	RootPid.
