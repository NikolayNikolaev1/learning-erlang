-module(proc_struct).
%-compile(export_all).
-export([
	 client/2,
	 exit_handler/0,
	 loop/1,
	 start/1,
	 update_child/3,
	 update_struct/2
	]).

client(Server, Request) ->
	rpc(Server, Request).
%%-----------------------------------------------------------------------------------
exit_handler() ->
	process_flag(trap_exit, true),
	receive
		{create_link, Pid} ->
			link(Pid);
		{'EXIT', _Pid, {state, State}} -> 
			NewProc = spawn_link(proc_struct, loop, [State]),
			update_struct(State, NewProc);
		{'Exit', Pid, Why} ->
			io:fomrat("Pid: ~p Exit Handler Why: ~p~n", [Pid, Why]);
		Other ->
			io:format("Exit Handler: ~p~n", [Other])
	end,
	exit_handler().

loop({0, _, _, _, _, _, _}) -> ok;

loop(State = {Level, ExitHandler, Type, Parent, LChild, RChild}) ->
	receive
		{Client, get_info} ->
			Response = {info, Parent, self(), LChild, RChild},
			Client ! {self(), Response},
			NewState = State;
		create_children ->
			%% Creates left and right children and links them to the exit handler.
			NewLChild = spawn(proc_struct, loop, [{Level-1, ExitHandler, left, self(), [], []}]),
			NewRChild = spawn(proc_struct, loop, [{Level-1, ExitHandler, right, self(), [], []}]),
			ExitHandler ! {create_link, NewLChild},
			ExitHandler ! {create_link, NewRChild},
			NewState = {Level, ExitHandler, Type, Parent, NewLChild, NewRChild};
		{update_child, Type, NewChild} ->
			{NewLChild, NewRChild} = update_child({LChild, RChild}, Type, NewChild),
			NewState = {Level, ExitHandler, Type, Parent, NewLChild, NewRChild};
		{update_parent, NewParent} ->
			NewState = {Level, ExitHandler, Type, NewParent, LChild, RChild};
		proc_exit -> 
			exit({state, State}),
			NewState = State;
		Other ->
			io:format("Other from loop: ~p~n", [Other]),
			NewState = State
	end,
	loop(NewState).

rpc(Server, Request) ->
	Server ! {self(), Request},
	receive
		{Server, {info, Parent, Pid, LChild, RChild}} ->
			io:format("Currnet Pid: ~p~n\tParent: ~p~n\tLeft Child: ~p~n\tRight Child: ~p~n", [Pid, Parent, LChild, RChild]);
		Other ->
			io:format("Other from client: ~p~n", [Other])
	end.

start({N}) ->
	ExitHandler = spawn(proc_struct, exit_handler, []),
	Root = spawn(proc_struct, loop, [{N, ExitHandler, root, [], [], []}]),
	ExitHandler ! {create_link, Root},
	Root ! create_children,
	Root.

update_child({_, RChild}, left, NewChild) ->
	%% Updating left child.
	{NewChild, RChild};
update_child({LChild, _}, right, NewChild) ->
	%% Updating right child.
	{LChild, NewChild}.

%% Exit Handler updates the new process as the parent and child of the structure.
update_struct({_, _, root, [], LChild, RChild}, NewParent) ->
	%% Updating root.
	LChild ! {update_parent, NewParent},
	RChild ! {update_parent, NewParent};
update_struct({_, _, Type, Parent, [], []}, NewChild) ->
	%% Updating last children.
	Parent ! {update_child, Type, NewChild};
update_struct({_, _, Type, Parent, LChild, RChild}, NewProc) ->
	%% Updating processes between root and last level children.
	Parent ! {update_child, Type, NewProc},
	LChild ! {update_parent, NewProc},
	RChild ! {update_parent, NewProc}.
