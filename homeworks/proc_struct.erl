-module(proc_struct).
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

%% On exit handling process.
%% Receives exit signals with state from binary tree processes.
%% Creates new process and updates its parent and children.
exit_handler() ->
	receive
		{create_link, Pid} ->
			link(Pid);
		{'EXIT', _Pid, {state, State}} -> 
			NewProc = spawn_link(proc_struct, loop, [State]),
			io:format("New Pid: ~p~n", [NewProc]),
			update_struct(State, NewProc);
		{'Exit', Pid, Why} ->
			io:fomrat("Pid: ~p Exit Handler Why: ~p~n", [Pid, Why]);
		set_sysproc ->
			process_flag(trap_exit, true),
			io:format("SysProc ~p created.~n", [self()]);
		Other ->
			io:format("Exit Handler: ~p~n", [Other])
	end,
	exit_handler().

loop({0, _, _, _, _, _}) -> done;

loop(State = {Level, ExitHandler, Type, Parent, LChild, RChild}) ->
	receive
		{Client, get_info} ->
			%% Returns response to Client with info about current process.
			Response = {info, Parent, self(), LChild, RChild, Type},
			Client ! {self(), Response},
			NewState = State;
		{Client, proc_exit} ->
			%% Sends exit signal to ExitHandler with State information.
			Client ! {self(), exited},
			exit({state, State}),
			NewState = State;
		create_children ->
			%% Creates left and right children and links them to the exit handler.
			NewLChild = create_child(Level-1, ExitHandler, {Level-1, ExitHandler, left, self(), [], []}),
			NewRChild = create_child(Level-1, ExitHandler, {Level-1, ExitHandler, right, self(), [], []}),
			NewState = {Level, ExitHandler, Type, Parent, NewLChild, NewRChild};
		{update_child, ChildType, NewChild} ->
			%% Update parent of exited child.
			{NewLChild, NewRChild} = update_child({LChild, RChild}, ChildType, NewChild),
			NewState = {Level, ExitHandler, Type, Parent, NewLChild, NewRChild};
		{update_parent, NewParent} ->
			%% Update child of exited parent.
			NewState = {Level, ExitHandler, Type, NewParent, LChild, RChild};
		Other ->
			io:format("Other from loop ~p: ~p~n", [self(), Other]),
			NewState = State
	end,
	loop(NewState).

rpc(Server, Request) ->
	Server ! {self(), Request},
	receive
		{Server, exited} ->
			io:format("Pid: ~p exited.~n", [Server]);
		{Server, {info, Parent, Pid, LChild, RChild, Type}} ->
			io:format("Current Pid: ~p(~p)~n\tParent: ~p~n\tLeft Child: ~p~n\tRight Child: ~p~n", [Pid, Type, Parent, LChild, RChild]);
		Other ->
			io:format("Other from client: ~p~n", [Other])
	end.

%% Creates SysProcess - ExitHandler and creates Root process of binary tree.
start({N}) ->
	ExitHandler = spawn(proc_struct, exit_handler, []),
	ExitHandler ! set_sysproc,
	Root = spawn(proc_struct, loop, [{N, ExitHandler, root, [], [], []}]),
	io:format("~p. Process ~p created.~n", [N, Root]),
	ExitHandler ! {create_link, Root},
	Root ! create_children,
	Root.

%% Creates child process and links it to ExitHandler.
%% Sends creat)children message until Level gets to 0.
create_child(0, _, _) -> [];
create_child(N, ExitHandler, State) ->
	Child = spawn(proc_struct, loop, [State]),
	ExitHandler ! {create_link, Child},
	io:format("~p. Process ~p created.~n", [N, Child]),
	Child ! create_children,
	Child.

%% Updates parents of processes that exited.
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
