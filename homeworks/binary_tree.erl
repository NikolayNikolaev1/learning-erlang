-module(binary_tree).
-export([createChild/2, loop/1, start/1]).

createChild(Level, ParentPid) ->
	spawn(binary_tree, loop, [{Level, ParentPid, [], []}]).

loop({1, Parent, LChild, RChild}) ->
	Level = 1,
	State = {Level, Parent, LChild, RChild},

	receive
		create_child -> NewState = State;
		{RootPid, get_tree, TreeList}
		Any -> Any,
		       NewState = State
	end,

	loop(NewState);

loop({Level, Parent, LChild, RChild}) ->
	State = {Level, Parent, LChild, RChild},

	receive
		create_child -> NewLChild = createChild(Level-1, self()),
				NewRChild = createChild(Level-1, self()),
				NewLChild ! create_child,
				NewRChild ! create_child,
				NewState = {Level, Parent, NewLChild, NewRChild};
		{get_children, TreeList} -> Parent ! {get_children, [{children, LChild, RChild} | TreeList]},
					    NewState = State;
		{RootPid, get_tree, TreeList} -> BinaryTree  = [{Level, self()} | TreeList],
						 LChild ! {RootPid, get_tree, BinaryTree},
						 RChild ! {RootPid, get_tree, BinaryTree},
						 NewState = State;
		Any -> Any,
		       NewState = State
	end,

	loop(NewState).

rpc(TreePid, Request, TreeList) ->
	Tree ! {self(), Request},
	receive
		{get_tree, TreeList} -> 
	end.

start({Size}) ->
	RootPid = spawn(binary_tree, loop, [{Size, self(), [], []}]),
	spawn(binary_tree, rpc, [RootPid, [], ]).
