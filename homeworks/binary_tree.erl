-module(binary_tree).
-export([loop/1, start/1]).

createChild(Level, ParentPid) ->
	spawn(binary_tree, loop, [{Level, ParentPid, [[] []]}]).

loop({1, Parent, LChild, RChild}) ->
	Size = 1,
	Stage = {Size, Parent, LChild, RChild},

	receive
		create_child -> Parent ! get_left
		Any -> Any,
		       NewStage = Stage
	end,

	loop(NewStage);

loop({Level, Parent, LChild, RChild}) ->
	Stage = {Size, Parent, LChild, RChild},

	receive
		create_child -> NewLChild = createChild(Level-1, self()),
				NewRChild = createChild(Level-1, self()),
				NewLChild ! create_child,
				NewRChild ! create_child,
				NewStage = {Size, Parent, NewLChild, NewRChild};
		{double, Num} -> 
		Any -> Any,
		       NewStage = Stage
	end,

	loop(NewStage).


start({Size}) ->
	RootPid = spawn(binary_tree, loop, [{Size, [], [], []}]),

	receive
		Response -> Response
	end,

	RootPid.
