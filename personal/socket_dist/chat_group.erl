-module(chat_group).
-export([start/2]).
-import([lib_chan_mm, [send/2, controller/2]]).
-import(lists, [foreach/2, reverse/2]).

delete(Pid, [{Pid, Nick}|T], L) -> {Nick, reverse(T, L)};
delete(Pid, [H|T], L) -> delete(Pid, T, [H|L]);
delete(_, [], L) -> {'????', L}.

group_controller([]) -> exit(allGone);
group_controller(L) ->
	receive
		{C, {relay, Nick, Str}} ->
			foreach(fun({Pid, _}) -> Pid ! {msg, Nick, C, Str} end, L),
			group_controller(L);
		{login, C, Nick} ->
			controller(C, self()),
			C ! ack,
			self() ! {C, {relay, Nick, "I'm joining the group"}},
			group_controller([{C, Nick}|L]);
		{close, C} ->
			{Nick, L1} = delete(C, L, []),
			self() ! {C, {relay, Nick, "I'm leaving the group"}},
			group_controller(L1);
		Other ->
			io:format("group controller received Msg=~p~n", [Other]),
			group_controller(L)
	end.

start(C, Nick) ->
	process_flag(trap_exit, true),
	controller(C, self()),
	C ! ack,
	self() ! {C, {relay, Nick, "I'm starting the group"}},
	group_controller([{C, Nick}]).
