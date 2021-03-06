-module(chat_server).
-compile(export_all).
-import(lib_chan_mm, [send/2, controller/2]).
-import(lists, [delete/2, foreach/2, map/2, member/2, reverse/2]).

lookup(G, [{G, Pid}|_]) -> {ok, Pid};
lookup(G, [_|T]) -> lookup(G, T);
lookup(_, []) -> [].

remove_group(Pid, [{G, Pid}]|T) -> io:format("~p removed~n", [G]), T;
remove_group(Pid, [H|T]) -> [H|remove_group(Pid, T)];
remove_group(_, []) -> [].

server_loop(L) ->
	receive
		{mm, Channel, {login, Group, Nick}} ->
			case lookup(Group, L) of
				{ok, Pid} ->
					Pid ! {login, Channel, Nick},
					server_loop(L);
				error ->
					Pid = spawn_link(fun() -> chat_group:start(Channel, Nick) end),
					server_loop([{Group, Pid}|L])
			end;
		{mm_closed, _} ->
			server_loop(L);
		{'EXIT', Pid, allGone} ->
			L1 = remove_group(Pid, L),
			server_loop(L1);
		Msg ->
			io:format("Server received Msg=~p~n", [Msg]),
			server_loop(L)
	end.

start() ->
	start_server(),
	lib_chan:start_server("chat.conf").

start_server() ->
	register(chat_server, spawn(fun() -> process_flag(trap_exit, true), 
					     Val = (catch server_loop([])),
					     io:format("Server terminated with:~p~n", [Val])
				    end)).
