-module(mod_chat_controller).
-export([start/3]).
-import(lib_chan_mm, [send/2]).

loop(MM) ->
	receive
		{chan, Mm, Msg} ->
			chat_server ! {mm, MM, Msg},
			loop(MM);
		{'EXIT', MM, _Why} ->
			chat_server ! {mm_closed, MM};
		Other ->
			io:format("mod_chat_controller unexpected message =~p (MM=~p)~n", [Other, MM]),
			loop(MM)
	end.

start(MM, _, _) ->
	process_flag(trap_exit, true),
	io:format("mod_chat_controller of we go ...~p~n", [MM]),
	loop(MM).
