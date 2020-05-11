-module(consumer).

-export([start/1, stop/0]).


start(ProducerPid) ->
    spawn(fun() -> init(ProducerPid) end).

init(ProducerPid) ->
    Monitor = monitor(process, ProducerPid),
    ProducerPid ! {hello, self()},
    consume(0, Monitor).

consume(M,Monitor) ->
    receive
      {ping, M} ->
	  io:format("El valor esperado es ~w~n", [M]),
      consume(M+1,Monitor);
      {ping, N} when N>M -> io:format("Warning~n", []),
      consume(N+1,Monitor);
      {'DOWN', Monitor, process, Object, Info} ->
        io:format("~w died; ~w~n", [Object, Info]),
        consume(M, Monitor);
      bye -> stop()
    end.

stop() -> io:format("BYE ~n", []).