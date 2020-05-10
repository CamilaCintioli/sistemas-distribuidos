-module(consumer).

-export([start/1, stop/0]).


start(ProducerPid) ->
    spawn(fun() -> init(ProducerPid) end).

init(ProducerPid) ->
    ProducerPid ! {hello, self()}, consume(0).

consume(M) ->
    receive
      {ping, M} ->
	  io:format("El valor esperado es ~w~n", [M]),
      consume(M+1);
      {ping, N} when N>M -> io:format("Warning~n", []),
      consume(N+1);
      bye -> stop()
    end.

stop() -> io:format("BYE ~n", []).