-module(dummy).
-export([init/1]).

init(Proxy) ->
    io:format("dummy: request to ~w~n", [Proxy]),
    Proxy ! {request, self()},
    receive 
  {reply, N, _Context} ->
      io:format("dummy: connected~n", []),
      loop(N)
    after 5000 ->
      io:format("dummy: time-out~n", [])
    end.

loop(N) ->
    receive 
  {data, N, _} ->
      % io:format("dummy: received ~w~n", [N]),
      loop(N+1);
  {data, I, _} ->
      io:format("dummy: received ~w expected ~w~n", [I,N]),
      loop(I+1);
  stop ->
      ok
    after 5000 ->
      ok
    end.