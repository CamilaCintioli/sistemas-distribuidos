-module(server).
-export([start/1, stop/0]).
-import(rudy, [init/1]).

start(Port) ->
    register(rudy, spawn(fun() -> init(Port) end)).

stop() ->
    exit(whereis(rudy), "time to die").
