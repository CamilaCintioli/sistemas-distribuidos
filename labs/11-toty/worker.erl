-module(worker).

-export([start/0]).

start() -> {ok, spawn(fun () -> init() end)}.

init() -> loop().

loop() ->
    receive _Msg -> io:format("message"), loop() end.
