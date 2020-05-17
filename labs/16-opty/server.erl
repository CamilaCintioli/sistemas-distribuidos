-module(server).
-export([open/1, start/1, stop/1]).

start(N) -> spawn(fun () -> init(N) end).

init(N) ->
    Store = store:new(N),
    Validator = validator:start(),
    server(Validator, Store).

open(Server) ->
        Server ! {open, self()},
        receive
            {transaction, Validator, Store} ->
                handler:start(self(), Validator, Store)
        end.

server(Validator, Store) ->
        receive
            {open, Client} ->
                 %completar
                server(Validator, Store);
            stop ->
                 store:stop(Store)
        end.
