-module(handler).
-export([start/3]).

start(Client, Validator, Store) ->
    spawn_link(fun () -> init(Client, Validator, Store)
         end).

init(Client, Validator, Store) ->
    handler(Client, Validator, Store, [], []).

handler(Client, Validator, Store, Reads, Writes) ->
    receive
        {read, Ref, N} ->
            case lists:keysearch(N, 1, Writes) of
                {value, {N, _, Value}} ->
                      Client ! {Ref, Value},
                    handler(Client, Validator, Store, Reads, Writes);
                false ->
                      Pid = store:lookup(N, Store),
                      Pid ! {read, Ref, self()},
                    handler(Client, Validator, Store, Reads, Writes)
            end;
        {Ref, Entry, Value, Time} ->
              Client ! {Ref,Value},
            handler(Client, Validator, Store, [{Entry,Time}|Reads], Writes);
        {write, N, Value} ->
            Entry = store:lookup(N, Store),
            Added = [{N, Entry, Value}|Writes],
            handler(Client, Validator, Store, Reads, Added);
        {commit, Ref} ->
            Validator ! {validate, Ref, Reads, Writes, Client};
        abort -> ok
    end. 