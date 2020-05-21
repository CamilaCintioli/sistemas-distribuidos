-module(client).
-export([start/1, read/2, write/3, commit/1]).

start(Server) -> 
    spawn(fun() -> init(Server)end).

init(Server)->
    Handler = server:open(Server),
    client(Handler).

client(Handler) ->
    receive
        {read, N}->
            Value = read(Handler,N),
            io:format("~s~n",[Value]),
            client(Handler);
        {write, N, Value} ->
            write(Handler,N,Value),
            client(Handler);
        commit ->
            case commit(Handler) of
                ok -> io:format("Commit ok~n",[]);
                abort -> io:format("Commit abortado~n",[])
            end;
        abort ->
            Handler ! abort
    end.

read(Handler,N) -> 
    Ref = make_ref(),    
    Handler ! {read,Ref,N},
    receive
        {Ref,Value} -> Value
    end.

write(Handler,N,Value) ->
    Handler ! {write,N,Value}.

commit(Handler) ->
    Ref = make_ref(),
    Handler ! {commit, Ref},
    receive
        {Ref, ok} -> ok;
        {Ref, abort} -> abort
    end.
