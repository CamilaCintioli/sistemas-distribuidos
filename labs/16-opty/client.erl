-module(client).
-export([start/1]).

start(Server) -> 
    spawn(fun() -> init(Server)end).

init(Server)->
    Handler = server:open(Server),
    client(Handler).

client(Handler) ->
    receive
        {read, N}-> read(Handler,N),
        client(Handler);
        {write, N, Value} -> Handler ! {write,N,Value},
        client(Handler);
        commit -> commit(Handler);
        abort -> Handler ! abort
    end.

read(Handler,N) -> 
    Ref = make_ref(),    
    Handler ! {read,Ref,N},
    receive
        {Ref,Value} -> io:format("~s~n",[Value])
    end.

commit(Handler) ->
    Ref = make_ref(),
    Handler ! {commit, Ref},
    receive
        {Ref, ok} -> io:format("Commit ok~n",[]);
        {Ref,abort} -> io:format("Commit abortado~n",[])
    end.