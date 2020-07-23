-module(test).


-export([direct/0]).
-export([proxy/0, client/1, client/2, dummy/0, dummy/1, stress/1, stress/2]).

-export([dist/0]).
-export([root/0, branch/1, branch/2, leaf/0, leaf/1]).

-define(Cast, {cast, "streaming211.radionomy.com", 80, "/Alterna"}).

-define(Port, 3001).



direct() ->
    Proxy = spawn(proxy, init, [?Cast]),
    spawn(client, init, [Proxy, ?Port]).



proxy() ->
    register(icy, spawn(proxy, init, [?Cast])).

client(Port) ->
    spawn(client, init, [icy, Port]).

client(Proxy, Port) ->
    spawn(client, init, [Proxy, Port]).

dummy() ->
    spawn(dummy, init, [icy]).

dummy(Proxy) ->
    spawn(dummy, init, [Proxy]).

stress(M) ->
    stress(M, icy).

stress(M, Proxy) ->
    case M of
    0 ->
        ok;
    N when N > 0 ->
        dummy(Proxy),
        stress(N-1, Proxy)
    end.

dist() ->
    Proxy = spawn(proxy, init, [?Cast]),
    register(icy, spawn(dist, init, [Proxy])).

root() ->
    Proxy = spawn(proxy, init, [?Cast]),
    register(icy, spawn(root, init, [Proxy])).

branch(Port) ->
    Branch = spawn(branch, init, [icy]),
    spawn(client, init, [Branch, Port]).

branch(Proxy, Port) ->
    Branch = spawn(branch, init, [Proxy]),
    spawn(client, init, [Branch, Port]).
