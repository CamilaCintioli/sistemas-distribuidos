-module(test).

-export([direct/0]).

-define(Cast, {cast, "localhost", 80, "/pepita"}).

-define(Port, 8080).

direct() ->
    Proxy = spawn(proxy, init, [?Cast]),
    spawn(client, init, [Proxy, ?Port]).
