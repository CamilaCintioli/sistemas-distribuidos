-module(test).

-export([direct/0]).

-define(Cast, {cast, "audiophile.no-ip.org", 8000, "/stream"}).

-define(Port, 8079).

direct() ->
    Proxy = spawn(proxy, init, [?Cast]),
    spawn(client, init, [Proxy, ?Port]).
