-module(basic_multicaster).

-export([start/1]).

-define(default_jitter, 1000).

start(Peers) -> {ok, spawn(fun () -> init(Peers) end)}.

init(Peers) -> server(Peers).

server(Peers) ->
    receive
      {send, Msg} -> mcast(Msg, Peers), server(Peers)
    end.

mcast(Msg, Peers) ->
    lists:foreach(fun (Peer) -> jitter(), Peer ! Msg end,
		  Peers).

jitter() -> jitter(?default_jitter).

jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).
