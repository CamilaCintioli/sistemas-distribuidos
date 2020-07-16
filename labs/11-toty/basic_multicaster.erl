-module(basic_multicaster).

-export([start/4]).

%Id, self(), Seed, Jitter
start(Id, Master, Seed, Jitter) ->
  spawn(fun() -> init(Id, Master, Seed, Jitter) end).

init(Id, Master, Seed, Jitter) ->
  random:seed(Seed, Seed, Seed),
  receive
  {peers, Nodes} ->
    server(Id, Master, Nodes, Jitter)
  end.
    
server(Id, Master, Nodes, Jitter) ->
  receive
    {send, Msg} ->
      multicast(Msg, Nodes, Jitter),
      server(Id, Master, Nodes, Jitter);
    {multicast, _From, Msg} ->
      Master ! {deliver, Msg},
      server(Id, Master, Nodes, Jitter);
    stop ->
      ok
  end.

multicast(Msg, Nodes, 0) ->
  Self = self(),
  lists:foreach(fun(Node) ->
    Node ! {multicast, Self, Msg}
    end,
    Nodes);

multicast(Msg, Nodes, Jitter) ->
  Self = self(),
  lists:foreach(fun(Node) ->
    timer:sleep(random:uniform(Jitter)),
    Node ! {multicast, Self, Msg}
    end,
    Nodes).