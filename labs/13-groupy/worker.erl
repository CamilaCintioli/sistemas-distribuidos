- module(worker).

- export([start/2, start/3]).

start(Gms, Id) ->
  spawn(fun () -> init(Gms, Id) end).

start(Gms, Id, Grp) ->
  spawn(fun () -> init(Gms, Id, Grp) end).

init(Gms, Id) -> 
  {ok, Leader} = Gms:start(Id),
  stand_by(Leader).

init(Gms, Id, Grp) ->
  {ok, Slave} = Gms:start(Id, Grp),
  stand_by(Slave).

stand_by(Member) ->
  receive
    {join, Wrk, Peer} ->
      Member ! {join, Wrk, Peer},
      stand_by(Member);
    {view, Group} -> 
      io:format("~w - view: ~w~n", [Member, Group]),
      stand_by(Member);
    {error, ErrorMessage} ->
      io:format("~w - error: ~w~n", [Member, ErrorMessage]);
    stop -> 
      Member ! stop,
      ok;
    Msg -> 
      io:format("~w msg: ~w~n", [Member, Msg])
  end.
