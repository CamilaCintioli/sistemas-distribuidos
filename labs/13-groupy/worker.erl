- module(worker).

- export([start/2, start/3]).

start(Gms, Id) ->
  spawn(fun () -> init(Gms, Id) end).

start(Gms, Id, Grp) ->
  spawn(fun () -> init(Gms, Id, Grp) end).

init_gui(Name) ->
  spawn_link(gui, init, [Name]).


init(Gms, Id) -> 
  {ok, Leader} = Gms:start(Id),
  Gui = init_gui(Id),
  stand_by(Leader, Gui).

init(Gms, Id, Grp) ->
  {ok, Slave} = Gms:start(Id, Grp),
  Gui = init_gui(Id),
  stand_by(Slave, Gui).

stand_by(Member, Gui) ->
  timer:sleep(1000),
  receive
    {join, Wrk, Peer} ->
      Member ! {join, Wrk, Peer},
      Gui ! {join, Wrk, Peer},
      stand_by(Member, Gui);
    {view, Group} -> 
      io:format("~w - view: ~w~n", [Member, Group]),
      Gui ! {view, Group},
      stand_by(Member, Gui);
    {error, ErrorMessage} ->
      io:format("~w - error: ~w~n", [Member, ErrorMessage]),
      Gui ! {error, ErrorMessage},
      join_error(Gui);
    stop -> 
      Member ! stop,
      Gui ! stop,
      ok;
    Msg -> 
      io:format("~w msg: ~w~n", [Member, Msg]),
      Gui ! Msg,
      stand_by(Member, Gui)
  end.

join_error(Gui) ->
  receive
    stop -> 
      Gui ! stop,
      ok
  end.
