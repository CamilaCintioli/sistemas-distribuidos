-module(gms2).
-export([start/1, start/2]).

-define(arghh, 100).
-define(timeout, 1000).

% Inicializacion de unico nodo en un grupo sin integrantes
start(Id) ->
    Self = self(),
    {ok, spawn(fun()-> init(Id, Self) end)}.

init(Id, Master) ->
    leader(Id, Master, [], [Master]).

% Unirse a grupo que ya existe.
start(Id, Grp) ->
    Self = self(),
    {ok, spawn(fun()-> init(Id, Grp, Self) end)}.

init(Id, Grp, Master) ->
    Self = self(),
    Grp ! {join, Master, Self},
    receive
        {view, [Leader|Slaves], Group} ->
        Master ! {view, Group},
        slave_monitor(Id, Master, Leader, Slaves, Group)
    after ?timeout ->
        Master ! {error, "no reply from leader"}
    end.

bcast(Id, Msg, Nodes) ->
    lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).

leader(Id, Master, Slaves, Group) ->
    io:format("~w soy un leader~n",[Id]),
    receive
        {mcast, Msg} ->
            bcast(Id, {msg, Msg}, Slaves),
            Master ! Msg,
            leader(Id, Master, Slaves, Group);
        {join, Wrk, Peer} ->
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            bcast(Id, {view, [self()|Slaves2], Group2}, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master, Slaves2, Group2);
        stop -> ok
    end.

slave_monitor(Id, Master, Leader, Slaves, Group) ->
    erlang:monitor(process, Leader),
    slave(Id, Master, Leader, Slaves, Group).

slave(Id, Master, Leader, Slaves, Group) ->
    io:format("~w soy un nuevo esclavo~n",[Id]),
    receive
        {mcast, Msg} ->
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader, Slaves, Group);
        {join, Wrk, Peer} ->
            Leader ! {join, Wrk, Peer},
            slave(Id, Master, Leader, Slaves, Group);
        {msg, Msg} ->
            Master ! Msg,
            slave(Id, Master, Leader, Slaves, Group);
        {view, [Leader|Slaves2], Group2} ->
            Master ! {view, Group2},
            slave(Id, Master, Leader, Slaves2, Group2);
        {'DOWN', _Ref, process, Leader, _Reason} ->
                io:format("~w he crasheado ~n",[Leader]),
                election(Id, Master, Slaves, Group);
        stop ->
            ok
    end.


crash(Id) ->
  case random:uniform(?arghh) of
      ?arghh ->
          io:format("leader ~w: crash~n", [Id]),
          exit(no_luck);
      _ -> ok
  end.


election(Id, Master, Slaves, [_|Group]) ->
    Self = self(),
    case Slaves of
        [Self|Rest] ->
            % Yo soy el nuevo lider
            io:format("Nuevo Lider: ~w~n", [Self]),
            bcast(Id, {view, Slaves, Group}, Rest),
            Master ! {view, Group},
            leader(Id, Master, Rest, Group);
        [Leader|Rest] ->
            slave_monitor(Id, Master, Leader, Rest, Group)
    end.
