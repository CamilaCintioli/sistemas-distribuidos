-module(gms3).
-export([start/1, start/2]).

-define(arghh, 100).
-define(timeout, 1000).

% Inicializacion de unico nodo en un grupo sin integrantes
start(Id) ->
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Self) end)}.

init(Id, Master) ->
    leader(Id, Master, 1, [], [Master]).

% Unirse a grupo que ya existe.
start(Id, Grp) ->
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Grp, Self) end)}.

init(Id, Grp, Master) ->
    Self = self(),
    Grp ! {join, Master, Self},
    receive
        {view, N, [Leader|Slaves], Group} ->
        LastMessage = {view, N, [Leader|Slaves],Group},
        Master ! {view, Group},
        slave_monitor(Id, Master, Leader,N+1,LastMessage, Slaves, Group)
    after ?timeout ->
        Master ! {error, "no reply from leader"}
    end.

bcast(Id, Msg, Nodes) ->
    lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).


leader(Id, Master,N, Slaves, Group) ->
    io:format("~s soy un leader~n",[Id]),
    receive
        {mcast, Msg} ->
            bcast(Id, {msg,N, Msg}, Slaves),
            Master ! Msg,
            leader(Id, Master,N+1, Slaves, Group);
        {join, Wrk, Peer} ->
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            bcast(Id, {view, N, [self()|Slaves2], Group2}, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master,N+1, Slaves2, Group2);
        stop -> ok
    end.

slave_monitor(Id, Master,Leader,N,Last, Slaves, Group) ->
    erlang:monitor(process, Leader),
    slave(Id, Master, Leader,N,Last, Slaves, Group).

slave(Id, Master, Leader, N, Last, Slaves, Group) ->
    io:format("~s soy un nuevo esclavo~n",[Id]),
    receive
        {mcast, Msg} ->
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader,N,Last, Slaves, Group);
        {join, Wrk, Peer} ->
            Leader ! {join, Wrk, Peer},
            slave(Id, Master, Leader,N,Last,Slaves, Group);
        {msg, I, _Msg} when I < N ->
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {msg, N, Msg} ->
            Master ! Msg,
            NewLastMessage = {msg,N,Msg},
            slave(Id, Master, Leader,N+1,NewLastMessage,Slaves, Group);
        {view, I, Leader, _} when I < N ->
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {view, N, [Leader|Slaves2], Group2} ->
            Master ! {view, Group2},
            NewLastMessage = {view,N,[Leader|Slaves2], Group2},
            slave(Id, Master, Leader,N+1,NewLastMessage, Slaves2, Group2);
        {'DOWN', _Ref, process, Leader, _Reason} ->
                io:format("~w he crasheado ~n",[Leader]),
                election(Id, Master,N, Last, Slaves, Group);
        stop ->
            ok
    end.


crash(Id) ->
  case random:uniform(?arghh) of
      ?arghh ->
          io:format("leader ~s: crash~n", [Id]),
          exit(no_luck);
      _ -> ok
  end.


election(Id, Master,N, Last, Slaves, [_|Group]) ->
    Self = self(),
    case Slaves of
        [Self|Rest] ->
            % Yo soy el nuevo lider
            io:format("Nuevo Lider: ~s~n", [Id]),
            bcast(Id, Last, Rest),
            bcast(Id, {view,N, Slaves, Group}, Rest),
            Master ! {view, Group},
            leader(Id, Master, N+1, Rest, Group); 
        [Leader|Rest] ->
            slave_monitor(Id, Master, Leader,N,Last, Rest, Group)
    end.
