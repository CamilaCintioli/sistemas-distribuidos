-module(lock3).
-export([start/1]).

start(Id) -> spawn(fun () -> init(Id) end).

init(Priority) ->
    receive
      {peers, Peers} -> open(Peers, Priority, [], time:zero());
      stop -> ok
    end.

open(Nodes, Priority, Waiting, Clock) ->
    receive
      {take, Master} ->
        Refs = requests(Nodes, Priority, Clock),
        wait(Nodes, Master, Refs, Priority, Waiting, Clock);
      {request, From, Ref, _, _} ->
        From ! {ok, Ref},
        open(Nodes, Priority, Waiting, time:inc(Clock));
      stop -> ok
    end.

requests(Nodes, Priority, Clock) ->
    lists:map(fun (P) ->
          R = make_ref(),
          P ! {request, self(), R, Priority, Clock},
          R
        end,
        Nodes).


wait(Nodes, Master, [], Priority, Waiting, Clock) ->
    Master ! taken,
    held(Nodes, Priority, Waiting,Clock);
wait(Nodes, Master, Refs, Priority, Waiting, Clock) -> 
    receive
        {request, From, Ref, PeerPriority, PeerClock} ->
            IsLess = time:less(PeerClock, Clock),
            IsEq = time:eq(PeerClock, Clock),
            if IsLess or (IsEq and PeerPriority < Priority) ->
                From ! {ok, Ref},
                wait(Nodes, Master, Ref, Priority, Waiting, time:inc(Clock));
            true ->
                wait(Nodes, Master, Refs, Priority, [{From, Ref} | Waiting], time:inc(Clock))
        end; 
      {ok, Ref} ->
        Refs2 = lists:delete(Ref, Refs),
        wait(Nodes, Master, Refs2, Priority, Waiting, Clock);
      release ->
        ok(Waiting),
        open(Nodes, Priority, [], Clock)
    end.

ok(Waiting) ->
    lists:foreach(fun ({F, R}) -> F ! {ok, R} end, Waiting).

held(Nodes, Priority, Waiting,Clock) ->
    receive
      {request, From, Ref, _,_} ->
        held(Nodes, Priority, [{From, Ref} | Waiting],time:inc(Clock));
      release ->
        ok(Waiting),
        open(Nodes, Priority, [],Clock)
    end.
