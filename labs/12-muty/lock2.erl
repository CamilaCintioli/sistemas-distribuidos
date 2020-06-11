-module(lock2).

-export([start/1]).

start(Id) -> spawn(fun () -> init(Id) end).

init(Priority) ->
    receive
      {peers, Peers} -> open(Peers, Priority, []);
      stop -> ok
    end.

open(Nodes, Priority, Waiting) ->
    receive
      {take, Master} ->
        Refs = requests(Nodes, Priority),
        wait(Nodes, Master, Refs, Priority, Waiting);
      {request, From, Ref, PeerPriority} ->
        if PeerPriority < Priority -> From ! {ok, Ref};
          true -> open(Nodes,Priority, [{From, Ref} | Waiting])
        end,
        open(Nodes, Priority, Waiting);
      stop -> ok
    end.

requests(Nodes, Priority) ->
    lists:map(fun (P) ->
          R = make_ref(),
          P ! {request, self(), R, Priority},
          R
	      end,
	      Nodes).

wait(Nodes, Master, [], Priority, Waiting) ->
    Master ! taken,
    held(Nodes, Priority, Waiting);
wait(Nodes, Master, Refs, Priority, Waiting) ->
    receive
      {request, From, Ref} ->
    	  wait(Nodes, Master, Refs, Priority, [{From, Ref} | Waiting]);
      {ok, Ref} ->
        Refs2 = lists:delete(Ref, Refs),
        wait(Nodes, Master, Refs2, Priority, Waiting);
      release ->
        ok(Waiting),
        open(Nodes, Priority, [])
    end.

ok(Waiting) ->
    lists:foreach(fun ({F, R}) -> F ! {ok, R} end, Waiting).

held(Nodes, Priority, Waiting) ->
    receive
      {request, From, Ref} ->
	      held(Nodes, Priority, [{From, Ref} | Waiting]);
      release ->
        ok(Waiting),
        open(Nodes, Priority, [])
    end.
