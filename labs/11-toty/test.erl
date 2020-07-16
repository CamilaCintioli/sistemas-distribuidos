-module(test).

-export([start/4, stop/0]).

start(Module, N, Sleep, Jitter) ->
  register(test, spawn(fun() -> init(Module, N, Sleep, Jitter,N) end)).

stop() ->
  test ! stop.

init(_Module,N,_Sleep,_Jitter,M) when N < 1 ->
  collect(M);
init(Module,N,Sleep,Jitter,M) ->
  Self = self(),
  worker:start(integer_to_list(N), Self, Module, N, Sleep, Jitter),
  init(Module,N-1,Sleep,Jitter,M).


collect(N) -> collect(N, [], []).

collect(N, Workers, Peers) when N < 1 ->
  run(Workers, Peers);
collect(N, Workers, Peers) ->
  receive
    {join, W, P} -> 
      collect(N-1, [W|Workers], [P|Peers])
  end.
  
run(Workers, Peers) ->
  Color = {0,0,0},
  lists:foreach(
    fun(W) ->
      W ! {state, Color, Peers}
    end,
    Workers),
  receive
    stop ->
    lists:foreach(
      fun(W) ->
        W ! stop
      end,
      Workers)
  end.

%c(worker),c(basic_multicaster),c(test),c(gui),c(seq).
%test:start(basic_multicaster,3,1000,0).
%test:stop().