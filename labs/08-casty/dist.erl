-module(dist).
-export([init/1]).

-define(TimeOut, 5000).

init(Proxy) ->
    Proxy ! {request, self()},
    receive 
    {reply, N, Context} ->
      io:format("dist: connected~n", []),
      loop([], N, Context)
    after ?TimeOut ->
      io:format("dist: could not connect~n", []),
      ok
    end.


loop(Clients, N, Context) ->
    receive 
  {data, N, Data} ->
      multicast(Clients,{data, N, Data}),
      loop(Clients, N+1, Context);
  {request, From} ->
      io:format("dist: adding client ~w~n", [From]),
      Ref = erlang:monitor(process, From),
      From ! {reply, N, Context},
      loop([{Ref, From}|Clients], N, Context);
  {'DOWN', Ref, process, Pid, _} ->
        io:format("dist: client down ~w~n", [Pid]),
        {_, _, NewClients} = lists:keytake(Ref,1,Clients),
      loop(NewClients, N, Context);
  stop ->
      io:format("dist: received stop message~n", []),
      ok;
  stat ->
      io:format("dist: clients ~w ~n", [length(Clients)]),
      loop(Clients, N, Context);        
  Strange ->
      io:format("dist: received strange message~w~n", [Strange]),
      loop(Clients, N, Context)
    end.


multicast([], _Msg) ->      
    ok;
multicast([{_,Pid}|Rest], Msg) ->
    Pid ! Msg,
    multicast(Rest, Msg).