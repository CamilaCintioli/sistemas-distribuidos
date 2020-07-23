-module(branch).

-export([init/1]).

-define(Branching, 2).

init(Proxy) ->
    receive 
      {request, Client} ->
        connect(Proxy, Client)
    end.

connect(Proxy, Client) ->
  Proxy ! {request, self()},
  receive 
    {reply, Context} ->
      io:format("source: connected~n", []),
      Client ! {reply, Context},
      loop([], Client, Context),
      io:format("source: terminating~n", []);
    {redirect, NewProxy} ->
      connect(NewProxy, Client);
    stop ->
      io:format("source: terminating~n", []),
      ok
    after 5000 ->
      io:format("source: could not connect~n", []),
      ok
  end.

%Monitorear cliente.
loop(Clients, N, Context) ->
    receive
        {data,N,Data} ->
            multicast(Clients, {data, N, Data}),
            loop(Clients,N+1,Context);
        {request, From} ->
            L = length(Clients),
            if
              L < 2 ->
                From ! {reply, N, Context},
                loop([From|Clients], N+1, Context);
              true -> 
                [Left,Right] = Clients,
                From ! {redirect, Left},
                loop([Right,Left],N,Context)
            end
    end.

multicast([], _Msg) ->
    ok;
multicast([{_,Pid}|Rest], Msg) ->
    Pid ! Msg,
    multicast(Rest, Msg).

% una vez conectado fowardea la data al cliente.
% debe estar abierto para requests de otras branches. 