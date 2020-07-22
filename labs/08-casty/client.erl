-module(client).

-export([init/2, loop/2]).

-define(Opt,[binary, {packet, 0}, {reuseaddr, true}, {active, true}, {nodelay, true}]).

-define(TimeOut, 5000).

init(Proxy, Port) ->
    {ok, Listen} = gen_tcp:listen(Port, ?Opt),
    {ok, Socket} = gen_tcp:accept(Listen),
    case request(Socket) of
	{ok, Request, _} ->
	    io:format("client: received request ~p~n", [Request]),
	    case connect(Proxy) of
		{ok, N, Context} ->
		    io:format("client: connected ~n", []),
		    send_reply(Context, Socket),
		    {ok, Msg} = loop(N, Socket),
		    io:format("client: ~s~n", [Msg]);
		{error, Error} ->
		    io:format("client: ~s~n", [Error])
	    end;
	{error, Error} ->
	    io:format("client: ~s~n", [Error])
    end.

connect(Proxy) ->
    Proxy ! {request, self()},
    receive 
	{reply, N, Context} ->
	    {ok, N, Context}
    after ?TimeOut ->
	    {error, "time out"}
    end.


loop(_, Socket) ->
    receive
	{data, N, Data} ->
	    %io:format("client: received ~w~n", [N]),
	    send_data(Data, Socket),
	    client:loop(N+1, Socket);
	{tcp_closed, Socket} ->
	    {ok, "player closes"}
    after ?TimeOut ->
	    {ok, "time out"}
    end.


send_data(Data, Socket) ->
    icy:send_data(Data, fun(Bin)-> gen_tcp:send(Socket, Bin) end).

send_reply(Context, Socket) ->
    icy:send_reply(Context, fun(Bin)-> gen_tcp:send(Socket, Bin) end).

    
request(Socket) ->
    reader(fun()-> icy:request(<<>>) end, Socket).
	    
reader(Cont, Socket) ->
    case Cont() of
	{ok, Parsed, Rest} ->
	    {ok, Parsed, Rest};
	{more, Fun} ->
	    receive
		{tcp, Socket, More} ->
		    reader(fun() -> Fun(More) end,  Socket);
		{tcp_closed, Socket} ->
		    {error, "server closed connection"}
	    after ?TimeOut ->
		    {error, "time out"}
	    end;
	{error, Error} ->
	    {error, Error}
    end.
