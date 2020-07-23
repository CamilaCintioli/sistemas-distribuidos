-module(proxy).

-export([init/1]).

-define(Opt, [binary, {packet, 0}, {active, true}, {nodelay, true}]).
-define(TimeOut, 5000).

init(Cast) -> 
    receive 
  {request, Client} ->
      io:format("proxy: received request~n", []),
      Ref = erlang:monitor(process, Client),
      case attach(Cast, Ref) of 
    {ok, Stream, Cont, Context} -> 
        io:format("proxy: attached~n", []),
        N = 0,
        Client ! {reply, N, Context}, 
        {ok, Msg} = loop(Cont, N, Stream, Client, Ref),
        io:format("proxy: ~s~n", [Msg]);
    {error, Error} -> 
        io:format("proxy: ~s~n", [Error])
      end
    end.

attach({cast, Host, Port, Feed}, Ref) ->
    case gen_tcp:connect(Host, Port, ?Opt) of
      {ok, Stream} ->
        case request(Host, Feed, Stream) of
          ok ->
            case reply(Stream, Ref) of
                {ok, Cont, Context} ->
                  {ok, Stream, Cont, Context};
                {error, Error} ->
                  {error, Error}
            end;
          _ ->
            {error, "unable to send request"}
          end;
      _ ->
        {error, "unable to connect to server"}
    end.

loop(Cont, N, Stream, Client, Ref) ->
    case reader(Cont, Stream, Ref) of
  {ok, Data, Rest} ->
      Client ! {data, N, Data},
      loop(Rest, N+1, Stream, Client, Ref);
  {error, Error} ->
      {ok, Error}
    end.
  

request(Host, Feed, Stream) ->
    icy:send_request(Host, Feed, fun(Bin) -> gen_tcp:send(Stream, Bin) end).

reply(Stream, Ref) ->
  reader(fun()-> icy:reply(<<>>) end, Stream, Ref).


reader(Parser, Stream, Ref) ->
    case Parser() of
  {ok, Parsed, Rest} ->
      {ok, Parsed, Rest};
  {more, Cont} ->
      receive
    {tcp, Stream, More} ->
        reader(fun() -> Cont(More) end,  Stream, Ref);
    {tcp_closed, Stream} ->
        {error, "icy server closed connection"};    
    {'DOWN', Ref, process, _, _}  ->
        {error, "client died"}
      after ?TimeOut ->
        {error, "time out"}
      end;
  {error, Error} ->
      {error, Error}
    end.
