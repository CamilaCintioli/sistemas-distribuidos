-module(icy).

-export([request/1, reply/1, send_request/3, send_reply/2, send_data/2]).
-export([meta/3, padding/1]).

send_request(Host, Feed, Sender) ->
    Request = "GET " ++ Feed ++" HTTP/1.0\r\n" ++ 
    "Host: " ++ Host ++ "\r\n" ++
    "User-Agent: Ecast\r\n" ++
    "Icy-MetaData: 1\r\n" ++ "\r\n",
    Sender(list_to_binary(Request)).


send_reply(Header, Sender) ->
    Status = "ICY 200 OK\r\n",
    Reply = Status ++ to_header(Header),
    %% io:format("icy: reply ---~n~s~n", [Reply]),
    Sender(list_to_binary(Reply)).

to_header([]) ->
    "\r\n";
to_header([{Name, Arg}|Rest]) ->
    Str = atom_to_list(Name),
    Str ++ ":" ++ Arg ++ "\r\n" ++ to_header(Rest).


send_data({Audio, Meta}, Sender) ->
    send_audio(Audio, Sender),
    send_meta(Meta, Sender).

send_audio([], _) -> ok;
send_audio([Bin|Rest], Sender) ->
    Sender(Bin),
    send_audio(Rest, Sender).

send_meta(Meta, Sender) ->
    {K, Padded} = padding(Meta),
    %% io:format("icy: send meta k(~w): ~s~n", [K, Meta]),
    Sender(<<K/integer, Padded/binary>>).


padding(Meta) ->
    N = length(Meta),
    R = (N rem 16),
    MetaBin = list_to_binary(Meta),
    if 
  R == 0 ->
      {N div 16, MetaBin};
  true -> 
      {(N div 16)+1, <<MetaBin/binary, 0:(8*(16-R))>>}
    end.

reply(Bin) ->
    case read_line(Bin) of
  {ok, "ICY 200 OK", R1} ->
      case header(R1, []) of
    {ok, Header, R2} ->
        %% io:format("icy: header ~w~n", [Header]),
        MetaInt = metaint(Header),
        {ok, fun() -> data(R2, MetaInt) end, Header};
     more ->
        {more, fun(More) -> reply(<<Bin/binary, More/binary>>) end}
      end;
  {ok, Resp, _} ->
      {error, "invalid response: " ++ Resp};
  
        more ->
      {more, fun(More) -> reply(<<Bin/binary, More/binary>>) end }
    end.

request(Bin) ->
    case read_line(Bin) of
  {ok, "GET / HTTP/1.0", R1} ->
      case header(R1, []) of
    {ok, Header, R2} ->
        {ok, Header, R2};
    more ->
        {more, fun(More) -> request(<<Bin/binary, More/binary>>) end}
      end;
  {ok, "GET / HTTP/1.1", R1} ->
      case header(R1, []) of
    {ok, Header, R2} ->
        {ok, Header, R2};
    more ->
        {more, fun(More) -> request(<<Bin/binary, More/binary>>) end}
      end;
  {ok, Req, _} ->
      {error, "invalid request: " ++ Req};
  more ->
      {more, fun(More) -> request(<<Bin/binary, More/binary>>) end}
    end.
           

header(Bin, Sofar) ->
    case read_line(Bin) of
  {ok, [], Rest} ->
      {ok, header_encode(Sofar, []), Rest};
  {ok, Line, Rest} ->
      header(Rest, [Line|Sofar]);
   more ->
      more
    end.

header_encode([], All) ->
    All;
header_encode([Line|Lines], Sofar) ->
    {Name, [58|Arg]} = lists:splitwith(fun(X) -> X =/= 58 end, Line),
    header_encode(Lines, [{list_to_atom(Name), Arg}|Sofar]).


metaint([]) ->
    8192;
metaint([{'icy-metaint', Arg}|_]) ->
    {Int, _} = string:to_integer(string:strip(Arg, left, 32)),
    Int;
metaint([_|Rest]) ->
    metaint(Rest).

data(Bin, M) ->
    audio(Bin, [], M, M).

audio(Bin, Sofar, N, M) ->
    Size = size(Bin),
    %io:format("recieved ~w bytes~n", [Size]),
    if 
  Size >= N ->
      {Chunk, Rest} = split_binary(Bin,N),
      %io:format("recieved segment ~n", []),      
      meta(Rest, lists:reverse([Chunk|Sofar]), M);
  true ->
      {more, fun(More) -> audio(More, [Bin|Sofar], N-Size, M) end}
    end.

meta(<<>>, Audio, M) ->
    {more, fun(More) -> meta(More, Audio, M) end};
meta(<<K/integer, R0/binary>> = Bin, Audio, M) ->    
    Size = size(R0),
    H = K*16,
    if
  Size >= H ->
      {Padded, R2} = split_binary(R0,H),
      Meta = [C || C <- binary_to_list(Padded), C > 0],
      %% io:format("icy: received meta k(~w): ~s~n",[K, Meta]),      
      {ok, {Audio, Meta},  fun() -> data(R2, M) end};
  true ->
      %% io:format("icy: need more meta data~n",[]),      
      {more, fun(More) -> meta(<<Bin/binary, More/binary>>, Audio, M) end}
    end.

         
read_line(Bin) ->
    read_line(Bin, []).

read_line(<<>>, _) ->
    more;
read_line(<<13, 10, Rest/binary>>, Sofar) ->
    {ok, lists:reverse(Sofar), Rest};
read_line(<<Char/integer, Rest/binary>>, Sofar) ->
    read_line(Rest, [Char|Sofar]).
