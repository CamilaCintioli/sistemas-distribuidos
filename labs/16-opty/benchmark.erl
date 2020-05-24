-module(benchmark).

-export([start/4]).

start(StoreSize, ClientCount, ReadCount, WriteCount) ->
    Server = server:start(StoreSize),
    Reporter = self(),
    Clients = init_clients(ClientCount, Reporter, Server, ReadCount, WriteCount, StoreSize),
    Reports = reports(Clients),
    io:format(format_reports(Reports)).

format_reports([]) -> "~n";
format_reports([{_, Time, ok} | Reports]) -> "Time: " ++ integer_to_list(Time) ++ " microseconds - Result: Ok~n" ++ format_reports(Reports);
format_reports([{_, Time, abort} | Reports]) -> "Time: " ++ integer_to_list(Time) ++ " microseconds - Result: Aborted~n" ++ format_reports(Reports).

reports([]) -> [];
reports([_Client|Clients]) ->
    receive
        {report, Time, Result} ->
                [{report, Time, Result} | reports(Clients)]
    end.

init_clients(0, _Reporter, _Server, _ReadCount, _WriteCount, _MaxPosition) -> [];
init_clients(ClientCount, Reporter, Server, ReadCount, WriteCount, MaxPosition) -> [init_client(Reporter, Server, ReadCount, WriteCount, MaxPosition)|init_clients(ClientCount - 1, Reporter, Server, ReadCount, WriteCount, MaxPosition)].

init_client(Reporter, Server, ReadCount, WriteCount, MaxPosition) -> 
    spawn(fun () -> 
        {Time, Result} = timer:tc(fun () -> client(Server, ReadCount, WriteCount, MaxPosition) end),
        Reporter ! {report, Time, Result}
    end).

client(Server, ReadCount, WriteCount, MaxPosition) ->
    Handler = server:open(Server),
    Operations = make_operations(ReadCount, WriteCount,
				 MaxPosition),
    client(Handler, Operations).

client(Handler, []) -> client:commit(Handler);
client(Handler, [{read, Position} | Operations]) ->
    client:read(Handler, Position),
    client(Handler, Operations);
client(Handler,
       [{write, Position, Value} | Operations]) ->
    client:write(Handler, Position, Value),
    client(Handler, Operations).

make_operations(ReadCount, WriteCount, MaxPosition) ->
    shuffle:list(reads(ReadCount, MaxPosition) ++
		   writes(WriteCount, MaxPosition)).

reads(0, _MaxPosition) -> [];
reads(N, MaxPosition) ->
    [{read, 1 + trunc(rand:uniform() * MaxPosition)} | reads(N - 1,
						  MaxPosition)].

writes(0, _MaxPosition) -> [];
writes(N, MaxPosition) ->
    [{write, 1 + trunc(rand:uniform() * MaxPosition), make_ref()}
     | reads(N - 1, MaxPosition)].
