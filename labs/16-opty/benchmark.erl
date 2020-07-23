-module(benchmark).

-export([start/4]).

start(StoreSize, ClientCount, ReadCount, WriteCount) ->
    Server = server:start(StoreSize),
    Reporter = self(),
    {Time, Reports} = timer:tc(fun() -> 
        Clients = init_clients(ClientCount, Reporter, Server, ReadCount, WriteCount, StoreSize), reports(Clients) end),
    io:format(format_reports(Reports)),
    Aggregations = report_aggregations(Reports),
    io:format(format_report_aggregations(Aggregations)),
    io:format(format_duration(Time)),
    io:format(format_averages_transaction_per_second(Aggregations, Time)).

format_reports([]) -> "~n";
format_reports([{_, Time, ok} | Reports]) -> "Time: " ++ integer_to_list(Time) ++ " microseconds - Result: Ok~n" ++ format_reports(Reports);
format_reports([{_, Time, abort} | Reports]) -> "Time: " ++ integer_to_list(Time) ++ " microseconds - Result: Aborted~n" ++ format_reports(Reports).

format_report_aggregations({MaxTimeOk, MinTimeOk, MaxTimeAbort, MinTimeAbort, TotalOk, TotalAbort, Total}) ->
    "MaxTimeOk: " ++ integer_to_list(MaxTimeOk) ++ " - MinTimeOk: " ++ integer_to_list(MinTimeOk) ++ "~nMaxTimeAbort: " ++ integer_to_list(MaxTimeAbort) ++ " - MinTimeAbort: " ++ integer_to_list(MinTimeAbort) ++ "~nTotalOk: " ++ integer_to_list(TotalOk) ++ " - TotalAbort: " ++ integer_to_list(TotalAbort) ++ " - Total: " ++ integer_to_list(Total) ++ "~n".

report_aggregations([]) -> {0, 0, 0, 0, 0, 0, 0};
report_aggregations([{_, Time, ok} | Reports]) -> case report_aggregations(Reports) of 
{MaxTimeOk, 0, MaxTimeAbort, MinTimeAbort, TotalOk, TotalAbort, Total} ->
    {max(Time, MaxTimeOk), Time, MaxTimeAbort, MinTimeAbort, TotalOk + 1, TotalAbort, Total + 1};
{MaxTimeOk, MinTimeOk, MaxTimeAbort, MinTimeAbort, TotalOk, TotalAbort, Total} ->
    {max(Time, MaxTimeOk), min(Time, MinTimeOk), MaxTimeAbort, MinTimeAbort, TotalOk + 1, TotalAbort, Total + 1}
end;
report_aggregations([{_, Time, abort} | Reports]) -> case report_aggregations(Reports) of 
{MaxTimeOk, MinTimeOk, MaxTimeAbort, 0, TotalOk, TotalAbort, Total} ->
    {MaxTimeOk, MinTimeOk, max(Time, MaxTimeAbort), Time, TotalOk, TotalAbort + 1, Total + 1};
{MaxTimeOk, MinTimeOk, MaxTimeAbort, MinTimeAbort, TotalOk, TotalAbort, Total} ->
    {MaxTimeOk, MinTimeOk, max(Time, MaxTimeAbort), min(Time,MinTimeAbort), TotalOk, TotalAbort + 1, Total + 1}
end.

format_duration(Time) ->
    "Benchmark duration: " ++ integer_to_list(Time) ++ " microseconds~n".

format_averages_transaction_per_second({_, _, _, _, _, _, Total}, Time) ->
    "Average transactions per second: " ++ integer_to_list(trunc((Total / Time) * 1000 * 1000)) ++ "~n".

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
