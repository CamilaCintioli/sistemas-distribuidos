-module(loggeador).
-export([start/1, stop/1]).

start(Nodes) ->
    spawn_link(fun() ->init(Nodes) end).

stop(Logger) ->
    Logger ! stop.

init(Nodes) ->
    Clock = time:clock(Nodes),
    Queue = pqueue:new(),
    loop(Clock, Queue).

loop(Clock, Queue) ->
    receive
        {log, From, Time, Msg} ->
            % encolar mensaje
            Queue2 = pqueue:queue(Time, {From, Time, Msg}, Queue)

            % updatear clock
            Clock2 = time:update(From, Time, Clock),

            % si time is safe, log everithing up to time
            case time:safe(Time,Clock2) of
                true ->
                    Queue3 = log_up_to(Time, Queue2),
                    loop(Clock2, Queue3);
                false -> 
                    loop(Clock2, Queue2)
            end;
         stop ->
            ok
    end.

log_up_to(Time, Queue) ->
    {LowerQueue, UpperQueue} = pqueue:split(Time, Queue)
    log_queue(LowerQueue),
    UpperQueue.

log_queue(Queue) ->
    case pqueue:is_empty(Queue) ->
        true -> ok;
        false ->
            {{From, Time, Msg}, Queue2} = pqueue:dequeue(Queue),
            log(From, Time, Msg),
            log_queue(Queue2)
    end.

log(From, Time, Msg) ->
    io:format("log: ~w ~w ~p~n", [Time, From, Msg]).
