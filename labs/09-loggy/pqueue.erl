-module(pqueue).
-export([new/0,in/3,split/2,is_empty/1,out/1]).

new() ->
    dict:new().

in(Priority, Value, PQueue) ->
    dict:update(
        Priority,
        fun(Queue) ->
            queue:in(Value, Queue)
        end,
        queue:in(Value,queue:new()),
        PQueue
    ).

split(Priority, PQueue) ->
    dict:fold(
        fun(Key, Queue, {LowerPQueue, UpperPQueue}) ->
            case Priority > Key of
                true -> {LowerPQueue, in(Key, Queue, UpperPQueue)};
                false -> {in(Key, Queue, LowerPQueue), UpperPQueue}
            end
        end,
        {dict:new(), dict:new()},
        PQueue
    ).

is_empty(PQueue) ->
    dict:fold(
        fun(_, Queue, Acc) ->
            Acc and queue:is_empty(Queue)
        end,
        true,
        PQueue
    ).

out(PQueue) ->
    % buscar la key con una queue no vacia mas baja
    {LowestPriority, LowestQueue, PQueue2} = split_lowest_priority(PQueue),
    % hacer queue out de esta cola
    {{value, Item}, LowestQueue2} = queue:out(LowestQueue),
    case queue:is_empty(LowestQueue2) of
        % eliminar la queue si esta vacia
        true -> {Item, PQueue2};
        % retornar el elemento y la pqueue
        false -> {Item, dict:append(LowestPriority, LowestQueue2, PQueue2)}
    end.

split_lowest_priority(PQueue) ->
    LowestPriority = find_lowest_priority(PQueue),
    {LowestQueue, PQueue2} = dict:take(LowestPriority, PQueue),
    {LowestPriority, LowestQueue, PQueue2}.

find_lowest_priority(PQueue) ->
    Keys = dict:fetch_keys(PQueue),
    lists:min(Keys).
