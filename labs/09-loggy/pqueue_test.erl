-module(pqueue_test).
-export([run/0]).

run() ->
    test_out_on_single_element_pqueue_returns_it_and_an_empty_pqueue(),
    test_out_returns_the_one_with_lowest_priority().

test_out_on_single_element_pqueue_returns_it_and_an_empty_pqueue() ->
    Value = value,
    PQueue = pqueue:new(),
    PQueue2 = pqueue:in(1, Value, PQueue),
    {Value, PQueue3} = pqueue:out(PQueue2),
    ExpectedEmpty = true,
    ExpectedEmpty = pqueue:is_empty(PQueue3).
 
test_out_returns_the_one_with_lowest_priority() ->
    LowPriorityValue = low,
    HighPriorityValue = high,
    PQueue = pqueue:new(),
    PQueue2 = pqueue:in(2, LowPriorityValue, PQueue),
    PQueue3 = pqueue:in(1, HighPriorityValue, PQueue2),
    {HighPriorityValue, PQueue4} = pqueue:out(PQueue3),
    ExpectedEmpty = false,
    ExpectedEmpty = pqueue:is_empty(PQueue4),
    {LowPriorityValue, PQueue5} = pqueue:out(PQueue4),
    ExpectedEmpty2 = true,
    ExpectedEmpty2 = pqueue:is_empty(PQueue5).

