-module(pqueue_test).
-export([run/0]).

run() ->
    test_out_on_single_element_pqueue_returns_it_and_an_empty_pqueue(),
    test_out_returns_the_ones_with_same_priority_in_order(),
    test_out_on_empty_pqueue_returns_empty_atom_and_same_pqueue(),
    test_out_returns_the_one_with_lowest_priority().

test_out_on_single_element_pqueue_returns_it_and_an_empty_pqueue() ->
    Value = pepita,
    PQueue = pqueue:new(),
    PQueue2 = pqueue:in(1, Value, PQueue),

    {{value, Value}, PQueue3} = pqueue:out(PQueue2),

    ExpectedEmpty = true,
    ExpectedEmpty = pqueue:is_empty(PQueue3).
 
test_out_returns_the_one_with_lowest_priority() ->
    LowPriorityValue = low,
    HighPriorityValue = high,
    PQueue = pqueue:new(),

    PQueue2 = pqueue:in(2, LowPriorityValue, PQueue),
    PQueue3 = pqueue:in(1, HighPriorityValue, PQueue2),

    {{value, HighPriorityValue}, PQueue4} = pqueue:out(PQueue3),
    {{value, LowPriorityValue}, _} = pqueue:out(PQueue4).

test_out_returns_the_ones_with_same_priority_in_order() ->
    FirstValue = first,
    SecondValue = second,
    ThirdValue = third,
    Priority = 20,

    PQueue = pqueue:new(),
    PQueue2 = pqueue:in(Priority, FirstValue, PQueue),
    PQueue3 = pqueue:in(Priority, SecondValue, PQueue2),
    PQueue4 = pqueue:in(Priority, ThirdValue, PQueue3),

    {{value, FirstValue}, PQueue5} = pqueue:out(PQueue4),
    {{value, SecondValue}, PQueue6} = pqueue:out(PQueue5),
    {{value, ThirdValue}, _} = pqueue:out(PQueue6).

test_out_on_empty_pqueue_returns_empty_atom_and_same_pqueue() ->
    EmptyPQueue = pqueue:new(),
    {empty, EmptyPQueue} = pqueue:out(EmptyPQueue).
