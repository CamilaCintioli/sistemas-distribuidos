-module(time).
-export([zero/0, inc/1, merge/2, leq/2,clock/1,safe/2,update/3,less/2,eq/2]).

zero() -> 0.

inc(T) -> T + 1.

merge(Ti, Tj) -> max(Ti, Tj).

leq(Ti, Tj) -> Ti =< Tj.

less(Ti,Tj) -> Ti < Tj.

eq(Ti,Tj) -> Ti =:= Tj.

clock(Nodes) ->
    lists:map(fun(Node) -> {Node, zero()} end, Nodes).

update(Node, Time, Clock) ->
    lists:map(fun(Record) ->
        case Record of
            {Node, _} -> {Node, Time};
            X -> X
        end
    end, Clock).

% devuelve true, si todos los records tienen un time mayor que el dado 
% retorna si es seguro enviar el mensaje de log de un evento que ocurrió en el tiempo Time dado.
safe(Time, Clock) ->
    lists:all(
    fun ({ _, Time2 }) -> Time2 >= Time end,
    Clock).

