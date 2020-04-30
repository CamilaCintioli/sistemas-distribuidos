-module(pong).
-export([pong/0]).

pong() -> 
    receive
        {finish} -> io:format("pong ended~n");
        {ping, Pid,N} ->     
        Pid ! {pong,self(),N},
        io:format("pong~n",[]),
        pong()
    end.
