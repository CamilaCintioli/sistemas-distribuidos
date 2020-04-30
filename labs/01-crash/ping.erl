-module(ping).
-export([ping/0]).

ping() -> 
    receive
        {pong, Pid, 0} -> 
            io:format("ping ended~n",[]),
            Pid ! {finish};
        {_,_,N} when N<0 ->
            io:format("~w can't be less than 0~n",[N]);
        {pong, Pid,N} ->   
        io:format("ping ~n", []),
        Pid ! {ping, self(),N-1},
        ping()
    end.





