-module(worker).

-export([peers/2, start/5, stop/1]).

start(Name, Logger, Seed, Sleep, Jitter) ->
    spawn_link(fun () ->
           init(Name, Logger, Seed, Sleep, Jitter)
         end).

stop(Worker) -> Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
    random:seed(Seed, Seed, Seed),
    receive
      {peers, Peers} -> loop(Name, Log, Peers, Sleep, Jitter);
      stop -> ok
    end.

peers ( Wrk , Peers ) -> Wrk ! { peers , Peers }. 

loop(Name, Log, Peers, Sleep, Jitter)-> 
    loop(Name, Log, Peers, Sleep, Jitter, time:zero()).
loop(Name, Log, Peers, Sleep, Jitter, Time)->
    Wait = random:uniform(Sleep),
    receive
        {msg, Time2, Msg} ->
            NewTime = time:inc(Name, time:merge(Time, Time2)),
            Log ! {log, Name, NewTime, {received, Msg}},
            loop(Name, Log, Peers, Sleep, Jitter, NewTime);
        stop ->
            ok;
        Error ->
            Log ! {log, Name, time, {error, Error}}
    after Wait ->
            NewTime = time:inc(Name, Time),
            Selected = select(Peers),
            Message = {hello, random:uniform(100)},
            Selected ! {msg, NewTime, Message},
            jitter(Jitter),
            Log ! {log, Name, NewTime, {sending, Message}},
            loop(Name, Log, Peers, Sleep, Jitter, NewTime)
    end.

select(Peers) ->
        lists:nth(random:uniform(length(Peers)), Peers).
    
jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).
