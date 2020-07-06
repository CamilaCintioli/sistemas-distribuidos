-module(total_multicaster).

-export([start/2]).

-define(default_jitter, 1000).

start(Master, Nodes) -> {ok, spawn(fun () -> init(Master, Nodes) end)}.

init(Master, Nodes) -> server(Master, 0, Nodes, lists:new(), queue:new(), ?default_jitter).

server(Master, Next, Nodes, Cast, Queue, Jitter) ->
    receive {send, Msg} ->
        Ref = make_ref(),
        request(Nodes, Ref, Msg, Jitter),
        Cast2 = cast(Ref, Nodes, Cast),
        server(Master, Next, Nodes, Cast2, Queue, Jitter);
        {request, From, Ref, Msg} ->
            From ! {proposal, Ref, Next},
            Queue2 = insert(?, Ref, Next, Queue),
            Next2 = increment(Next),
            server(Master, Next2, Nodes, Cast, Queue2, Jitter);
            {proposal, Ref, Proposal} ->
                case proposal(?, ?, ?) of
                    {agreed, Seq, Cast2} ->
                        agree(?, ?, ?),
                        server(Master, Next, Nodes, Cast2, Queue, Jitter);
                    Cast2 ->
                        server(Master, Next, Nodes, Cast2, Queue, Jitter)
                end;
                {agreed, Ref, Seq} ->
                    Updated = update(?, ?, ?),
                    {Agreed, Queue2} = agreed(?, ?),
                    deliver(?, ?),
                    Next2 = increment(?, ?),
                    server(Master, Next2, Nodes, Cast, Queue2, Jitter)
    end.

request(Nodes, Ref, Msg, Jitter) ->
    Self = self(),
    lists:foreach(fun (Node) -> jitter(Jitter), Node ! {request, Self, Ref, Msg} end, Nodes).

jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).

cast(Ref, Nodes, Cast) ->
    L = length(Nodes),
    lists:keystore(Ref, 1, Cast, {Ref, L, 0}).

increment(Next) ->
    Next + 1

insert(Stage, Ref, Next, Queue) ->
    queue:in({Stage, Ref, Next}, Queue),
