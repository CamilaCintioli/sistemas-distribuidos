-module(total_multicaster).

-export([start/4]).

start(Id, Master, Seed, Jitter) ->
  spawn(fun() -> init(Id, Master, Seed, Jitter) end).

init(Id, Master, Seed, Jitter) ->
  random:seed(Seed, Seed, Seed),
  receive
    {peers, Nodes} ->
      server(Master, seq:new(Id), seq:new(Id), Nodes, [], [], Jitter)
  end.

server(Master, NextProposal, Agree, Nodes, Cast, Queue, Jitter) ->
  receive
    {send, Msg} ->                                                          %{send,Msg}
      Ref = make_ref(),                                                     %Crea referencia
      request(Ref, Msg, Nodes, Jitter),                                     %Hace un request 
      NewCast = cast(Ref, Nodes, Cast),                                     %Hace un cast
      server(Master, NextProposal, Agree, Nodes, NewCast, Queue, Jitter);       %Recursion con un nuevo cast

                                                                        %-------------------------------------------%

    {request, From, Ref, Msg} ->                                            %{request,From,Ref,Msg}
      From ! {proposal, Ref, NextProposal},                                      %Le manda al emisor un propsal con la ref y el NextProposal
      NewQueue = insert(Ref, Msg,  NextProposal, Queue),                         %Arma una nueva queue con el msg
      NewNextProposal = seq:increment(seq:max(NextProposal, Agree)),                 %Calcula el NextProposal == {max entre NextProposal & Agree+1,Id}
      server(Master, NewNextProposal, Agree, Nodes, Cast, NewQueue, Jitter);    %Recursion con el nuevo NextProposal

                                                                        %-------------------------------------------%

    {proposal, Ref, Proposal} ->                                            %{proposal,Ref,Proposal}
      case proposal(Ref, Proposal, Cast) of
        {agreed, MaxSeq, NewCast} ->                                        %Si se acuerda
          agree(Ref , MaxSeq, Nodes),                                       %agree
          server(Master, NextProposal, MaxSeq, Nodes, NewCast, Queue, Jitter);   %Recursion

        NewCast ->                                                          %NuevoCast (asumo que es desacuerdo)
          server(Master, NextProposal, Agree, Nodes, NewCast, Queue, Jitter)    %Recursion con un nuevoCast
        end;    

                                                                        %-------------------------------------------%

    {agreed, Ref, Seq} ->                                                   %{agreed,Ref,Seq}
      Updated = update(Ref, Seq, Queue),                                    %update
      {Agreed, NewQueue} = agreed(Updated),                                 %
      deliver(Master, Agreed),                                              %deliver al worker lo acordado
      NewAgree = seq:max(Seq, Agree),                                     %Calculo de nueva max agr
      server(Master, NextProposal, NewAgree, Nodes, Cast, NewQueue, Jitter);    %Recursion con nuevo maximo agr

    stop ->
      ok
  end.  



multicast(Receivers,Msg) -> 
    lists:foreach(fun(Receiver) -> 
        Receiver ! Msg
    end,
    Receivers).
multicastSleep(Receivers,Msg,Sleep) ->
    lists:foreach(fun(Receiver) -> 
        timer:sleep(Sleep),
        Receiver ! Msg        
    end,
    Receivers).


request(Ref, Msg, Nodes, 0) ->
  multicast(Nodes,{request,self(),Ref,Msg});
request(Ref, Msg, Nodes, Jitter) ->
    Sleep = random:uniform(Jitter),
    multicastSleep(Nodes,{request,self(),Ref,Msg},Sleep).

agree(Ref, Seq, Nodes)->
    multicast(Nodes, {agreed,Ref,Seq}).

deliver(Master, Messages) ->
  lists:foreach(fun(Msg)->
      Master ! {deliver, Msg}
    end,
    Messages).

cast(Ref, Nodes, Cast) ->
  L = length(Nodes),
  [{Ref, L, seq:new()}|Cast].

proposal(Ref, Proposal, [{Ref, 1, Sofar}|Rest])->
  {agreed, seq:max(Proposal, Sofar), Rest};

proposal(Ref, Proposal, [{Ref, N, Sofar}|Rest])->
  [{Ref, N-1, seq:max(Proposal, Sofar)}|Rest];

proposal(Ref, Proposal, [Entry|Rest])->
  case proposal(Ref, Proposal, Rest) of
    {agreed, Agreed, Rst} ->
      {agreed, Agreed, [Entry|Rst]};
    Updated ->
      [Entry|Updated]
  end.

agreed([{_Ref, Msg, agrd, _Agr}|Queue]) ->
  {Agreed, Rest} = agreed(Queue),
  {[Msg|Agreed], Rest};

agreed(Queue) ->
  {[], Queue}.

%---------------------------------------------------- QUEUE ------------------------------------------------------------

update(Ref, Agreed, [{Ref, Msg, propsd, _}|Rest])->
  queue(Ref, Msg, agrd, Agreed, Rest);

update(Ref, Agreed, [Entry|Rest])->
  [Entry|update(Ref, Agreed, Rest)].

insert(Ref, Msg, Proposal, Queue) ->
  queue(Ref, Msg, propsd, Proposal, Queue).

queue(Ref, Msg, State, Proposal, []) ->
  [{Ref, Msg, State, Proposal}];

queue(Ref, Msg, State, Proposal, Queue) ->
  [Entry|Rest] = Queue,
  {_,_,_,Next} = Entry,
  case seq:lessthan(Proposal, Next) of
    true ->
      [{Ref, Msg, State, Proposal}|Queue];
    false ->
      [Entry|queue(Ref, Msg, State, Proposal, Rest)]
  end.

