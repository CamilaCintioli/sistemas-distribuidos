-module(proposal_queue).
-export([update/3,insert/4]).

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

