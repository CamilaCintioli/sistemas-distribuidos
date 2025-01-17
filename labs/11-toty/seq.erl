-module(seq).
-export([new/0, new/1, increment/1, max/2, lessthan/2]).

new() ->
  {0,0}.

new(Id) ->
  {0, Id}.

increment({Pn, Pi}) ->
  {Pn+1, Pi}.

max(Proposal, Sofar) ->
  case lessthan(Proposal, Sofar) of
    true ->
      Sofar;
    false ->
      Proposal
  end.

lessthan({Pn, Pi}, {Nn, Ni}) ->
  (Pn < Nn) or ((Pn == Nn) and (Pi < Ni)).