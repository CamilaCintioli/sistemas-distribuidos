- module(groupy_test).
- export([start/1, start/2]).


start(Groupy) ->
  {_,Leader} = Groupy:start(john),
  {_,Slave1} = Groupy:start(paul, Leader),
  {_,Slave2} = Groupy:start(ringo, Leader),
  {_,Slave3} = Groupy:start(falopero, Leader).

start(_Groupy, N) when N < 1 -> { error };
start(Groupy, 1) -> Groupy:start(1);
start(Groupy, N) -> 
  {_, Leader} = start(Groupy, N - 1),
  Groupy:start(N, Leader).


f().
c(worker),c(gms3),c(gui).
Leader = worker:start(gms3,"John").

S1 = worker:start(gms3,"Paul",Leader).
S2 = worker:start(gms3,"Ringo",S1).
S3 = worker:start(gms3,"George",S2).
S4 = worker:start(gms3,"Pepe",S3).


