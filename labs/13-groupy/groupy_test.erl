- module(groupy_test).
- export([start/1]).


start(Groupy) ->
  {_,Leader} = Groupy:start(john),
  {_,Slave1} = Groupy:start(paul, Leader),
  {_,Slave2} = Groupy:start(ringo, Leader),
  {_,Slave3} = Groupy:start(falopero, Leader).
