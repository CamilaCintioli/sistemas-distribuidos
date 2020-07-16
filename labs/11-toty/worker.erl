-module(worker).

-export([start/6]).

-define(change, 20).

start(Id, Grp, Multicaster, Seed, Sleep, Jitter) ->    
  spawn(fun() -> init(Id, Grp, Multicaster, Seed, Sleep, Jitter) end).


init(Id, Grp, Multicaster, Seed, Sleep, Jitter) ->
  random:seed(Seed, Seed, Seed),
  Gui = gui:start(Id),                                
  Cast = Multicaster:start(Id, self(), Seed, Jitter), 
  Grp ! {join, self(), Cast},                         
  receive
    {state, Color, Peers} ->                          
      Cast ! {peers, Peers},                          
      Gui ! {color, Color},                           
      cast_change(Id, Cast, Sleep),                   
      worker(Id, Cast, Color, Gui, Sleep),            
      Cast ! stop,                                    
      Gui ! stop                                      
  end.


worker(Id,Multicaster,Color,Gui,Sleep) ->
  receive
    {deliver,{Id,N}} ->
      NewColor = change_color(Gui,N,Color),
      cast_change(Id,Multicaster,Sleep),
      worker(Id,Multicaster,NewColor,Gui,Sleep);
    {deliver,{_From,N}} ->
      NewColor = change_color(Gui,N,Color),
      worker(Id,Multicaster,NewColor,Gui,Sleep);
    stop -> ok;
    Error ->
      io:format("Error ~w~n",[Error]),
      worker(Id,Multicaster,Color,Gui,Sleep)
  end.

change_color(Gui, N, Color) ->
  NewColor = get_color(N,Color),
  Gui ! {color, NewColor},
  NewColor.

get_color(N, {R,G,B}) ->
  {G, B, ((R+N) rem 256)}.

cast_change(Id, Cast, Sleep) ->
  Msg = {Id, random:uniform(?change)},
  timer:send_after(Sleep, Cast, {send, Msg}).