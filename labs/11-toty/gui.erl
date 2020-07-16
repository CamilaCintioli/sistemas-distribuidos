-module(gui).

-export([start/1]).

-define(width, 200).
-define(height, 200).

-include_lib("wx/include/wx.hrl").

start(Id) ->
  spawn_link(fun() -> init(Id) end).

init(Name) ->
  Width = 200,
  Height = 200,
  Server = wx:new(), %Server will be the parent for the Frame
  Frame = wxFrame:new(Server, -1, Name, [{size,{Width, Height}}]),
  wxFrame:setBackgroundColour(Frame, ?wxBLACK),  
  wxFrame:show(Frame),
  wxFrame:connect(Frame, close_window),
  loop(Frame).

loop(Frame)->
  receive
    #wx{event=#wxClose{}} ->
      wxWindow:destroy(Frame),
      ok;
    {color, Color} ->
      color(Frame, Color),
      loop(Frame);
    stop ->
      ok;
    Error ->
      io:format("gui: strange message ~w ~n", [Error]),
      loop(Frame)
  end.

color(Frame, Color) ->
  wxFrame:setBackgroundColour(Frame, Color),
  wxFrame:refresh(Frame).