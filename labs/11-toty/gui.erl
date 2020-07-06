-module(gui).

-export([init/1, start/1]).

-include_lib("wx/include/wx.hrl").

start(Name) -> spawn(gui, init, [Name]).

init(Name) ->
    Width = 200,
    Height = 200,
    Server =
    wx:new(), %Server will be the parent for the Frame
    Frame = wxFrame:new(Server, -1, Name,
            [{size, {Width, Height}}]),
    wxFrame:show(Frame),
    loop(Frame).

loop(Frame) ->
    receive
      _Msg ->
        wxFrame:setBackgroundColour(Frame, {255, 255, 255}),
        wxFrame:refresh(Frame),
        loop(Frame)
    end.
