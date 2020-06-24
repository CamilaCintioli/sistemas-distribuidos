-module(gui).

-export([start/1, init/1]).

-include_lib("wx/include/wx.hrl").

start(Name) ->
    spawn(gui, init, [Name]).

init(Name) ->
    Width = 200,
    Height = 200,
    Server = wx:new(), %Server will be the parent for the Frame
    Frame = wxFrame:new(Server, -1, Name, []),
    StaticText = wxStaticText:new(Frame, 1, "Sin grupo",[]),
    wxFrame:show(Frame),
    loop(Frame, StaticText).

loop(Frame, StaticText) -> 
    receive
        leader ->
            wxFrame:setBackgroundColour(Frame, {0,255,0}),
            wxStaticText:setLabel(StaticText, "Leader"),
            wxStaticText:refresh(StaticText),
            wxFrame:refresh(Frame),
            loop(Frame, StaticText);
        slave ->
            wxFrame:setBackgroundColour(Frame, {0,255,0}),
            wxStaticText:setLabel(StaticText, "Slave"),
            wxStaticText:refresh(StaticText),
            wxFrame:refresh(Frame),
            loop(Frame, StaticText);
        {join, _Wrk, _Peer} ->
            wxFrame:setBackgroundColour(Frame, {0,0,255}),
            wxFrame:refresh(Frame),
            loop(Frame, StaticText);
        {error, _ErrorMessage} ->
            wxFrame:setBackgroundColour(Frame, ?wxRED),
            wxFrame:refresh(Frame),
            loop(Frame, StaticText);
        stop -> 
            wxFrame:setBackgroundColour(Frame, {0, 0, 0}),
            wxFrame:refresh(Frame),
            timer:sleep(1000),
            ok;
        _Msg ->
            wxFrame:setBackgroundColour(Frame, {255, 255, 255}),
            wxFrame:refresh(Frame),
            loop(Frame, StaticText)
    end.
