-module(wxclient).

-include_lib("wx/include/wx.hrl").

-export([start/0]).

-define(HELLO,100).
-define(WORLD,101).

start() ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx,?wxID_ANY,"HelloWorld"),
    setup(Frame),
    wxFrame:connect(Frame, close_window),
    wxFrame:connect(Frame,command_button_clicked),

    wxFrame:show(Frame),
    loop(Frame),

    wxFrame:destroy(Frame).

setup(Frame) ->
    Panel = wxPanel:new(Frame),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    Button1 = wxButton:new(Panel,?HELLO,[{label,"hello"}]),
    Button2 = wxButton:new(Panel,?WORLD,[{label,"momo"}]),
    wxBoxSizer:add(Sizer,Button1),
    wxBoxSizer:add(Sizer,Button2),
    wxPanel:setSizer(Panel,Sizer).

loop(Frame) ->
    receive
        #wx{id=?HELLO,event=#wxCommand{type=command_button_clicked}} ->
            io:format("hehehehe~n"),
            loop(Frame);

        #wx{event=#wxClose{}} ->
            io:format("aimajamamamam~n")
    end.
