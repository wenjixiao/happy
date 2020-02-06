-module(test_wx).

-include_lib("wx/include/wx.hrl").

-define(MYBUTTON,100).

-export([start/0]).

start() ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx,?wxID_ANY,"HelloWorld"),
    setup(Frame),

    wxFrame:connect(Frame, close_window),
    wxFrame:connect(Frame,command_button_clicked),
    % wxFrame:connect(Frame,command_text_enter),

    wxFrame:show(Frame),
    loop(Frame),
    wxFrame:destroy(Frame).

setup(Frame) ->
    Panel = wxPanel:new(Frame),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    Button1 = wxButton:new(Panel,?MYBUTTON,[{label,"hello"}]),
    % Input = wxTextCtrl:new(Panel,?MYINPUT,[{style,?wxTE_PROCESS_ENTER}]),
    % wxTextCtrl:setFocus(Input),
    wxBoxSizer:add(Sizer,Button1,[{proportion,1},{flag,?wxEXPAND}]),
    % wxBoxSizer:add(Sizer,Input,[{proportion,0},{flag,?wxEXPAND}]),
    wxPanel:setSizer(Panel,Sizer).

loop(Frame) ->
    receive
        #wx{id=?MYBUTTON,event=#wxCommand{type=command_button_clicked}} -> 
            io:format("hello~n"),
            loop(Frame);
        #wx{event=#wxClose{}} -> io:format("window closed~n")
    end.
