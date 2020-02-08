-module(test_mydialog).

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
    wxBoxSizer:add(Sizer,Button1,[{proportion,1},{flag,?wxEXPAND}]),
    wxPanel:setSizer(Panel,Sizer).

loop(Frame) ->
    receive
        #wx{id=?MYBUTTON,event=#wxCommand{type=command_button_clicked}} -> 
            io:format("start dialog~n"),
            WxDialog = mydialog:new(Frame,self()),
            io:format("mydialog:~p~n",[WxDialog]),
            mydialog:show(WxDialog),
            loop(Frame);

        #wx{event=#wxClose{}} -> 
            io:format("window closed~n");

        {dialog_result,ok,Result} -> 
            io:format("dialog ok:~p~n",[Result]);

        {dialog_result,cancel} ->
            io:format("dialog cancel")
    end.
