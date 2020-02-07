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
            DD = mydialog:new_dialog(Frame,self()),
            io:format("mydialog:~p~n",[DD]),
            mydialog:show_dialog(DD),
            % Result = mydialog:get_result(DD),
            % io:format("result is:~p~n",[Result]),
            % mydialog:destroy_dialog(DD),

            loop(Frame);

        #wx{event=#wxClose{}} -> io:format("window closed~n");
        {dialog_result,Result} -> io:format("ok,i received the dialog_result:~p~n",[Result])
    end.
