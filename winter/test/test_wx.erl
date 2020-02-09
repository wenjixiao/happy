-module(test_wx).

-include_lib("wx/include/wx.hrl").

-define(MYBUTTON,100).
-define(MYBUTTON1,105).

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
            io:format("lowest:~p,highest:~p~n",[?wxID_LOWEST,?wxID_HIGHEST]),
            io:format("frame:~p~n",[Frame]),
            % Pid = spawn(mydialog1,start,[Frame,self(),wx:get_env()]),
            Result = mydialog1:start(Frame,self(),wx:get_env()),
            io:format("result:~p~n",[Result]),
            loop(Frame);

        #wx{id=?MYBUTTON1} ->
            io:format("mybutton1~n"),
            loop(Frame);

        {dialog,Msg} -> 
            io:format("~p~n",[Msg]),
            loop(Frame);

        #wx{event=#wxClose{}} -> io:format("window closed~n")
    end.
