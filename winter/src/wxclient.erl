-module(wxclient).

-include("msgs.hrl").

-include_lib("wx/include/wx.hrl").

-export([start/0]).

-define(MYOUTPUT,100).
-define(MYINPUT,101).

start() ->
    case gen_tcp:connect("localhost",20000,[binary,{packet,4},{active,true}]) of
        {ok,Socket} -> 
            Wx = wx:new(),
            Frame = wxFrame:new(Wx,?wxID_ANY,"HelloWorld"),
            setup(Frame),

            wxFrame:connect(Frame, close_window),
            % wxFrame:connect(Frame,command_button_clicked),
            wxFrame:connect(Frame,command_text_enter),

            wxFrame:show(Frame),
            loop(Frame,Socket),
            wxFrame:destroy(Frame);

        {error,Reason} -> io:format("connect error:~w~n",[Reason])
    end.

setup(Frame) ->
    Panel = wxPanel:new(Frame),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    Output = wxTextCtrl:new(Panel,?MYOUTPUT,[{style,?wxTE_MULTILINE}]),
    Input = wxTextCtrl:new(Panel,?MYINPUT,[{style,?wxTE_PROCESS_ENTER}]),
    wxTextCtrl:setFocus(Input),
    wxBoxSizer:add(Sizer,Output,[{proportion,1},{flag,?wxEXPAND}]),
    wxBoxSizer:add(Sizer,Input,[{proportion,0},{flag,?wxEXPAND}]),
    wxPanel:setSizer(Panel,Sizer).

loop(Frame,Socket) ->
    receive
        #wx{id=?MYOUTPUT,event=#wxCommand{type=command_button_clicked}} ->
            Login = #login{pid="wen",password="123"},
            gen_tcp:send(Socket,term_to_binary(Login)),
            loop(Frame,Socket);

        #wx{id=?MYINPUT,event=#wxCommand{type=command_text_enter}} ->
            Win = wx:typeCast(wxWindow:findWindowById(?MYINPUT),wxTextCtrl),
            Line = string:trim(wxTextCtrl:getValue(Win)),
            Len = length(Line),
            if 
                Len > 0 ->
                    [Cmd|Params] = string:split(Line," ",all),
                    case Cmd of
                        "exit" -> ok;
                        "login" -> 
                            [Pid,Password] = Params,
                            Login = #login{pid=Pid,password=Password},
                            gen_tcp:send(Socket,term_to_binary(Login)),
                            wxTextCtrl:clear(Win),
                            loop(Frame,Socket)
                    end;

                Len =< 0 -> loop(Frame,Socket)
            end;

        #wx{event=#wxClose{}} ->
            io:format("aimajamamamam~n");

        {tcp,Socket,Data} -> 
            Msg = binary_to_term(Data),
            io:format("received data: ~p~n",[Msg]),
            Win = wx:typeCast(wxWindow:findWindowById(?MYOUTPUT),wxTextCtrl),
            wxTextCtrl:setValue(Win,io_lib:format("~w",[Msg])),
            loop(Frame,Socket)
    end.
