-module(test_dialog).

-include_lib("wx/include/wx.hrl").

-define(MYBUTTON,100).
-define(MYDIALOG,101).
-define(MYTEXT,102).

-define(MYOK,103).
-define(MYCANCEL,104).


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

start_dialog(Frame) ->
    Dialog = wxDialog:new(Frame,?MYDIALOG,"my dialog test"),
    setup_dialog(Dialog),
    wxDialog:show(Dialog).
    % wxDialog:showModal(Dialog),

setup_dialog(Dialog) ->
    Panel = wxPanel:new(Dialog),
    Sizer = wxBoxSizer:new(?wxVERTICAL),

    Text = wxTextCtrl:new(Panel,?MYTEXT),

    ButtonOk = wxButton:new(Panel,?wxID_OK,[{label,"OK"}]),

    FunOk = fun(EventRecord,EventObject) -> 
        Line = string:trim(wxTextCtrl:getValue(Text)),
        io:format("~p~n",[Line])
        % io:format("~p~n~p~n",[A,B]),
        % wxDialog:destroy(Dialog)
        % wxDialog:endModal(Dialog,?wxID_OK) 
    end,
    GetValue = fun() -> string:trim(wxTextCtrl:getValue(Text)) end,
    % wxButton:connect(ButtonOk,command_button_clicked),
    wxButton:connect(ButtonOk,command_button_clicked,[{skip,true},{userData,GetValue}]),

    ButtonCancel = wxButton:new(Panel,?wxID_CANCEL,[{label,"CANCEL"}]),

    wxBoxSizer:add(Sizer,Text,[{proportion,1},{flag,?wxEXPAND}]),
    wxBoxSizer:add(Sizer,ButtonOk,[{proportion,1},{flag,?wxEXPAND}]),
    wxBoxSizer:add(Sizer,ButtonCancel,[{proportion,1},{flag,?wxEXPAND}]),
    wxPanel:setSizer(Panel,Sizer).

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
            io:format("start dialog~n"),
            start_dialog(Frame),
            loop(Frame);
        #wx{id=?wxID_OK,userData=GetValue,event=#wxCommand{type=command_button_clicked}} -> 
            io:format("~p~n",[GetValue()]),
            loop(Frame);
        #wx{event=#wxClose{}} -> io:format("window closed~n")
    end.
