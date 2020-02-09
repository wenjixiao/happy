-module(mydialog1).

-include_lib("wx/include/wx.hrl").

-compile(export_all).

-define(MYTEXT,102).
-define(MYOK,103).
-define(MYCANCEL,104).
-define(MYBUTTON1,105).
-define(MYDIALOG,1001).

%%
%% modal dialog的处理，必须用callback的方式！
%% 因为进不去loop消息处理循环。
%% 

start(Parent,FromPid,Env) -> 
    io:format("parent:~p,from_pid:~p~n",[Parent,FromPid]),
    wx:set_env(Env),
    Dialog = wxDialog:new(Parent,?MYDIALOG,"my dialog"),

    Panel = wxPanel:new(Dialog),
    Sizer = wxBoxSizer:new(?wxVERTICAL),

    Text = wxTextCtrl:new(Panel,?MYTEXT),
    Button = wxButton:new(Panel,?MYBUTTON1,[{label,"mybutton1"}]),

    ButtonOk = wxButton:new(Panel,?MYOK,[{label,"OK"}]),
    OkFun = fun(A,B) ->
        io:format("helloooo~n"),
        FromPid ! {dialog,"hello wrold!!"},
        wxDialog:endModal(Dialog,1)
    end,
    wxButton:connect(ButtonOk,command_button_clicked,[{callback,OkFun}]),

    ButtonCancel = wxButton:new(Panel,?MYCANCEL,[{label,"CANCEL"}]),

    wxBoxSizer:add(Sizer,Text,[{proportion,1},{flag,?wxEXPAND}]),
    wxBoxSizer:add(Sizer,Button,[{proportion,1},{flag,?wxEXPAND}]),
    wxBoxSizer:add(Sizer,ButtonOk,[{proportion,1},{flag,?wxEXPAND}]),
    wxBoxSizer:add(Sizer,ButtonCancel,[{proportion,1},{flag,?wxEXPAND}]),

    wxPanel:setSizer(Panel,Sizer),

    wxDialog:connect(Dialog,command_button_clicked),
    %% showModal以后，会卡在那里，因为要等一个Result值。
    %% 由于进入不了loop，所以，这个方式不行！
    Result = wxDialog:showModal(Dialog).
    %% loop can't run!!!!
    % loop(Dialog).

loop(Dialog) ->
    receive
        #wx{id=?MYBUTTON1} ->
            io:format("hehehhehe...~n"),
            loop(Dialog);
        #wx{id=?MYOK} ->
            % wxDialog:destroy(Dialog),
            wxDialog:endModal(Dialog,ok);
        #wx{id=?MYCANCEL} ->
            % wxDialog:destroy(Dialog),
            wxDialog:endModal(Dialog,cancel)
    end.
