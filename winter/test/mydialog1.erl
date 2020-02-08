-module(mydialog1).

-include_lib("wx/include/wx.hrl").

-compile(export_all).

-define(MYTEXT,102).
-define(MYOK,103).
-define(MYCANCEL,104).
-define(MYBUTTON,105).
-define(MYDIALOG,1001).

start(Parent,FromPid,Env) -> 
    io:format("parent:~p,from_pid:~p~n",[Parent,FromPid]),
    wx:set_env(Env),
    Dialog = wxDialog:new(Parent,?MYDIALOG,"my dialog"),

    Panel = wxPanel:new(Dialog),
    Sizer = wxBoxSizer:new(?wxVERTICAL),

    Text = wxTextCtrl:new(Panel,?MYTEXT),
    Button = wxButton:new(Panel,?MYBUTTON,[{label,"sayhello"}]),
    ButtonOk = wxButton:new(Panel,?MYOK,[{label,"OK"}]),
    ButtonCancel = wxButton:new(Panel,?MYCANCEL,[{label,"CANCEL"}]),

    wxBoxSizer:add(Sizer,Text,[{proportion,1},{flag,?wxEXPAND}]),
    wxBoxSizer:add(Sizer,Button,[{proportion,1},{flag,?wxEXPAND}]),
    wxBoxSizer:add(Sizer,ButtonOk,[{proportion,1},{flag,?wxEXPAND}]),
    wxBoxSizer:add(Sizer,ButtonCancel,[{proportion,1},{flag,?wxEXPAND}]),

    wxPanel:setSizer(Panel,Sizer),

    wxDialog:connect(Dialog,command_button_clicked),
    wxDialog:show(Dialog),
    loop(Dialog).

    % {Dialog,#state{win=Dialog,parent=Parent,data=Text,fromPid=FromPid}}.

loop(Dialog) ->
    receive
        #wx{id=?MYBUTTON} ->
            io:format("hehehhehe...~n"),
            loop(Dialog)
    end.

% terminate(Reason,State) -> 
%     io:format("wx_object stopped!~n"),
%     ok.

% handle_call(show,_From,State) ->
%     wxDialog:show(State#state.win),
%     {reply,ok,State}.

% handle_event(#wx{id=?MYBUTTON},State) -> 
%     io:format("hehehhehe...~n"),
%     {noreply,State};

% handle_event(#wx{id=?MYOK},State) -> 
%     Result = string:trim(wxTextCtrl:getValue(State#state.data)),
%     State#state.fromPid ! {dialog_result,ok,Result},
%     {stop,normal,State};

% handle_event(#wx{id=?MYCANCEL},State) -> 
%     State#state.fromPid ! {dialog_result,cancel},
%     {stop,normal,State}.
