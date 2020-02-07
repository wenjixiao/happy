-module(mydialog).

-include_lib("wx/include/wx.hrl").

-behavior(wx_object).

-record(state,{win,parent,mytext,result,fromPid}).

-compile(export_all).

-define(MYTEXT,102).
-define(MYOK,103).
-define(MYCANCEL,104).

new_dialog(Parent,FromPid) -> wx_object:start(?MODULE,[Parent,FromPid],[]).
show_dialog(Dialog) -> wx_object:call(Dialog,show).

init([Parent,FromPid]) -> 
    Dialog = wxDialog:new(Parent,1001,"my dialog"),

    Panel = wxPanel:new(Dialog),
    Sizer = wxBoxSizer:new(?wxVERTICAL),

    Text = wxTextCtrl:new(Panel,?MYTEXT),
    ButtonOk = wxButton:new(Panel,?MYOK,[{label,"OK"}]),
    ButtonCancel = wxButton:new(Panel,?MYCANCEL,[{label,"CANCEL"}]),

    wxBoxSizer:add(Sizer,Text,[{proportion,1},{flag,?wxEXPAND}]),
    wxBoxSizer:add(Sizer,ButtonOk,[{proportion,1},{flag,?wxEXPAND}]),
    wxBoxSizer:add(Sizer,ButtonCancel,[{proportion,1},{flag,?wxEXPAND}]),

    wxPanel:setSizer(Panel,Sizer),

    wxDialog:connect(Dialog,command_button_clicked),

    {Dialog,#state{win=Dialog,parent=Parent,mytext=Text,fromPid=FromPid}}.

terminate(Reason,State) -> 
    io:format("wx_object stopped!~n"),
    ok.

handle_call(show,_From,State) ->
    wxDialog:show(State#state.win),
    {reply,ok,State}.

handle_event(#wx{id=?MYOK},State) -> 
    io:format("user clicked button~n",[]),
    Result = string:trim(wxTextCtrl:getValue(State#state.mytext)),
    State#state.fromPid ! {dialog_result,Result},
    io:format("rrrr:~p~n",[Result]),
    wxDialog:destroy(State#state.win),
    {noreply,State#state{result=Result}};

handle_event(#wx{id=?MYCANCEL},State) -> 
    io:format("user clicked button~n",[]),
    wxDialog:destroy(State#state.win),
    {noreply,State}.
