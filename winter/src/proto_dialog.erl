-module(proto_dialog).

-include_lib("wx/include/wx.hrl").

-behavior(wx_object).

-record(ctrls,{who_first,range_zi,tie_mu,clock_bao_liu,clock_du_miao,clock_times,clock_per_time}).
-record(state,{win,parent,ctrls,fromPid}).

-include("msgs.hrl").

-compile(export_all).

-define(WHO_FIRST_CHOICE,10).
-define(RANGE_ZI_CHOICE,11).
-define(TIE_MU_CHOICE,12).

-define(CLOCK_BAO_LIU,13).
-define(CLOCK_DU_MIAO,14).
-define(CLOCK_TIMES,15).
-define(CLOCK_PER_TIME,16).


-define(MYOK,103).
-define(MYCANCEL,104).

% -define(MYDIALOG,1001).

new(Parent,FromPid) -> wx_object:start(?MODULE,[Parent,FromPid],[]).
show(Dialog) -> wx_object:call(Dialog,show).

init([Parent,FromPid]) -> 
    Dialog = wxDialog:new(Parent,?wxID_ANY,"proto dialog",[{size,{400,400}}]),

    Panel = wxPanel:new(Dialog),
    Sizer = wxBoxSizer:new(?wxVERTICAL),

    WhoFirstCtrl = wxChoice:new(Panel,?WHO_FIRST_CHOICE,[{choices,["random","black","white"]}]),
    wxChoice:setSelection(WhoFirstCtrl,0),

    RangeZiCtrl = wxChoice:new(Panel,?RANGE_ZI_CHOICE,[{choices,"012345678"}]),
    wxChoice:setSelection(RangeZiCtrl,0),

    TieMuCtrl = wxChoice:new(Panel,?TIE_MU_CHOICE,[{choices,["6.5","7.5","5.5"]}]),
    wxChoice:setSelection(TieMuCtrl,0),

    ButtonOk = wxButton:new(Panel,?MYOK,[{label,"OK"}]),
    ButtonCancel = wxButton:new(Panel,?MYCANCEL,[{label,"CANCEL"}]),

    wxBoxSizer:add(Sizer,WhoFirstCtrl,[{proportion,0},{flag,?wxEXPAND}]),
    wxBoxSizer:add(Sizer,RangeZiCtrl,[{proportion,0},{flag,?wxEXPAND}]),
    wxBoxSizer:add(Sizer,TieMuCtrl,[{proportion,0},{flag,?wxEXPAND}]),

    ClockPanel = wxPanel:new(Panel),
    GridSizer = wxGridSizer:new(2),

    ClockBaoLiu = wxTextCtrl:new(ClockPanel,?CLOCK_BAO_LIU),
    wxTextCtrl:setValue(ClockBaoLiu,"20"),
    wxGridSizer:add(GridSizer,wxStaticText:new(ClockPanel,?wxID_ANY,"bao liu"),[{proportion,0},{flag,?wxEXPAND}]),
    wxGridSizer:add(GridSizer,ClockBaoLiu,[{proportion,0},{flag,?wxEXPAND}]),

    ClockDuMiao = wxTextCtrl:new(ClockPanel,?CLOCK_DU_MIAO),
    wxTextCtrl:setValue(ClockDuMiao,"30"),
    wxGridSizer:add(GridSizer,wxStaticText:new(ClockPanel,?wxID_ANY,"du miao"),[{proportion,0},{flag,?wxEXPAND}]),
    wxGridSizer:add(GridSizer,ClockDuMiao,[{proportion,0},{flag,?wxEXPAND}]),

    ClockTimes = wxTextCtrl:new(ClockPanel,?CLOCK_TIMES),
    wxTextCtrl:setValue(ClockTimes,"3"),
    wxGridSizer:add(GridSizer,wxStaticText:new(ClockPanel,?wxID_ANY,"times"),[{proportion,0},{flag,?wxEXPAND}]),
    wxGridSizer:add(GridSizer,ClockTimes,[{proportion,0},{flag,?wxEXPAND}]),

    ClockPerTime = wxTextCtrl:new(ClockPanel,?CLOCK_PER_TIME),
    wxTextCtrl:setValue(ClockPerTime,"60"),
    wxGridSizer:add(GridSizer,wxStaticText:new(ClockPanel,?wxID_ANY,"per time"),[{proportion,0},{flag,?wxEXPAND}]),
    wxGridSizer:add(GridSizer,ClockPerTime,[{proportion,0},{flag,?wxEXPAND}]),

    wxPanel:setSizer(ClockPanel,GridSizer),
    wxBoxSizer:add(Sizer,ClockPanel,[{proportion,0},{flag,?wxEXPAND}]),

    OkCancelSizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxBoxSizer:add(OkCancelSizer,ButtonOk,[{proportion,1},{flag,?wxEXPAND}]),
    wxBoxSizer:add(OkCancelSizer,ButtonCancel,[{proportion,1},{flag,?wxEXPAND}]),
    wxBoxSizer:add(Sizer,OkCancelSizer,[{proportion,0},{flag,?wxEXPAND}]),

    wxPanel:setSizer(Panel,Sizer),

    wxDialog:connect(Dialog,command_button_clicked),

    Ctrls = #ctrls{who_first=WhoFirstCtrl,range_zi=RangeZiCtrl,tie_mu=TieMuCtrl,
        clock_bao_liu=ClockBaoLiu,clock_du_miao=ClockDuMiao,clock_times=ClockTimes,clock_per_time=ClockPerTime},

    {Dialog,#state{win=Dialog,parent=Parent,ctrls=Ctrls,fromPid=FromPid}}.

terminate(Reason,State) -> 
    io:format("wx_object stopped!~n"),
    ok.

handle_call(show,_From,State) ->
    wxDialog:show(State#state.win),
    {reply,ok,State}.

handle_event(#wx{id=?MYOK},State) -> 
    WhoFirst = wxChoice:getSelection(State#state.ctrls#ctrls.who_first),
    MyWhoFirst = case WhoFirst of
        0 -> random;
        1 -> black;
        2 -> white
    end,
    RangeZi = wxChoice:getSelection(State#state.ctrls#ctrls.range_zi),
    TieMu = wxChoice:getSelection(State#state.ctrls#ctrls.tie_mu),
    MyTieMu = case TieMu of
        0 -> 6.5;
        1 -> 7.6;
        2 -> 5.5
    end,

    ClockBaoLiu = wxTextCtrl:getValue(State#state.ctrls#ctrls.clock_bao_liu),
    ClockDuMiao = wxTextCtrl:getValue(State#state.ctrls#ctrls.clock_du_miao),
    ClockTimes = wxTextCtrl:getValue(State#state.ctrls#ctrls.clock_times),
    ClockPerTime = wxTextCtrl:getValue(State#state.ctrls#ctrls.clock_per_time),

    ClockDef = #clock_def{bao_liu=list_to_integer(ClockBaoLiu),du_miao=list_to_integer(ClockDuMiao),
        times=list_to_integer(ClockTimes),per_time=list_to_integer(ClockPerTime)},
    Proto = #proto{who_first=MyWhoFirst,range_zi=RangeZi,tie_mu=MyTieMu,clock_def=ClockDef},
    State#state.fromPid ! {dialog_result,ok,Proto},
    {stop,normal,State};

handle_event(#wx{id=?MYCANCEL},State) -> 
    State#state.fromPid ! {dialog_result,cancel},
    {stop,normal,State}.