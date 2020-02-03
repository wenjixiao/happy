-module(wxclient).

-include("msgs.hrl").

-include_lib("wx/include/wx.hrl").

-export([start/0]).

-define(MYOUTPUT,100).
-define(MYINPUT,101).

-record(context,{player}).

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
            loop(Frame,Socket,#context{}),
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

loop(Frame,Socket,Context) ->
    receive
        #wx{id=?MYINPUT,event=#wxCommand{type=command_text_enter}} ->
            InputTextCtrl = wx:typeCast(wxWindow:findWindowById(?MYINPUT),wxTextCtrl),
            Line = string:trim(wxTextCtrl:getValue(InputTextCtrl)),
            Len = length(Line),
            if 
                Len > 0 ->
                    [Cmd|Params] = string:split(Line," ",all),
                    io:format("cmd things: ~p,~p~n",[Cmd,Params]),
                    case Cmd of
                        "exit" -> ok;

                        "login" -> 
                            [Name,Password] = Params,
                            Login = #login{name=Name,password=Password},
                            gen_tcp:send(Socket,term_to_binary(Login)),
                            wxTextCtrl:clear(InputTextCtrl),
                            loop(Frame,Socket,Context);

                        "invite" ->
                            [Name] = Params,
                            Invite = #invite{toUid=Name},
                            gen_tcp:send(Socket,term_to_binary(Invite)),
                            wxTextCtrl:clear(InputTextCtrl),
                            loop(Frame,Socket,Context);

                        "invite_ok" ->
                            [Name] = Params,
                            InviteOk = #invite_ok{toUid=Name},
                            gen_tcp:send(Socket,term_to_binary(InviteOk)),
                            wxTextCtrl:clear(InputTextCtrl),
                            loop(Frame,Socket,Context)

                    end;

                true -> loop(Frame,Socket,Context)
            end;

        #wx{event=#wxClose{}} -> io:format("window closed~n");

        {tcp,Socket,Data} -> 
            Msg = binary_to_term(Data),
            io:format("received data: ~p~n",[Msg]),

            OutputTextCtrl = wx:typeCast(wxWindow:findWindowById(?MYOUTPUT),wxTextCtrl),
            wxTextCtrl:setValue(OutputTextCtrl,io_lib:format("~p",[Msg])),

            case Msg of
                #login_ok{player=MyPlayer} -> 
                    wxFrame:setLabel(Frame,MyPlayer#player.name),
                    loop(Frame,Socket,Context#context{player=MyPlayer});

                #login_fail{} -> loop(Frame,Socket,Context);

                #invite{} -> 
                    io:format("invite: ~p~n",[Msg]),
                    loop(Frame,Socket,Context)
            end
    end.

% =============================================================================

get_per_time(Proto) -> Proto#proto.clock_def#clock_def.per_time.

% switch_clock(Game) -> 
%     case Game of
%         #game{color=black,clocks=#clocks{black=Clock},proto=Proto} ->
%             Game#game{color=white,clocks=#clocks{black=Clock#clock{per_time=get_per_time(Proto)}}};
%         #game{color=white,clocks=#clocks{white=Clock},proto=Proto} ->
%             Game#game{color=black,clocks=#clocks{white=Clock#clock{per_time=get_per_time(Proto)}}}
%     end.

countdown(Clock,Proto) -> countdown1(Clock,Proto,bao_liu).

countdown1(Clock,Proto,Channel) ->
    case Channel of
        bao_liu -> 
            BaoLiu = Clock#clock.bao_liu,
            if
                BaoLiu > 0 -> Clock#clock{bao_liu=BaoLiu-1};
                true -> countdown1(Clock,Proto,du_miao)
            end;
        du_miao ->
            DuMiao = Clock#clock.du_miao,
            if 
                DuMiao > 0 -> Clock#clock{du_miao=DuMiao-1};
                true -> countdown1(Clock,Proto,times)
            end;
        times ->
            Times = Clock#clock.times,
            if
                Times > 0 -> Clock#clock{times=Times-1,per_time=get_per_time(Proto)};
                true -> timeout
            end;
        per_time ->
            PerTime = Clock#clock.per_time,
            if 
                PerTime > 0 -> Clock#clock{per_time=PerTime-1};
                true -> countdown1(Clock,Proto,times)
            end
    end.
