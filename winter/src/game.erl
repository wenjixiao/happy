-module(game).

-record(stone,{color,x,y}).
-record(clock,{bao_liu,du_miao,times,per_time}).
-record(clock_def,{bao_liu,du_miao,times,per_time}).
% who_first_rule: assign,random
-record(proto,{who_first_rule,range_zi,tie_mu,clock_def}).
% state: prepare,running,paused,ended
% end type: admit,count,timeout,line_broken
-record(myproxy,{uid,proxy_pid}).
-record(result,{winner,end_type,mount}).
-record(players,{black,white}).
-record(clocks,{black,white}).
-record(broken_colors,{black,white}).
-record(game,{gid,state,color,init_players,players,clocks,broken_colors,stones,proto,result}).

-behavior(gen_server).

-compile(export_all).

create_game([PlayerPids]) -> gen_server:start(?MODULE,[#game{init_players=PlayerPids}],[]).

destroy_game(GamePid) -> gen_server:stop(GamePid).

put_stone(GamePid,Stone) -> gen_server:cast(GamePid,{put_stone,Stone}).

gameover(GamePid,Result) -> gen_server:cast(GamePid,{gameover,Result}).

line_broken(GamePid,Uid) -> gen_server:cast(GamePid,{line_broken,Uid}).

come_back(GamePid,Uid,ProxyPid) -> gen_server:cast(GamePid,{come_back,Uid,ProxyPid}).

get_gid(GamePid) -> gen_server:call(GamePid,get_gid).

% =============================================================================

init(Game) -> {ok,Game}.

terminate(_Reason,_State) -> ok.

handle_call(get_gid,From,Game) -> {reply,Game#game.gid,Game}.

handle_cast({put_stone,Stone},Game) ->
    case Stone#stone.color of
        black -> proxy:send_msg(Game#game.players#players.white,#put_stone{gid=Game#game.gid,stone=Stone});
        white -> proxy:send_msg(Game#game.players#players.black,#put_stone{gid=Game#game.gid,stone=Stone})
    end,
    {noreply,Game#game{stones=Stones++[Stone]}};
    
handle_cast({line_broken,Uid},Game) ->
    line_broken_manager:line_broken(Game#game.gid,self(),Uid),
    
    Players = Game#game.players,
    case Uid of 
        Players#players.black#myproxy.uid -> Game#game{broken_colors=#broken_colors{black=true}};
        Players#players.white#myproxy.uid -> Game#game{broken_colors=#broken_colors{white=true}}
    end,
    {noreply,Game};

handle_cast({come_back,Uid,ProxyPid},Game) ->
    Players = Game#game.players,
    BrokenColors = Game#game.broken_colors,
    case Uid of 
        Players#players.black#myproxy.uid -> 
            NewPlayers = Players#players{black=#myproxy{uid=Uid,proxy_pid=ProxyPid}},
            NewBrokenColors = BrokenColors#broken_colors{black=false},
            {noreply,Game#game{players=NewPlayers,broken_colors=NewBrokenColors}};
        Players#players.white#myproxy.uid -> 
            NewPlayers = Players#players{white=#myproxy{uid=Uid,proxy_pid=ProxyPid}},
            NewBrokenColors = BrokenColors#broken_colors{white=false},
            {noreply,Game#game{players=NewPlayers,broken_colors=NewBrokenColors}};
    end.

handle_cast({clock_notify,Clock},Game) ->
    {noreply,Game};

handle_cast(countdown,Game) ->
    case Game of
        #game{color=black,clocks=#clocks{black=Clock},proto=Proto} -> 
            case countdown(Clock,Proto) of
                timeout -> {noreply,Game#game{result=#result{winner=white,end_type=timeout}}};
                NewClock -> {noreply,Game#game{clocks=#clocks{black=NewClock}}}
            end;
        #game{color=white,clocks=#clocks{white=Clock},proto=Proto} -> 
            case countdown(Clock,Proto) of
                timeout -> {noreply,Game#game{result=#result{winner=black,end_type=timeout}}};
                NewClock -> {noreply,Game#game{clocks=#clocks{white=NewClock}}}
            end
    end;

handle_cast({gameover,Result},Game) -> {stop,"gameover",Game}.


% =============================================================================

is_line_broken(Game) -> 
    BrokenColors = Game#game.broken_colors,
    if 
        BrokenColors#broken_colors.black or BrokenColors#broken_colors.white -> true;
        true -> false
    end.

get_per_time(Proto) -> Proto#proto.clock_def#clock_def.per_time.

switch_clock(Game) -> 
    case Game of
        #game{color=black,clocks=#clocks{black=Clock},proto=Proto} ->
            Game#game{color=white,clocks=#clocks{black=Clock#clock{per_time=get_per_time(Proto)}}};
        #game{color=white,clocks=#clocks{white=Clock},proto=Proto} ->
            Game#game{color=black,clocks=#clocks{white=Clock#clock{per_time=get_per_time(Proto)}}}
    end.

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
