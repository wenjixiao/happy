-module(game).

-include("msg.hrl").

-record(myproxy,{uid,proxy_pid}).
-record(players,{black,white}).
-record(clocks,{black,white}).
-record(broken_colors,{black,white}).
-record(game,{gid,state,color,init_players,players,clocks,broken_colors,stones,proto,result}).

-behavior(gen_server).

-compile(export_all).

start([PlayerPids]) -> gen_server:start(?MODULE,[#game{init_players=PlayerPids}],[]).
stop(GamePid) -> gen_server:stop(GamePid).

put_stone(GamePid,Stone) -> gen_server:cast(GamePid,{put_stone,Stone}).
game_over(GamePid,Result) -> gen_server:cast(GamePid,{game_over,Result}).
line_broken(GamePid,Uid) -> gen_server:cast(GamePid,{line_broken,Uid}).
come_back(GamePid,Uid,ProxyPid) -> gen_server:cast(GamePid,{come_back,Uid,ProxyPid}).

% =============================================================================

init(Game) -> {ok,Game}.
terminate(_Reason,_State) -> ok.

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

handle_cast({game_over,Result},Game) -> {stop,_,Game}.


% =============================================================================

is_line_broken(Game) -> 
    BrokenColors = Game#game.broken_colors,
    if 
        BrokenColors#broken_colors.black or BrokenColors#broken_colors.white -> true;
        true -> false
    end.
