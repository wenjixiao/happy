-module(proxy_manager).

-include("msgs.hrl").
-include("data.hrl").

-behavior(gen_server).

-compile(export_all).

% =============================================================================

start() -> gen_server:start({local,?MODULE},?MODULE,{[],init_players()},[]).

stop() -> gen_server:stop(?MODULE).

add_player_proxy(PlayerPid) -> gen_server:cast(?MODULE,{add,PlayerPid}).

remove_player_proxy(PlayerPid) -> gen_server:cast(?MODULE,{remove,PlayerPid}).

update_player_proxy(PlayerPid) -> gen_server:cast(?MODULE,{update,PlayerPid}).

get_player(Name,Password) -> gen_server:call(?MODULE,{get_player,Name,Password}).

send_msg(Name,Msg) -> gen_server:cast(?MODULE,{send_msg,Name,Msg}).

% =============================================================================

init_players() ->
    P1 = #player{name="wen",password="123",level="3d"},
    P2 = #player{name="zhong",password="456",level="18k"},
    [P1,P2].

% =============================================================================

init({PlayerPids,Players}) -> {ok,{PlayerPids,Players}}.

terminate(_Reason,_State) -> ok.

handle_cast({add,PlayerPid},{PlayerPids,Players}) -> {noreply,{[PlayerPid|PlayerPids],Players}};

handle_cast({update,PlayerPid},{PlayerPids,Players}) ->
    Fun = fun(PP) ->
        case PP#player_pid.pid == PlayerPid#player_pid.pid of
            true -> PP#player_pid{player=PlayerPid#player_pid.player};
            _ -> PP
        end
    end,
    {noreply,{lists:map(Fun,PlayerPids),Players}};

handle_cast({remove,PlayerPid},{PlayerPids,Players}) -> {noreply,{lists:delete(PlayerPid,PlayerPids),Players}};

handle_cast({send_msg,Name,Msg},{PlayerPids,Players}) ->
    io:format("in send_msg: ~p,~p~n",[Name,Msg]),
    Fun = fun(PP) ->
        case PP#player_pid.player of
            undefined -> ok;
            Player -> 
                if 
                   Player#player.name == Name -> PP#player_pid.pid ! {send,Msg};
                   true -> ok
                end 
        end
    end,
    lists:map(Fun,PlayerPids),
    {noreply,{PlayerPids,Players}}.

handle_call({get_player,Name,Password},From,{PlayerPids,Players}) ->
    Fun = fun(Player) ->
        if
            (Player#player.name == Name) and (Player#player.password == Password) -> true;
            true -> false
        end
    end,

    Result = lists:filter(Fun,Players),

    Len = length(Result),

    Reply = if 
        Len > 0 -> 
            [Head|_] = Result,
            {ok,Head};
        Len =< 0 -> {error,"no that player!"}
    end,
    {reply,Reply,{PlayerPids,Players}}.

% =============================================================================