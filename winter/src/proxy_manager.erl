-module(proxy_manager).

-include("msgs.hrl").
-include("data.hrl").

-behavior(gen_server).

-compile(export_all).


% =============================================================================

start() -> gen_server:start({local,?MODULE},?MODULE,{[],init_players()},[]).

stop() -> gen_server:stop(?MODULE).

add_player_proxy(PlayerProxy) -> gen_server:cast(?MODULE,{add,PlayerProxy}).

remove_player_proxy(PlayerProxy) -> gen_server:cast(?MODULE,{remove,PlayerProxy}).

update_player_proxy(PlayerProxy) -> gen_server:cast(?MODULE,{update,PlayerProxy}).

get_player(Name,Password) -> gen_server:call(?MODULE,{get_player,Name,Password}).

send_msg(Name,Msg) -> gen_server:cast(?MODULE,{send_msg,Name,Msg}).

% =============================================================================

init_players() ->
    P1 = #player{name="wen",password="123",level="3d"},
    P2 = #player{name="zhong",password="456",level="18k"},
    [P1,P2].

% =============================================================================

init({PP,Players}) -> {ok,{PP,Players}}.

terminate(_Reason,_State) -> ok.

handle_cast({add,PlayerProxy},{PP,Players}) -> {noreply,{[PlayerProxy|PP],Players}};

handle_cast({update,PlayerProxy},{PP,Players}) ->
    Fun = fun(MyPP) ->
        case MyPP#pp.proxy_pid == PlayerProxy#pp.proxy_pid of
            true -> MyPP#pp{player=PlayerProxy#pp.player};
            _ -> MyPP
        end
    end,
    {noreply,{lists:map(Fun,PP),Players}};

handle_cast({remove,PlayerProxy},{PP,Players}) -> {noreply,{lists:delete(PlayerProxy,PP),Players}};

handle_cast({send_msg,Name,Msg},{PP,Players}) ->
    % pp = {player,proxy_pid}
    Fun = fun(MyPP) ->
        case MyPP#pp.player of
            undefined -> ok;
            Player -> 
                if 
                   Player#player.name == Name -> MyPP#pp.proxy_pid ! {send,Msg};
                   Player#player.name /= Name -> ok
                end 
        end
    end,
    lists:map(Fun,PP),
    {noreply,{PP,Players}}.

handle_call({get_player,Name,Password},From,{PP,Players}) ->
    Fun = fun(Player) ->
        if
            (Player#player.name == Name) and (Player#player.password == Password) -> true;
            (Player#player.name /= Name) or (Player#player.password /= Password) -> false
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
    {reply,Reply,{PP,Players}}.

% =============================================================================