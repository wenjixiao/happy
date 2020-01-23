-module(proxy_manager).

-include("msgs.hrl").

-behavior(gen_server).

-compile(export_all).

-record(pp,{player,proxy}).

% =============================================================================

start() ->
    gen_server:start({local,?MODULE},?MODULE,{[],init_players()},[]).

stop() ->
    gen_server:stop(?MODULE).

add_player_proxy(PlayerProxy) -> gen_server:cast(?MODULE,{add,PlayerProxy}).

remove_player_proxy(PlayerProxy) -> gen_server:cast(?MODULE,{remove,PlayerProxy}).

update_player_proxy(PlayerProxy) -> gen_server:cast(?MODULE,{update,PlayerProxy}).

get_player(Pid,Password) -> gen_server:call(?MODULE,{get_player,Pid,Password}).

% =============================================================================

init_players() ->
    P1 = #player{pid="wen",password="123",level="3d"},
    P2 = #player{pid="zhong",password="456",level="18k"},
    [P1,P2].

% =============================================================================

init({PP,Players}) -> {ok,{PP,Players}}.

terminate(_Reason,_State) -> ok.

handle_cast({add,PlayerProxy},{PP,Players}) -> {noreply,{[PlayerProxy|PP],Players}};

handle_cast({update,PlayerProxy},{PP,Players}) ->
    Fun = fun(MyPP) ->
        if 
            MyPP#pp.proxy == PlayerProxy#pp.proxy -> MyPP#pp{player=PlayerProxy#pp.player};
            MyPP#pp.proxy /= PlayerProxy#pp.proxy -> MyPP
        end
    end,
    {noreply,{lists:map(Fun,PP),Players}};

handle_cast({remove,PlayerProxy},{PP,Players}) -> {noreply,{lists:delete(PlayerProxy,PP),Players}}.

handle_call({get_player,Pid,Password},From,{PP,Players}) ->
    Fun = fun(Player) ->
        if
            (Player#player.pid == Pid) and (Player#player.password == Password) -> true;
            (Player#player.pid /= Pid) or (Player#player.password /= Password) -> false
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