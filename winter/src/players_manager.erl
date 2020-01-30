-module(players_manager).

-include("msgs.hrl").
-include("data.hrl").

-behavior(gen_server).

-compile(export_all).

% =============================================================================

start() -> gen_server:start({local,?MODULE},?MODULE,[],[]).

stop() -> gen_server:stop(?MODULE).

get_player(Name,Password) -> gen_server:call(?MODULE,{get_player,Name,Password}).

% =============================================================================

init() -> {ok,[init_players()]}.

terminate(_Reason,_State) -> ok.

handle_call({get_player,Name,Password},From,Players) ->
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
    {reply,Reply,UidPlayers}.

% =============================================================================

init_players() ->
    P1 = #player{name="wen",password="123",level="3d"},
    P2 = #player{name="zhong",password="456",level="18k"},
    [P1,P2].
