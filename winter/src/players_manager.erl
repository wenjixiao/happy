-module(players_manager).

-include("msgs.hrl").

-behavior(gen_server).

-compile(export_all).

% =============================================================================
% 目前，都有谁在server上。
% 在db中，存取player资料。
% =============================================================================

start() -> gen_server:start({local,?MODULE},?MODULE,[],[]).
stop() -> gen_server:stop(?MODULE).

get_db_player(Name,Password) -> gen_server:call(?MODULE,{get_db_player,Name,Password}).

add_player(Player) -> gen_server:call(?MODULE,{add,Player}).
remove_player(Player) -> gen_server:call(?MODULE,{remove,Player}).
get_player(Name) -> gen_server:call(?MODULE,{get_player,Name}).

% =============================================================================

init() -> {ok,[PlayersDict,init_players()]}.
terminate(_Reason,_State) -> ok.

handle_cast({add,Player},{PlayersDict,DbPlayers}) -> {noreply,{dict:store(Player#player.name,Player,PlayersDict),DbPlayers}};

handle_cast({remove,Name},{PlayersDict,DbPlayers}) -> {noreply,{dict:erase(Name,PlayersDict),DbPlayers}}.
    
handle_call({get_player,Name},From,{PlayersDict,DbPlayers}) ->
    Reply = try dict:fetch(Name,PlayersDict) of
        Player -> {ok,Player}
    catch
        error:_ -> {error,"no that player!"}
    end,
    {reply,Reply,{PlayersDict,DbPlayers}}.

handle_call({get_db_player,Name,Password},From,{PlayersDict,DbPlayers}) ->
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
    {reply,Reply,{PlayersDict,DbPlayers}}.

% =============================================================================

init_players() ->
    P1 = #player{name="wen",password="123",level="3d"},
    P2 = #player{name="zhong",password="456",level="18k"},
    [P1,P2].
