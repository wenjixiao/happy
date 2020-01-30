-module(proxys_manager).

-include("msgs.hrl").
-include("data.hrl").

-behavior(gen_server).

-compile(export_all).

% =============================================================================

start() -> gen_server:start({local,?MODULE},?MODULE,[],[]).

stop() -> gen_server:stop(?MODULE).

add_player_pid(PlayerPid) -> gen_server:cast(?MODULE,{add,PlayerPid}).

remove_player_pid(PlayerPid) -> gen_server:cast(?MODULE,{remove,PlayerPid}).

update_player_pid(PlayerPid) -> gen_server:cast(?MODULE,{update,PlayerPid}).

send_msg(Player,Msg) -> gen_server:cast(?MODULE,{send_msg,Player,Msg}).

% =============================================================================

init(PlayerPids) -> {ok,PlayerPids}.

terminate(_Reason,_State) -> ok.

handle_cast({add,PlayerPid},PlayerPids) -> {noreply,[PlayerPid|PlayerPids]};

handle_cast({update,PlayerPid},PlayerPids) ->
    Fun = fun(PP) ->
        case PP#player_pid.pid == PlayerPid#player_pid.pid of
            true -> PP#player_pid{player=PlayerPid#player};
            _ -> PP
        end
    end,
    {noreply,lists:map(Fun,PlayerPids)};

handle_cast({remove,PlayerPid},PlayerPids) -> {noreply,lists:delete(PlayerPid,PlayerPids)};

handle_cast({send_msg,Player,Msg},PlayerPids) ->
    io:format("in send_msg: ~p,~p~n",[Uid,Msg]),
    Fun = fun(PP) ->
        case PP#player_pid.player of
            undefined -> ok;
            MyPlayer -> 
                if 
                   MyPlayer == Player -> PP#player_pid.pid ! {send,Msg};
                   true -> ok
                end 
        end
    end,
    lists:map(Fun,PlayerPids),
    {noreply,PlayerPids}.
% =============================================================================