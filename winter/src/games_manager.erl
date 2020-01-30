-module(games_manager).

-behavior(gen_server).

-record(gid_pid,{gid,pid}).

-compile(export_all).

put_stone(GameId,Stone) -> gen_server:cast(?MODULE,{put_stone,GameId,Stone}).

line_broken(Player) -> gen_server:cast(?MODULE,{line_broken,Player}).

% =============================================================================

init() -> {ok,[]}.

terminate(_Reason,_State) -> ok.

handle_cast({put_stone,GameId,Stone},GidPids) -> 
    Reply = case lists:keyfind(GameId,1,GidPids) of
        false -> not_find;
        GidPid -> game:put_stone(GidPid#gid_pid.pid,Stone)
    end;

handle_cast({line_broken,Player},GidPids) ->
    {noreply,GidPids};

handle_cast({come_back,Player},GidPids) ->
    {noreply,GidPids}.

% =============================================================================
