-module(line_broken_manager).

-behavior(gen_server).

-compile(export_all).

register_player_gid(Uid,Gid) -> gen_server:cast(?MODULE,{register,Uid,Gid}).
unregister_player_gid(Uid,Gid) -> gen_server:cast(?MODULE,{unregister,Uid,Gid}).
line_broken(Uid) -> gen_server:cast(?MODULE,{line_broken,Uid}).
come_back(Uid) -> gen_server:cast(?MODULE,{come_back,Uid}).

% =============================================================================

init() -> {ok,dict:new()}.

terminate(_Reason,_State) -> ok.

handle_cast({register,Uid,Gid},UidGidDict) -> 
    case dict:is_key(Uid,UidGidDict) of
        true -> {noreply,dict:append(Uid,Gid,UidGidDict)};
        false -> {noreply,dict:store(Uid,[Gid],UidGidDict)}
    end;

handle_cast({unregister,Uid,Gid},UidGidDict) -> 
    case dict:is_key(Uid,UidGidDict) of
        true -> 
            Gids = dict:fetch(Uid,UidGidDict),
            NewGids = lists:delete(Gid,Gids),
            case length(NewGids) of
                0 -> {noreply,dict:erase(Uid,UidGidDict)};
                _ -> {noreply,dict:store(Uid,NewGids,UidGidDict)}
            end,
        false -> {noreply,UidGidDict}
    end;

handle_cast({line_broken,Uid},UidGidDict) ->
    case dict:is_key(Uid,UidGidDict) of
        true -> lists:foreach(fun(Gid) -> games_manager:line_broken(Uid,Gid) end,dict:fetch(Uid,UidGidDict));
        false -> ok
    end,
    {noreply,UidGidDict};

handle_cast({come_back,Uid},UidGidDict) -> 
    case dict:is_key(Uid,UidGidDict) of
        true -> lists:foreach(fun(Gid) -> games_manager:come_back(Uid,Gid) end,dict:fetch(Uid,UidGidDict));
        false -> ok
    end,
    {noreply,UidGidDict}.

