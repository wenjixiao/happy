-module(games_manager).

-behavior(gen_server).

-compile(export_all).

put_stone(Gid,Stone) -> gen_server:cast(?MODULE,{put_stone,Gid,Stone}).

come_back(Player,Gid) -> gen_server:cast(?MODULE,{come_back,Player,Gid}).

line_broken(Player,Gid) -> gen_server:cast(?MODULE,{line_broken,Player,Gid}).

% =============================================================================
% here we use dict,gid->pid is one-to-one
init() -> {ok,dict:new()}.

terminate(_Reason,_State) -> ok.

handle_cast({put_stone,Gid,Stone},GidPidDict) -> game:put_stone(dict:fetch(Gid,GidPidDict),Stone);

handle_cast({come_back,Uid,Gid},GidPidDict) -> game:come_back(dict:fetch(Gid,GidPidDict),Uid).

handle_cast({line_broken,Uid,Gid},GidPidDict) -> game:line_broken(dict:fetch(Gid,GidPidDict),Uid).

% =============================================================================

create_game(PlayerPid1,PlayerPid2,Proto) -> 
    