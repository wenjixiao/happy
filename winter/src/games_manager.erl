-module(games_manager).

-behavior(gen_server).

-compile(export_all).

% =============================================================================
% 对于正在运行的game，做一个统一的记录。
% 不关心具体事务，只是知道有这个game的存在。
% 必要时，可查。
% 我不能不知道“当前有多少个game在运行，都是谁”这样的问题。
% =============================================================================

start() -> gen_server:start({local,?MODULE},?MODULE,[],[]).
stop() -> gen_server:stop(?MODULE).

add(Gid,GamePid) -> gen_server:cast(?MODULE,{add,Gid,GamePid}).
remove(Gid) -> gen_server:cast(?MODULE,{remove,Gid}).

% =============================================================================
% here we use dict,gid->pid is one-to-one
init() -> {ok,dict:new()}.
terminate(_Reason,_State) -> ok.

handle_cast({add,Gid,GamePid},GidPidDict) -> {noreply,dict:store(Gid,GamePid,GidPidDict)}.
handle_cast({remove,Gid},GidPidDict) -> {noreply,dict:erase(Gid,GidPidDict)}.

% =============================================================================
