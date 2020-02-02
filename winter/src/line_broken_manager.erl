-module(line_broken_manager).

-behavior(gen_server).

-record(mylinebroken,{gid,game_pid,uid}).

-compile(export_all).

start() -> gen_server:start({local,?MODULE},?MODULE,[],[]).
stop() -> gen_server:stop(?MODULE).

line_broken(Gid,GamePid,Uid) -> gen_server:cast(?MODULE,{line_broken,Gid,GamePid,Uid}).
gameover_with_line_broken(Gid) -> gen_server:cast(?MODULE,{gameover_with_line_broken,Gid}).
come_back(Uid,ProxyPid) -> gen_server:cast(?MODULE,{come_back,Uid,ProxyPid}).

% =============================================================================

init(_Args) -> {ok,[]}.
terminate(_Reason,_State) -> ok.

handle_cast({line_broken,Gid,GamePid,Uid},MyLineBrokens) -> 
    {noreply,MyLineBrokens++[#mylinebroken{gid=Gid,game_pid=GamePid,uid=Uid}]};

handle_cast({come_back,Uid,ProxyPid},MyLineBrokens) -> 
    {YesMyBrokens,NotMyBrokens} = lists:partition(fun(R)-> R#mylinebroken.uid == Uid end,MyLineBrokens),
    lists:map(fun(R)-> game:come_back(R#mylinebroken.game_pid,Uid,ProxyPid) end,YesMyBrokens),
    {noreply,NotMyBrokens};

handle_cast({gameover_with_line_broken,Gid},MyLineBrokens) ->
    {noreply,lists:filter(fun(LineBroken)-> LineBroken#mylinebroken.gid /= Gid end,MyLineBrokens)}.

