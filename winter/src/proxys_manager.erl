-module(proxys_manager).

-behavior(gen_server).

-compile(export_all).

% =============================================================================

start() -> gen_server:start({local,?MODULE},?MODULE,[],[]).

stop() -> gen_server:stop(?MODULE).

add(Uid,Pid) -> gen_server:cast(?MODULE,{add,Uid,Pid}).

remove(Uid) -> gen_server:cast(?MODULE,{remove,Uid}).

get_pid(Uid) -> gen_server:call(?MODULE,{get_pid,Uid}).

send_msg(Uid,Msg) -> gen_server:cast(?MODULE,{send_msg,Uid,Msg}).

% =============================================================================

init() -> {ok,dict:new()}.

terminate(_Reason,_State) -> ok.

handle_call({get_pid,Uid},From,UidPidDict) -> {reply,dict:fetch(Uid,UidPidDict),UidPidDict}.

handle_cast({add,Uid,Pid},UidPidDict) -> {noreply,dict:store(Uid,Pid,UidPidDict)};

handle_cast({remove,Uid},UidPidDict) -> {noreply,dict:erase(Uid,UidPidDict)};

handle_cast({send_msg,Uid,Msg},UidPidDict) ->
    io:format("in send_msg: ~p,~p~n",[Name,Msg]),
    dict:fetch(Uid,UidPidDict) ! {send,Msg},
    {noreply,UidPidDict}.
% =============================================================================