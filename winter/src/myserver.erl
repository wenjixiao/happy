-module(myserver).

-include("msgs.hrl").

-export([start/0]).

-record(context,{players_db,proxy_manager,games}).
-record(proxy,{sock,player}).
-record(pp,{player,proxy}).

init_players() ->
    P1 = #player{pid="wen",password="123",level="3d"},
    P2 = #player{pid="zhong",password="456",level="18k"},
    [P1,P2].

% =============================================================================

default_context() -> 
    Players = init_players(),
    ProxyManager = spawn(?MODULE,proxy_manager_start,[]),
    #context{players_db=Players,proxy_manager=ProxyManager}.

start() ->
    Context = default_context(),
    case gen_tcp:listen(20000,[binary,{packet,4},{reuseaddr,true},{active,true}]) of
        {ok,ListenSocket} -> 
            listen_loop(ListenSocket,Context),
            gen_tcp:close(ListenSocket);
        {error,Reason} -> io:format("listen: ~p~n",[Reason])
    end.

listen_loop(ListenSocket,Context) ->
    case gen_tcp:accept(ListenSocket) of
        {ok,Socket} -> 
            Proxy = #proxy{sock=Socket},
            ProxyPid = spawn(?MODULE,proxy_read_loop,[Proxy]),
            Context#context.proxy_manager ! {add,ProxyPid},
            listen_loop(ListenSocket,Context);
        {error,Reason} -> 
            io:format("accept: ~p~n",[Reason])
    end.

proxy_read_loop(Proxy) ->
    Sock = Proxy#proxy.sock,
    receive
        {tcp,Sock,Data} ->
            Msg = binary_to_term(Data),
            io:format("received data: ~p~n",[Msg]),
            case Msg of 
                #login{pid=Pid,password=Password} ->
                    io:format("login ok:~p,~p~n",[Pid,Password]),
                    proxy_read_loop(Proxy),
                    gen_tcp:send(Sock,term_to_binary(Msg))
            end;
        {tcp_closed,_} -> io:format("closed~n")
    end.

% =============================================================================

proxy_manager_start() -> 
    Players = init_players(),
    proxy_manager_loop([],Players).

proxy_manager_loop(PP,Players) ->
    receive
        {add,PlayerProxy} -> [PlayerProxy|PP];
        {update,PlayerProxy} ->
            myfun = fun(MyPP) ->
                if 
                    MyPP#pp.proxy == PlayerProxy#pp.proxy -> MyPP#pp{player=PlayerProxy#pp.player};
                    MyPP#pp.proxy /= PlayerProxy#pp.proxy -> MyPP
                end
            end,
            proxy_manager_loop(lists:map(myfun,PP),Players);

        {remove,PlayerProxy} -> proxy_manager_loop(lists:delete(PlayerProxy,PP),Players);

        {request,{Ref,Pid},Msg} -> 
            case Msg of
                {get_player,MyPid,Password} ->
                    myfun = fun(Player) ->
                        if
                            (Player#player.pid == MyPid) and (Player#player.password == Password) -> true;
                            (Player#player.pid /= MyPid) or (Player#player.password /= Password) -> false
                        end
                    end,

                    Result = lists:filter(myfun,Players),

                    Len = length(Result),

                    Reply = if 
                        Len > 0 -> 
                            [Head|_] = Result,
                            {ok,Head};
                        Len =< 0 -> {error,"no that player!"}
                    end,
                    reply({Ref,Pid},Reply)
            end
    end.

% =============================================================================

call(Pid,Msg) ->
    Ref = make_ref(),
    Pid ! {request,{Ref,self()},Msg},
    receive
        {reply,Ref,Reply} -> Reply
    end.

reply({Ref,Pid},Message) ->
    Pid ! {reply,Ref,Message}.
% =============================================================================