-module(listening).

-export([start/0]).

basic_services() ->
    players_manager:start(),
    proxys_manager:start(),
    line_broken_manager:start(),
    id_pool:start(),
    games_manager:start().

start() ->
    basic_services(),

    case gen_tcp:listen(20000,[binary,{packet,4},{reuseaddr,true},{active,true}]) of
        {ok,ListenSocket} -> 
            listen_loop(ListenSocket),
            gen_tcp:close(ListenSocket);
        {error,Reason} -> io:format("start listen: ~p~n",[Reason])
    end.

listen_loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok,Socket} -> 
            case proxy:start(Socket) of
                {ok,ProxyPid} -> 
                    gen_tcp:controlling_process(Socket,ProxyPid),
                    listen_loop(ListenSocket);
                {error,Reason} -> io:format("start read socket: ~p~n",[Reason])
            end;
        {error,Reason} -> 
            io:format("accept socket: ~p~n",[Reason])
    end.
