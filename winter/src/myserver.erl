-module(myserver).

-include("msgs.hrl").

-record(proxy,{sock,player,games}).

-export([start/0,listen_loop/1,proxy_read_loop/1]).

start() ->
    proxys_manager:start(),
    case gen_tcp:listen(20000,[binary,{packet,4},{reuseaddr,true},{active,true}]) of
        {ok,ListenSocket} -> 
            listen_loop(ListenSocket),
            gen_tcp:close(ListenSocket);
        {error,Reason} -> io:format("listen: ~p~n",[Reason])
    end.

listen_loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok,Socket} -> 
            Proxy = #proxy{sock=Socket,games=[]},
            ProxyPid = spawn(?MODULE,proxy_read_loop,[Proxy]),
            gen_tcp:controlling_process(Socket,ProxyPid),
            listen_loop(ListenSocket);
        {error,Reason} -> 
            io:format("accept: ~p~n",[Reason])
    end.

proxy_read_loop(Proxy) ->
    Sock = Proxy#proxy.sock,
    io:format("proxy process reading:~p ~p~n",[self(),Sock]),
    receive
        {tcp,Sock,Data} ->
            Msg = binary_to_term(Data),
            io:format("received data: ~p~n",[Msg]),
            case Msg of 
                #login{name=Name,password=Password} ->
                    case players_manager:get_db_player(Name,Password) of
                        {ok,Player} -> 
                            io:format("login ok:~p,~p~n",[Name,Password]),
                            % players_manager:add(Player),
                            proxys_manager:add(Player#player.name,self()),
                            % line_broken_manager:come_back(Uid),
                            send_msg(Sock,#login_ok{player=Player}),
                            proxy_read_loop(Proxy#proxy{player=Player});
                        {error,Reason} -> 
                            io:format("login failed: ~w~n",[Reason]),
                            send_msg(Sock,#login_fail{reason=Reason}),
                            proxy_read_loop(Proxy)
                    end;

                #invite{toName=ToName} -> 
                    proxys_manager:send_msg(ToName,Msg),
                    proxy_read_loop(Proxy)
            end;

        {send,Msg} -> 
            send_msg(Sock,Msg),
            proxy_read_loop(Proxy);

        {tcp_closed,_} -> 
            io:format("proxy read process closed:~w~n",[self()]),
            case Proxy#proxy.player of
                undefined -> ok;
                Player -> 
                    % players_manager:remove(Uid),
                    proxys_manager:remove(Player#player.name),
                    % line_broken_manager:line_broken(Uid)
            end
    end.

send_msg(Socket,Msg) -> gen_tcp:send(Socket,term_to_binary(Msg)).
