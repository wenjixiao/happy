-module(proxy).

-record(mygame,{gid,game_pid}).
-record(context,{sock,player,mygames}).

-behavior(gen_server).

-compile(export_all).

start(Socket) -> gen_server:start(?MODULE,[#context{sock=Socket,mygames=[]}],[]).
stop(ProxyPid) -> gen_server:stop(ProxyPid).

send_msg(ProxyPid,Msg) -> gen_server:cast(ProxyPid,{send,Msg}).

% =============================================================================

init(Context) -> {ok,Context}.
terminate(_Reason,_State) -> ok.

handle_cast({tcp,Sock,Data},Context) ->
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
                    self() ! {send,#login_ok{player=Player}},
                    
                    {noreply,Context#context{player=Player}};
                {error,Reason} -> 
                    io:format("login failed: ~w~n",[Reason]),
                    send_msg(Sock,#login_fail{reason=Reason}),
                    {noreply,Context}
            end;

        #invite{toName=ToName} -> 
            proxys_manager:send_msg(ToName,Msg),
            {noreply,Context};

        #put_stone{gid=Gid,stone=Stone} ->
            MyGames = Context#context.mygames,
            Fun = fun(MyGame) ->
                if 
                    MyGame#mygame.gid == Gid -> game:put_stone(MyGame#mygame.game_pid,Stone);
                    true -> ok
                end
            end,
            lists:map(Fun,MyGames)
    end,
    {noreply,Context};

handle_cast({tcp_closed,Sock},Context) ->
    Fun = fun(MyGame) -> game:line_broken(MyGame#mygame.game_pid,Context#context.player#player.name) end,
    lists:map(Fun,Context#context.games),
    {noreply,Context};

handle_cast({send,Msg},Context) ->
    gen_tcp:send(Context#context.sock,term_to_binary(Msg)),
    {noreply,Context}.

handle_call(get_player,From,Context) -> {reply,Context#context.player,Context}.

% =============================================================================
