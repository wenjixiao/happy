-module(proxy).

-include("msgs.hrl").

-record(mygame,{gid,game_pid}).
-record(context,{sock,player,mygames}).


-behavior(gen_server).

-compile(export_all).

start(Socket) -> gen_server:start(?MODULE,Socket,[]).
stop(ProxyPid) -> gen_server:stop(ProxyPid).

send_msg(ProxyPid,Msg) -> gen_server:cast(ProxyPid,{send,Msg}).
game_created(ProxyPid,Game,GamePid) -> gen_server:cast(ProxyPid,{game_created,Game,GamePid}).

% =============================================================================

init(Socket) -> 
    Context = #context{sock=Socket,mygames=[]},
    io:format("proxy started: ~p,~p~n",[Context,self()]),
    {ok,Context}.

terminate(_Reason,_State) -> 
    io:format("proxy stopped: ~p~n",[self()]),
    ok.

handle_info({tcp,Sock,Data},Context) ->
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
                    send_msg(self(),#login_ok{player=Player}),
                    {noreply,Context#context{player=Player}};
                {error,Reason} -> 
                    io:format("login failed: ~w~n",[Reason]),
                    send_msg(Sock,#login_fail{reason=Reason}),
                    {noreply,Context}
            end;

        #invite{toUid=ToUid,proto=Proto} -> 
            proxys_manager:send_msg(ToUid,Msg),
            {noreply,Context};

        #invite_ok{toUid=ToUid,fromUid=FromUid,proto=Proto} ->
            FromProxy = #myproxy{uid=FromUid,proxy_pid=proxys_manager:get_pid(FromUid)},
            ToProxy = #myproxy{uid=ToUid,proxy_pid=proxys_manager:get_pid(ToUid)},
            case game:start([FromProxy,ToProxy],Proto) of
                {ok,GamePid} -> {noreply,Context};
                {error,Reason} -> io:format("game start: ~p~n",[Reason])
            end;

        #put_stone{gid=Gid,stone=Stone} ->
            MyGames = Context#context.mygames,
            Fun = fun(MyGame) ->
                if 
                    MyGame#mygame.gid == Gid -> game:put_stone(MyGame#mygame.game_pid,Stone);
                    true -> ok
                end
            end,
            lists:map(Fun,MyGames),
            {noreply,Context}
    end;

handle_info({tcp_closed,Sock},Context) ->
    Fun = fun(MyGame) -> game:line_broken(MyGame#mygame.game_pid,Context#context.player#player.name) end,
    lists:map(Fun,Context#context.mygames),
    {stop,normal,Context}.

handle_cast({game_created,Game,GamePid},Context) -> 
    MyGame = #mygame{gid=Game#game.gid,game_pid=GamePid},
    NewMyGames = Context#context.mygames ++ [MyGame],

    gen_tcp:send(Context#context.sock,term_to_binary(Game)),

    {noreply,Context#context{mygames=NewMyGames}};

handle_cast({send,Msg},Context) ->
    gen_tcp:send(Context#context.sock,term_to_binary(Msg)),
    {noreply,Context}.

handle_call(get_player,From,Context) -> {reply,Context#context.player,Context}.

% =============================================================================
