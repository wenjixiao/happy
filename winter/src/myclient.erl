-module(myclient).

-include("msgs.hrl").

-export([start/0]).

start() ->
    case gen_tcp:connect("localhost",20000,[binary,{packet,4},{active,true}]) of
        {ok,Socket} -> cmd_loop(Socket);
        {error,Reason} -> io:format("connect error:~w~n",[Reason])
    end.

cmd_loop(Socket) ->
    receive
        {tcp,Socket,Data} -> 
            Msg = binary_to_term(Data),
            io:format("received data: ~p~n",[Msg])
    after 2000 -> true
    end,

    case string:trim(io:get_line(">")) of
        eof -> io:format("ok,eof,exit!");

        {error,ErrorDescription} -> io:format("error:~w~n",[ErrorDescription]);

        Line -> 
            [Cmd|Params] = string:split(Line," ",all),
            case Cmd of
                "exit" -> ok;
                "login" -> 
                    [Pid,Password] = Params,
                    Login = #login{pid=Pid,password=Password},
                    gen_tcp:send(Socket,term_to_binary(Login)),
                    cmd_loop(Socket)
            end
    end,

    gen_tcp:close(Socket).
