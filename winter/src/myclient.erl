-module(myclient).

-include("msgs.hrl").

-export([start/0]).

start() ->
    {ok,Socket} = gen_tcp:connect("localhost",20000,[binary,{packet,4},{active,true}]),
    LoginMsg = #login{pid="wen",password="123"},
    gen_tcp:send(Socket,term_to_binary(LoginMsg)),
    receive
        {tcp,Socket,Data} -> 
            Msg = binary_to_term(Data),
            io:format("received data: ~p~n",[Msg])
    end,
    gen_tcp:close(Socket).
