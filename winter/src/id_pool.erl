-module(id_pool).

-record(idpool,{size,step,numbers}).

-behavior(gen_server).

-compile(export_all).

start() -> gen_server:start({local,?MODULE},?MODULE,[],[]).
stop() -> gen_server:stop(?MODULE).

alloc_gid() -> gen_server:call(?MODULE,alloc_gid).
release_gid(Gid) -> gen_server:cast(?MODULE,{release_gid,Gid}).

% =============================================================================

init([]) -> {ok,#idpool{size=0,step=10,numbers=[]}}.
terminate(_Reason,_State) -> ok.

handle_call(alloc_gid,From,IdPool) -> 
    Nums = IdPool#idpool.numbers,
    LenNow = length(Nums),
    if
        LenNow == 0 -> 
            NewNums = grow_nums(IdPool#idpool.size,IdPool#idpool.step),
            NewSize = IdPool#idpool.size + length(NewNums),
            [Head|Rest] = NewNums,
            {reply,Head,IdPool#idpool{size=NewSize,numbers=Rest}};
        LenNow > 0 -> 
            [Head|Rest] = Nums,
            {reply,Head,IdPool#idpool{numbers=Rest}}
    end.

handle_cast({release_gid,Gid},IdPool) -> {noreply,IdPool#idpool{numbers=lists:sort(IdPool#idpool.numbers ++ [Gid])}}.

% =============================================================================

grow_nums(Size,Step) -> grow_nums_acc(Size,Step,[]).
    
grow_nums_acc(Size,Step,Nums) ->
    Len = length(Nums),
    if
        Len < Step -> grow_nums_acc(Size,Step,Nums ++ [Size+Len+1]);
        Len >= Step -> Nums
    end.

