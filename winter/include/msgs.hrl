-record(login,{name,password}).
-record(login_ok,{player}).
-record(login_fail,{reason}).
-record(invite,{fromUid,toUid,proto}).
-record(invite_ok,{fromUid,toUid,proto}).
-record(put_stone,{gid,stone}).

-record(player,{name,password,level}).
-record(stone,{color,x,y,is_pass}).
-record(clock,{bao_liu,du_miao,times,per_time}).
-record(clock_def,{bao_liu,du_miao,times,per_time}).
% who_first_rule: {assign,black/white},random
-record(proto,{who_first,range_zi,tie_mu,clock_def}).
% end type: admit,count,timeout,line_broken
-record(result,{winner,end_type,mount}).

-record(myproxy,{uid,proxy_pid}).
-record(players,{black,white}).
-record(clocks,{black,white}).
-record(broken_colors,{black,white}).

-record(game,{gid,state,color,players,clocks,broken_colors,stones,proto,result}).
