-record(login,{name,password}).
-record(login_ok,{player}).
-record(login_fail,{reason}).
-record(invite,{fromName,toName}).
-record(put_stone,{gid,stone}).

-record(player,{name,password,level}).
-record(stone,{color,x,y,is_pass}).
-record(clock,{bao_liu,du_miao,times,per_time}).
-record(clock_def,{bao_liu,du_miao,times,per_time}).
% who_first_rule: assign,random
-record(proto,{who_first_rule,range_zi,tie_mu,clock_def}).
% state: prepare,running,paused,ended
% end type: admit,count,timeout,line_broken
-record(result,{winner,end_type,mount}).

