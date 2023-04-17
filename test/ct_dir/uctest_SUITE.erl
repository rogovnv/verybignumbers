%%%-------------------------------------------------------------------
%%% @author mt
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% Test groups:
%%% On start, Correct data, Wrong data, Admin file
%%% TCP shutdown not testing, bcoz of root permissions
%%% 
%%% @end
%%% Created : 18. дек. 2022 11:25
%%%-------------------------------------------------------------------
-module(uctest_SUITE).
-author("mt").
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [{group, correct_data}, {group, on_start}, {group, wrong_data}, {group, admin_file}].

%%     {group, wrong_data},
groups() ->
    [
        {correct_data, [parallel], [
            get_getinfo,
            typical_task_one, %% complex nested code example
            typical_task_two, %% generated long file 
            typical_task_three, %% pi count with a 6 digits precision
            typical_task_four %% play with braces!
        ]},
         {wrong_data, [parallel], [
            odd_brace,
            odd_brace2,
            not_bound,
            no_eq_sign,
            bad_lbr,
            bad_lbr2,
            bad_oper,
            bad_rbr,
            no_range,
            no_aout, 
            no_range_aout,
            odd_cond,
            baddowhile_bgn,
            var_in_cond,
            in_cond,
            in_cond2,
            in_cond3,
            in_cond4,
            zero,
            overchunk,
            timeouted
        ]},
        {admin_file, [], [
            test_five
            ]},
        {on_start, [], [
            %% fullhouse, %% correct starting
            go_nopa, %% no conf file
            go_nopb, %% wrong conf file
            go_ok
            ]}
        ].

init_per_suite(Config) ->
	application:start(sasl),
    {ok, Name}=inet:gethostname(),
    ct:print("~n~p~n", [Name]),
    {ok, Med}=inet:gethostbyname(Name, inet),
    [Addr]=element(tuple_size(Med), Med),
    ct:print("~n~p~n", [Med]),
    %% {ok,{hostent, Name, _Noneed, inet, 4, [Addr]}}=Med,
    Haddr={haddr, Addr},
    {A, B, C, D}=Addr,
    Saddr={saddr, integer_to_list(A)++"."++integer_to_list(B)++"."++integer_to_list(C)++"."++integer_to_list(D)++":"},
    Bgn={bgn_hp, "POST / HTTP/1.1\r\nHost: "},
    Before_len_fle={prepare_len_fle, "\r\nUser-Agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:109.0) Gecko/20100101 Firefox/109.0\r\nAccept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8\r\nAccept-Language: ru-RU,ru;q=0.8,en-US;q=0.5,en;q=0.3\r\nAccept-Encoding: gzip, deflate\r\nContent-Type: multipart/form-data; boundary=---------------------------270019682022871984891320620731\r\nContent-Length: "},
    Before_len_form={prepare_len_form, "\r\nUser-Agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:109.0) Gecko/20100101 Firefox/109.0\r\nAccept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8\r\nAccept-Language: ru-RU,ru;q=0.8,en-US;q=0.5,en;q=0.3\r\nAccept-Encoding: gzip, deflate\r\nContent-Type: text/plain\r\nContent-Length: "},
    Rpt_HP={repeat_hp, "\r\nOrigin: http://"},
    HP_agn={hp_agn, "\r\nConnection: keep-alive\r\nReferer: http://"},
    Before_payload={before_payload, "/\r\nUpgrade-Insecure-Requests: 1\r\n\r\n"},
    Payload={payload, "-----------------------------270019682022871984891320620731\r\nContent-Disposition: form-data; name=\"File\"; filename=\"tst2.txt\"\r\nContent-Type: text/plain\r\n\r\n"},
    Finish={finish, "\r\n-----------------------------270019682022871984891320620731--\r\n"},
    %% enter correct data
    S_port={s_port, 2000}, %% EQ inet port from /conn_conf
	Priv_dir={pdir, "/media/download/base_calc-1.0.0/priv/"},
    Wdir={wdir, "/media/download/bc/"},
    %% before start
    [S_port, Priv_dir, Wdir, Haddr, Saddr, Bgn, Before_len_fle, Before_len_form, Rpt_HP, HP_agn, Before_payload, Payload, Finish|Config].

init_per_group(on_start, Config) ->
    ct:sleep(500),
    Config;
init_per_group(correct_data, Config) ->
    Name=?config(pdir, Config)++"/conn_conf",
    file:delete(Name),
    Data2="wdir "++?config(wdir, Config)++"\n"++"conns 2"++[10]++"port "++integer_to_list(?config(s_port, Config))++[10]++"maxmem 500000000"++[10],
    file:write_file(Name, Data2),
    application:start(base_calc),
    ct:sleep(500),
    ct:print("~nCorrect Part~n"),
    Config;

init_per_group(wrong_data, Config) -> 
    Name=?config(pdir, Config)++"/conn_conf",
    file:delete(Name),
    Data2="wdir "++?config(wdir, Config)++"\n"++"conns 3"++[10]++"port "++integer_to_list(?config(s_port, Config)+1)++[10]++"maxmem 500000000"++[10],
    file:write_file(Name, Data2),
    application:start(base_calc),
    ct:print("~nWrong part~n"),
    ct:sleep(500),
    Config;

init_per_group(admin_file, Config) ->
    Name=?config(pdir, Config)++"/conn_conf",
    file:delete(Name),
    Data2="wdir "++?config(wdir, Config)++"\n"++"conns 2"++[10]++"port "++integer_to_list(?config(s_port, Config)+2)++[10]++"maxmem 500000000"++[10],
    file:write_file(Name, Data2),
    application:start(base_calc),
    ct:print("~nWOmdmin part~n"),
    ct:sleep(500),
    Config.

end_per_group(on_start, Config) ->
    application:stop(base_calc),

    Config;
end_per_group(correct_data, _Config) ->
    %% ct:run_test([{spec, "uctest.spec"}]),
    application:stop(base_calc),
    %% application:unload(base_calc),
    ct:sleep(500);
end_per_group(wrong_data, _Config) ->
    application:stop(base_calc),
    ct:sleep(500);
end_per_group(admin_file, Config) ->
    Name=?config(pdir, Config)++"/conn_conf",
    file:delete(Name),
    Data2="wdir "++?config(wdir, Config)++"\n"++"conns 2"++[10]++"port "++integer_to_list(?config(s_port, Config))++[10]++"maxmem 500000000"++[10],
    file:write_file(Name, Data2),
    application:stop(base_calc),
    Config.

end_per_suite(Config) ->
    Name=?config(pdir, Config)++"/conn_conf",
    application:stop(base_calc),
    application:stop(sasl),
    file:delete(Name),
    Data2="wdir "++?config(wdir, Config)++"\n"++"conns 2"++[10]++"port "++integer_to_list(?config(s_port, Config))++[10]++"maxmem 500000000"++[10],
    file:write_file(Name, Data2),
    Config.

%%%%%%%%%%%%%%%%% ON start section

go_nopa(Config) ->
    Name=?config(pdir, Config)++"/conn_conf",
    filelib:is_file(Name) andalso file:delete(Name),
    F=application:start(base_calc),
    application:stop(base_calc),
    %% application:unload(base_calc),
    ?assert(F/=ok),
    ct:sleep(500),
    Config.

go_nopb(Config) ->
    Name=?config(pdir, Config)++"/conn_conf",
    filelib:is_file(Name) andalso file:delete(Name),
    Pname=?config(pdir, Config)++"/err.log",
    filelib:is_file(Pname) andalso file:delete(Pname),
    Data="conns 2"++[10]++"port "++integer_to_list(?config(s_port, Config))++[10]++"maxmem 500000000"++[10],
    file:write_file(Name, Data),
    application:start(base_calc),
    application:stop(base_calc),
    {ok, Bin}=file:read_file(Pname),
    L=binary_to_list(Bin),
    F=re:run(L, "no params"),
    file:delete(Name),
    Data2="wdir "++?config(wdir, Config)++"\n"++"conns 2"++[10]++"port "++integer_to_list(?config(s_port, Config))++[10]++"maxmem 500000000"++[10],
    file:write_file(Name, Data2),
    ct:sleep(500),
    ?assert(F/=nomatch),
    Config.

go_ok(Config) ->
    Name=?config(pdir, Config)++"/conn_conf",
    file:delete(Name),
    Data2="wdir "++?config(wdir, Config)++"\n"++"conns 2"++[10]++"port "++integer_to_list(?config(s_port, Config)+3)++[10]++"maxmem 500000000"++[10],
    file:write_file(Name, Data2),
    application:start(base_calc),
    ct:sleep(500),
    {ok, Sock}=gen_tcp:connect(?config(haddr, Config), ?config(s_port, Config)+3, [list, {active, true}], 3),
    gen_tcp:send(Sock, "GET"),
    F=receive
        {tcp, Sock, Answer} -> 
            re:run(Answer, "Calculator.Main_page")
    end,
    ?assert(nomatch/=F),
    file:delete(Name),
    Data3="wdir "++?config(wdir, Config)++"\n"++"conns 2"++[10]++"port "++integer_to_list(?config(s_port, Config))++[10]++"maxmem 500000000"++[10],
    file:write_file(Name, Data3),
    application:stop(base_calc),
    Config.
%%%%%%%%%%%%%%%%% CORRECT data section

typical_task_one(Config) ->
    Task="R=2 A=(V VTWO VONE)\nNULL=0\nLIM=2\nCOND=NULL\nV=0\nTHREE=3\nBGN_A=3\n?BGN_A>NULL\n\tBGN_B=3\n\tBGN_A=BGN_A-1\n\t\t?BGN_B>NULL\n\t\t\tBGN_B=BGN_B-1\n\t\t\tV=V+1\n\t\t?\n\t\tBGN_C=0\n\t\t??\n\t\t\tBGN_C=BGN_C+1\n\t\t\tV=V+1\n\t\t??BGN_C<THREE\n?\nVTWO=-(120.0e-2+0)\nVONE=1.00+(-1+1)*1.0e2+(11.11-1.11+(1.0e2-100))\n",
    %% {"2023-2-21-21-51-13-3", {{192,168,5,3},57486}, "tst2.txt", [{"V",1800},{"VTWO",-120},{"VONE",1100}]}
    FF="{\"V\",1800},{\"VTWO\",-120},{\"VONE\",1100}",
    F=request(Config, Task, FF, 500, ?config(s_port, Config)),
    ?assert(nomatch/=F),
    Config.

get_getinfo(Config) ->
    {ok, Sock}=gen_tcp:connect(?config(haddr, Config), ?config(s_port, Config), [list, {active, true}], 3),
    gen_tcp:send(Sock, "GET"),
    F=receive
       {tcp, Sock, Answer} -> 
           re:run(Answer, "Calculator.Main_page")
       end,
    ?assert(nomatch/=F),
    gen_tcp:close(Sock),
    {ok, Sock2}=gen_tcp:connect(?config(haddr, Config), ?config(s_port, Config), [list, {active, true}], 3),
    gen_tcp:send(Sock2, "GET /info"),
    F2=receive
       {tcp, Sock2, Answer2} -> 
           re:run(Answer2, "<title>INFO</title>")
       end,
    ?assert(nomatch/=F2),
    gen_tcp:close(Sock2),
    Config.

typical_task_two(Config) ->
    Task="R=2 A=(F)\nF=0\n"++lists:flatten(lists:duplicate(500, "F=F+1\n")),
    %% {"2023-3-21-14-4-51-1",{{192,168,5,3},54566},"long.txt",[{"F",50000}]}
    FF="{\"F\",50000}",
    F=request(Config, Task, FF, 500, ?config(s_port, Config)),
    ?assert(nomatch/=F),
    Config.

typical_task_three(Config) ->
    Task="R=6 A=(ACC CUR MED)\nACC=4\nCUR=3\nSIGN=-1\nMED=100\nNULL=0\n?MED<>NULL\n\tMED=(4/CUR)*SIGN\n\tACC=ACC+MED\n\tSIGN=SIGN*-1\n\tCUR=CUR+2\n?\n\ncalculates Pi with precision 6 digits abt 3 minutes",
    %% {"2023-3-21-22-10-44-1", {{192,168,5,3},36836}, "nested.txt", [{"ACC",3141548},{"CUR",1000003000000},{"MED",0}]}
    FF="3141548}",
    F=request(Config, Task, FF, {minutes, 3}, ?config(s_port, Config)),
    ct:print("~nPi ~p~n", [F]),
    ct:sleep(4000),
    ?assert(nomatch/=F),
    Config.

typical_task_four(Config) ->
    Task="R=2 A=(OUT_PUT E)\nA=1\nB=1\nC=1\nOUT_PUT=((-A+B)+(C-A)*(A+((C-A)*(A/16+B))))+A-C+B-B+C-A\nE=23.23456-23.2345e-2\n??\nE=E-23\n??A>B\n",
    %% 
    FF="{\"OUT_PUT\",0},{\"E\",0}",
    F=request(Config, Task, FF, 500, ?config(s_port, Config)),
    ?assert(nomatch/=F),
    Config.

%%%%%%%%%% WRONG data section
%%% preproc
odd_brace(Config) ->
    Task="R=2 A=(M)\nM=(12+3\n",
    %%
    FF="wrong_brace_num",
    F=request(Config, Task, FF, 500, ?config(s_port, Config)+1),
    ?assert(nomatch/=F),
    Config.

odd_brace2(Config) ->
    Task="R=2 A=(M)\nM=((12+3)\n",
    %%
    FF="wrong_brace_num",
    F=request(Config, Task, FF, 500, ?config(s_port, Config)+1),
    ?assert(nomatch/=F),
    Config.

not_bound(Config) ->
    Task="R=2 A=(F)\nF=0+AA\n",
    %% 
    FF="var_not_bound",
    F=request(Config, Task, FF, 500, ?config(s_port, Config)+1),
    ?assert(nomatch/=F),
    Config.

no_eq_sign(Config) ->
    Task="R=2 A=(F)\nF0-25\n",
    %% 
    FF="{error,wrong_begin}",
    F=request(Config, Task, FF, 500, ?config(s_port, Config)+1),
    ?assert(nomatch/=F),
    Config.

uminus(Config) ->
    Task="R=2 A=(F)\nF=0\n"++lists:flatten(lists:duplicate(500, "F=F+1\n")),
    %% {"2023-3-21-14-4-51-1",{{192,168,5,3},54566},"long.txt",[{"F",50000}]}
    FF="{\"F\",50000}",
    F=request(Config, Task, FF, 500, ?config(s_port, Config)+1),
    ?assert(nomatch/=F),
    Config.

bad_lbr(Config) ->
    Task="R=2 A=(F)\nF=0\nP=--F+2\n",
    %% 
    FF="{error,wrong_uminus}",
    F=request(Config, Task, FF, 500, ?config(s_port, Config)+1),
    ?assert(nomatch/=F),
    Config.

bad_lbr2(Config) ->
    Task="R=2 A=(F)\nF=(+23-8)\n",
    %% {"2023-3-21-14-4-51-1",{{192,168,5,3},54566},"long.txt",[{"F",50000}]}
    FF="lbr_bad_number",
    F=request(Config, Task, FF, 500, ?config(s_port, Config)+1),
    ?assert(nomatch/=F),
    Config.

bad_oper(Config) ->
    Task="R=2 A=(F)\nF=8#56\n",
    %% 
    FF="oper_bad_number",
    F=request(Config, Task, FF, 500, ?config(s_port, Config)+1),
    ?assert(nomatch/=F),
    Config.

bad_rbr(Config) ->
    Task="R=2 A=(F)\nF=0-wrong\n",
    %% {"2023-3-21-14-4-51-1",{{192,168,5,3},54566},"long.txt",[{"F",50000}]}
    FF="{error,rbr_bad_number}",
    F=request(Config, Task, FF, 500, ?config(s_port, Config)+1),
    ?assert(nomatch/=F),
    Config.
%%% /preproc
%%% task_handler
no_range(Config) ->
    Task="A=(F)\nF=0\n",
    %% 
    FF="{error,no_range}",
    F=request(Config, Task, FF, 500, ?config(s_port, Config)+1),
    ?assert(nomatch/=F),
    Config.

no_aout(Config) ->
    Task="R=2 A\nF=0\n",
    %% 
    FF="{error,no_aout}",
    F=request(Config, Task, FF, 500, ?config(s_port, Config)+1),
    ?assert(nomatch/=F),
    Config.

no_range_aout(Config) ->
    Task="R= A\nF=0\n"++lists:flatten(lists:duplicate(500, "F=F+1\n")),
    %% 
    FF="{no_range,no_aout}",
    F=request(Config, Task, FF, 500, ?config(s_port, Config)+1),
    ?assert(nomatch/=F),
    Config.

odd_cond(Config) ->
    Task="R=2 A=(F)\nF=0\nD=2\n??D==F",
    %% 
    FF="baddowhile_bgn",
    F=request(Config, Task, FF, 500, ?config(s_port, Config)+1),
    ?assert(nomatch/=F),
    Config.

baddowhile_bgn(Config) ->
    Task="R=2 A=(F)\nF=0\n??",
    %% 
    FF="{error,cond_not_closed}",
    F=request(Config, Task, FF, 500, ?config(s_port, Config)+1),
    ?assert(nomatch/=F),
    Config.

var_in_cond(Config) ->
    Task="R=2 A=(F)\nF=0\n?L>F\nF=23+2\n?\n",
    %% 
    FF="wrong_var_in_cond",
    F=request(Config, Task, FF, 500, ?config(s_port, Config)+1),
    ?assert(nomatch/=F),
    Config.

in_cond(Config) ->
    Task="R=2 A=(F)\nF=0\nP=2\n?P#F\nP=P-1\n?\n",
    %% 
    FF="in_cond",
    F=request(Config, Task, FF, 500, ?config(s_port, Config)+1),
    ?assert(nomatch/=F),
    Config.

in_cond2(Config) ->
    Task="R=2 A=(F)\nF=0\nP=2\n?123\nP=P-1\n?\n",
    %% 
    FF="in_cond",
    F=request(Config, Task, FF, 500, ?config(s_port, Config)+1),
    ?assert(nomatch/=F),
    Config.

in_cond3(Config) ->
    Task="R=2 A=(F)\nF=0\nP=2\n??\nP=P-1\n??F!P\n",
    %% 
    FF="in_cond",
    F=request(Config, Task, FF, 500, ?config(s_port, Config)+1),
    ?assert(nomatch/=F),
    Config.

in_cond4(Config) ->
    Task="R=2 A=(F)\nF=0\nP=2\n??\nP=P-1\n??P>Q\n",
    %% 
    FF="in_cond",
    F=request(Config, Task, FF, 500, ?config(s_port, Config)+1),
    ?assert(nomatch/=F),
    Config.

zero(Config) ->
    Task="R=2 A=(F)\nF=23456/0+12-6\n",
    %% 
    FF="zero",
    F=request(Config, Task, FF, 500, ?config(s_port, Config)+1),
    ?assert(nomatch/=F),
    Config.

overchunk(Config) ->
    ct:pal("~nOVERCHUNKED~n"),
    {ok, Sock}=gen_tcp:connect(?config(haddr, Config), ?config(s_port, Config)+1, [list, {active, true}], 3),
    Port=?config(s_port, Config)+1,
    Data=lists:duplicate(180000, "Ii"),
    Str=?config(bgn_hp, Config)++?config(saddr, Config)++integer_to_list(Port)++?config(prepare_len_fle, Config)++integer_to_list(250000)++?config(repeat_hp, Config)++?config(saddr, Config)++integer_to_list(Port)++?config(hp_agn, Config)++?config(saddr, Config)++"2001"++?config(before_payload, Config)++?config(payload, Config)++Data++?config(finish, Config),
    gen_tcp:send(Sock, list_to_binary(Str)),
    F=receive
        {tcp_closed, Sock} ->
            closed;
        Data ->
            ct:pal("~n~overchunk~n p~n", [Data])
        after 3000 ->
            down
        end,
    ct:pal("~n~p~n", [F]),
    gen_tcp:close(Sock),
    ?assert(F==closed),
    Config.

timeouted(Config) ->
    {ok, Sock}=gen_tcp:connect(?config(haddr, Config), ?config(s_port, Config)+1, [list, {active, true}], 3),
    Port=?config(s_port, Config)+1,
    Data=lists:duplicate(100, "Ii"),
    Str=?config(bgn_hp, Config)++?config(saddr, Config)++integer_to_list(Port)++?config(prepare_len_fle, Config)++integer_to_list(250000)++?config(repeat_hp, Config)++?config(saddr, Config)++integer_to_list(Port)++?config(hp_agn, Config)++?config(saddr, Config)++"2001"++?config(before_payload, Config)++?config(payload, Config)++Data,
    gen_tcp:send(Sock, list_to_binary(Str)),
    ct:sleep(3500),
    F=receive
        {tcp_closed, Sock} ->
            ok
        end,
    ?assert(F==ok),
    Config.

%%%%%%%%%% ADMIN
%% [{f,"/media/download/bc/long_generated.txt","2023-4-14-20-38-16-5"}, {f,"/media/download/base_calc-1.0.0/test/generated.txt", "2023-4-14-20-38-16-4"}]
test_five(Config) ->%% f-ile l-ist d-elete k-illall m-emory
    Fle="R=2 A=(F)\nF=0\n"++lists:flatten(lists:duplicate(500, "F=F+1\n")),
    Rge=["10", "20", "30", "40"],
    Pi="R=6 A=(ACC CUR MED)\nACC=4\nCUR=3\nSIGN=-1\nMED=100\nNULL=0\n?MED<>NULL\n\tMED=(4/CUR)*SIGN\n\tACC=ACC+MED\n\tSIGN=SIGN*-1\n\tCUR=CUR+2\n?\n\ncalculates Pi with precision 6 digits abt 3 minutes",
    [file:write_file(?config(wdir, Config)++Nme, list_to_binary(Fle))||Nme<-Rge],
    file:write_file(?config(wdir, Config)++"50", list_to_binary(Pi)),
    Exe=["f "++?config(wdir, Config)++Nme++"\n"||Nme<-Rge]++"f "++?config(wdir, Config)++"50\n",
    file:write_file(?config(wdir, Config)++"doit.txt", list_to_binary(Exe)),
    {ok, Sock}=gen_tcp:connect(?config(haddr, Config), ?config(s_port, Config)+2, [list, {active, true}], 3),
    gen_tcp:send(Sock, "GET /admin"),
    F=receive
        {tcp, Sock, Answer} -> 
            {match, Rre}=re:run(Answer, "[0-9]+\-[0-9]+\-[0-9]+\-[0-9]+\-[0-9]+\-[0-9]+\-[0-9]+", [global]),
            ct:pal("~ntasks ~n~p~n", [Answer]),
            [lists:sublist(Answer, Start+1, Len)||[{Start, Len}]<-Rre]
    end,
    gen_tcp:close(Sock),
    ?assert(length(F)==5),
    %% gen_tcp:close(Sock),
    ct:sleep(500),
    file:write_file(?config(wdir, Config)++"doit.txt", list_to_binary("l")),
    {ok, Sock2}=gen_tcp:connect(?config(haddr, Config), ?config(s_port, Config)+2, [list, {active, true}], 3), 
    gen_tcp:send(Sock2, "GET /admin"),
    F2=receive
        {tcp, Sock2,Answer2} ->
            re:run(Answer2, "aout"),
            ct:pal("~naout ~p~n", [Answer2])
    end,
    gen_tcp:close(Sock2),
    ?assert(F2/=nomatch),
    {ok, Bin}=file:read_file(?config(wdir, Config)++"_aout.txt"),
    Lst=binary_to_list(Bin),
    {match, [{S, L}]}=re:run(Lst, "[0-9]+\-[0-9]+\-[0-9]+\-[0-9]+\-[0-9]+\-[0-9]+\-[0-9]+"),
    Tid=lists:sublist(Lst, S+1, L),
    Wrk=re:run(Lst, "working"),
    ?assert(Wrk/=nomatch),
    file:write_file(?config(wdir, Config)++"doit.txt", list_to_binary("d "++Tid)),
    {ok, Sock3}=gen_tcp:connect(?config(haddr, Config), ?config(s_port, Config)+2, [list, {active, true}], 3),  
    gen_tcp:send(Sock3, "GET /admin"),
    F3=receive
        {tcp, Sock3,Answer3} ->
            re:run(Answer3, "d,ok"),
            ct:pal("~naout ~p~n", [Answer3])
    end,
    gen_tcp:close(Sock3),
    ?assert(F3/=nomatch),
    ct:sleep(500),
    file:write_file(?config(wdir, Config)++"doit.txt", list_to_binary("l")),
    {ok, Sock4}=gen_tcp:connect(?config(haddr, Config), ?config(s_port, Config)+2, [list, {active, true}], 3),
    gen_tcp:send(Sock4, "GET /admin"),
    receive
        {tcp, Sock4,Answer4} ->
            re:run(Answer4, "aout"),
            ct:pal("~naout ~p~n", [Answer4])
    end,
    gen_tcp:close(Sock4),
    {ok, Bin2}=file:read_file(?config(wdir, Config)++"_aout.txt"),
    Lst2=binary_to_list(Bin2),
    F4=re:run(Lst2, "working"),
    ?assert(F4==nomatch),
    file:write_file(?config(wdir, Config)++"doit.txt", list_to_binary("f "++?config(wdir, Config)++"50\n")),
    {ok, Sock5}=gen_tcp:connect(?config(haddr, Config), ?config(s_port, Config)+2, [list, {active, true}], 3),
    gen_tcp:send(Sock5, "GET /admin"),
    receive
        {tcp, Sock5,Answer5} ->
            ct:pal("~naout ~p~n", [Answer5]),
            re:run(Answer5, "[0-9]+\-[0-9]+\-[0-9]+\-[0-9]+\-[0-9]+\-[0-9]+\-[0-9]+")
    end,
    gen_tcp:close(Sock5),
    ct:sleep(500),
    file:write_file(?config(wdir, Config)++"doit.txt", list_to_binary("k")),
    {ok, Sock6}=gen_tcp:connect(?config(haddr, Config), ?config(s_port, Config)+2, [list, {active, true}], 3),
    gen_tcp:send(Sock6, "GET /admin"),
    F6=receive
        {tcp, Sock6,Answer6} ->
            ct:pal("~naout ~p~n", [Answer6]),
            re:run(Answer6, "killed")
    end,
    gen_tcp:close(Sock6),
    ?assert(F6/=nomatch),
    file:write_file(?config(wdir, Config)++"doit.txt", list_to_binary("m 20000")),
    {ok, Sock7}=gen_tcp:connect(?config(haddr, Config), ?config(s_port, Config)+2, [list, {active, true}], 3),
    gen_tcp:send(Sock7, "GET /admin"),
    F7=receive
        {tcp, Sock7,Answer7} ->
            ct:pal("~naout ~p~n", [Answer7]),
            re:run(Answer7, "mem")
    end,
    gen_tcp:close(Sock7),
    ?assert(F7/=nomatch),
    ct:sleep(1200),
    {ok, Sock8}=gen_tcp:connect(?config(haddr, Config), ?config(s_port, Config)+2, [list, {active, true}], 3),
    Port=?config(s_port, Config)+2,
    gen_tcp:send(Sock8, list_to_binary(?config(bgn_hp, Config)++?config(saddr, Config)++integer_to_list(Port)++?config(prepare_len_fle, Config)++integer_to_list(size(list_to_binary(Fle)))++?config(repeat_hp, Config)++?config(saddr, Config)++integer_to_list(Port)++?config(hp_agn, Config)++?config(saddr, Config)++"2001"++?config(before_payload, Config)++?config(payload, Config)++Fle++?config(finish, Config))),
    F8=receive
        {tcp, Sock8, Answer8} ->
            ct:pal("~naout ~p", [Answer8]),
            re:run(Answer8, "OVERHEAP")
        end,
    ?assert(F8/=nomatch),
    file:write_file(?config(wdir, Config)++"doit.txt", list_to_binary("s")),
    {ok, Sock9}=gen_tcp:connect(?config(haddr, Config), ?config(s_port, Config)+2, [list, {active, true}], 3),
    gen_tcp:send(Sock9, "GET /admin"),
    F9=receive
        {tcp_closed, _Sock9} ->
            closed;
        Answer9 ->
            ct:pal("~naout ~p~n", [Answer9])
        after 3000 ->
            closed
    end,
    ?assert(F9==closed),
    Config.

%% ct:run_test([{spec, "/media/download/base_calc-1.0.0/test/ct_dir/uctest.spec"}]).
%% {incl_app, base_calc,  details}.
%% {incl_mods, [base_calc, bfun, calculate, conn_handler, conn_starter, conn_sup, g_repo, left_one_sup, preproc, right_all_sup, task_handler, th_sup, top_sup, var]}.

request(Config, Data, Pattern, Time_to_wait, Port) ->
    Str=?config(bgn_hp, Config)++?config(saddr, Config)++integer_to_list(Port)++?config(prepare_len_fle, Config)++integer_to_list(size(list_to_binary(Data)))++?config(repeat_hp, Config)++?config(saddr, Config)++integer_to_list(Port)++?config(hp_agn, Config)++?config(saddr, Config)++"2001"++?config(before_payload, Config)++?config(payload, Config)++Data++?config(finish, Config),
    ct:print("~n~p ~p~n", [?config(saddr, Config), Port]),
    {ok, Sock}=gen_tcp:connect(?config(haddr, Config), Port, [list, {active, true}], 3),
    gen_tcp:send(Sock, list_to_binary(Str)),
    %% 2023-2-2-22-8-20-1
    Res=receive
        {tcp, Sock, Answer} ->
            {match, [{Start, Len}]}=re:run(Answer, "[0-9]+\-[0-9]+\-[0-9]+\-[0-9]+\-[0-9]+\-[0-9]+\-[0-9]+"),
            lists:sublist(Answer, Start+1, Len)
        end,
    gen_tcp:close(Sock),
    ct:sleep(Time_to_wait),
    Tid_str="Task ID="++Res++"\n\r",
    Str_f=?config(bgn_hp, Config)++?config(saddr, Config)++integer_to_list(?config(s_port, Config))++?config(prepare_len_form, Config)++integer_to_list(size(list_to_binary(Tid_str))+2)++?config(repeat_hp, Config)++?config(saddr, Config)++"2001"++?config(hp_agn, Config)++?config(saddr, Config)++integer_to_list(Port)++?config(before_payload, Config)++?config(payload, Config)++Tid_str,
    {ok, Sock2}=gen_tcp:connect(?config(haddr, Config), Port, [list, {active, true}], 3),
    gen_tcp:send(Sock2, Str_f),
    F=receive
        {tcp, Sock2, Answer2} ->
            ct:pal("~n~p~n", [Answer2]),
            re:run(Answer2, Pattern)
        end,
    gen_tcp:close(Sock2),
    F.