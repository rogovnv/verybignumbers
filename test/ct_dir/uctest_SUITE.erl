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

groups() ->
    [
        {correct_data, [], [
            get_getinfo,
            typical_task_one, %% complex nested code example
            typical_task_two, %% generated long file with chunked boundary sign
            typical_task_three, %% generated long file(MTU 1500)
            typical_task_four %% pi count with a 6 digits precision
            ]},
        {wrong_data, [], [
            wrongd,
            l_br, %% odd left brace
            r_br, %% odd right brace
            wrong_op, %%
            not_bound, %% trying to use variable without value
            wrong_var,%% variable not in correct place
            wrong_num, %% number not in correct place
            no_out, %% variable(s) for output not set
            no_range, %% range not set
            no_eq, %% no eq symbol
            zero, %% diviseng by zero
            cond_closer, %% ? cond not closed
            docond_closer, %% ?? cond not closed
            conn_timeout, %% file transferring timeout
            flood, %%random data
            wrong_form_data, %% wrong form data
            num_in_cond, %% number in condition
            sign_in_cond, %% wrong/lost signs in condition
            do_,
            while_,
            do_close,
            while_close
            ]},
        {admin_file, [], [
            test_five,
            test_stop
            ]},
        {on_start, [], [
            %% fullhouse, %% correct starting
            go_nopa, %% no conf file
            go_nopb %% wrong conf file
            ]}
        ].

init_per_suite(Config) ->
	application:start(sasl),
    {ok, Name}=inet:gethostname(),
    {ok,{hostent,Name,_,inet,4,[Addr]}}=inet:gethostbyname(Name, inet),
    Haddr={haddr, Addr},
    {A, B, C, D}=Addr,
    Saddr={saddr, integer_to_list(A)++"."++integer_to_list(B)++"."++integer_to_list(C)++"."++integer_to_list(D)++":"},
    Bgn={bgn_hp, "POST / HTTP/1.1\r\nHost: "},
    Before_len_fle={prepare_len_fle, "\r\nUser-Agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:109.0) Gecko/20100101 Firefox/109.0\r\nAccept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8\r\nAccept-Language: ru-RU,ru;q=0.8,en-US;q=0.5,en;q=0.3\r\nAccept-Encoding: gzip, deflate\r\nContent-Type: multipart/form-data; boundary=---------------------------270019682022871984891320620731\r\nContent-Length:"},
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
    ok=application:start(base_calc),
    ct:sleep(500),
    [S_port, Priv_dir, Wdir, Haddr, Saddr, Bgn, Before_len_fle, Before_len_form, Rpt_HP, HP_agn, Before_payload, Payload, Finish|Config].

init_per_group(on_start, Config) ->
    
    Config;
init_per_group(correct_data, Config) ->
    
    Config;
init_per_group(wrong_data, Config) ->
    
    Config;
init_per_group(admin_file, Config) ->
    
    Config.

end_per_group(on_start, Config) ->
    
    Config;
end_per_group(correct_data, Config) ->

    Config;
end_per_group(wrong_data, Config) ->
    
    Config;
end_per_group(admin_file, Config) ->
    
    Config.

end_per_suite(Config) ->
    application:stop(sasl),
    application:stop(base_calc),
    Config.

%%%%%%%%%%%%%%%%% ON start section

go_nopa(Config) ->
    Name=?config(pdir, Config)++"/conn_conf",
    filelib:is_file(Name) andalso file:delete(Name),
    F=application:start(base_calc),
    ok/=application:stop(base_calc),
    ?assert(F/=ok),
    ct:sleep(500),
    Config.

go_nopb(Config) ->
    Name=?config(pdir, Config)++"/conn_conf",
    filelib:is_file(Name) andalso file:delete(Name),
    Data="wdir "++"conns 2"++[10]++"port 2100"++[10]++"maxmem 500000000"++[10],
    file:write_file(Name, Data),
    F=application:start(base_calc),
    ok/=application:stop(base_calc),
    Name=?config(pdir, Config)++"/conn_conf",
    file:delete(Name),
    Data2="wdir "++?config(wdir, Config)++"conns 2"++[10]++"port 2100"++[10]++"maxmem 500000000"++[10],
    file:write_file(Name, Data2),
    ?assert(F/=ok),
    ct:sleep(500),
    Config.

%%%%%%%%%%%%%%%%% CORRECT data section

typical_task_one(Config) ->
    Task="R=2 A=(V VTWO VONE)\nNULL=0\nLIM=2\nCOND=NULL\nV=0\nTHREE=3\nBGN_A=3\n?BGN_A>NULL\n\tBGN_B=3\n\tBGN_A=BGN_A-1\n\t\t?BGN_B>NULL\n\t\t\tBGN_B=BGN_B-1\n\t\t\tV=V+1\n\t\t?\n\t\tBGN_C=0\n\t\t??\n\t\t\tBGN_C=BGN_C+1\n\t\t\tV=V+1\n\t\t??BGN_C<THREE\n?\nVTWO=-(120.0e-2+0)\nVONE=1.00+(-1+1)*1.0e2+(11.11-1.11+(1.0e2-100))\n",
    Str=?config(bgn_hp, Config)++?config(saddr, Config)++integer_to_list(?config(s_port, Config))++?config(prepare_len_fle, Config)++integer_to_list(size(list_to_binary(Task)))++?config(repeat_hp, Config)++?config(saddr, Config)++"2001"++?config(hp_agn, Config)++?config(saddr, Config)++"2001"++?config(before_payload, Config)++?config(payload, Config)++Task++?config(finish, Config),
    {ok, Sock}=gen_tcp:connect(?config(haddr, Config), ?config(s_port, Config), [list, {active, true}], 3),
    gen_tcp:send(Sock, list_to_binary(Str)),
    %% 2023-2-2-22-8-20-1
    Res=receive
        {tcp, Sock, Answer} ->
            {match, [{Start, Len}]}=re:run(Answer, "[0-9]+\-[0-9]+\-[0-9]+\-[0-9]+\-[0-9]+\-[0-9]+\-[0-9]"),
            lists:sublist(Answer, Start+1, Len)
        end,
    gen_tcp:close(Sock),
    ct:sleep(500),
    Tid_str="Task ID="++Res++"\n\r",
    Str_f=?config(bgn_hp, Config)++?config(saddr, Config)++integer_to_list(?config(s_port, Config))++?config(prepare_len_form, Config)++integer_to_list(size(list_to_binary(Tid_str))+2)++?config(repeat_hp, Config)++?config(saddr, Config)++"2001"++?config(hp_agn, Config)++?config(saddr, Config)++"2001"++?config(before_payload, Config)++?config(payload, Config)++Tid_str,
    {ok, Sock2}=gen_tcp:connect(?config(haddr, Config), ?config(s_port, Config), [list, {active, true}], 3),
    gen_tcp:send(Sock2, Str_f),
    %% {"2023-2-21-21-51-13-3", {{192,168,5,3},57486}, "tst2.txt", [{"V",1800},{"VTWO",-120},{"VONE",1100}]}
    FF="{\"V\",1800},{\"VTWO\",-120},{\"VONE\",1100}",
    F=receive
        {tcp, Sock2, Answer2} ->
            string:find(Answer2, FF)
        end,
    ?assert(nomatch/=F),
    gen_tcp:close(Sock2),
    Config.

get_getinfo(Config) ->
    {ok, Sock}=gen_tcp:connect(?config(haddr, Config), ?config(s_port, Config), [list, {active, true}], 3),
    gen_tcp:send(Sock, "GET"),
    F=receive
       {tcp, Sock, Answer} -> 
           string:find(Answer, "Calculator.Main_page")
       end,
    ?assert(nomatch/=F),
    gen_tcp:close(Sock),
    {ok, Sock2}=gen_tcp:connect(?config(haddr, Config), ?config(s_port, Config), [list, {active, true}], 3),
    gen_tcp:send(Sock2, "GET /info"),
    F2=receive
       {tcp, Sock2, Answer2} -> 
           string:find(Answer2, "<title>INFO</title>")
       end,
    ?assert(nomatch/=F2),
    gen_tcp:close(Sock2),
    Config.

%%%%%%%%%% WRONG data section

wrongd(Config) ->
    Config.

%%%%%%%%%% ADMIN

test_five(Config) ->
    Config.

%% ct:run_test([{spec, "/media/download/base_calc-1.0.0/test/ct_dir/uctest.spec"}]).
%% {incl_app, base_calc,  details}.
%% {incl_mods, [base_calc, bfun, calculate, conn_handler, conn_starter, conn_sup, g_repo, left_one_sup, preproc, right_all_sup, task_handler, th_sup, top_sup, var]}.
