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
    [startapp, cont_app].

init_per_suite(Config) ->
	application:start(sasl),
    %% ct:run_testspec("uctest.spec"),
    Config.

init_per_testcase(_, Config) ->
    
    Config.

end_per_testcase(_, Config) ->

    Config.

end_per_suite(Config) ->
    application:stop(sasl),
    Config.

startapp(Config) ->
	Priv=?config(priv_dir, Config),
    Dta=?config(data_dir, Config),
    Data="wdir "++Dta++[10]++"conns 2"++[10]++"port 2000"++[10]++"maxmem 500000000"++[10],
    file:write_file(Priv++"/conn_conf", Data),
	ok=application:start(base_calc),
	ct:sleep(1000),
	Config.

cont_app(Config) ->
    %% [ct:print("~nKey: ~p Value: ~p", [K, V])||{K, V}<-Config],
    ct:sleep(2000),
    %% {ok, Bin}=file:read_file(Priv++"/conn_conf"),
    %% ct:print("~n~w~n", [binary_to_list(Bin)]),
    {ok, Sock}=gen_tcp:connect({192, 168, 5, 3} , 2000, [list, {active, once}], 3),
    gen_tcp:send(Sock, "GET"),
    ok=receive
        Answer -> 
            ct:print("~nData ~n~p~n", [Answer]),
            ok
    end,
    ct:sleep(5000),
	gen_tcp:close(Sock),
    ok=application:stop(base_calc),
    ct:sleep(5000),
	Config.

%% ct:run_test([{spec, "/media/download/base_calc-1.0.0/test/ct_dir/uctest.spec"}]).
%% {incl_app, base_calc,  details}.
%% {incl_mods, [base_calc, bfun, calculate, conn_handler, conn_starter, conn_sup, g_repo, left_one_sup, preproc, right_all_sup, task_handler, th_sup, top_sup, var]}.
