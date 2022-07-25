-module(pres).
-export([tryme/0, tryth/0]).

tryth() ->
	Me=self(),
	Fpath="/media/download/test/task1.txt",
	{ok, VP}=gen_server:start(var, 3, []),
	{ok, Pid}=gen_statem:start(task_handler, {Me, VP, Fpath, 1}, []),
	var:setmp(VP, Pid),
	Aout=receive
		Data -> Data
	end,
	io:format("~n~p~n", [Aout]),
	gen_statem:stop(Pid),
	gen_server:stop(VP).

tryme() ->
	L2="GG_RR=100.1",
	L3="FF=-(100+-100)*-100+(GG_RR+-RRF)",
	L1="RRF=-(1.1+-1.1)*1.1+1.1/(1.1+1.1)-(1.1-1.1)+(123-123)",% 0.5 eq 500
	Range=3,
	Me=self(),
	{_, VP}=gen_server:start(var, {Range, Me}, []),
	Str1=var:set_task_str(VP, L1),
	Str2=var:set_task_str(VP, L2),
	Str3=var:set_task_str(VP, L3),
	{_, PP1}=gen_statem:start(preproc, {VP, Str1}, []),
	{_,{_,E1}}=receive
							 Ddata1-> Ddata1
						 end,
	{_, PP2}=gen_statem:start(preproc, {VP, Str2}, []),
	{_,{_,E2}}=receive
							 Ddata2-> Ddata2
						 end,
	{_, PP3}=gen_statem:start(preproc, {VP, Str3}, []),
	{_,{_,E3}}=receive
								 Ddata3-> Ddata3
							 end,
	io:format("~nStr1-2-3~n ~p~n~p~n~p~n",[E1, E2, E3]),
	%%Ls=lists:sort(fun(X, Y) -> X<Y end, [E1, E2, E3]),
	{_,C1}=gen_statem:start(calculate, {VP, E1}, []),
	{_,{_,F1}}=receive
							 Ddata4 -> Ddata4
						 end,
	io:format("~nF1 ~p", [F1]),
	{_,C2}=gen_statem:start(calculate, {VP, E2}, []),
	{_,{_,F2}}=receive
							 Ddata5 -> Ddata5
						 end,
	io:format("~nF2 ~p", [F2]),
	{_,C3}=gen_statem:start(calculate, {VP, E3}, []),
	{_,{_,F3}}=receive
							 Ddata6 -> Ddata6
						 end,
	io:format("~nF3 ~p", [F3]),
	io:format("~nFF= ~p~nMedievals ~p ~p ~p~n", [var:getvar(VP, "FF"), F1, F2, F3]),
	gen_statem:cast(C1, get),
	G1=receive
			 Data1 -> Data1
		 end,
	gen_statem:cast(C2, get),
	G2=receive
			 Data2 -> Data2
		 end,
	gen_statem:cast(C3, get),
	G3=receive
			 Data3 -> Data3
		 end,
	io:format("~nResult  ~p ~n~p ~n~p ~n~p~n", [var:getvar(VP, "FF"), G1, G2, G3]),
	Slst=[PP1, PP2, PP3, C1, C2, C3],
	[gen_statem:stop(X)|| X <- Slst],
	gen_server:stop(VP).
	