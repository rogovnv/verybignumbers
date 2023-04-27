%% @author midtower@yandex.ru
%% @doc storage for variables, strings and precision range, it is global for each task


-module(var).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/1, stop/1, setvar/3, getvar/2, get_task_str/2, set_task_str/2, getrange/1, add_pid/2]).
-export([initv/2, is_existv/2]).

start(Prange) ->
  gen_server:start(?MODULE, Prange, []).

stop(Pid) ->
  gen_server:cast(Pid, stop).

init(Prange) ->
  process_flag(trap_exit, true),
  ListStr=#{},
  M=#{},
  {ok, {M, ListStr, 0, Prange, []}}.
%% M variables, L numbers,N 0 strings count, Pr precision range

get_task_str(Pid, N) ->
  gen_server:call(Pid, {gets, N}).

set_task_str(Pid, List) ->
  gen_server:call(Pid, {sets, List}).

setvar(Pid, V, N) ->
  gen_server:cast(Pid, {set, V, N}).

getvar(Pid, V) ->
  gen_server:call(Pid, {get, V}).

getrange(Pid) ->
  gen_server:call(Pid, range).

initv(Pid, Vname) ->
  gen_server:cast(Pid, {initv, Vname}).

is_existv(Pid, Vname) ->
  gen_server:call(Pid, {is_existv, Vname}).

add_pid(VPid, Pid) ->
	gen_server:cast(VPid, {add_pid, Pid}).

handle_call({is_existv, Vname}, _From, {M, L, N, Pr, Lpid}) ->
  P=maps:is_key(Vname, M),
  Out = case P of
    false -> badkey;
    true -> true
  end,
  {reply, Out, {M, L, N, Pr, Lpid}};

handle_call(range, _From, {M, L, N, Pr, Lpid}) ->
  {reply, Pr, {M, L, N, Pr, Lpid}};

handle_call({gets, Num}, _From, {M, L, N, Pr, Lpid}) ->
  F=maps:get(Num, L),
  {reply, F, {M, L, N, Pr, Lpid}};

handle_call({sets, List}, _From, {M, L, N, Pr, Lpid}) ->
  N2=N+1,
  L2=maps:put(N2, List, L),
  {reply, N2, {M, L2, N2, Pr, Lpid}};

handle_call({get, V}, _From, {M, L, N, Pr, Lpid}) ->
  Out=maps:get(V, M),
  {reply, Out, {M, L, N, Pr, Lpid}}.

handle_cast({set, V, Num}, {M, L, N, Pr, Lpid}) ->
  M2=maps:put(V, Num, maps:remove(V, M)),
  {noreply, {M2, L, N, Pr, Lpid}};

handle_cast({initv, Vname}, {M, L, N, Pr, Lpid}) ->
  M2=maps:put(Vname, 0, M),
   {noreply, {M2, L, N, Pr, Lpid}};

handle_cast(stop, Data) ->
  {stop, normal, Data};

handle_cast({add_pid, Pid}, {M, L, N, Pr, Lpid}) ->
	{noreply, {M, L, N, Pr, lists:flatten([Pid| Lpid])}}.

handle_info(_Any, Data) ->
  {noreply, Data}.

terminate(_Reason, {_M, _L, _N, _Pr, Lpid}) ->
  [exit(Pid, kill)||Pid<- Lpid],
  ok.

code_change(_, Data, _) ->
  {ok, Data}.



