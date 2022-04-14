%% @author midtower@yandex.ru
%% @doc storage for variables and strings, it is global for each task


-module(var).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0, stop/1, setvar/3, getvar/2, get_task_str/2, set_task_str/2]).

start() ->
  gen_server:start(?MODULE, [], []).

stop(Pid) ->
  gen_server:cast(Pid, stop).

init(_) ->
  ListStr=ets:new(taskstr, []),
  M=mapgen(),
  {ok, {M, ListStr, 0}}.

mapgen() ->
  M=#{},
  gen($A, M).

gen(Alf, M) ->
  M1=M#{Alf => 0},
  gen(Alf+1, M1);

gen($Y, M) ->
  M#{$Z => 0}.

get_task_str(Pid, N) ->
  gen_server:call(Pid, {gets, N}).

set_task_str(Pid, List) ->
  gen_server:call(Pid, {sets, List}).

setvar(Pid, V, N) ->
  gen_server:cast(Pid, {set, V, N}).

getvar(Pid, V) ->
  gen_server:call(Pid, {get, V}).

handle_call({gets, Num}, _From, {M, L, N}) ->
  [F]=ets:lookup(L, Num),
  {reply, F, {M, L, N}};

handle_call({sets, List}, _From, {M, L, N}) ->
  N2=N+1,
  L2=ets:insert(L, {N2, List}),
  {reply, N2, {M, L2, N2}};

handle_call({get, V}, _From, {M, L, N}) ->
  #{V := Out}=M,
  {reply, Out, {M, L, N}}.

handle_cast({set, V, N}, {M, L, N}) ->
  {noreply, {M#{V := N}, L, N}};

handle_cast(stop, {M, L, N}) ->
  {stop, normal, {M, L, N}}.

terminate(_, {_, L, _}) ->
	ets:delete(L),
  ok.

handle_info(_, State) ->
  {noreply, State}.

code_change(_, {M, L, N}, _) ->
  {ok, {M, L, N}}.



