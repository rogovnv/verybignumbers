%% @author midtower@yandex.ru
%% @doc storage for variables, strings and precision range, it is global for each task


-module(var).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/1, stop/1, setvar/3, getvar/2, get_task_str/2, set_task_str/2, getrange/1, getmaster/1]).
-export([initv/2, is_existv/2]).

start({Prange, MasterPid}) ->
  gen_server:start(?MODULE, [{Prange, MasterPid}], []).

stop(Pid) ->
  gen_server:cast(Pid, stop).

init({Prange, MasterPid}) ->
  ListStr=ets:new(taskstr, []),
  M=#{},
  {ok, {M, ListStr, 0, Prange, MasterPid}}.
%% M variables, L numbers,N 0 numbers count, Pr precision range, MP Pid of outer process to return output

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

getmaster(Pid) ->
  gen_server:call(Pid, master).

initv(Pid, Vname) ->
  gen_server:cast(Pid, {initv, Vname}).

is_existv(Pid, Vname) ->
  gen_server:call(Pid, {is_existv, Vname}).

handle_call({is_existv, Vname}, _From, {M, L, N, Pr, MP}) ->
  P=map_get(Vname, M),
  Out = case P of
    {badkey, _} -> badkey;
    _ -> P
  end,
  {reply, Out, {M, L, N, Pr, MP}};

handle_call(master, _From, {M, L, N, Pr, MP}) ->
  {reply, MP, {M, L, N, Pr, MP}};

handle_call(range, _From, {M, L, N, Pr, MP}) ->
  {reply, Pr, {M, L, N, Pr, MP}};

handle_call({gets, Num}, _From, {M, L, N, Pr, MP}) ->
  [F]=ets:lookup(L, Num),
  {reply, F, {M, L, N, Pr, MP}};

handle_call({sets, List}, _From, {M, L, N, Pr, MP}) ->
  N2=N+1,
  L2=ets:insert(L, {N2, List}),
  {reply, N2, {M, L2, N2, Pr, MP}};

handle_call({get, V}, _From, {M, L, N, Pr, MP}) ->
  #{V := Out}=M,
  {reply, Out, {M, L, N, Pr, MP}}.

handle_cast({initv, Vname}, {M, L, N, Pr, MP}) ->
  P=map_get(Vname, M),
  case P of
    {badkey, _} -> {noreply, {M#{Vname => 0}, L, N, Pr, MP}};
    _ -> {noreply, {M, L, N, Pr, MP}}
  end;

handle_cast({set, V, N}, {M, L, N, Pr, MP}) ->
  {noreply, {M#{V := N}, L, N, Pr, MP}};

handle_cast(stop, {M, L, N, Pr, MP}) ->
  {stop, normal, {M, L, N, Pr, MP}}.

terminate(_, {_, L, _}) ->
	ets:delete(L),
  ok.

handle_info(_, State) ->
  {noreply, State}.

code_change(_, {M, L, N, Pr, MP}, _) ->
  {ok, {M, L, N, Pr, MP}}.



