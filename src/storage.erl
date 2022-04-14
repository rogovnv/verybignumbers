%% @author midtower@yandex.ru
%% @doc storage for numbers, it is local for each string


-module(storage).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0, getnum/2, add/2, stop/1]).

start() ->
  gen_server:start(?MODULE, [], []).

getnum(Pid, N) ->
  gen_server:call(Pid, {get, N}).%%control in external data list length

add(Pid, N) ->
  gen_server:cast(Pid, {add, N}).

stop(Pid) ->
  gen_server:cast(Pid, stop).

init(_) ->%% it returns list of 26 not initialized variables and empty list for numbers
  process_flag(trap_exit, true),
  ListVar=ets:new(listvar,[]),
  {ok, {ListVar, 0}}.

handle_call({get, N}, _From, {ListVar, Len}) ->
  [F]=ets:lookup(ListVar, N),
  {reply, F, {ListVar, Len}};

handle_call({add, Num}, _From, {ListVar, Len}) ->
  Len2=Len+1,
  ListVar2=ets:insert(ListVar, {Len2, Num}),
  {reply, Len2, {Len2, ListVar2}};

handle_call(Other, _From, {ListVar, L}) ->
  {reply, {badarg, Other}, {ListVar, L}}.

handle_cast(stop, State) ->
  {stop, normal, State}.

handle_info(_, State) ->
  {noreply, State}.

terminate(_, {ListVar, _}) ->
  ets:delete(ListVar),
  ok.

code_change(_, State, _) ->
  {ok, State}.



