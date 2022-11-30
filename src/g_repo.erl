%%%-------------------------------------------------------------------
%%% @author mt
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% get_num(pid) returns next number for task
%%% answer(pid, Tid) returns task answer
%%% setmaxmem(newvalue) sets new memory limits
%%% aout(result) gets task result
%%% inet_box_status tcp connection 
%%% @end
%%% Created : 13. окт. 2022 21:16
%%%-------------------------------------------------------------------
-module(g_repo).
-author("mt").

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-export([get_num/0, answer/1, setmaxmem/1, aout/1, inet_box_status/0, inet_box_status/1]).

-define(SERVER, ?MODULE).

-record(gr_state, {num, repo, hour, inet_box, memo, tref, mem_status, dir}).

%% num number of tasks, loop per hour
%% repo answers storage: Tid(date, time, num), IP, fname, result 
%% inet_box tcp conn status

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
start_link(Wdir, Maxmem) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Wdir, Maxmem], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #gr_state{}} | {ok, State :: #gr_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Wdir, Maxmem]) ->
  process_flag(trap_exit, true),
  {ok, Tref}=timer:send_interval(1000, testit),
  {ok, #gr_state{repo=#{}, num=1, hour=255, tref=Tref, memo=Maxmem, mem_status = false, dir=Wdir}}. %% memory in bytes

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #gr_state{}) ->
  {reply, Reply :: term(), NewState :: #gr_state{}} |
  {reply, Reply :: term(), NewState :: #gr_state{}, timeout() | hibernate} |
  {noreply, NewState :: #gr_state{}} |
  {noreply, NewState :: #gr_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #gr_state{}} |
  {stop, Reason :: term(), NewState :: #gr_state{}}).
handle_call(get_num, _From, #gr_state{hour=H, num=N}=State) ->
  {Hour, _, _}=erlang:time(),
  case H==Hour of
    true ->
      {reply, N+1, State#gr_state{num=N+1}};
    false -> 
      {reply, 1, State#gr_state{num=1, hour=Hour}}
  end;

handle_call(inet_box, _From, #gr_state{inet_box = InetBox}=State) ->
  {reply, InetBox, State};

handle_call({answer, Tid}, _From, #gr_state{repo=Repo}=State) ->
  Bool=maps:is_key(Tid, Repo),
  case Bool of
    true ->
      A=maps:get(Tid, Repo),
      {reply, A, State#gr_state{repo=maps:remove(Tid, Repo)}};
    false ->
      A=supervisor:get_childspec(th_sup, Tid),
      case A of
        {error, _} ->
          {reply, notexists, State};
        _Other ->
          {reply, working, State}
      end
  end.

%% @private
%% @doc Handling cast messages
 
handle_cast({maxmem, Mem}, State) ->
  {noreply, State#gr_state{memo=Mem}};

handle_cast({inet_box, Data}, State) ->
  {noreply, State#gr_state{inet_box = Data}};

handle_cast({aout, Res}, #gr_state{repo=Repo}=State) ->
  {Tid, _, _,_}=Res,
  supervisor:terminate_child(th_sup, Tid),
  supervisor:delete_child(th_sup, Tid),
  {noreply, State#gr_state{repo=maps:put(Tid, Res, Repo)}};

handle_cast(stop, State) ->
  {stop, normal, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #gr_state{}) ->
  {noreply, NewState :: #gr_state{}} |
  {noreply, NewState :: #gr_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #gr_state{}}).
%% time to memtest
handle_info(testit, #gr_state{memo=M, mem_status=St, dir=Dir, repo=Repo}=State) -> %% memory timer
  Fun=fun(T, R) ->
    Bool=maps:is_key(T, R),
  case Bool of
    true ->
      A=maps:get(T, R),
      {reply, A, State#gr_state{repo=maps:remove(T, R)}};
    false ->
      noresult
    end
  end,
  Diff=erlang:memory(total)-M,
  BoolLess=(Diff<0) andalso St==true, %% memory ok -> send ok
  BoolMore=(Diff>0) andalso St==false, %% memory over -> send over
  if
    BoolLess ->
      L=supervisor:which_children(conn_sup),
      [gen_statem:cast(Pid, overheapoff)||{_, Pid, _, _}<- L],
      {noreply, State#gr_state{mem_status =false}};
    BoolMore ->
      L=supervisor:which_children(conn_sup),
      lists:foreach(fun({_, Pid, _, _}) -> gen_statem:cast(Pid, overheap), garbage_collect(Pid) end, L),
      {noreply, State#gr_state{mem_status = true}};
    St==true andalso Diff>0 ->
      L=supervisor:which_children(th_sup),
      Aout=[{Id, process_info(Pid, memory), Fun(Id, Repo)}||{Id, Pid, _, _}<- L],
      To_file=iolist_to_binary(io_lib:format("~p~n", [Aout])),
      file:write_file(Dir++"_overheap.txt", To_file),
      L=supervisor:which_children(th_sup),
      [supervisor:terminate_child(th_sup, Id)||{Id, _, _, _}<- L],
      [supervisor:delete_child(th_sup, Id)||{Id, _, _, _}<- L],
      {noreply, State#gr_state{mem_status=false}};
    true -> {noreply, State}
  end.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #gr_state{}) -> term()).
terminate(_Reason, State) ->
  timer:cancel(State#gr_state.tref),
  erlang:unregister(g_repo),
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #gr_state{},
    Extra :: term()) ->
  {ok, NewState :: #gr_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #gr_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_num() ->
  gen_server:call(g_repo, get_num).

answer(Tid) ->
  gen_server:call(g_repo, {answer, Tid}).

setmaxmem(Mem) ->
  gen_server:cast(g_repo, Mem).

aout(Answer) ->
  gen_server:cast(g_repo, {aout, Answer}).

inet_box_status() ->
  gen_server:call(g_repo, inet_box).

inet_box_status(Data) ->
  gen_server:cast(g_repo, {inet_box, Data}).
