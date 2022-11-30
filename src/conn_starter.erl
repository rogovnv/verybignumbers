%%%-------------------------------------------------------------------
%%% @author mt
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. нояб. 2022 16:36
%%%-------------------------------------------------------------------
-module(conn_starter).
-author("mt").

-behaviour(gen_server).

%% API
-export([start_link/4, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-record(cs_state, {lsock, mon, port, n_of, struct, dir, fle}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
start_link(Wdir, Port, Conns, Fle) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Wdir, Port, Conns, Fle], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #cs_state{}} | {ok, State :: #cs_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Wdir, Port, Conns, Fle]) ->
	process_flag(trap_exit, true),
  gen_server:cast(self(), afterinit),
  {ok, #cs_state{port=Port, n_of = Conns, dir=Wdir, mon=false, lsock=false, fle=Fle}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #cs_state{}) ->
  {reply, Reply :: term(), NewState :: #cs_state{}} |
  {reply, Reply :: term(), NewState :: #cs_state{}, timeout() | hibernate} |
  {noreply, NewState :: #cs_state{}} |
  {noreply, NewState :: #cs_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #cs_state{}} |
  {stop, Reason :: term(), NewState :: #cs_state{}}).
handle_call(_Request, _From, State = #cs_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #cs_state{}) ->
  {noreply, NewState :: #cs_state{}} |
  {noreply, NewState :: #cs_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #cs_state{}}).
handle_cast(afterinit, State) ->
  {ok, Lsock}=gen_tcp:listen(State#cs_state.port, [list, {active, once}]),
  g_repo:inet_box_status(hold_it),
  M=inet:monitor(Lsock),
  Sup_struct=#{
    start => {conn_handler, start_link, [Lsock, State#cs_state.dir, State#cs_state.fle]},
    restart => permanent,
    type => worker
  },
  F2=fun (Num) -> proceed(Sup_struct, Num) end,
  lists:foreach(F2, lists:seq(1, State#cs_state.n_of)),
  {noreply, State#cs_state{lsock=Lsock, mon=M, struct=Sup_struct}};

handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast(_Data, State) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #cs_state{}) ->
  {noreply, NewState :: #cs_state{}} |
  {noreply, NewState :: #cs_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #cs_state{}}).
handle_info({'DOWN', M, _Type, _Object, _Info}, #cs_state{lsock=L, mon=M, port=Port}=State) ->
  g_repo:inet_box_status(foll_down),
  inet:cancel_monitor(M),
  gen_tcp:close(L),
  L=supervisor:which_children(conn_sup),
  [supervisor:terminate_child(conn_sup , Id)||{Id, _, _, _}<- L],
  [supervisor:delete_child(conn_sup, Id)||{Id, _, _, _}<- L],
  {ok, Lsock}=gen_tcp:listen(Port, [list, {active, once}]),
  g_repo:inet_box_status(hold_it),
  F2=fun (Num) -> proceed(State#cs_state.struct, Num) end,
  lists:foreach(F2, lists:seq(1, State#cs_state.n_of)),
  Mon=inet:monitor(Lsock),
  {noreply, State#cs_state{lsock=Lsock, mon=Mon}};

handle_info(_Data, State) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #cs_state{}) -> term()).
terminate(_Reason, State) ->
  State#cs_state.mon/=false andalso inet:cancel_monitor(State#cs_state.mon),
  State#cs_state.lsock/=false andalso gen_tcp:close(State#cs_state.lsock).

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #cs_state{},
    Extra :: term()) ->
  {ok, NewState :: #cs_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #cs_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

proceed(Sup_struct, N) ->
  Id="conn"++integer_to_list(N),
  St=maps:put(id, Id, Sup_struct),
  supervisor:start_child(conn_sup, St).

stop() ->
  g_repo:inet_box_status(foll_down),
  L=supervisor:which_children(conn_sup),
  [supervisor:terminate_child(conn_sup, Id)||{Id, _, _, _}<- L],
  [supervisor:delete_child(conn_sup, Id)||{Id, _, _, _}<- L],
  gen_server:cast(self(), stop).