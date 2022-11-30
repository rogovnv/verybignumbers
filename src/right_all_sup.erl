%%%-------------------------------------------------------------------
%%% @author mt
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. нояб. 2022 19:06
%%%-------------------------------------------------------------------
-module(right_all_sup).
-author("mt").

-behaviour(supervisor).

%% API
-export([start_link/1, stop/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor
start_link(Data) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [Data]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]}}
  | ignore | {error, Reason :: term()}).
init([{Port, Conns, Wdir, Fle}]) ->
  MaxRestarts = 5,
  MaxSecondsBetweenRestarts = 3600,
  SupFlags = #{strategy => one_for_all,
    intensity => MaxRestarts,
    period => MaxSecondsBetweenRestarts},

  AChild1 = #{id => conn_sp,
    start => {conn_sup, start_link, []},
    restart => permanent,
    type => supervisor,
    modules => []},

  AChild2 = #{id => conn_st,
    start => {conn_starter, start_link, [Wdir, Port, Conns, Fle]},
    restart => transient,
    type => worker,
    modules => []},

  {ok, {SupFlags, [AChild1, AChild2]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

stop() ->
  conn_starter:stop().