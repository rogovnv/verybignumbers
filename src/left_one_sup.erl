%%%-------------------------------------------------------------------
%%% @author mt
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. нояб. 2022 19:06
%%%-------------------------------------------------------------------
-module(left_one_sup).
-author("mt").

-behaviour(supervisor).

%% API
-export([start_link/1]).

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
init([{Wdir, Maxmem}]) ->
  MaxRestarts = 5,
  MaxSecondsBetweenRestarts = 3600,
  SupFlags = #{strategy => one_for_one,
    intensity => MaxRestarts,
    period => MaxSecondsBetweenRestarts},

  AChild1 = #{id => tass,
    start => {th_sup, start_link, []},
    restart => permanent,
    type => supervisor,
    modules => []},

  AChild2 = #{id => g_repo,
    start => {g_repo, start_link, [Wdir, Maxmem]},
    restart => permanent,
    type => worker,
    modules => []},

  {ok, {SupFlags, [AChild2, AChild1]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
