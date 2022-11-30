%%%-------------------------------------------------------------------
%%% @author mt
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. окт. 2022 18:41
%%%-------------------------------------------------------------------
-module(top_sup).
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
  supervisor:start_link({local, ?SERVER}, ?MODULE, Data).

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
init([{Wdir, Port, Conns, Maxmem, Fle}]) ->
  MaxRestarts = 20,
  MaxSecondsBetweenRestarts = 3600,
  SupFlags = #{strategy => one_for_one,
    intensity => MaxRestarts,
    period => MaxSecondsBetweenRestarts},

  L_child = #{id => 'lchild',
    start => {left_one_sup, start_link, [{Wdir, Maxmem}]},
    restart => permanent,
    type => supervisor},

  R_child = #{id => 'rchild',
    start => {right_all_sup, start_link, [{Port, Conns, Wdir, Fle}]},
    restart => permanent,
    type => supervisor},

  {ok, {SupFlags, [L_child, R_child]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

stop() ->
  right_all_sup:stop(),
  timer:sleep(2000),
  application:stop(base_calc).
