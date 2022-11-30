%%%-------------------------------------------------------------------
%%% @author mt
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(conn_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
%%  AChild = #{id => 'AName',
%%    start => {conn_handler, start_link, [Lsock, "./calc"]},
%%    restart => permanent,
%%    shutdown => 3600,
%%    type => worker,
%%    modules => []},


  {ok, {#{strategy => one_for_one,
    intensity => 20,
    period => 3600},
    []}
  }.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ./priv/conn_conf mandatory
%% wdir Path
%% conns Number of conn handlers
%% port Port
%% maxmem memory
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%