%%%-------------------------------------------------------------------
%%% @author midtower@yandex.ru
%%% @copyright (C) 2022,
%%% @doc
%%% it's preparing model and calculating the string
%%% @end
%%% Created : 03. май 2022 19:00
%%%-------------------------------------------------------------------
-module(calculate).
-author("mt").

%% API
-behaviour(gen_statem).
-export([callback_mode/0, start/1, init/1]).
-export([handle_event/4]).


callback_mode() ->
  handle_event_function.

start({VarPid, StringId}) ->
  gen_statem:start(?MODULE, {VarPid, StringId}, []).

init({VarPid, SId}) ->
  process_flag(trap_exit, true),
  Str=gen_server:call(VarPid, {gets, SId}),
  F=re:run(Str, "(", [global]),
  case F of
    {match, LBr} ->
      {ok, bracket, {Str, LBr}, [{next_event, internal, []}]};
    true ->
      {ok, muldiv, {[], [], 2, Str}, [{next_event, internal, []}]}
  end.
  .

%% States:
%% bracket: handling sequences in br inner to outer
%% it cuts attaches and instead inserts {f, FunPid}
%% attaches and looked up string are handling in states below
%% muldiv, addsub, neg
%% each iteration creating {f, FunPid} instead of (a operator b)

handle_event(internal, _, bracket, {Str, LBr}) when LBr == [] ->
  {next_state, muldiv, {[], LBr, 2, Str}, [{next_event, internal, []}]};

handle_event(internal, _, bracket, {Str, LBr}) ->
  {match, RBr}=re:run(Str, ")"),
  {Bgn, _}=lists:nth(length(LBr), Str),
  LBr2=lists:sublist(LBr, 0, length(LBr)-1),
  SubStr1=lists:sublist(Str, Bgn+1, RBr-1),
  SubStr2=re:replace(Str, lists:flatten($(++SubStr1++$)), $z, [{return, list}]),
  {next_state, muldiv, {SubStr2, LBr2, 1, SubStr1}, [{next_event, internal, []}]};

handle_event(internal, _, muldiv, {_Str2, _LBr, IsPart, Str}) ->




