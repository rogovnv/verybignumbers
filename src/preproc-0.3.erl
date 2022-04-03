%% @author midtower@yandex.ru
%% @doc @todo Add description to syntprep.

-module(preproc).

-behaviour(gen_statem).
-export([callback_mode/0, start/1, init/1, handle_event/4]).

%% undef - typical state for grabbing  undefined data, number or brackets
%% op - state for operator or brackets


callback_mode() ->
  state_functions.

start({Pid, Id}) ->
  gen_statem:start(?MODULE, {Pid, Id}, []).

init({Pid, Id}) ->
  process_flag(trap_exit, true),
  gen_server:cast(Pid, Id),
  {ok, undef, []}.



