%% @author midtower@yandex.ru
%% @doc @todo Add description to.

-module(preproc).

-behaviour(gen_statem).
-export([callback_mode/0, start/1, init/1]).
-export([undef/3, lbr/3, rbr/3, oper/3, stop/1, terminate/3]).

%% undef - initial state, takes variable and = sign
%% oper - state for operator
%% oper - get and normalize number
%% lbr rbr - bracker handlers


callback_mode() ->
  state_functions.

start({VarPid, StringId}) ->
  gen_statem:start(?MODULE, {VarPid, StringId}, []).

init({VarPid, StringId}) ->
  process_flag(trap_exit, true),
  Str=gen_server:call(VarPid, {gets, StringId}),

  Vars=re:run(Str, "[A-Z|_]+", [global]),
  Varv=[{X, {v,lists:sublist(Str, X, Y)}}|| {X, Y} <- Vars],
  Str2=replacevar(Str, Varv),

  NegN=re:run(Str2, "=-[0-9]+(.[0-9])*+(e[0-9]+)*|(-[0-9]+(.[0-9])*+(e[0-9]+)*", [global]),
  NegNv=[{X, {n, lists:sublist(Str2, X+1, Y)}}|| {X, Y} <- NegN],
  Str3=replacevar(Str2, NegNv),

  PosN=re:run(Str3, "[0-9]+(.[0-9])*+(e[0-9]+)*", [global]),
  PosNv=[{X, {n, lists:sublist(Str3, X, Y)}}|| {X, Y} <- PosN],
  Str4=replacevar(Str3, PosNv),

  Ins=lists:flatten(lists:sort(fun({A, _C}, {B, _D}) -> B>A end, Varv++NegNv++PosNv)),
  Insert=[F||{_, F}<-Ins],

  %% Str4 is AOut
  {ok, undef, {VarPid, Str4, Insert, [], 0, StringId}, [{next_event, internal, []}]}.
%% Data is: VarPid curent Pid on global data for task
%% Str4&Insert original task string,
%% Aout [] output in erlang terms ({v|n, Var or nmber of Number} | operator | brackets)
%% Bracket number and sequence of l and r brackets
%% initial "Var =" checks for the first insertion

%% Output: wrong_begin, wrong_bracket_number, wrong_lbracket, wrong_lvar, bad_number,
%% wrong_oper, wrong_rbacket, wrong_rvar

undef(internal, _, {VarPid, Str, Ins, [], LRBr, SId}) ->
  [{A, B}|Ins2]=Ins,
  [[Z, X]|T]=Str,
  if
    [Z, X]==["v="] and A=="v" ->
      gen_server:cast(VarPid, {initv, B}),
      [P|Str2]=T,
      {next_state, lbr, {VarPid, Str2, Ins2, [{A, B}, "="], LRBr, SId}, [{next_event, internal, P}]};
    true ->
      MP=gen_server:call(VarPid, master),
      gen_server:reply(MP, {SId, wrong_begin}),
      keep_state_and_data
  end.

%% end handling in left state
lbr(internal, _Data, {VarPid, Str, _Ins, Aout, LRBr, SId}) when Str == [] ->
  MP=gen_server:call(VarPid, master),
  Answer= case LRBr of
            0 -> {SId, Aout};
            true -> {SId, wrong_bracket_number}
          end,
  gen_server:reply(MP, {SId, Answer}),
  keep_state_and_data;

%% left bracket, keepstate
lbr(internal, Data, {VarPid, Str, Ins, Aout, LRBr, SId}) when Data == $( ->
  if
    LRBr >= 0 ->
      [H|T]=Str,
      {keep_state, {VarPid, T, Aout++$(, Ins, LRBr+1, SId}, [{next_event, internal, H}]};
    true ->
      MP=gen_server:call(VarPid, master),
      gen_server:reply(MP, {SId, wrong_lbracket}),
      keep_state_and_data
  end;

%% var or number, switch to oper
lbr(internal, Data, {VarPid, Str, Ins, Aout, LRBr, SId}) when Data == $v ->
  [H|T]=Str,
  [HI|TI]=Ins,
  case HI of
    {n, Num} ->
      HIns={n, perform_num(VarPid, Num)},
      {next_state, oper, {VarPid, T, TI, Aout++HIns, LRBr, SId}, [{next_event, internal, H}]};
    {v, Vname} -> %% var
      Bool=gen_server:call(VarPid, {is_existv, Vname}),
      case Bool of
        badkey ->
          MP=gen_server:call(VarPid, master),
          gen_server:reply(MP, {SId, wrong_lvar}),
          keep_state_and_data;
        true -> {next_state, oper, {VarPid, T, TI, Aout++HI, LRBr, SId}, [{next_event, internal, H}]}
      end
  end;

%% wrong symbol
lbr(internal, _Data, {VarPid, _Str, _Aout , _Ins, _LRBr, SId}) ->
  MP=gen_server:call(VarPid, master),
  gen_server:reply(MP, {SId, bad_number}),
  keep_state_and_data.

rbr(internal, _Data, {VarPid, Str, _Aout , _Ins, _LRBr, SId}) when Str==[] ->
  MP=gen_server:call(VarPid, master),
  gen_server:reply(MP, {SId, wrong_oper}),
  keep_state_and_data;

%% transition
rbr(internal, Data, {VarPid, Str, Aout, Ins, LRBr, SId}) when Data == $( ->
  {next_state, lbr, {VarPid, Str, Aout, Ins, LRBr, SId}, [{next_event, internal, Data}]};

rbr(internal, Data, {VarPid, Str, Ins, Aout, LRBr, SId}) when Data ==$v ->
  [H|T]=Str,
  [HI|TI]=Ins,
  case HI of
    {n, Num} ->
      HIns={n, perform_num(VarPid, Num)},
      {next_state, oper, {VarPid, T, TI, Aout++HIns, LRBr, SId}, [{next_event, internal, H}]};
    {v, Vname} -> %% var
      Bool=gen_server:call(VarPid, {is_existv, Vname}),
      case Bool of
        badkey ->
          MP=gen_server:call(VarPid, master),
          gen_server:reply(MP, {SId, wrong_rvar}),
          keep_state_and_data;
        true -> {next_state, oper, {VarPid, T, TI, Aout++HI, LRBr, SId}, [{next_event, internal, H}]}
      end
  end.

oper(internal, _Data, {VarPid, Str, _Ins, Aout, LRBr, SId})  when Str == [] ->
  MP=gen_server:call(VarPid, master),
  Answer= case LRBr of
            0 -> {SId, Aout};
            true -> {SId, wrong_bracket_number}
          end,
  gen_server:reply(MP, {SId, Answer}),
  keep_state_and_data;

oper(internal, Data, {VarPid, Str, Ins, Aout, LRBr, SId}) ->
  [H|T]=Str,
  if
    Data ==$+ orelse Data ==$- orelse Data ==$* orelse Data ==$/ ->
      {next_state, rbr, {VarPid, T, Ins, Aout++Data, LRBr, SId}, [{next_event, internal, H}]};
    Data ==$) ->
      {keep_state, {VarPid, T, Ins, Aout++Data, LRBr, SId}, [{next_event, internal, H}]};
    true ->
      MP=gen_server:call(VarPid, master),
      gen_server:reply(MP, {SId, wrong_oper}),
      keep_state_and_data
  end.

stop(Pid) ->
  gen_statem:stop(Pid).

terminate(_Reason, State, Data) ->
  {ok, State, Data}.

perform_num(VarPid, Num) ->
  Range=gen_server:call(VarPid, range),
  Split=re:split(Num, "[.e]", [{return, list}]),
  case length(Split) of
    1 ->
      list_to_integer(lists:flatten(Num++lists:duplicate(Range, $0)));
    2 ->
      F=lists:nth(Split, 2),
      L=length(F),
      D= if
           L > Range ->
             lists:sublist(F, 0, Range);
           true ->
             F++lists:duplicate(Range-L, $0)
         end,
      list_to_integer(lists:flatten(lists:nth(Split, 1)++D));
    3 ->%% calculate and keep in mind dot pos
      F=lists:nth(Split, 2),%% numbers after dot
      L=length(F),
      R=list_to_integer(lists:nth(Split, 3)),%% exp
      Med=lists:nth(Split, 1)++F,
      L1=length(Med),
      if
        R >= 0 ->
          if
            R-L+Range>=0 ->
              Med2=Med++lists:duplicate(R-L+Range, $0);
            true ->
              Med2=lists:sublist(Med, 0, L1-R+L-Range)
          end;
          true ->
            if
              -R+L >= Range ->
                Med2=lists:duplicate(-R, $0)++lists:sublist(Med, 0, L1-(-R+L-Range));
              true ->
                Med2=lists:duplicate(-R, $0)++Med
            end
      end,
      list_to_integer(lists:flatten(Med2))
  end.

replacevar(Str, []) ->
  Str;
replacevar(Str, Vars) ->
  {_, Z}=hd(Vars),
  F=re:replace(Str, Z, "v", [{return, list}]),
  replacevar(F, tl(Str)).



