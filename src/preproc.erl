%% @author midtower@yandex.ru
%% @doc preprocessor

-module(preproc).

-behaviour(gen_statem).
-export([callback_mode/0, start/1, init/1]).
-export([undef/3, lbr/3, rbr/3, oper/3, stop/1, terminate/3]).

%% undef - initial state, takes variable and = sign
%% oper - state for operator
%% oper - get and normalize number
%% lbr rbr - bracket handlers


callback_mode() ->
  state_functions.

start({VarPid, StringId}) ->
  gen_statem:start(?MODULE, {VarPid, StringId}, []).

init({VarPid, StringId}) ->
  process_flag(trap_exit, true),
  Str=gen_server:call(VarPid, {gets, StringId}),
  {match, Vara}=re:run(Str, "[A-Z|_]+", [global]),
  Varb=lists:flatten(Vara),
  Varv=[{X+1, {v,lists:sublist(Str, X+1, Y)}}|| {X, Y} <- Varb],
  Str2=replacevar(Str, Varv),
  N=re:run(Str2, "[-+*/=(]-[0-9]+.[0-9]+e[0-9]+|[-+*/=(]-[0-9]+.[0-9]+|[-+*/=(]-[0-9]+", [global]),
  case N of
    {match, NegN} ->
      E=lists:flatten(NegN),
      Negv=[{X+2, {n,lists:sublist(Str2, X+2, Y-1)}}|| {X, Y} <- E],
      Str3=replacevar(Str2, Negv);
    _ ->
      Str3=Str2,
      Negv=[]
  end,
  R=re:run(Str3, "[0-9]+.[0-9]+e[0-9]+|[0-9]+.[0-9]+|[0-9]+", [global]),
  Str5=Str3,
  case R of
    {match, Posa} ->
      Posb=lists:flatten(Posa),
      %%Str3 = Str3,
      Posv=[{X+1, {n, lists:sublist(Str5, X+1, Y)}}||{X, Y} <- Posb],
      Str4=replacevar(Str5, Posv);
    _ ->
      %%Str5 = Str3, beeaach
      Posv=[],
      Str4=Str5
  end,
  Ins=lists:flatten(lists:sort(fun({A, _C}, {B, _D}) -> B>A end, Varv++Negv++Posv)),
  Insert=[F||{_, F}<-Ins],
  {ok, undef, {VarPid, Str4, Insert, [], 0, StringId}, [{next_event, internal, []}]}.
%% State is: VarPid curent Pid on global data for task
%% Str4&Insert original task string,
%% Aout [] output in erlang terms ({v|n, Var or nmber of Number} | operator | brackets)
%% Bracket number and sequence of l and r brackets
%% initial "Var =" checks for the first insertion

%% Output: wrong_begin, wrong_bracket_number, wrong_lbracket, wrong_lvar, bad_number,
%% wrong_oper, wrong_rbacket, wrong_rvar

undef(internal, _Data, {VarPid, Str, Ins, [], LRBr, SId}) ->
  [{A, B}|Ins2]=Ins,
  [Q,Z|T2]=Str,
  Bool=A==v,
  Booool=[Q, Z]=="v=",
  R=Bool and Booool,
  case R of
    true ->
      gen_server:cast(VarPid, {initv, B}),
      [P|Str2]=T2,
      {next_state, lbr, {VarPid, Str2, Ins2, Q, LRBr, SId}, [{next_event, internal, P}]};%% eq symbol fired
    _ ->
      MP=gen_server:call(VarPid, master),
      gen_server:cast(MP, {SId, wrong_begin}),
      keep_state_and_data
  end.

%% left bracket, keepstate
lbr(internal, Data, {VarPid, Str, Ins, Aout, LRBr, SId}) when Data == $(->
  if
    LRBr >= 0 ->
      [H, T]=Str,
      {keep_state, {VarPid, T, Ins, [Aout,"("], LRBr+1, SId}, [{next_event, internal, H}]};
    true ->
      MP=gen_server:call(VarPid, master),
      gen_server:cast(MP, {SId, wrong_bracket}),
      keep_state_and_data
  end;

%% var or number, switch to oper
lbr(internal, Data, {VarPid, Str, Ins, Aout, LRBr, SId}) when Data == $v->
  [H|T]=Str,
  [{A, B}|TI]=Ins,
  case {A, B} of
    {n, Num} ->
      HIns={n, perform_num(VarPid, Num)},
      {next_state, oper, {VarPid, T, TI, [Aout,HIns], LRBr, SId}, [{next_event, internal, H}]};
    {v, Vname} -> %% var
      Bool=gen_server:call(VarPid, {is_existv, Vname}),
      case Bool of
        badkey ->
          MP=gen_server:call(VarPid, master),
          gen_server:cast(MP, {SId, wrong_lvar}),
          keep_state_and_data;
        _ ->
          {next_state, oper, {VarPid, T, TI, [Aout,{A, B}], LRBr, SId}, [{next_event, internal, H}]}
      end
  end;

%% wrong symbol
lbr(internal, _Data, {VarPid, _Str, _Ins , _Aout, _LRBr, SId}) ->
  MP=gen_server:call(VarPid, master),
  gen_server:cast(MP, {SId, bad_number}),
  keep_state_and_data.

%% transition
rbr(internal, Data, {VarPid, Str, Ins, Aout, LRBr, SId}) when Data == $( ->
  [H|T]=Str,
  {next_state, lbr, {VarPid, T, Ins, [Aout,Data], LRBr+1, SId}, [{next_event, internal, H}]};

rbr(internal, Data, {VarPid, Str, Ins, Aout, LRBr, SId}) when Data ==$v ->
  [H|T]=Str,
  [{A, B}|TI]=Ins,
  case {A, B} of
    {n, Num} ->
      HIns={n, perform_num(VarPid, Num)},
      {next_state, oper, {VarPid, T, TI, [Aout,HIns], LRBr, SId}, [{next_event, internal, H}]};
    {v, Vname} -> %% var
      Bool=gen_server:call(VarPid, {is_existv, Vname}),
      case Bool of
        badkey ->
          MP=gen_server:call(VarPid, master),
          gen_server:cast(MP, {SId, wrong_rvar}),
          keep_state_and_data;
        _ ->
          {next_state, oper, {VarPid, T, TI, [Aout,{A, B}], LRBr, SId}, [{next_event, internal, H}]}
      end
  end.

oper(internal, Data, {VarPid, [], _Ins, Aout, LRBr, SId})  when Data == " "  ->
  MP=gen_server:call(VarPid, master),
  Answer= if
            LRBr == 0 -> {SId, Aout};
            true -> {SId, wrong_bracket_number}
          end,
  gen_server:cast(MP, Answer),
  keep_state_and_data;

oper(internal, Data, {VarPid, Str, Ins, Aout, LRBr, SId}) when Data ==$+ orelse Data ==$- orelse Data ==$* orelse Data ==$/ ->
  [H|T]=Str,
  {next_state, rbr, {VarPid, T, Ins, [Aout,Data], LRBr, SId}, [{next_event, internal, H}]};

oper(internal, Data, {VarPid, Str, Ins, Aout, LRBr, SId}) when Data == $) ->
  Bool=Str =:= [],
  case Bool of
    true ->
      H=" ",
      T=[];
    false ->
      [H|T]=Str
  end,
  {keep_state, {VarPid, T, Ins, [Aout,Data], LRBr-1, SId}, [{next_event, internal, H}]};

oper(internal, Data, {VarPid, Str, Ins, Aout, LRBr, SId}) when Data == $( ->
  [H|T]=Str,
  {next_state, lbr, {VarPid, T, Ins, [Aout,Data], LRBr+1, SId}, [{next_event, internal, H}]};

oper(internal, _Data, {VarPid, _Ins, _Str, _Aout, _LRBr, SId}) ->
  MP=gen_server:call(VarPid, master),
  gen_server:cast(MP, {SId, wrong_oper}),
  keep_state_and_data.

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
      F=lists:nth(2, Split),
      L=length(F),
      D= if
           L > Range ->
             lists:sublist(F, 0, Range);
           true ->
             F++lists:duplicate(Range-L, $0)
         end,
      list_to_integer(lists:flatten(lists:nth(1, Split)++D));
    3 ->%% calculate and keep in mind dot pos
      F=lists:nth(2, Split),%% numbers after dot
      L=length(lists:nth(1,Split)),
      R=list_to_integer(lists:nth(3, Split)),%% exp
      Med=lists:nth(1, Split)++F++lists:duplicate(Range, $0),
      D=L+R,
      Dot= if
             D<0 ->
               Med2=lists:duplicate(-D, $0)++Med,
               0;
             true ->
               Med2=Med++lists:duplicate(R, $0),
               D
           end,
      list_to_integer(lists:sublist(lists:flatten(Med2), 1, Dot+Range))
  end.

replacevar(Str, []) ->
  Str;
replacevar(Str, Vars) ->
  {_,{_, Z}}=hd(Vars),
  F=re:replace(Str, Z, "v", [{return, list}]),
  replacevar(F, tl(Vars)).



