%% @author midtower@yandex.ru
%%% @doc
%%% string preprocessor
%%% var names is [A-Z|_]
%%% numbers can be with e or without, of arbitrary length
%%% you can use unary minus once at op and lbracket: F=-(-(-AZ_QWER+-17.23e4)*-2)/38.123456789
%%% brackets only can consist full sentence, e. g. RIGHT(A+B), not just number or var: WRONG(17.3)
%%% do not insert into brackets unary  minus: WRONG(-2345.3e12)
%%% @end
-module(preproc).

-behaviour(gen_statem).
-export([callback_mode/0, start/1, init/1]).
-export([undef/3, lbr/3, rbr/3, oper/3, terminate/3, afterinit/3]).

%% undef - initial state, takes variable and = sign
%% oper - state for operator
%% oper - get and normalize number
%% lbr rbr - bracket handlers


callback_mode() ->
  state_functions.

start({VarPid, StringId}) ->
  gen_statem:start(?MODULE, {VarPid, StringId}, []).

terminate(_Reason, _State, _Data) ->
  ok.

init({VarPid, StringId}) ->
  process_flag(trap_exit, true),
  Str=var:get_task_str(VarPid, StringId),
  {ok, afterinit, {VarPid, StringId}, [{next_event, internal, [Str]}]}.

%% State is: VarPid curent Pid on global data for task
%% Str4&Insert original task string,
%% Aout [] output in erlang terms ({v|n, Var or nmber of Number} | operator | brackets)
%% Bracket number and sequence of l and r brackets
%% initial "Var =" checks for the first insertion

%% Output: wrong_begin, wrong_bracket_number, wrong_lbracket, wrong_lvar, bad_number,
%% wrong_oper, wrong_rbacket, wrong_rvar

afterinit(internal, Strr, {VarPid, StringId}) ->
  %% extract vars
  Str=lists:flatten(Strr),
  {match, Vara}=re:run(Str, "[A-Z|_]+", [global]),
  Varb=lists:flatten(Vara),
  Varv=[{X+1, {v,lists:sublist(Str, X+1, Y)}}|| {X, Y} <- Varb],
  %% extract negative numbers(with unary minus)
  {Str2, Extract}=replacevar(Str, Varv, Str),
  N=re:run(Extract, "[-+*/=(]-[0-9]+\\.[0-9]+e[0-9]+|[-+*/=(]-[0-9]+\\.[0-9]+|[-+*/=(]-[0-9]+", [global]),
  {Negv, {Str3, Extract2}}=case N of
                             {match, NegN} ->
                                  E=lists:flatten(NegN),
                                  Q=[{X+2, {n,lists:sublist(Extract, X+2, Y-1)}}|| {X, Y} <- E],
                                  {A, B}=replacevar(Str2, Q, Extract),
                                  {Q, {A, B}};
                             _Other ->
                                  {[],{Str2, Extract}}
                              end,
  %% extract positive numbers
  R=re:run(Extract2, "[0-9]+\\.[0-9]+e[0-9]+|[0-9]+\\.[0-9]+|[0-9]+", [global]),
  {Posv, Str4}= case R of
                  {match, Posa} ->
                        Posb=lists:flatten(Posa),
                        M=[{X+1, {n, lists:sublist(Extract2, X+1, Y)}}||{X, Y} <- Posb],
                    {STR, _Any}=replacevar(Str3, M, Extract2),
                        {M, STR};
                  _ ->
                        {[], Str3}
                    end,
  Ins=lists:flatten(lists:sort(fun({A, _C}, {B, _D}) -> B>A end, Varv++Negv++Posv)),
  Insert=[F||{_, F}<-Ins],%% after sort deleting string pos
  %%if header and body has same vname
  {v, Vn}=hd(Insert),
  WrongDoubleVname=lists:member({v, Vn}, tl(Insert)),
  Bool_bound=var:is_existv(VarPid, Vn)==true,
  Wrong_bound=WrongDoubleVname and (not Bool_bound),
  case Wrong_bound of
    true ->
      MP=var:getmaster(VarPid),
      gen_server:cast(MP, {StringId, wrong_double_vname}),
      keep_state_and_data;
    false ->
      {next_state, undef, {VarPid, Str4, Insert, [], 0, StringId}, [{next_event, internal, []}]}
  end;

afterinit(cast, stop, Data) ->
  {stop, normal, Data}.

undef(internal, _Data, {VarPid, Str, Ins, [], LRBr, SId}) ->
  [{A, B}|Ins2]=Ins,
  [Q,Z|T2]=Str,
  Bool=A==v,
  Booool=[Q, Z]=="v=",
  R=Bool and Booool,
  case R of
    true ->
      var:initv(VarPid, B),
      [P|Str2]=T2,
      {next_state, lbr, {VarPid, Str2, Ins2, [{A, B}, Z], LRBr, SId}, [{next_event, internal, P}]};%% eq symbol fired
    _ ->
      MP=var:getmaster(VarPid),
      gen_server:cast(MP, {SId, wrong_begin}),
      keep_state_and_data
  end;

undef(cast, stop, Data) ->
  {stop, normal, Data}.

%% left bracket, keepstate
lbr(internal, Data, {VarPid, Str, Ins, Aout, LRBr, SId}) when Data == $(->
  MP=var:getmaster(VarPid),
  if
    LRBr >= 0 ->
      [H|T]=Str,
      case H of
        $- -> %% unary minus
          [H1, T1]=T,
          Bool=H1==$( orelse H1==$v, %% prevent loop
          case Bool of
            true ->
              {A, _}=hd(Ins),
              %% unary minus only for var or expressions in brackets
              Boool=A==n,
              Bool=H1 == $v,
              case Bool and Boool of
                false ->
                  {keep_state, {VarPid, T1, Ins, [Aout,"(", H], LRBr+1, SId}, [{next_event, internal, H1}]};
                true ->
                  gen_server:cast(MP, {SId, too_much_minuses}),
                  keep_state_and_data
              end;
            false ->
              gen_server:cast(MP, {SId, wrong_uminus}),
              keep_state_and_data
          end;
        _ ->
          {keep_state, {VarPid, T, Ins, [Aout,"("], LRBr+1, SId}, [{next_event, internal, H}]}
      end;
    true ->
      gen_server:cast(MP, {SId, wrong_bracket}),
      keep_state_and_data
  end;

%% var or number, switch to oper

lbr(internal, Data, {VarPid, [], Ins, Aout, LRBr, SId}) when Data == $v ->
  [{A, B}|TI]=Ins,
  case {A, B} of
    {n, Num} ->
      HIns={n, handle_num(VarPid, Num)},
      {next_state, oper, {VarPid, [], TI, [Aout,HIns], LRBr, SId}, [{next_event, internal, " "}]};
    {v, Vname} -> %% var
      Bool=gen_server:call(VarPid, {is_existv, Vname}),
      case Bool of
        badkey ->
          MP=var:getmaster(VarPid),
          gen_server:cast(MP, {SId, wrong_lvar}),
          keep_state_and_data;
        _ ->
          {next_state, oper, {VarPid, [], TI, [Aout,{A, B}], LRBr, SId}, [{next_event, internal, " "}]}
      end
  end;

lbr(internal, Data, {VarPid, Str, Ins, Aout, LRBr, SId}) when Data == $v ->
  [H|T]=Str,
  [{A, B}|TI]=Ins,
  case {A, B} of
    {n, Num} ->
      HIns={n, handle_num(VarPid, Num)},
      {next_state, oper, {VarPid, T, TI, [Aout,HIns], LRBr, SId}, [{next_event, internal, H}]};
    {v, Vname} -> %% var
      Bool=gen_server:call(VarPid, {is_existv, Vname}),
      case Bool of
        badkey ->
          MP=var:getmaster(VarPid),
          gen_server:cast(MP, {SId, wrong_lvar}),
          keep_state_and_data;
        _ ->
          {next_state, oper, {VarPid, T, TI, [Aout,{A, B}], LRBr, SId}, [{next_event, internal, H}]}
      end
  end;

lbr(internal, Data, {VarPid, Str, Ins, Aout, LRBr, SId}) when Data == $- ->
  B=lists:nth(length(Aout), Aout),
  Bool=B==$= orelse B==$(,
  MP=var:getmaster(VarPid),
  case Bool of
    true ->
      [H|T]=Str,
      Bool=H==$( orelse H==$v, %% prevent loop
      case Bool of
        true ->
          {A, _}=hd(Ins),
          %% unary minus only for var or expressions in brackets
          Boool=A==n,
          Booool=H==$v,
          case Booool and Boool of
            false ->
              {keep_state, {VarPid, T, Ins, [Aout,$-], LRBr, SId}, [{next_event, internal, H}]};
            true ->
              gen_server:cast(MP, {SId, too_much_minuses}),
              keep_state_and_data
          end;
        false ->
          gen_server:cast(MP, {SId, wrong_uminus}),
          keep_state_and_data
      end;
      %% {keep_state, {VarPid, T, Ins, [Aout,"-"], LRBr, SId}, [{next_event, internal, H}]};
    false ->
      gen_server:cast(MP, {SId, wrong_uminus}),
      keep_state_and_data
  end;

%% wrong symbol
lbr(internal, _Data, {VarPid, _Str, _Ins , _Aout, _LRBr, SId}) ->
  MP=var:getmaster(VarPid),
  gen_server:cast(MP, {SId, bad_number}),
  keep_state_and_data;

lbr(cast, stop, Data) ->
  {stop, normal, Data}.

rbr(internal, Data, {VarPid, Str, Ins, Aout, LRBr, SId}) when Data == $- ->
  MP=gen_server:call(VarPid, master),
  [H|T]=Str,
  %% prevent loop
  case H==$v of
    true ->
      {A, _}=hd(Ins),
      %% unary minus only for var or expressions in brackets
      case A =:= n of
        false -> %% variable eg. S*-E
          {keep_state, {VarPid, T, Ins, [Aout,"-"], LRBr, SId}, [{next_event, internal, H}]};
        true ->
          gen_server:cast(MP, {SId, too_much_minuses}),
          keep_state_and_data
      end;
    false ->
      gen_server:cast(MP, {SId, wrong_uminus}),
      keep_state_and_data
  end;

%% transition
rbr(internal, Data, {VarPid, Str, Ins, Aout, LRBr, SId}) when Data == $( ->
  [H|T]=Str,
  {next_state, lbr, {VarPid, T, Ins, [Aout,Data], LRBr+1, SId}, [{next_event, internal, H}]};

rbr(internal, Data, {VarPid, Str, Ins, Aout, LRBr, SId}) when Data ==$v ->
  [H|T]= case Str of
           [] -> [[]|[]];
           _Other -> Str
         end,
  [{A, B}|TI]=Ins,
  case {A, B} of
    {n, Num} ->
      HIns={n, handle_num(VarPid, Num)},
      {next_state, oper, {VarPid, T, TI, [Aout,HIns], LRBr, SId}, [{next_event, internal, H}]};
    {v, Vname} -> %% var
      Bool=var:is_existv(VarPid, Vname),
      case Bool of
        badkey ->
          MP=var:getmaster(VarPid),
          gen_server:cast(MP, {SId, wrong_rvar}),
          keep_state_and_data;
        _ ->
          {next_state, oper, {VarPid, T, TI, [Aout,{A, B}], LRBr, SId}, [{next_event, internal, H}]}
      end
  end;

rbr(cast, stop, Data) ->
  {stop, normal, Data}.
%% Answer
oper(internal, Data, {VarPid, [], _Ins, Aout, _LRBr, SId})  when Data == []  ->
  MP=var:getmaster(VarPid),
  StrId=gen_server:call(VarPid, {sets, lists:flatten(Aout)}),
  gen_server:cast(MP, {SId, StrId}),
  keep_state_and_data;

%% Answer
oper(internal, Data, {VarPid, [], _Ins, Aout, _LRBr, SId})  when Data == " "  ->
  MP=var:getmaster(VarPid),
  StrId=gen_server:call(VarPid, {sets, lists:flatten(Aout)}),
  gen_server:cast(MP, {SId, StrId}),
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
  MP=var:getmaster(VarPid),
  gen_server:cast(MP, {SId, wrong_oper}),
  keep_state_and_data;

oper(cast, stop, Data) ->
  {stop, normal, Data}.

handle_num(VarPid, Num) ->
  Range=var:getrange(VarPid),
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
      {Med2, Dot}= if
             D<0 ->
               {lists:duplicate(-D, $0)++Med, 0};
             true ->
               {Med++lists:duplicate(R, $0), D}
           end,
      list_to_integer(lists:sublist(lists:flatten(Med2), 1, Dot+Range))
  end.

replacevar(Str, [], Ext) ->
  {Str,Ext};
replacevar(Str, Vars, Ext) ->
  {_,{_, Z}}=hd(Vars),
  F=re:replace(Str, Z, "v", [{return, list}]),
  Seq=lists:duplicate(length(Z), $z),
  P=re:replace(Ext, Z, Seq, [{return, list}]),
  replacevar(F, tl(Vars), P).



