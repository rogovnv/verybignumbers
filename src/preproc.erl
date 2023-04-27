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
-export([afterinit/3, undef/3, lbr/3, rbr/3, oper/3, terminate/3]).

%% undef - initial state, takes variable and = sign
%% oper - state for operator
%% oper - get and normalize number
%% lbr rbr - bracket handlers

callback_mode() ->
    state_functions.

start({VarPid, StringId, MPid}) ->
    gen_statem:start(?MODULE, {VarPid, StringId, MPid}, []).
  
terminate(_Reason, _State, _Data) ->
    ok.

init({VarPid, StringId, MP}) ->
    process_flag(trap_exit, true),
    Str=var:get_task_str(VarPid, StringId),
    {ok, afterinit, {VarPid, StringId, MP}, [{next_event, internal, [Str]}]}.

afterinit(cast, stop, Data) ->
    {stop, normal, Data};

afterinit(internal, Strr, {VarPid, StringId, MP}) ->
    %% extract vars
    Fn=fun(R) ->
      case R of
        {_, {v, Name}} ->
          case var:is_existv(VarPid, Name) of
            true -> true;
            _Else -> false
          end;
        _ -> false
        end
    end,
    Str=lists:flatten(Strr),
    Aa=re:run(Strr, "\\(", [global]), %% chek for even braces
    Bb=re:run(Strr, "\\)", [global]),
    BoolA=Aa==Bb andalso Aa==nomatch,
    BoolB=Aa/=nomatch andalso Bb/=nomatch,
    Brace_res=if 
      BoolA==true ->
        eq;
      BoolB==true ->
        Left=length(lists:flatten(element(2, Aa))),
        Right=length(lists:flatten(element(2, Bb))),
        case Left==Right of
          true ->
            eq;
          false ->
            nop
        end;
      true ->
        nop
    end,
    case Brace_res==eq of
      false ->
        gen_server:cast(MP, {StringId, {error, wrong_brace_num}}),
        keep_state_and_data;
      true ->
        {match, Vara}=re:run(Str, "[A-Z|_]+", [global]),
        Varb=lists:flatten(Vara),
        Varv=[{X+1, {v,lists:sublist(Str, X+1, Y)}}|| {X, Y} <- Varb],
        VnameBool=case length(Varv) of
                    1 ->
                      true;
                    _More ->
                      lists:foldl(fun(A, Acc)->A and Acc end, true, [Fn(X)||X <-tl(Varv)])
                  end,
        %% extract negative numbers(with unary minus)
        {Str2, Extract}=replacevar(Str, Varv, Str),
        N=re:run(Extract, "[-+*/=(]-[0-9]+\\.[0-9]+e[0-9]+|[-+*/=(]-[0-9]+\\.[0-9]+e-[0-9]+|[-+*/=(]-[0-9]+\\.[0-9]+|[-+*/=(]-[0-9]+", [global]),
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
        R=re:run(Extract2, "[0-9]+\\.[0-9]+e[0-9]+|[0-9]+\\.[0-9]+e-[0-9]+|[0-9]+\\.[0-9]+|[0-9]+", [global]),
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
        case VnameBool of
          false ->
            gen_server:cast(MP, {StringId, {error, var_not_bound}}),
            keep_state_and_data;
          true ->
            {next_state, undef, {VarPid, Str4, Insert, [], 0, StringId, MP}, {next_event, internal, []}}
        end 
    end.

undef(internal, _Data, {VarPid, Str, Ins, [], LRBr, SId, MP}) ->
    [{A, B}|Ins2]=Ins,
    [Q,Z|T2]=Str,
    Bool=A==v,
    Booool=[Q, Z]=="v=",
    R=Bool and Booool,
    case R of
      true ->
        var:initv(VarPid, B),
        [P|Str2]=T2,
        {next_state, lbr, {VarPid, Str2, Ins2, [{A, B}, Z], LRBr, SId, MP}, [{next_event, internal, P}]};
      _ ->
        gen_server:cast(MP, {SId, {error, wrong_begin}}),
        keep_state_and_data
    end;

undef(cast, stop, Data) ->
    {stop, normal, Data}.

lbr(internal, Data, {VarPid, Str, Ins, Aout, LRBr, SId, MP}) when Data == $- ->
  [H|T]=Str,
  {What, _}=hd(Ins),
  Bool=(H == $v andalso What == v) orelse H == $(,
  case Bool of
    false ->
      gen_server:cast(MP, {SId, {error, wrong_uminus}}),
      keep_state_and_data;
    true -> %% var or (
      {keep_state, {VarPid, T, Ins, [Aout,$-], LRBr, SId, MP}, [{next_event, internal, H}]}
  end;

lbr(internal, Data, {VarPid, Str, Ins, Aout, LRBr, SId, MP}) when Data == $v ->
  [{A, B}|TI]=Ins,
  Add=case A of
    n ->
      {n, handle_num(VarPid, B)};
    v ->
      {v, B}
    end,
  case Str of
    [] ->
      StrId=gen_server:call(VarPid, {sets, lists:flatten([Aout, Add])}),
      gen_server:cast(MP, {SId, StrId}),
      keep_state_and_data;
    [H|T] ->
      {next_state, oper, {VarPid, T, TI, [Aout,Add], LRBr, SId, MP}, {next_event, internal, H}}
  end;

lbr(internal, Data, {VarPid, Str, Ins, Aout, LRBr, SId, MP}) when Data == $(->
  [H|T]=Str,
  {keep_state, {VarPid, T, Ins, [Aout, $(], LRBr+1, SId, MP}, [{next_event, internal, H}]};

lbr(internal, _Data, {_VarPid, _Str, _Ins, _Aout, _LRBr, SId, MP}) ->
  gen_server:cast(MP, {SId, {error, lbr_bad_number}}),
  keep_state_and_data.

oper(internal, Data, {VarPid, Str, Ins, Aout, LRBr, SId, MP}) when Data ==$+ orelse Data ==$- orelse Data ==$* orelse Data ==$/ ->
  [H|T]=Str,
  {next_state, rbr, {VarPid, T, Ins, [Aout,Data], LRBr, SId, MP}, [{next_event, internal, H}]};

oper(internal, Data, {VarPid, Str, Ins, Aout, LRBr, SId, MP}) when Data == $) ->
  case Str of
    [] ->
      StrId=gen_server:call(VarPid, {sets, lists:flatten([Aout, $)])}),
      gen_server:cast(MP, {SId, StrId}),
      keep_state_and_data;
    [H|T] ->
      {keep_state, {VarPid, T, Ins, [Aout,$)], LRBr, SId, MP}, {next_event, internal, H}}
  end;

oper(internal, _Data, {_VarPid, _Str, _Ins, _Aout, _LRBr, SId, MP}) ->
  gen_server:cast(MP, {SId, {error, oper_bad_number}}),
  keep_state_and_data.


rbr(internal, Data, {VarPid, Str, Ins, Aout, LRBr, SId, MP}) when Data == $- ->
  [H|T]=Str,
  {What, _}=hd(Ins),
  Bool=H == $v andalso What == v,
  case Bool of
    false ->
      gen_server:cast(MP, {SId, {error, wrong_uminus}}),
      keep_state_and_data;
    true -> %% var 
      {keep_state, {VarPid, T, Ins, [Aout,$-], LRBr, SId, MP}, [{next_event, internal, H}]}
  end;

rbr(internal, Data, {VarPid, Str, Ins, Aout, LRBr, SId, MP}) when Data == $v ->
  [{A, B}|TI]=Ins,
  Add=case A of
    n ->
      {n, handle_num(VarPid, B)};
    v ->
      {v, B}
    end,
  case Str of
    [] ->
      StrId=gen_server:call(VarPid, {sets, lists:flatten([Aout, Add])}),
      gen_server:cast(MP, {SId, StrId}),
      keep_state_and_data;
    [H|T] ->
      {next_state, oper, {VarPid, T, TI, [Aout,Add], LRBr, SId, MP}, {next_event, internal, H}}
  end;

rbr(internal, Data, {VarPid, Str, Ins, Aout, LRBr, SId, MP}) when Data == $( ->
  {next_state, lbr, {VarPid, Str, Ins, Aout, LRBr, SId, MP}, {next_event, internal, Data}};

rbr(internal, _Data, {_VarPid, _Str, _Ins, _Aout, _LRBr, SId, MP}) ->
  gen_server:cast(MP, {SId, {error, rbr_bad_number}}),
  keep_state_and_data.

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
               lists:sublist(F, 1, Range);
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
                 {lists:duplicate(abs(D), $0)++Med, 0};
               true ->
                 {Med++lists:duplicate(abs(R), $0), D}
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
  