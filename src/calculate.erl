%%%-------------------------------------------------------------------
%%% @author midtower@yandex.ru
%%% @copyright (C) 2022,
%%% @doc
%%% this module preparing model and evaluating the string
%%% @end
%%% Created : 03. май 2022 19:00
%%%-------------------------------------------------------------------
-module(calculate).
-author("mt").

-behaviour(gen_statem).
-export([callback_mode/0, start/1, init/1]).
-export([extract/3, get_br/3, muldiv/3, exe/3, terminate/3]).


callback_mode() ->
  state_functions.

start({VarPid, StringId, MPid}) ->
  gen_statem:start(?MODULE, {VarPid, StringId, MPid}, []).

terminate(_Reason, _State, _Data) ->

  ok.

init({VarPid, SId, MP}) ->
  process_flag(trap_exit, true),
  Str=var:get_task_str(VarPid, SId),
  {ok, extract, {Str, SId, VarPid, MP}, [{next_event, internal, []}]}.

%% States:
%% extract handles unary minus and extracts lbrackets position
%% get_br it cuts attaches and instead inserts {f, FunPid}, that returns muldiv
%% muldiv each iteration creating {f, FunPid} instead of (a operator b), where operator is * / + -

%% corner case 1
extract(internal, _, {[{v, Vname}, $=, {Type, Val}], SId, VarPid, MP}) ->
  gen_server:cast(MP, {SId, ok}),
  {next_state, exe, {[{v, Vname}, $=, {Type, Val}], SId, VarPid, MP}};

%% corner case 1 neg
extract(internal, _, {[{v, Vname}, $=, $-, {v, Val}], SId, VarPid, MP}) ->
  S=spawn(bfun, uminus, {{v, Val}, VarPid}),
  gen_server:cast(MP, {SId, ok}),
  {next_state, exe, {[{v, Vname}, $=, $-, {f, S}], SId, VarPid, MP}};

extract(internal, _, {Str, SId, VarPid, MP}) ->
  L=u_minus(Str, [], v, VarPid),
  LBr=get_lbr(L), %% backward order is normal
  R=list_to_integer("1"++lists:duplicate(gen_server:call(VarPid, range), $0)),
  {next_state, get_br, {R, L, SId, VarPid, LBr, MP}, [{next_event, internal, []}]};

extract(cast, stop, State) ->
  {stop, normal, State}.

%% catch 1
get_br(internal, _, {_R, [{v, Vname}, $=, {What, S}], SId, VarPid, [], MP})  ->
  %% to Master
  gen_server:cast(MP, {SId, ok}),
  {next_state, exe, {{v, Vname}, $=, {What, S}, SId, VarPid, MP}};

%% here I normalizing operations: a-b to a+(-B) and a/b to a*(1/b) for parallel executing

get_br(internal, _, {R, Str, SId, VarPid, LBr, MP}) ->%cut string
  Str1=u_minus(Str, [], f, VarPid),
  case LBr of
    [] -> % no braces
      Str2M=reverse_minus(Str1, VarPid),
      Str2=reverse_division(Str2M, VarPid, R),
      {next_state, muldiv, {R, [], SId, VarPid, LBr, Str2, MP}, [{next_event, internal, $*}]};
    _Other ->
      Hlbr=hd(LBr),
      RBr=get_rbr(Str1, 1, 0),
      Len=RBr-Hlbr+1,
      L=lists:sublist(Str, Hlbr, Len),
      L2=L--"()",
      Str2=lists:flatten([lists:sublist(Str1, 1, Hlbr-1), lists:sublist(Str1, RBr+1, length(Str1)-RBr)]),
      L3=reverse_minus(L2, VarPid),
      {next_state, muldiv, {R, Str2, SId, VarPid, LBr, L3, MP}, [{next_event, internal, $*}]}
  end;

get_br(cast, stop, State) ->
  {stop, normal, State}.

muldiv(internal, Op, {R, Str, SId, VarPid, LBr, L, MP}) ->
  %% general handling of * +
  BoolM=lists:member(Op, L), %% * +
  case BoolM of
    true ->
      Aout=chg_pair(L, Op, [], VarPid, R),%cutting off the pairs
      {keep_state, {R, Str, SId, VarPid, LBr, Aout, MP}, [{next_event, internal, Op}]};
    false ->
      NxOp= case Op of
              $* -> $+;
              $+ -> $?
            end,
      case NxOp of
        $? -> %% end of current bracket or whole line
          BoolLbr=LBr=:=[],
          case BoolLbr of
            true ->% catch 2
              Len=length(L),
              case Len of
                3 ->
                  gen_server:cast(MP, {SId, ok}),
                  {next_state, exe, {L, SId, VarPid, MP}};
                _Other ->
                  gen_server:cast(MP, {SId, error}),
                  keep_state_and_data
              end;
            false ->
              [H|Tlbr]=LBr,
              Len=length(Str)-H+1,
              Str2=lists:flatten([lists:sublist(Str, 1, H-1), L, lists:sublist(Str, H, Len)]),
              Str3=u_minus(Str2, [], f, VarPid),
              {next_state, get_br, {R, Str3, SId, VarPid, Tlbr, MP}, [{next_event, internal, []}]}
          end;
        _Other ->
          {keep_state, {R, Str, SId, VarPid, LBr, L, MP}, [{next_event, internal, NxOp}]}
      end
  end;

muldiv(cast, stop, State) ->
  {stop, normal, State}.

exe(cast, stop, {Str, SId, VP, Vname, MP}) ->
  {stop, normal, {Str, SId, VP, Vname, MP}};

exe(cast, get, {[{v, Vname}, $=, {What, Val}], SId, VarPid, MP}) ->
  Answer=case What of
           n ->
              Val;
           v ->
             var:getvar(VarPid, Val);
           f ->
             get_answer(Val)
  end,
  var:setvar(VarPid, Vname, Answer),
  Bool=var:is_existv(VarPid, "zero"),
  case Bool of
    true ->
      gen_server:cast(MP, {SId, zero});
    _Ok ->
      gen_server:cast(MP, {SId, ok})
  end,
  keep_state_and_data;

exe(cast, stop, {Str, SId, VarPid, MP}) ->
  clear_data(Str),
  {stop, normal, {Str, SId, VarPid, MP}}.

get_lbr(Str)->
  ex(Str, [], 1).
ex([], LBr, _) -> LBr;
ex(Str, LBr, C) ->
  [H|T]=Str,
  case H of
    $( -> ex(T, [C|LBr], C+1);
    _Other -> ex(T, LBr, C+1)
  end.

u_minus([A, $-, {What, B}|T], Acc, What, VarPid) when A == $= orelse A == $- orelse A == $+ orelse A == $* orelse A == $/ orelse A == $( orelse A == $= ->
  S=spawn(bfun, uminus, [{{What, B}, VarPid}]),
  u_minus(T, [{f, S}, A|Acc], What, VarPid);
u_minus([A|T], Acc, What, VarPid) -> u_minus(T,[A|Acc], What, VarPid);
u_minus([], Acc, _What, _VarPid) -> lists:reverse(lists:flatten([Acc])).

chg_pair([{X, A}, Sym, {Y, B}|T], Sym, Acc, VarPid, Range) ->
  S=case Sym of
      $- ->
        Med=spawn(bfun, uminus, [{{Y, B}, VarPid}]),
        spawn(bfun, bfun, [{{X, A}, {f, Med}, VarPid, $+, Range}]);
      _Other ->
        spawn(bfun, bfun, [{{X, A}, {Y, B}, VarPid, Sym, Range}])
    end,
  chg_pair(T, Sym, [{f,S}|Acc], VarPid, Range);
chg_pair([H|T], Sym, Acc, VarPid, Range) ->
  chg_pair(T, Sym, [H|Acc], VarPid, Range);
chg_pair([], _Sym, Acc, _VarPid, _Range) ->
  lists:reverse(lists:flatten([Acc])).

get_rbr([$)|T], Num, _Aout) -> get_rbr(T, Num+1, Num);
get_rbr([_H|T], Num, Aout) -> get_rbr(T, Num+1, Aout);
get_rbr([], _Num, Aout) -> Aout.

clear_data({What, A}) ->
  case What of
    f ->
      A ! stop;
    _Any -> ok
  end;
clear_data([{What, A}|T]) ->
  case What of
    f ->
      A ! stop;
    _Any -> ok
  end,
  clear_data([T]).

get_answer(Pid) ->
  My=self(),
  Pid ! {get, My},
  {My, Answer}=receive
                 Data -> Data
               end,
  Answer.

reverse_minus(L, VP) ->
  rm_(L, VP, []).

rm_([{X, Y}, $-,{A, B}|L], VP, Acc) ->
  S=spawn(bfun, uminus, [{{A, B}, VP}]),
  rm_([{f, S}|L], VP, [$+, {X, Y}|Acc]);
rm_([Data|L], VP, Acc) ->
  rm_(L, VP, [Data|Acc]);
rm_([], _VP, Acc) ->
  lists:reverse(lists:flatten(Acc)).

reverse_division(L, VP, Range) ->
  rd_(L, VP, [], Range).

rd_([{X, Y}, $/, {A, B}|L], VP, Acc, Range) ->
  S=spawn(bfun, bfun, [{{n, Range}, {A, B}, VP, $/, Range}]),
  rd_([{f, S}|L], VP, [$*, {X, Y}|Acc], Range);
rd_([Data|L], VP, Acc, Range) ->
  rd_(L, VP, [Data|Acc], Range);
rd_([], _VP, Acc, _Range) ->
  lists:reverse(lists:flatten(Acc)).

