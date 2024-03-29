%%%-------------------------------------------------------------------
%%% @author mt
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% gets the file from FS(local) or inet and initiates calculating
%%%
%%% @end
%%% Created : 05. июль 2022 17:20
%%%-------------------------------------------------------------------
-module(task_handler).
-author("mt").

-behaviour(gen_statem).

%% API
-export([start_link/1, errhandle/3, extract/3, condt/3, expression/3, calc_stage/3, start_calc/3]).

%% gen_statem callbacks
-export([init/1, format_status/2, terminate/3, code_change/4, callback_mode/0]).

-record(th_state, {vp, fdata, blanks, range, aout, tid, addr, fname}).

%% datatype while, goto, do, dowhile, str
%% what while_r, str_data, Ngoto

%% vp VarPid
%% fdata downloaded file as list of strings
%% blanks compiled re pattern for deleting blanks(cr lf tab space)
%% range precision range
%% aout list of vars, that has a result
%% str_l list of str_r (Pids preproc&calculate and/or condition data)
%% count string counter
%% pcount count of process lines, position pointer
%% tid task id

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start_link({Taskdata, Task}) ->
  gen_statem:start(?MODULE, {Taskdata, Task}, []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init({#{addr:=Addr, fname:=Fname, tid:=Tid}, Task}) -> %% TH_struct=#{addr=> {"localhost"}, fname => Value, tid => Tid}
  process_flag(trap_exit, true),
  {ok, Blanks}=re:compile([$[, 8, 9, 10, 13, "\s", "\t", "\r", $], $+]),
  {ok, extract, #th_state{tid=Tid, blanks=Blanks, fdata=Task, addr=Addr, fname=Fname}, {next_event, internal, []}}.

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() ->
  state_functions.

%% @private
%% @doc Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
format_status(_Opt, [_PDict, _StateName, _State]) ->
  Status = some_term,
  Status.

%% @doc 1-st string is R=NN A=(Var, Var ...), where NN integer()>=0
%% condition has two modes:
%% while
%% ?VAR[>|>=|<|=<|==]VAR
%% ...
%% ?
%% do while
%% ??
%% ...
%% ??VAR[>|>=|<|=<|==]VAR

extract(internal, [], State) ->
  [LH|LT]=string:split(State#th_state.fdata, "\n", all),
  Rget=re:run(LH, "R=[0-9]+"),
  Aget=re:run(LH, "A=\\([A-Z|_|\s]+\\)"),
  Bool={Rget==nomatch, Aget==nomatch},
  case Bool of
    {false, false} ->
      {match, Rdata}=Rget,
      {match, [{B, L}]}=Aget,
      [{RX, RL}]=Rdata,
      Rp=list_to_integer(lists:sublist(LH, RX+3, RL-2)),
      Alst=lists:sublist(LH, B+4, L-4),
      ALst=string:split(Alst, " ", all),
      [H|T]=[re:replace(Ll, State#th_state.blanks, "", [global, {return, list}])||Ll <- LT],
      {ok, VarPid}=gen_server:start(var, Rp, []),
      Lines=trace_conds([H|T]), %% #{key:stringpos, {while_|dowhile_|ggoto}}
      case is_map(Lines) of
        true ->  {next_state, condt, {State#th_state{fdata=T, vp=VarPid, range=Rp, aout=ALst}, Lines, 1, 0}, {next_event, internal, H}};
        false -> 
          D={State#th_state.tid, State#th_state.addr, State#th_state.fname, {error, Lines}},
          g_repo:aout(D),
          keep_state_and_data
          %% {next_state, errhandle, {State, [], 0, 0}, {next_event, internal, {error, Lines}}}
      end;
    {true, true} ->
      D={State#th_state.tid, State#th_state.addr, State#th_state.fname, {error, {no_range, no_aout}}},
      g_repo:aout(D),
      keep_state_and_data;
      %% {next_state, errhandle, {State, [], 0, 0}, {next_event, internal, {error, {no_range, no_aout}}}};
    _Other ->
      Answer=case element(1, Bool) of
               true ->
                 no_range;
               false ->
                 no_aout
             end,
      D={State#th_state.tid, State#th_state.addr, State#th_state.fname, {error, Answer}},
      g_repo:aout(D),
      keep_state_and_data
      %% {next_state, errhandle, {State, [], 0, 0}, {next_event, internal, {error, Answer}}}
  end;

extract(cast, stop, State) ->
  {stop, normal, State};

extract(info, {'EXIT', _, _}, State) ->
  {stop, shutdown, State}.

condt(internal, [], State) ->
  {next_state, calc_stage, State, {next_event, internal, []}};

condt(internal, "?", {State, Lines, Cnt, Pcnt}) -> %% ? while end
  NewCount=Cnt+1,
  BoolS=State#th_state.fdata == [],
  [HL|TL]=case BoolS of
            true -> [[]|[]];
            false -> State#th_state.fdata
          end,
  Is_cond=re:run(HL, "\\?")==nomatch,
  case Is_cond of
    true -> {keep_state, {State#th_state{fdata=TL}, Lines, NewCount, Pcnt}, {next_event, internal, HL}};
    false -> {next_state, expression, {State#th_state{fdata=TL}, Lines, NewCount, Pcnt}, {next_event, internal, HL}}
  end;

condt(internal, "??", {State, Lines, Cnt, Pcnt}) ->
  NewCount=Cnt+1,
  BoolS=State#th_state.fdata == [],
  [HL|TL]=case BoolS of
            true -> [[]|[]];
            false -> State#th_state.fdata
          end,
  Is_cond=re:run(HL, "\\?")==nomatch,
  case Is_cond of
    true -> {keep_state, {State#th_state{fdata=TL}, Lines, NewCount, Pcnt}, {next_event, internal, HL}};
    false -> {next_state, expression, {State#th_state{fdata=TL}, Lines, NewCount, Pcnt}, {next_event, internal, HL}}
  end;

condt(internal, [$?, $?|Str], {State, Lines, Cnt, Pcnt}) ->%% dowhile
  NewCount=Cnt+1,
  BoolS=State#th_state.fdata == [],
  [HL|TL]=case BoolS of
            true -> [[]|[]];
            false -> State#th_state.fdata
          end,
  Is_cond=re:run(HL, "\\?")==nomatch,
  While_correct=re:run(Str, "[A-Z|_]+", [global]),
  case While_correct of
    {match, [[{A, B}], [{F, D}]]} ->
      [Var1, Var2]=[lists:sublist(Str, A+1, B), lists:sublist(Str, F+1, D)],
      Str2=re:replace(Str, Var1, "", [{return, list}]),
      Str3=re:replace(Str2, Var2, "", [{return, list}]),
      C=case Str3 of
          "==" -> eq;
          ">=" -> eqgt;
          "=<" -> eqlt;
          ">" -> gt;
          "<" -> lt;
          "<>" -> neq;
          _NotC -> wrong
        end,
      C_bool=C==wrong,
      case C_bool of
        false -> %% correct cond
          CurPos=maps:get(Cnt, Lines),
          Pos=maps:get(ngoto, CurPos),
          NewEl=#{what => dowhile_, var1 => Var1, var2 => Var2, op => C, ngoto => Pos},
          NewLines=maps:put(Cnt, NewEl, maps:remove(Cnt, Lines)),
          V1=case var:is_existv(State#th_state.vp, Var1) of
            true -> true;
            _Or1 -> false
          end,
          V2=case var:is_existv(State#th_state.vp, Var2) of
            true -> true;
            _Or2 -> false
          end,
          BoolVar=V1 andalso V2,
          case BoolVar of
            true ->
              case Is_cond of
                true ->  {keep_state, {State#th_state{fdata=TL}, NewLines, NewCount, Pcnt}, {next_event, internal, HL}};
                false ->  {next_state, expression, {State#th_state{fdata=TL}, NewLines, NewCount, Pcnt}, {next_event, internal, HL}}
              end;
            _NoVar ->
              {next_state, errhandle, {State, Lines, NewCount, Pcnt}, {next_event, internal, {error, wrong_var_in_cond, Cnt}}}
          end;
        true ->
          {next_state, errhandle, {State, Lines, NewCount, Pcnt} , {next_event, internal, [error, in_cond, Cnt]}}
        end;
        _NotMatch ->
          {next_state, errhandle, {State, Lines, NewCount, Pcnt}, {next_event, internal, [error, in_cond, Cnt]}}
  end;

condt(internal, [$?|Str], {State, Lines, Cnt, Pcnt}) ->%% while
  NewCount=Cnt+1,
  BoolS=State#th_state.fdata == [],
  [HL|TL]=case BoolS of
            true -> [[]|[]];
            false -> State#th_state.fdata
          end,
  Is_cond=re:run(HL, "\\?")==nomatch,
  While_correct=re:run(Str, "[A-Z|_]+", [global]),
  case While_correct of
    {match, [[{A, B}], [{F, D}]]} ->
      [Var1, Var2]=[lists:sublist(Str, A+1, B), lists:sublist(Str, F+1, D)],
      Str2=re:replace(Str, Var1, "", [{return, list}]),
      Str3=re:replace(Str2, Var2, "", [{return, list}]),
      C=case Str3 of
          "==" -> eq;
          ">=" -> eqgt;
          "=<" -> eqlt;
          ">" -> gt;
          "<" -> lt;
          "<>" -> neq;
          _NotC -> wrong
        end,
      C_bool=C==wrong,
      case C_bool of
        false -> %% correct cond
          CurPos=maps:get(Cnt, Lines),
          Pos=maps:get(ngoto, CurPos),
          NewEl=#{what => while_, var1 => Var1, var2 => Var2, op => C, ngoto => Pos},
          NewLines=maps:put(Cnt, NewEl, maps:remove(Cnt, Lines)),
          V1=case var:is_existv(State#th_state.vp, Var1) of
            true -> true;
            _Else1 -> false
          end,
          V2=case var:is_existv(State#th_state.vp, Var2) of
            true -> true;
            _Else2 -> false
          end,
          BoolVar=V1 andalso V2,
          case BoolVar of
            true ->
              case Is_cond of
                true -> {keep_state, {State#th_state{fdata=TL}, NewLines, NewCount, Pcnt}, {next_event, internal, HL}};
                false -> {next_state, expression, {State#th_state{fdata=TL}, NewLines, NewCount, Pcnt}, {next_event, internal, HL}}
              end;
            _NoVar ->
              {next_state, errhandle, {State, Lines, NewCount, Pcnt}, {next_event, internal, {error, wrong_var_in_cond, Cnt}}}
          end;
    true -> %% wrong cond
      {next_state, errhandle, {State, Lines, Cnt, Pcnt}, {next_event, internal, [error, in_cond, Cnt]}}
  end;
  _Notmatch ->
    {next_state, errhandle, {State, Lines, Cnt, Pcnt}, {next_event, internal, [error, in_cond, Cnt]}}
  end;

condt(internal, _Data, State) when State#th_state.fdata==[] ->
  {next_state, calc_stage, State, {next_event, internal, []}};

condt(internal, Data, State) ->
  {next_state, expression, State, {next_event, internal, Data}};

condt(cast, stop, State) ->
  {stop, normal, State};

condt(info, {'EXIT', _, _}, State) ->
  {stop, shutdown, State}.

expression(internal, [$?|T], State) ->
  {next_state, condt, State, {next_event, internal, [$?|T]}};

expression(internal, [], State) ->
  {next_state, calc_stage, State, {next_event, internal, []}};

expression(internal, _Data, {State, Lines, Cnt, Pcnt}) when State#th_state.fdata==[] ->
  {next_state, calc_stage, {State, Lines, Cnt, Pcnt}, {next_event, internal, []}};

expression(internal, Str, {State, Lines, Cnt, Pcnt}) ->
  NewPC=Pcnt+1,
  NStr=var:set_task_str(State#th_state.vp, Str),
  {ok, Pid}=gen_statem:start(preproc, {State#th_state.vp, NStr, self()}, []),
  NewEl=#{what => str, pstr => NStr, ppid => Pid},
  NewLines=maps:put(Cnt, NewEl, Lines),
  {keep_state, {State, NewLines, Cnt, NewPC}};

expression(cast, stop, State) ->
  {stop, normal, State};

expression(info, {'EXIT', _, _}, State) ->
  {stop, shutdown, State};

expression(cast, Data, {State, Lines, Cnt, Pcnt}) ->
  case Data of
    {Sid, {error, Err}} ->
      {next_state, errhandle, {State, Lines, Cnt, Pcnt}, {next_event, internal, [error, in_expr, [Sid, {error, Err}], Cnt]}};
    {_Sid, StrId} ->
      El=maps:get(Cnt, Lines),
      NewEl=El#{what => str, pstr => StrId},
      NewLines=maps:put(Cnt, NewEl, maps:remove(Cnt, Lines)),
      BoolS=State#th_state.fdata == [],
      [HL|TL]=case BoolS of
                true -> [[]|[]];
                false -> State#th_state.fdata
              end,
      Is_cond=re:run(HL, "\\?")==nomatch,
      case Is_cond of
        true -> {next_state, condt, {State#th_state{fdata=TL}, NewLines, Cnt+1, Pcnt}, {next_event, internal, HL}};
        false -> {keep_state, {State#th_state{fdata=TL}, NewLines, Cnt+1, Pcnt}, {next_event, internal, HL}}
      end;
    Mistake->
      {next_state, errhandle, {State, Lines, Cnt, Pcnt}, {next_event, internal, [error, in_expr, Mistake, Cnt]}}
  end.

calc_stage(internal, [], {State, Lines, Cnt, Pcnt}) ->
  NewLines=startcalc(State#th_state.vp, Lines),
  {keep_state, {State, NewLines, Cnt, Pcnt, Pcnt}};

calc_stage(internal, [], State) ->
  {next_state, errhandle, {State, [], [], []}, {next_event, internal, [error, wrong_condition_set]}};

calc_stage(cast, stop, State) ->
  {stop, normal, State};

calc_stage(info, {'EXIT', _, _}, State) ->
  {stop, shutdown, State};

calc_stage(cast, Data, {State, Lines, Cnt, Pcnt, Pcnt_}) ->
  case Data of
    {_SId, ok} ->
      NewPC=Pcnt_-1,
      case NewPC of
        0 ->
          {next_state, start_calc, {State, Lines, maps:size(Lines), 1}, {next_event, internal , []}};
        _Other ->
          {keep_state, {State, Lines, Cnt, Pcnt, NewPC}}
      end;
    {SId, _Error} ->
      {next_state, errhandle, {State, Lines, Cnt, Pcnt_}, {next_event, internal, [error, in_calc, SId]}}
  end.

%%result
start_calc(internal, [], {State, Lines, Cnt, Pcnt}) when Cnt < Pcnt ->
  Aout=[{Vname, var:getvar(State#th_state.vp, Vname)}|| Vname <- State#th_state.aout],
  {next_state, errhandle, {State, Lines, Cnt, Pcnt}, {next_event, internal, Aout}};

start_calc(internal, [], {State, Lines, Cnt, Pcnt}) ->
  Boool=Cnt >= Pcnt,
  case Boool of
    true ->
      Entry=maps:get(Pcnt, Lines),
  %% datatype while, ggoto, dowhile, str
      W=maps:get(what, Entry),
      case W of
        ggoto ->
          NewPC=maps:get(ngoto, Entry),
          {keep_state, {State, Lines, Cnt, NewPC}, {next_event, internal , []}};
        while_ ->
          Var1=var:getvar(State#th_state.vp, maps:get(var1, Entry)),
          Var2=var:getvar(State#th_state.vp, maps:get(var2, Entry)),
      %% eq eqgt eqlt gt lt neq
          Op=maps:get(op, Entry),
          Bool=case Op of
                eq -> Var1==Var2;
                eqgt -> Var1>=Var2;
                eqlt -> Var1=<Var2;
                gt -> Var1 > Var2;
                 lt -> Var1 < Var2;
                neq -> Var1 /= Var2
           end,
          NewPC=case Bool of
              false -> maps:get(ngoto, Entry);
              true -> Pcnt+1
            end,
          {keep_state, {State, Lines, Cnt, NewPC}, {next_event, internal, []}};
        dowhile_ ->
          Var1=var:getvar(State#th_state.vp, maps:get(var1, Entry)),
          Var2=var:getvar(State#th_state.vp, maps:get(var2, Entry)),
      %% eq eqgt eqlt gt lt
          Op=maps:get(op, Entry),
          Bool=case Op of
             eq -> Var1==Var2;
             eqgt -> Var1>=Var2;
             eqlt -> Var1=<Var2;
             gt -> Var1 > Var2;
             lt -> Var1 < Var2;
             neq -> Var1 /= Var2
           end,
          NewPC=case Bool of
              true -> maps:get(ngoto, Entry);
              false -> Pcnt+1
            end,
          {keep_state, {State, Lines, Cnt, NewPC}, {next_event, internal, []}};
        str ->
          gen_statem:cast(maps:get(cpid, Entry), get),
          {keep_state, {State, Lines, Cnt, Pcnt}}
      end;
    false ->
      Aout=[{Vname, var:getvar(State#th_state.vp, Vname)}|| Vname <- State#th_state.aout],
      {next_state, errhandle, {State, Lines, Cnt, Pcnt}, {next_event, internal, Aout}}
  end;

start_calc(cast, {A, B}, {State, Lines, Cnt, Pcnt}) ->
  case B of
    zero ->
      {next_state, errhandle, {State, Lines, Cnt, Pcnt}, {next_event, internal, [error, zero, A]}};
    ok ->
      {keep_state, {State, Lines, Cnt, Pcnt+1}, {next_event, internal, []}}
  end;

start_calc(cast, stop, State) ->
  {stop, normal, State};

start_calc(info, {'EXIT', _, _}, State) ->
  {stop, shutdown, State}.

errhandle(internal, Data, {State, Lines, Cnt, _Pnt}) ->
  D={State#th_state.tid, State#th_state.addr, State#th_state.fname, Data},
  g_repo:aout(D),
  %% Bool=lists:member(Staten, [errhandle, start_calc, expression, condt, calc_stage]),
  %% case Bool of
  %%   true ->
      F=fun(El) ->
        W=maps:get(what, El),
        case W of
          str ->
            P=maps:is_key(ppid, El),
            case P of
              true -> gen_statem:stop(maps:get(ppid, El));
              _Po ->
                ok
            end,
            C=maps:is_key(cpid, El),
            case C of
              true -> gen_statem:stop(maps:get(cpid, El));
              _Co ->
                ok
            end;
          _Other ->
            ok
        end
      end,
      case Cnt of
        0 -> ok;
        _Other -> 
          [maps:is_key(X, Lines) andalso F(maps:get(X, Lines))||X <- lists:seq(1, Cnt)]
      end,
    %% false ->
    %%   ok
  %% end,
  gen_server:cast(State#th_state.vp, stop),
  keep_state_and_data;

errhandle(cast, stop, State) ->
  {stop, normal, State};

errhandle(info, {'EXIT', _, _}, State) ->
  {stop, shutdown, State}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%% terminate(_Reason, Staten, {State, Lines, Cnt, _Pcnt}) ->
  
%%   ok;
terminate(_Reason, _State, _Data) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #th_state{}, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

startcalc(VP, Lines) ->
  Size=maps:size(Lines),
  sc(VP, Lines, Size+1, 1, #{}).

sc(VP, Lines, Sz, Num, New) ->
  case Num of
    Sz -> New;
    _Other ->
      El=maps:get(Num, Lines),
      W=maps:get(what, El),
      NewEl=case W of
              str ->
                {ok, Pid}=gen_statem:start(calculate, {VP, maps:get(pstr, El), self()}, []),
                maps:put(cpid, Pid, El);
              _Other2 ->
                El
            end,
      NewLines=maps:put(Num, NewEl, New),
      sc(VP, Lines, Sz, Num+1, NewLines)
  end.

trace_conds(L) ->
  tc(#{}, [], L, 1).

tc(M, [], [], _Any) -> M;
tc(_M, Any, [], _N) when Any /=[] -> cond_not_closed;
tc(M, Left, [H|T], N) ->
  Bool=Left==[],
  [Hl|Tl]=case Bool of
    true -> [[]|[]];
    false -> Left
  end,
  case H of
    "??" ->
      tc(M, [{ggoto, N}|Left], T, N+1);
    [$?, $?|_OtherDo] ->
      case Hl of
        [] -> baddowhile_bgn;
        _Other ->
          case element(1, Hl) of
            ggoto ->
              A=element(2, Hl),
              Modify=#{what=>ggoto, ngoto=> A+1},
              NewEl=#{what=>dowhile_, ngoto=> A+1},
              tc(maps:put(N, NewEl, maps:put(A, Modify, M)), Tl, T, N+1);
            _WrongDo ->
              baddowhile
          end
      end;
    "?" ->
      case Hl of
        [] -> badwhile_bgn;
        _Other_ ->
          case element(1, Hl) of
            while_ ->
              A=element(2, Hl),
              Modify=#{what=> while_, ngoto=> N+1},
              NewEl=#{what=>ggoto, ngoto=> A},
              tc(maps:put(N, NewEl, maps:put(A, Modify, M)), Tl, T, N+1);
            _WrongWh ->
              badwhile
          end
      end;
    [$?|_OtherWh] ->
      tc(M, [{while_, N}|Left], T, N+1);
    _NotCond ->
      tc(M, Left, T, N+1)
  end.


