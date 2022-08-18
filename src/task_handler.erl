%%%-------------------------------------------------------------------
%%% @author mt
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. июль 2022 17:20
%%%-------------------------------------------------------------------
-module(task_handler).
-author("mt").

-behaviour(gen_statem).

%% API
-export([start/1, afterinit/3, extract/3, condt/3, expression/3, calc_stage/3, start_calc/3]).

%% gen_statem callbacks
-export([init/1, format_status/2, state_name/3, terminate/3, code_change/4, callback_mode/0]).

-record(th_state, {mp, vp, fp, fdata, blanks, range, aout, str_l, goto, count, pcount, tid}).

-record(str_data, {pppid, ppn, cpid}).
-record(while_r, {var1, var2, condition, ngoto}).
-record(str_r, {datatype, what}).
%% datatype while, goto, do, dowhile, str
%% what while_r, str_data, Ngoto

%% mp MasterPid
%% vp VarPid
%% fp file path
%% fdata downloaded file as list of strings
%% blanks compiled re pattern for deleting blanks(cr lf tab space)
%% range precision range
%% aout list of vars, that has a result
%% str_l list of str_r (Pids preproc&calculate and/or condition data)
%% goto temp list for got handling
%% count string counter
%% pcount count of process lines, position pointer
%% tid task id

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start({MasterPid, VarPid, FilePath, TaskId}) ->
  gen_statem:start(?MODULE, {MasterPid, VarPid, FilePath, TaskId}, []).

%% first starts task_handler, next - var

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init({MasterPid, VarPid, FilePath, TaskId}) ->
  process_flag(trap_exit, true),
  {ok, afterinit, #th_state{mp=MasterPid, vp=VarPid, fp=FilePath, tid=TaskId}, [{next_event, internal, []}]}.

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

%% @private
%% @doc There should be one instance of this function for each possible
%% state name.  If callback_mode is state_functions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.
state_name(_EventType, _EventContent, State = #th_state{}) ->
  NextStateName = next_state,
  {next_state, NextStateName, State}.

afterinit(internal, [], State)->
  File=file:read_file(State#th_state.fp),
  case File of
    {ok, Bin} ->
      {ok, Blanks}=re:compile([$[, 8, 9, 10, 13, 32, $], $+]),
      Str=string:split(binary:bin_to_list(Bin), [10], all),
      {next_state, extract, State#th_state{fdata=Str, blanks=Blanks}, {next_event, internal, []}};
    {error, Reason} ->
      gen_server:cast(State#th_state.mp, {State#th_state.tid, [error, fileread, Reason]}),
      {keep_state_and_data}
  end.

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
  [LH|LT]=State#th_state.fdata,
  Rget=re:run(LH, "[0-9]+"),
  Aget=re:run(LH, "[A-Z|_]+", [global]),
  Bool1=Rget==nomatch,
  Bool2=Aget==nomatch,
  case Bool1 and Bool2 of
    true ->
      Answer=case Bool1 of
               true ->
                 no_range;
               false ->
                 no_aout
             end,
      gen_server:cast(State#th_state.mp, {State#th_state.tid, [error, Answer]}),
      keep_state_and_data;
    false ->
      {match, Rdata}=Rget,
      {match, Adata}=Aget,
      Addata=lists:flatten(Adata),
      [{RX, RL}]=Rdata,
      Rp=list_to_integer(lists:sublist(LH, RX+1, RL)),
      AData=lists:sublist(Addata, 3, length(Addata)-2),
      ALst=[lists:sublist(LH, X+1, Y)|| {X, Y} <- AData],
      [H|T]=LT,
      HH=re:replace(H, State#th_state.blanks, "", [{return, list}, global]),
      {next_state, condt, State#th_state{fdata=T, range=Rp, aout=ALst, count=1, pcount=0, str_l=[], goto=[]}, {next_event, internal, HH}}
  end.

condt(internal, "?", State) -> %% ? while end
  NewCount=State#th_state.count+1,
  [NgotoH|NgotoT]=State#th_state.goto,
  [HL|TL]=State#th_state.fdata,
  HLL=re:replace(HL, State#th_state.blanks, "", [{return, list}, global]),
  NStr=[#str_r{datatype=goto, what=NgotoH}|State#th_state.str_l],
  Pos=NewCount-NgotoH -1,
  OldEntry=lists:nth(Pos, State#th_state.str_l),
  Chg=OldEntry#str_r.what,
  Chg2=Chg#while_r{ngoto=NewCount},
  NewEntry=OldEntry#str_r{what=Chg2},
  NewStr=lists:flatten([lists:sublist(NStr, 1, Pos), NewEntry, lists:sublist(NStr, Pos+2, NgotoH)]),
  {keep_state, State#th_state{count=NewCount, goto=NgotoT, fdata=TL, str_l=NewStr}, {next_event, internal, HLL}};

condt(internal, "??", State) ->
  NewCount=State#th_state.count+1,
  [HL|TL]=State#th_state.fdata,
  HLL=re:replace(HL, State#th_state.blanks, "", [{return, list}, global]),
  NewStr=[#str_r{datatype=goto, what=NewCount}|State#th_state.str_l],
  NewGoto=[State#th_state.count|State#th_state.goto],
  {keep_state, State#th_state{goto=NewGoto, count=NewCount, str_l=NewStr, fdata=TL}, {next_event, internal, HLL}};

condt(internal, [$?|Str], State) ->
  NewCount=State#th_state.count+1,
  [HL|TL]=State#th_state.fdata,
  HLL=re:replace(HL, State#th_state.blanks, "", [{return, list}, global]),
      Do_bool=re:run(Str, "\\?")==nomatch,
      While_correct=re:run(Str, "[A-Z|_]+", [global]),
      case While_correct of
        {match, [[{A, B}], [{F, D}]]} ->
          [Var1, Var2]=[lists:sublist(Str, A+1, B), lists:sublist(Str, F+1, D)],
          Str2=re:replace(Str, Var1, "", [{return, list}]),
          Str3=re:replace(Str2, Var2, "", [{return, list}]),
          Str4=case Do_bool of
                 false -> re:replace(Str3, "\\?", "", [{return, list}]);
                 true -> Str3
               end,
          C=case Str4 of
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
              case Do_bool of
                true -> %% while
                  W=#while_r{var1=Var1, var2=Var2, condition=C},
                  NewStr=[#str_r{datatype=while, what=W}|State#th_state.str_l],
                  NewGoto=[State#th_state.count|State#th_state.goto],
                  {keep_state, State#th_state{goto=NewGoto, count=NewCount, str_l=NewStr, fdata=TL}, {next_event, internal, HLL}};
                false -> % dowhile
                  [NgotoH|NgotoT]=State#th_state.goto,
                  W=#while_r{var1=Var1, var2=Var2, condition=C, ngoto=NgotoH},
                  NewStr=[#str_r{datatype=dowhile, what=W}|State#th_state.str_l],
                  {keep_state, State#th_state{goto=NgotoT, count=NewCount, str_l=NewStr, fdata=TL}, {next_event, internal, HLL}}
              end;
            true -> %% wrong cond
              gen_server:cast(State#th_state.mp, {State#th_state.tid, [error, in_cond, State#th_state.count]}),
              keep_state_and_data;
            _Wrong ->
              gen_server:cast(State#th_state.mp, {State#th_state.tid, [error, in_cond, State#th_state.count]}),
              keep_state_and_data
          end;
        _Notmatch ->
          gen_server:cast(State#th_state.mp, {State#th_state.tid, [error, in_cond, State#th_state.count]}),
          keep_state_and_data
  end;

condt(internal, [], State) ->
  {next_state, calc_stage, State, {next_event, internal, []}};

condt(internal, _Data, State) when State#th_state.fdata==[] ->
  {next_state, calc_stage, State, {next_event, internal, []}};

condt(internal, Data, State) ->
  {next_state, expression, State, {next_event, internal, Data}};

condt(cast, stop, State) ->
  {stop, normal, State}.

expression(internal, [$?|T], State) ->
  {next_state, condt, State, {next_event, internal, [$?|T]}};

expression(internal, [], State) ->
  {next_state, calc_stage, State, {next_event, internal, []}};

expression(internal, _Data, State) when State#th_state.fdata==[] ->
  {next_state, calc_stage, State, {next_event, internal, []}};

expression(internal, Data, State) ->
  Str=re:replace(Data, State#th_state.blanks, "", [{return, list}, global]),
  NStr=var:set_task_str(State#th_state.vp, Str),
  {ok, Pid}=gen_statem:start(preproc, {State#th_state.vp, NStr}, []),
  Strdata=#str_data{pppid=Pid},
  NewStr=[#str_r{datatype=str, what=Strdata}|State#th_state.str_l],
  {keep_state, State#th_state{str_l=NewStr}};

expression(cast, stop, State) ->
  {stop, normal, State};

expression(cast, Data, State) ->
  case Data of
    {_Sid, StrId} ->
      [HS|TS]=State#th_state.str_l,
      What=HS#str_r.what,
      NWhat=What#str_data{ppn=StrId},
      NewStr=[HS#str_r{what=NWhat}|TS],
      NCount=State#th_state.count+1,
      [HL|TL]=State#th_state.fdata,
      HLL=re:replace(HL, State#th_state.blanks, "", [{return, list}, global]),
      NewPC=State#th_state.pcount+1,
      {keep_state, State#th_state{count=NCount, str_l=NewStr, fdata=TL, pcount=NewPC}, {next_event, internal, HLL}};
    Mistake->
      gen_server:cast(State#th_state.mp, {State#th_state.tid, [error, in_expr, Mistake, State#th_state.count]}),
      keep_state_and_data
  end.

calc_stage(internal, [], State) when State#th_state.goto == [] ->
  Str_l=lists:reverse(lists:flatten(State#th_state.str_l)),
  F=fun(Str_r) ->
    A=Str_r#str_r.datatype,
    B=Str_r#str_r.what,
    case A of
      str ->
        {ok, Pid}=gen_statem:start(calculate, {State#th_state.vp, B#str_data.ppn}, []),
        NewB=B#str_data{cpid=Pid},
        #str_r{datatype=str, what=NewB};
      _Other ->
        Str_r
    end
    end,
  NewStr=[F(X)|| X <- Str_l],
  {keep_state, State#th_state{str_l=NewStr}};

calc_stage(internal, [], State) ->
  gen_server:cast(State#th_state.mp, {State#th_state.tid, [error, wrong_condition_set]}),
  keep_state_and_data;

calc_stage(cast, stop, State) ->
  {stop, normal, State};

calc_stage(cast, Data, State) ->
  case Data of
    {_SId, ok} ->
      NewPC=State#th_state.pcount-1,
      case NewPC of
        0 ->
          {next_state, start_calc, State#th_state{pcount=1}, {next_event, internal , []}};
        _Other ->
          {keep_state, State#th_state{pcount=NewPC}}
      end;
    {SId, _Error} ->
      gen_server:cast(State#th_state.mp, {State#th_state.tid, [error, in_calc, SId]}),
      keep_state_and_data
  end.

start_calc(internal, [], State) when (State#th_state.count-1) < State#th_state.pcount ->
  Aout=[{Vname, var:getvar(State#th_state.vp, Vname)}|| Vname <- State#th_state.aout],
  gen_server:cast(State#th_state.mp, {State#th_state.tid, ok, Aout}),
  keep_state_and_data;

start_calc(internal, [], State) ->
  Boool=(State#th_state.count-1) >= State#th_state.pcount,
  case Boool of
    true ->
  Entry=lists:nth(State#th_state.pcount, State#th_state.str_l),
  %% datatype while, goto, dowhile, str
  case Entry#str_r.datatype of
    goto ->
      NewPC=Entry#str_r.what,
      {keep_state, State#th_state{pcount=NewPC}, {next_event, internal , []}};
    while ->
      While=Entry#str_r.what,
      Var1=var:getvar(State#th_state.vp, While#while_r.var1),
      Var2=var:getvar(State#th_state.vp, While#while_r.var2),
      %% eq eqgt eqlt gt lt neq
      Bool=case While#while_r.condition of
             eq -> Var1==Var2;
             eqgt -> Var1>=Var2;
             eqlt -> Var1=<Var2;
             gt -> Var1 > Var2;
             lt -> Var1 < Var2;
             neq -> Var1 /= Var2
           end,
      NewPC=case Bool of
              false -> While#while_r.ngoto;
              true -> State#th_state.pcount+1
            end,
      {keep_state, State#th_state{pcount=NewPC}, {next_event, internal, []}};
    dowhile ->
      While=Entry#str_r.what,
      Var1=var:getvar(State#th_state.vp, While#while_r.var1),
      Var2=var:getvar(State#th_state.vp, While#while_r.var2),
      %% eq eqgt eqlt gt lt
      Bool=case While#while_r.condition of
             eq -> Var1==Var2;
             eqgt -> Var1>=Var2;
             eqlt -> Var1=<Var2;
             gt -> Var1 > Var2;
             lt -> Var1 < Var2;
             neq -> Var1 /= Var2
           end,
      NewPC=case Bool of
              true -> While#while_r.ngoto;
              false -> State#th_state.pcount+1
            end,
      {keep_state, State#th_state{pcount=NewPC}, {next_event, internal, []}};
    str ->
      Strdata=Entry#str_r.what,
      gen_statem:cast(Strdata#str_data.cpid, get),
      {keep_state, State}
end;
    false ->
      Aout=[{Vname, var:getvar(State#th_state.vp, Vname)}|| Vname <- State#th_state.aout],
      gen_server:cast(State#th_state.mp, {State#th_state.tid, ok, Aout}),
      keep_state_and_data
  end;

start_calc(cast, {A, B}, State) ->
  case B of
    zero ->
      gen_server:cast(State#th_state.mp, {State#th_state.tid, [error, zero, A]}),
      keep_state_and_data;
    ok ->
      NewPC=State#th_state.pcount+1,
      {keep_state, State#th_state{pcount=NewPC}, {next_event, internal, []}}
  end.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, _State) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #th_state{}, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
