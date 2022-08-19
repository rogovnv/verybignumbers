%%%-------------------------------------------------------------------
%%% @author mt
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% gets the file from FS(local) or inet and initiates calculating
%%% file structure:
%%%
%%% R=Precision:non_neg_integer() A=(List of output variables with a comma)
%%% e. g. R=45 A=(Z, X, _ERT)
%%% Each next string is either condition, or expression
%%% No blank strings
%%% Minimal expression is Var1= Var2/Number operator Var3/Number2
%%% Typical expression isVar1= Expr1/Var2/Number1 operator Expr2/Var3/Number2 with brackets
%%% Bracket can consist minimal expression, no less
%%% You can use unary minus like: -A+-16/(-ZZ*FR_Q)+22.4
%%% Variable names only is in camel case and underscore
%%% Conditions is either like WHILE or DO...WHILE
%%% Cond operators is: == >= =< <>
%%% You can nest it
%%% WHILE is
%%% ?Var1 cond Var2
%%% Expressions/conditions
%%% ?
%%% DO...WHILE is
%%% ??
%%% Expressions/conditions
%%% ??Var1 cond Var2
%%% @end
%%% Created : 05. июль 2022 17:20
%%%-------------------------------------------------------------------
-module(task_handler).
-author("mt").

-behaviour(gen_statem).

%% API
-export([start/1, errhandle/3, afterinit/3, extract/3, extract_i/3, extract_it/3, extract_ir/3, condt/3, expression/3, calc_stage/3, start_calc/3, afterinit_i/3]).

%% gen_statem callbacks
-export([init/1, format_status/2, terminate/3, code_change/4, callback_mode/0]).

-record(th_state, {mp, vp, fp, fdata, blanks, range, aout, goto, tid, sid}).

%% datatype while, goto, do, dowhile, str
%% what while_r, str_data, Ngoto

%% mp MasterPid
%% vp VarPid
%% sid mp socket
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
start([{MasterPid, Src, TaskId}]) ->
  gen_statem:start(?MODULE, {MasterPid, Src, TaskId}, []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init({MasterPid, {SrcType, Src}, TaskId}) ->
  process_flag(trap_exit, true),
  {ok, Blanks}=re:compile([$[, 8, 9, 10, 13, 32, $], $+]),
  case SrcType of
    f ->
      {ok, afterinit, #th_state{mp=MasterPid, fp=Src, tid=TaskId, blanks=Blanks, sid=0}, {next_event, internal, []}};
    i ->
      {ok, afterinit_i, #th_state{mp=MasterPid, tid=TaskId, blanks=Blanks, sid=Src}, {next_event, internal, []}}
  end.%% Src for inet is listening socket

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

afterinit_i(internal, [], State) ->
  {ok, Sock}=gen_tcp:accept(State#th_state.sid),
  inet:setopts(Sock, [list, {active, true}]),
  {next_state, extract_i, State#th_state{fp=Sock, fdata=[]}}.

%% r result
%% t task
%% e eot

extract_i(info, {tcp, _Sock, "r"}, State) ->
  gen_tcp:send(State#th_state.fp, "OK"),
  {next_state, extract_ir, State};

extract_i(info, {tcp, _Sock, "t"}, State) ->
  gen_tcp:send(State#th_state.fp, "OK"),
  {next_state, extract_it, State};

extract_i(info, {tcp, _Sock, _Data}, State) ->
  gen_tcp:close(State#th_state.fp),
  {next_state, afterinit_i, State#th_state{fdata=[]}, {next_event, internal, []}};

extract_i(info, {tcp_closed, _Sock}, State) ->
  gen_server:cast(State#th_state.mp, {State#th_state.tid, [error, tcp_closed]}),
  gen_tcp:close(State#th_state.fp),
  {next_state, afterinit_i, State#th_state{fdata=[]}, {next_event, internal, []}};

extract_i(cast, stop, State) ->
  gen_tcp:close(State#th_state.fp),
  {stop, normal, State}.

extract_it(info, {tcp, _Sock, [$e]}, State) ->
  L=string:split(lists:flatten(lists:reverse(State#th_state.fdata)), [10], all),
  gen_server:cast(State#th_state.mp, {State#th_state.tid, [ok, more]}),
  gen_tcp:close(State#th_state.fp),
  {next_state, extract, State#th_state{fdata=L}, {next_event, internal, []}};

extract_it(info, {tcp, _Sock, Data}, State) ->
  NewData=[Data|State#th_state.fdata],
  {keep_state, State#th_state{fdata=NewData}};

extract_it(info, {tcp_closed, _Sock}, State) ->
  gen_server:cast(State#th_state.mp, {State#th_state.tid, [error, tcp_closed]}),
  gen_tcp:close(State#th_state.fp),
  {next_state, afterinit_i, State#th_state{fdata=[]}, {next_event, internal, []}};

extract_it(cast, stop, State) ->
  gen_tcp:close(State#th_state.fp),
  {stop, normal, State}.

extract_ir(info, {tcp, _Sock, Tnum}, State) ->
  Res=gen_server:call(State#th_state.mp, [result, Tnum]),
  [gen_tcp:send(State#th_state.fp, [L|10])||L<-Res],
  gen_tcp:close(State#th_state.fp),
  gen_statem:cast(State#th_state.mp, res_done),
  {next_state, afterinit_i, State#th_state{fdata=[]}, {next_event, internal, []}};

extract_ir(info, {tcp_closed, _Sock}, State) ->
  gen_server:cast(State#th_state.mp, {State#th_state.tid, [error, tcp_closed]}),
  gen_tcp:close(State#th_state.fp),
  {next_state, afterinit_i, State#th_state{fdata=[]}, {next_event, internal, []}}.

afterinit(internal, [], State)->
  File=file:read_file(State#th_state.fp),
  case File of
    {ok, Bin} ->
      Str=string:split(binary:bin_to_list(Bin), [10], all),
      {next_state, extract, State#th_state{fdata=Str}, {next_event, internal, []}};
    {error, Reason} ->
      gen_server:cast(State#th_state.mp, {State#th_state.tid, [error, {fileread, Reason}]}),
      keep_state_and_data
  end;

afterinit(cast, stop, State) ->
  {stop, normal, State}.

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
      {next_state, errhandle, State, {next_event, internal, [error, Answer]}};
      %% gen_server:cast(State#th_state.mp, {State#th_state.tid, [error, Answer]}),
      %% keep_state_and_data;
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
      {ok, VarPid}=gen_server:start(var, Rp, []),
      Me=self(),
      var:setmp(VarPid, Me),
      Lines=#{},
      {next_state, condt, {State#th_state{fdata=T, vp=VarPid, range=Rp, aout=ALst, goto=[]}, Lines, 0, 0}, {next_event, internal, HH}}
  end;

extract(cast, stop, State) ->
  {stop, normal, State}.

condt(internal, [], State) ->
  {next_state, calc_stage, State, {next_event, internal, []}};

condt(internal, "?", {State, Lines, Cnt, Pcnt}) -> %% ? while end
  NewCount=Cnt+1,
  [NgotoH|NgotoT]=State#th_state.goto,
  BoolS=State#th_state.fdata == [],
  [HL|TL]=case BoolS of
            true -> [[]|[]];
            false -> State#th_state.fdata
          end,
  HLL=re:replace(HL, State#th_state.blanks, "", [{return, list}, global]),
  %% #{W=what, V1=var1, V2=var2, O=op, goto}=maps:get(NgotoH, Lines),
  El=maps:get(NgotoH, Lines),
  %% NewEl=#{what => W, var1 => V1, var2 => V2, op => O, ngoto => NewCount+1},
  NewEl=El#{ngoto => NewCount+2},
  NewLines=maps:put(NgotoH, NewEl, maps:remove(NgotoH, Lines)),
  NewnewEl=#{what => ggoto, ngoto => NgotoH},
  NewnewLines=maps:put(NewCount, NewnewEl, NewLines),
  Is_cond=re:run(HLL, "\\?")==nomatch,
  case Is_cond of
    true -> {keep_state, {State#th_state{goto=NgotoT, fdata=TL}, NewnewLines, NewCount, Pcnt}, {next_event, internal, HLL}};
    false -> {next_state, expression, {State#th_state{goto=NgotoT, fdata=TL}, NewnewLines, NewCount, Pcnt}, {next_event, internal, HLL}}
  end;

condt(internal, "??", {State, Lines, Cnt, Pcnt}) ->
  NewCount=Cnt+1,
  BoolS=State#th_state.fdata == [],
  [HL|TL]=case BoolS of
            true -> [[]|[]];
            false -> State#th_state.fdata
          end,
  HLL=re:replace(HL, State#th_state.blanks, "", [{return, list}, global]),
  NewEl=#{what => ggoto, ngoto => NewCount+1},
  NewGoto=[NewCount|State#th_state.goto],
  NewLines=maps:put(NewCount, NewEl, Lines),
  Is_cond=re:run(HLL, "\\?")==nomatch,
  case Is_cond of
    true -> {keep_state, {State#th_state{goto=NewGoto, fdata=TL}, NewLines, NewCount, Pcnt}, {next_event, internal, HLL}};
    false -> {next_state, expression, {State#th_state{goto=NewGoto, fdata=TL}, NewLines, NewCount, Pcnt}, {next_event, internal, HLL}}
  end;

condt(internal, [$?, $?|Str], {State, Lines, Cnt, Pcnt}) ->%% dowhile
  NewCount=Cnt+1,
  BoolS=State#th_state.fdata == [],
  [HL|TL]=case BoolS of
            true -> [[]|[]];
            false -> State#th_state.fdata
          end,
  HLL=re:replace(HL, State#th_state.blanks, "", [{return, list}, global]),
  Is_cond=re:run(HLL, "\\?")==nomatch,
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
          [NgotoH|NgotoT]=State#th_state.goto,
          NewEl=#{what => dowhile_, var1 => Var1, var2 => Var2, op => C, ngoto => NgotoH},
          NewLines=maps:put(NewCount, NewEl, Lines),
          case Is_cond of
            true ->  {keep_state, {State#th_state{goto=NgotoT, fdata=TL}, NewLines, NewCount, Pcnt}, {next_event, internal, HLL}};
            false ->  {next_state, expression, {State#th_state{goto=NgotoT, fdata=TL}, NewLines, NewCount, Pcnt}, {next_event, internal, HLL}}
          end;
        true ->
          {next_state, errhandle, {State, Lines, NewCount, Pcnt} , {next_event, internal, [error, in_cond, NewCount]}}
        end;
        _NotMatch ->
          {next_state, errhandle, {State, Lines, NewCount, Pcnt}, {next_event, internal, [error, in_cond, NewCount]}}
  end;

condt(internal, [$?|Str], {State, Lines, Cnt, Pcnt}) ->%% while
  NewCount=Cnt+1,
  BoolS=State#th_state.fdata == [],
  [HL|TL]=case BoolS of
            true -> [[]|[]];
            false -> State#th_state.fdata
          end,
  HLL=re:replace(HL, State#th_state.blanks, "", [{return, list}, global]),
  Is_cond=re:run(HLL, "\\?")==nomatch,
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
          NewEl=#{what => while_, var1 => Var1, var2 => Var2, op => C, ngoto => NewCount},
          NewLines=maps:put(NewCount, NewEl, Lines),
          NewGoto=[NewCount|State#th_state.goto],
          case Is_cond of
            true -> {keep_state, {State#th_state{goto=NewGoto, fdata=TL}, NewLines, NewCount, Pcnt}, {next_event, internal, HLL}};
            false -> {next_state, expression, {State#th_state{goto=NewGoto, fdata=TL}, NewLines, NewCount, Pcnt}, {next_event, internal, HLL}}
          end;
    true -> %% wrong cond
      {next_state, errhandle, {State, Lines, NewCount, Pcnt}, {next_event, internal, [error, in_cond, NewCount]}}
  end;
  _Notmatch ->
    {next_state, errhandle, {State, Lines, NewCount, Pcnt}, {next_event, internal, [error, in_cond, NewCount]}}
  end;

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

expression(internal, _Data, {State, Lines, Cnt, Pcnt}) when State#th_state.fdata==[] ->
  {next_state, calc_stage, {State, Lines, Cnt, Pcnt}, {next_event, internal, []}};

expression(internal, Data, {State, Lines, Cnt, Pcnt}) ->
  NewCount=Cnt+1,
  NewPC=Pcnt+1,
  Str=re:replace(Data, State#th_state.blanks, "", [{return, list}, global]),
  NStr=var:set_task_str(State#th_state.vp, Str),
  {ok, Pid}=gen_statem:start(preproc, {State#th_state.vp, NStr}, []),
  NewEl=#{what => str, pstr => NStr, ppid => Pid},
  NewLines=maps:put(NewCount, NewEl, Lines),
  {keep_state, {State, NewLines, NewCount, NewPC}};

expression(cast, stop, State) ->
  {stop, normal, State};

expression(cast, Data, {State, Lines, Cnt, Pcnt}) ->
  case Data of
    {Sid, {error, Err}} ->
      {next_state, errhandle, {State, Lines, Cnt, Pcnt}, {next_event, internal, [error, in_expr, [Sid, {error, Err}], Cnt]}};
    {_Sid, StrId} ->
      %% #{what, pstr, PP=ppid, cpid}=maps:get(Cnt, Lines),
      El=maps:get(Cnt, Lines),
      %% NewEl=#{what => str, pstr => StrId, ppid => PP},
      NewEl=El#{what => str, pstr => StrId},
      NewLines=maps:put(Cnt, NewEl, maps:remove(Cnt, Lines)),
      BoolS=State#th_state.fdata == [],
      [HL|TL]=case BoolS of
                true -> [[]|[]];
                false -> State#th_state.fdata
              end,
      HLL=re:replace(HL, State#th_state.blanks, "", [{return, list}, global]),
      Is_cond=re:run(HLL, "\\?")==nomatch,
      case Is_cond of
        true -> {next_state, condt, {State#th_state{fdata=TL}, NewLines, Cnt, Pcnt}, {next_event, internal, HLL}};
        false -> {keep_state, {State#th_state{fdata=TL}, NewLines, Cnt, Pcnt}, {next_event, internal, HLL}}
      end;
    Mistake->
      {next_state, errhandle, {State, Lines, Cnt, Pcnt}, {next_event, internal, [error, in_expr, Mistake, Cnt]}}
  end.

calc_stage(internal, [], {State, Lines, Cnt, Pcnt}) when State#th_state.goto == [] ->
  NewLines=startcalc(State#th_state.vp, Lines),
  {keep_state, {State, NewLines, Cnt, Pcnt, Pcnt}};

calc_stage(internal, [], State) ->
  {next_state, errhandle, State, {next_event, internal, [error, wrong_condition_set]}};

calc_stage(cast, stop, State) ->
  {stop, normal, State};

calc_stage(cast, Data, {State, Lines, Cnt, Pcnt, Pcnt_}) ->
  case Data of
    {_SId, ok} ->
      NewPC=Pcnt_-1,
      case NewPC of
        0 ->
          {next_state, start_calc, {State, Lines, Cnt, 1}, {next_event, internal , []}};
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
  %% datatype while, goto, dowhile, str
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
      %% gen_server:cast(State#th_state.mp, {State#th_state.tid, ok, Aout}),
      %% keep_state_and_data
  end;

start_calc(cast, {A, B}, {State, Lines, Cnt, Pcnt}) ->
  case B of
    zero ->
      %% gen_server:cast(State#th_state.mp, {State#th_state.tid, [error, zero, A]}),
      %% keep_state_and_data;
      {next_state, errhandle, {State, Lines, Cnt, Pcnt}, {next_event, internal, [error, zero, A]}};
    ok ->
      {keep_state, {State, Lines, Cnt, Pcnt+1}, {next_event, internal, []}}
  end;

start_calc(cast, stop, State) ->
  {stop, normal, State}.

errhandle(internal, Data, {State, _Lines, _Cnt, _Pnt}) ->
  case State#th_state.sid of
    0 ->
      Fname=lists:flatten(lists:append(State#th_state.fp, ".aout")),
      {ok, Fd}=file:open(Fname, write),
      file:write(Fd, Data),
      file:close(Fd),
      Ddata=io_lib:format("~p", [Data]),
      io:format("~nFile writing to ~p ~n~p~n", [Fname, Ddata]),
      gen_server:cast(State#th_state.mp, [ok, {State#th_state.tid, done}]),
      keep_state_and_data;
    _Sock ->
      gen_server:cast(State#th_state.mp, [ok, {State#th_state.tid, Data}]),
      keep_state_and_data
  end;

errhandle(cast, stop, State) ->
  {stop, normal, State}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, Staten, {State, Lines, Cnt, _Pcnt}) ->
  Bool=lists:member(Staten, [errhandle, start_calc, expression, condt, calc_stage]),
  case Bool of
    true ->
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
      [F(maps:get(X, Lines))||X <- lists:seq(1, Cnt)];
    false ->
      ok
  end,
  gen_server:cast(State#th_state.vp, stop),
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
                {ok, Pid}=gen_statem:start(calculate, {VP, maps:get(pstr, El)}, []),
                maps:put(cpid, Pid, El);
              _Other2 ->
                El
            end,
      NewLines=maps:put(Num, NewEl, New),
      sc(VP, Lines, Sz, Num+1, NewLines)
  end.
