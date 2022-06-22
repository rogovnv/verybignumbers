%%%-------------------------------------------------------------------
%%% @author mt
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% unary&binary ops, keep a little time in straight processes
%%% @end
%%% Created : 20. июнь 2022 13:20
%%%-------------------------------------------------------------------
-module(bfun).
-author("mt").

%% API
-export([bfun/1, uminus/1]).

bfun({X, Y, VarPid, Op, Range}) ->
  F=fun({A, B}) ->
    case A of
      v -> gen_server:call(VarPid, {get, B});
      f ->
        My=self(),
        B ! {get, My},
        receive
          {My, Data} -> Data
        end;
      n -> B
    end
    end,
  FS=fun({A, B}) ->
    case A of
      f ->
        B ! stop;
      _Other -> ok
    end
    end,
  receive
    {get, Pid} ->
      P=F(X),
      Q=F(Y),
      Answer=case Op of
               $* ->
                 (P*Q) div Range;
               $/ ->
                 (P*Range) div Q;
               $+ ->
                 P+Q;
               $- ->
                 P-Q
             end,
      Pid ! {Pid, Answer},
      bfun({X, Y, VarPid, Op, Range});
    stop ->
      FS(X),
      FS(Y),
      bfun({X, Y, VarPid, Op, Range})
  end.

uminus({{What, M}, VarPid}) ->
  receive
    {get, Pid} ->
      Answer=case What of
               v -> {Pid, -gen_server:call(VarPid, {get, M})};
               f ->
                 My=self(),
                 M ! {get, My},
                 receive
                   {My, Data} -> {Pid, -Data}
                 end
             end,
      Pid ! Answer,
      uminus({{What, M}, VarPid});
    stop ->
      case What of
        f ->
          M ! stop;
        _Other -> ok
      end,
      ok;
    _Other ->
      uminus({{What, M}, VarPid})
  end.