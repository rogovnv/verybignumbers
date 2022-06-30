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

bfun({{A, B}, {C, D}, VarPid, Op, Range}) ->
  My=self(),
  receive
    {get, Pid} ->
      P=case A of
          v -> var:getvar(VarPid, B);
          f ->
            B ! {get, My},
            receive
              {My, Data} -> Data
            end;
          n -> B
        end,
      Q=case C of
          v -> var:getvar(VarPid, D);
          f ->
            D ! {get, My},
            receive
              {My, Ddata} -> Ddata
            end;
          n -> D
        end,
      Answer=case Op of
               $* ->
                 (P*Q) div Range;
               $/ ->
                 case Q of% division by zero
                   0 ->
                     var:initv(VarPid, "zero"),
                     0;
                   _Other ->
                     (P*Range) div Q
                 end;
               $+ ->
                 P+Q;
               $- ->
                 P-Q
             end,
      Pid ! {Pid, Answer},
      bfun({{A, B}, {C, D}, VarPid, Op, Range});
    stop ->
      if
        A==f ->
          B ! stop;
        true -> ok
      end,
      if
        C==f ->
          D ! stop;
        true -> ok
      end,
bfun({{A, B}, {C, D}, VarPid, Op, Range})
  end.

uminus({{What, M}, VarPid}) ->
  receive
    {get, Pid} ->
      Answer=case What of
               v -> {Pid, -var:getvar(VarPid, M)};
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