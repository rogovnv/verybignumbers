%%%-------------------------------------------------------------------
%%% @author mt
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. окт. 2022 18:37
%%%-------------------------------------------------------------------
-module(base_calc).
-author("mt").

-behaviour(application).

%% Application callbacks
-export([start/2,
  stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
  {ok, pid()} |
  {ok, pid(), State :: term()} |
  {error, Reason :: term()}).
start(_StartType, _StartArgs) -> %% /priv/conn_conf must have
  Path= code:priv_dir(base_calc),
  PathP=Path++"/conn_conf",
  F=file:read_file(PathP),
  Fn=fun(X) ->
    Y=string:split(X, " "),
    case Y of
    [A, B] ->
      {A, re:replace(B, "[\s\n\r\t]", "", [{return, list}])};
    [_Alone] ->
      {"nop", 2}
    end
  end,
  case F of
    {ok, Bin} ->
      Fle=Path++"/help.html",
      Data=binary:bin_to_list(Bin),
      L=string:split(Data, "\n", all),
      Params=lists:map(Fn, L),
      Wdir=proplists:get_value("wdir", Params, nop),
      Pdata=proplists:get_value("port", Params, nop),
      Port=case Pdata of
        nop ->
          nop;
        Pdata ->
          list_to_integer(Pdata)
        end,
      Cdata=proplists:get_value("conns", Params, nop),
      Conns=case Cdata of
        nop ->
          nop;
        Cdata ->
          list_to_integer(Cdata)
        end,
      Mdata=proplists:get_value("maxmem", Params, nop),
      Maxmem=case Mdata of
        nop ->
          nop;
        Mdata ->
          list_to_integer(Mdata)
        end,
      Bool=(Wdir/=nop) andalso (Port/=nop) andalso (Conns/=nop) andalso (Maxmem/=nop),
      case Bool of
        true ->
          Wdirr=case lists:nth(length(Wdir), Wdir) of
            $/ ->
              lists:sublist(Wdir, length(Wdir)-1);
            _Else ->
              Wdir
            end,
          case top_sup:start_link([{Wdirr, Port, Conns, Maxmem, Fle}]) of
            {ok, Pid} ->
              file:write_file(Path++"/err.log", "top_sup started"),
              {ok, Pid};
            Error ->
              file:write_file(Path++"/err.log", "top_sup error "++io_lib:fwrite("~p", [Error])),
              exit(self(), kill)
          end;
        false ->
          file:write_file(Path++"/err.log", "no params in /priv/conn_conf"),
          exit(self(), kill)
      end;
    Other ->
      file:write_file(Path++"/err.log", "no file /priv/conn_conf "++io_lib:fwrite("~p", [Other])),
      exit(self(), kill)
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) -> term()).
stop(_State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
