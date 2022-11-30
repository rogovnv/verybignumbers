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
    [Alone] ->
      {Alone, 2}
    end
  end,
  case F of
    {ok, Bin} ->
      Fle=Path++"/help.html",
      Data=binary:bin_to_list(Bin),
      L=string:split(Data, "\n", all),
      Params=lists:map(Fn, L),
      Wdir=element(2,lists:keyfind("wdir", 1, Params)),
      Port=list_to_integer(element(2, lists:keyfind("port" , 1, Params))),
      Conns=list_to_integer(element(2, lists:keyfind("conns", 1, Params))),
      Maxmem=list_to_integer(element(2, lists:keyfind("maxmem", 1, Params))),
      Bool=(Wdir==false) andalso (Port==false) andalso (Conns==false) andalso (Maxmem==false),
      case Bool of
        false ->
          case top_sup:start_link([{Wdir, Port, Conns, Maxmem, Fle}]) of
            {ok, Pid} ->
              file:write_file(Path++"/err.log", "top_sup started"),
              {ok, Pid};
            Error ->
              file:write_file(Path++"/err.log", "top_sup error "++io_lib:fwrite("~p", [Error]))
          end;
        true ->
          file:write_file(Path++"/err.log", "no params in /priv/conn_conf")
      end;
    Other ->
      file:write_file(Path++"/err.log", "no file /priv/conn_conf "++io_lib:fwrite("~p", [Other])),
      Other
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
