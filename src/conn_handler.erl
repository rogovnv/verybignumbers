%%%-------------------------------------------------------------------
%%% @author mt
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%  IF U FUCKING DONNA KNOW!
%%% THE ACCEPTED TCP CONNECTION, THAT'S OWNER IS ANY GEN_, CLOSES WHEN EITHER TERMINATES PROCESS OR EXPLICITLY CALLS GEN_TCP:CLOSE
%%% ONLY WHEN WILL FINISH SENDING RESPONSE
%%% @end
%%% Created : 26. сент. 2022 19:03
%%%-------------------------------------------------------------------
-module(conn_handler).
-author("mt").
%% erlc +debug_info  *.erl

-behaviour(gen_statem).

%% API
-export([start_link/3]).

%% gen_statem callbacks
-export([init/1, format_status/2, handle_event/4, terminate/3,
  code_change/4, callback_mode/0]).

%% -define(SERVER, ?MODULE).

-record(ch_state, {stop_tcp, sock, lsock, dir, part, bbord, buffer, b_cnt, chunk_cnt, help, response, main1, main2, head, boundary, fbgn, form_data, ftype, fname, th_data, overheap}).

%% stop_tcp 
%% ch_state:
%% sock accepted socket
%% lsock listen socket
%% dir working directory, gets data from doit.txt 
%% %% command SPACE DATA \n -> d-elete Tid, f-ile Name, k-ill all, l-ist all, m-emory Maxmem, s-top prog all
%% %% f bigdata.txt
%% %% l foo
%% %% k 2022-10-10-10-10-59-33
%% %% s bar
%% part partitioned data
%% bbord boundary string
%% l-ist returns to aout.txt in Dir
%% buffer The buffer
%% b_cnt buffer counter
%% chunk_cnt data chunk counter
%% help help page
%% response 
%% main1 main2 main page
%% header 
%% boundary regexp for boundary
%% fbgn regexp for file begin
%% form_data regexp for form data
%% ftype regexp for file type
%% src regexp for src addr:port
%% fname regexp for file name
%% th_data data for task handler: #{addr, fname, boundary, tid}
%% overheap out of memory, block starting tasks

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start_link(Sock, Wdir, Cite) ->
  gen_statem:start_link(?MODULE, [Sock, Wdir, Cite], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init([Sock, Wdir, Cite]) ->
  process_flag(trap_exit, true),
  Sq=$",
  %% add unicode:characters_to_bin for head
  Header="HTTP/1.1 200 OK\r\n Content-Type: text/html, charset=utf-8\r\n Connection: Close \r\nContent-Length: ",
  A=lists:flatten("<!DOCTYPE html>\r\n<html>\r\n <head>\r\n  <meta charset="++[Sq]++"utf-8"++[Sq]++">\r\n  <title>Calculator.Main_page</title>\r\n </head>\r\n <body>\r\n  <br>\r\n  <p>\r\n  <b>\r\n   File name for handling OR Task ID for result \r\n  </b>\r\n  </p>\r\n  <form enctype="++[Sq]++"multipart/form-data"++[Sq]++" method="++[Sq]++"post"++[Sq]++">\r\n   <input type="++[Sq]++"file"++[Sq]++" formmethod="++[Sq]++"post"++[Sq]++" name="++[Sq]++"File"++[Sq]++">\r\n   <input type="++[Sq]++"submit"++[Sq]++" value="++[Sq]++"Post File"++[Sq]++">\r\n  </form>\r\n  <br>\r\n  <form enctype="++[Sq]++"text/plain"++[Sq]++" method="++[Sq]++"post"++[Sq]++">\r\n   <input type="++[Sq]++"text"++[Sq]++" name="++[Sq]++"Task ID"++[Sq]++">\r\n   <input type="++[Sq]++"submit"++[Sq]++" value="++[Sq]++"Send Task ID"++[Sq]++">\r\n  </form>\r\n  <br>\r\n	"),
  B=lists:flatten("<p>\r\n   <a href="++[Sq]++"/info.html"++[Sq]++">INFO</a>\r\n  </p>\r\n </body>\r\n</html>\r\n"),
  {ok, afterinit, #ch_state{lsock=Sock, dir=Wdir, main1 = A, main2= B, help =Cite, part=false, bbord=[], stop_tcp=false, head=Header}, {next_event, internal, []}}.

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() ->
  handle_event_function.

%% @private
%% @doc Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
format_status(_Opt, [_PDict, _StateName, _State]) ->
  Status = some_term,
  Status.

%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
handle_event(internal, [], afterinit, State) ->
  {ok, Bound}=re:compile("[B|b]oundary=[\-]+[0-9]+"), %% find boundary data
  {ok, Type}=re:compile("[C|c]ontent-[T|t]ype: text"), %% file type
  {ok, F_begin}=re:compile("[C|c]ontent-[T|t]ype: text\/[a-z|A-Z|\-]+\\r\\n\\r\\n"), %% file begins
  {ok, Form_data}=re:compile("\\r\\n\\r\\nTask ID=[\s]?[a-z|A-Z|0-9|\-]+"), %% form data begin
  {ok, Fname}=re:compile("[F|f]ilename=\\\"[a-z|A-Z|0-9|\-|_]+\.[A-Za-z]{1,3}"), %% file name
  case g_repo:inet_box_status() of
    foll_down ->
      keep_state_and_data;
    hold_it ->
      {next_state, req, State#ch_state{part=false, overheap=false, boundary=Bound, ftype=Type, fbgn=F_begin, form_data=Form_data, fname=Fname}, {next_event, internal, connect}}
    end;

handle_event(internal, connect, req, State) ->
  #ch_state{lsock=Lsock}=State,
  Socket=gen_tcp:accept(Lsock, 3),
  case Socket of
    {ok, Sock} ->
      {keep_state, State#ch_state{sock=Sock}};
    {error, timeout} ->
      {keep_state_and_data, {next_event, internal, connect}};
    _Error ->
      keep_state_and_data
  end;

handle_event(cast, overheap, _S, State) ->
  {keep_state, State#ch_state{overheap=true}};

handle_event(cast, overheapoff, _S, State) ->
  {keep_state, State#ch_state{overheap=false}};

handle_event(internal, response, req, State) ->
  #ch_state{response = R, sock=S}=State,
  case State#ch_state.part of
    true ->
      {next_state, req, State};
    false ->
      gen_tcp:send(S, R),
      gen_tcp:close(S),
      case g_repo:inet_box_status() of
        hold_it ->
          {keep_state, State#ch_state{response = []}, {next_event, internal, connect}};
        foll_down ->
          keep_state_and_data
      end
end;
  
handle_event(info, {tcp_closed, Sock}, req, #ch_state{sock=Sock}=State) ->
  case g_repo:inet_box_status() of
    hold_it ->
      gen_tcp:close(Sock),
      {keep_state, State#ch_state{part=false, bbord = [], buffer = [], chunk_cnt = 0}, {next_event, internal, connect}};
    foll_down ->
      {keep_state, State#ch_state{part=false, bbord = [], buffer = [], chunk_cnt = 0}}
  end;

handle_event(info, {tcp, Sock, [$G, $E, $T, 32, $/, $i, $n, $f, $o|_T]}, req, #ch_state{sock=Sock}=State) ->
  #ch_state{help = Help, head=Header}=State,
  {ok, Fle}=file:read_file(Help),
  L=io_lib:fwrite("~ts", [Fle]),
  B=unicode:characters_to_binary(lists:flatten(L)),
  H=io_lib:fwrite("~s~p~s~s", [Header, size(B),list_to_binary("\r\n\r\n"), B]),
  {keep_state, State#ch_state{response=H, part=false}, {next_event, internal, response}};

handle_event(info, {tcp, Sock, [$G, $E, $T, 32, $/, $f, $a, $v, $i, $c, $o|_T]}, req, #ch_state{sock=Sock}=State) ->
  {keep_state, State#ch_state{part=false, response = unicode:characters_to_binary("200 OK")}, {next_event, internal, response}};

handle_event(cast, stop, _S, State) ->
  g_repo:inet_box_status(foll_down),
  gen_tcp:close(State#ch_state.sock),
  {stop, normal, State};

handle_event(info, {tcp, Sock, [$G, $E, $T, 32, $/, $a, $d, $m, $i, $n|_T]}, req, #ch_state{sock=Sock}=State) ->
  #ch_state{dir=Dir, head=H, main1=M1, main2=M2}=State,
  File=file:read_file(Dir++"/doit.txt"),
  Res=case File of %% d-elete, f-ile, k-ill all, l-ist all, m-emory, s-top prog 
    {ok, Bin} ->
      L=string:split(binary:bin_to_list(Bin), [10], all),
      file:delete(Dir++"/doit.txt"),
      Answer=unicode:characters_to_binary(M1++admin_list(L, Dir)++M2),
      io_lib:fwrite("~s~p~s~s", [H, size(Answer), list_to_binary("\r\n\r\n"), Answer]);
    Error ->
      A=unicode:characters_to_binary(M1++io_lib:fwrite("~p", [Error])++M2),
      io_lib:fwrite("~s~p~s~s", [H, size(A), list_to_binary("\r\n\r\n"), A])
  end,
  {keep_state, State#ch_state{part=false, buffer=[], chunk_cnt = 0, response=Res}, {next_event, internal, response}};

handle_event(info, {tcp, Sock, [$G, $E, $T|_T]}, req, #ch_state{sock=Sock}=State) ->
  #ch_state{head=H, main1=M1, main2=M2}=State,
  M=unicode:characters_to_binary(M1++M2),
  {keep_state, State#ch_state{response=io_lib:fwrite("~s~p~s~s", [list_to_binary(H), size(M), list_to_binary("\r\n\r\n"), M]), part=false}, {next_event, internal, response}};

handle_event(info, {tcp, Sock, [$P, $O, $S, $T|Data]}, req, #ch_state{sock=Sock}=State) ->
  #ch_state{overheap=OHp, head=H, main1=M1, main2=M2}=State,
  Form_data=re:run(Data, State#ch_state.form_data),
  Type=re:run(Data, State#ch_state.ftype),
  Bool=(Type==nomatch),
  case Bool of %% common data
    false -> %% common data match
      case Form_data of %% is form data
        nomatch -> %% file
          {match, [{Bg, Ln}]}=re:run(Data, State#ch_state.fname), %% fname TH
          Fname=lists:sublist(Data, Bg+11, Ln-10),
          F_begin=re:run(Data, State#ch_state.fbgn),
          BoundaryWhere=re:run(Data, State#ch_state.boundary), %% boundary
          Boundary= case BoundaryWhere of
            nomatch ->
              [];
            {match, [{Start, Length}]} ->
              lists:sublist(Data, Start+1, Length)
            end,
          [_Word|Bbord]=string:split(Boundary, "="),
          Bool2=(Fname==nomatch) or (F_begin==nomatch) or (BoundaryWhere==nomatch), 
          case Bool2 of
            false -> %% match data for receiving
              Is_full=re:run(Data, Bbord, [global]),%% number of boundaries
              {match, [{Bgn, Offs}]}=F_begin,
              case Is_full of
                nomatch -> %% wrong data
                  ok; 
                {match, N_of} ->
                  N=length(N_of),
                  {ok, Addr}=inet:peername(Sock), %% addr TH
                  {{Y, M, D}, {Hr, Min, S}}=calendar:local_time(),
                  Num=g_repo:get_num(),
                  Tid=lists:flatten([integer_to_list(Y), $-, integer_to_list(M), $-, integer_to_list(D), $-,integer_to_list(Hr), $-, integer_to_list(Min), $-, integer_to_list(S), $-, integer_to_list(Num)]),
                  TH_struct=#{addr=>Addr, fname=>Fname, tid=>Tid},
                  case N of
                    2 -> %% file > MTU
                      M1=maps:new(),
                      M2=maps:put(1, lists:sublist(Data, Bgn+Offs+1, length(Data)-Bgn-Offs), M1),
                      inet:setopts(Sock, [list, {active, once}]),
                      {keep_state, State#ch_state{th_data = TH_struct, bbord=Bbord, chunk_cnt = 1, buffer = M2, part=true, response=[]}};
                    3 ->%% small file
                      case OHp of
                        false ->
                          [{Point, _Len}]=lists:nth(3, element(2, Is_full)),
                          Buf=lists:sublist(Data, Bgn+Offs+1, Point-Bgn-Offs-2),
                          Sup_struct=#{
                          id=>Tid,
                          start=>{task_handler, start_link, [{TH_struct, Buf}]},
                          restart=>temporary,
                          type=>worker},
                          supervisor:start_child(th_sup, Sup_struct),
                          Ts=unicode:characters_to_binary(M1++Tid++M2),
                          Tr=io_lib:fwrite("~s~p~s~s", [H, size(Ts), list_to_binary("\r\n\r\n"), Ts]),
                          {keep_state, State#ch_state{th_data = [], b_cnt=0, response = Tr, chunk_cnt = 0, buffer=[]}, {next_event, internal, response}};
                        true -> %% overheap detected
                          O=unicode:characters_to_binary(M1++"OVERHEAP"++M2),
                          D=io_lib:fwrite("~s~p~s~s", [H, size(O), list_to_binary("\r\n\r\n"), O]),
                          {keep_state, State#ch_state{th_data = [], b_cnt=0, chunk_cnt = 0, buffer=[], part=false, response = D}, {next_event, internal, response}}
                        end;
                    _Other -> 
                      ok,
                      A=unicode:characters_to_binary(M1++"WRONG DATA"++M2),
                      Dd=io_lib:fwrite("~s~p~s~s", [H, size(A), list_to_binary("\r\n\r\n"), A]),
                      {keep_state, State#ch_state{part=false, buffer=[], chunk_cnt = 0, response=Dd}, {next_event, internal, response}}
                    end
              end;
            true ->
              ok,
              A=unicode:characters_to_binary(M1++"WRONG DATA"++M2),
              Dd=io_lib:fwrite("~s~p~s~s", [H, size(A), list_to_binary("\r\n\r\n"), A]),
              {keep_state, State#ch_state{part=false, buffer=[], chunk_cnt = 0, response=Dd}, {next_event, internal, response}}
          end;
        {match, [{Pos, Len}]} -> %% form data: get result
          [_, Tid]=string:split(lists:sublist(Data, Pos+1, Len), "="),
          Answer=unicode:characters_to_binary(M1++io_lib:fwrite("~p", [g_repo:answer(Tid)])++M2),
          Db=io_lib:fwrite("~s~p~s~s", [H, size(Answer), list_to_binary("\r\n\r\n"), Answer]),
          {keep_state, State#ch_state{part=false, bbord = [], buffer = [], chunk_cnt = 0, response = Db}, {next_event, internal, response}}
        end;
    true -> %% no match common data
      ok,
      A=unicode:characters_to_binary(M1++"WRONG DATA"++M2),
      Dd=io_lib:fwrite("~s~p~s~s", [H, size(A), list_to_binary("\r\n\r\n"), A]),
      {keep_state, State#ch_state{part=false, buffer=[], chunk_cnt = 0, response=Dd}, {next_event, internal, response}}
    end;

handle_event(info, {tcp, Sock, Data}, req, #ch_state{sock=Sock}=State) ->
  #ch_state{chunk_cnt = C_cnt, buffer=Buffer, th_data = TH_struct, overheap = OHp, bbord=Bbord, head=H, main1=M1, main2=M2}=State,
  Bound=re:run(Data, Bbord),
  case Bound of
    nomatch -> %% not the end yet
      Bool=State#ch_state.part==true,
      case Bool of
        true -> %% parted file
          inet:setopts(Sock, [list, {active, once}]),
          {keep_state, State#ch_state{buffer=maps:put(C_cnt+1, Data, Buffer), chunk_cnt = C_cnt+1, response=[]}};
        false -> 
          {keep_state, State#ch_state{response = io_lib:fwrite("~s~p~s~s", [H, size(M1)+size(M2), M1, M2])}, {next_event, internal, response}}
      end;
    {match, [{_Point, _Len}]} -> %% end of file Point
      case OHp of %% overheap
        false ->
          Tid=maps:get(tid, TH_struct),
          [Chunk, _]=string:split(Data, State#ch_state.bbord), 
          Buf=maps:put(C_cnt+1, Chunk, Buffer),
          Whole=lists:flatten(maps:fold(fun(_Key, L, Acc) -> [L|Acc] end, [], Buf)),
          Sup_struct=#{
          id=>Tid,
          start => {task_handler, start_link, [{TH_struct, Whole}]},
          restart => temporary,
          type => worker},
          supervisor:start_child(th_sup, Sup_struct),
          Tb=unicode:characters_to_binary(M1++Tid++M2),
          D=io_lib:fwrite("~s~p~s~s", [H, size(Tb), list_to_binary("\r\n\r\n"), Tb]),
          {keep_state, State#ch_state{chunk_cnt = 0, b_cnt = 0, th_data = [], buffer = [], response = D}, {next_event, internal, response}};
        true ->
          O=unicode:characters_to_binary(M1++"OVERHEAP"++M2),
          D=io_lib:fwrite("~s~p~s~s", [H, size(O), list_to_binary("\r\n\r\n"), O]),
          {keep_state, State#ch_state{chunk_cnt = 0, b_cnt = 0, th_data = [], buffer = [], response =D}, {next_event, internal, response}}
      end
  end.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, #ch_state{sock=Sock}) ->
  g_repo:inet_box_status(foll_down),
  gen_tcp:close(Sock).

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #ch_state{}, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

admin_list(L, Dir) ->
  admin_list(L, Dir, []).

admin_list([], _dir, []) -> "ok";
admin_list([], _Dir, Acc) -> Acc;
admin_list([H|T], Dir, Acc) ->
  [Key|V]=string:split(re:replace(H, "  ", " ", [{return, list}]), " "),
  Value=re:replace(V, "[\s|\n|\r|\t]+", "", [global]),
  case Key of %% d-elete, f-ile, k-ill all, l-ist all, m-emory, s-top prog all
    "d" ->
      Is_has=supervisor:get_childspec(th_sup, Value),
      case Is_has of
        {error, Error} ->
          admin_list(T, Dir, [{d, Value, Error}|Acc]);
        _Ok ->
          _D=g_repo:answer(Value),
          supervisor:terminate_child(th_sup, Value),
          supervisor:delete_child(th_sup, Value),
          admin_list(T, Dir, [{d, ok, Value}|Acc]) 
      end;
    "f" ->
      File=file:read_file(Dir++Value),
      case File of
        {ok, Bin} ->
          Data=binary:bin_to_list(Bin),
          Num=g_repo:get_num(),
          {{Y, M, D}, {H, Min, S}}=calendar:local_time(),
          Tid=lists:flatten([integer_to_list(Y), $-, integer_to_list(M), $-, integer_to_list(D), $-,integer_to_list(H), $-, integer_to_list(Min), $-, integer_to_list(S), $-, integer_to_list(Num)]),
          TH_struct=#{addr=> {"localhost"}, fname => Value, tid => Tid},
          Sup_struct=#{
          id => Tid,
          start => {task_handler, start_link, [{TH_struct, Data}]},
          restart => temporary,
          type => worker},
          supervisor:start_child(th_sup, Sup_struct),
          admin_list(T, Dir, [{f, Value, Tid}|Acc]);
        Error ->
          admin_list(T, Dir, [{f, Value, Error}|Acc])
      end;
    "k" ->
      L=supervisor:which_children(th_sup),
      [supervisor:terminate_child(th_sup, Id)||{Id, _, _, _}<- L],
      [supervisor:delete_child(th_sup, Id)||{Id, _, _, _}<- L],
      admin_list(T, Dir, [{killed}|Acc]);
    "l" ->
      L=supervisor:which_children(th_sup),
      Aout=[{Id, process_info(Pid, memory), g_repo:answer(Id)}||{Id, Pid, _, _}<- L],
      To_file=iolist_to_binary(io_lib:format("~p~n", [Aout])),
      file:write_file(Dir++"_aout.txt", To_file),
      admin_list(T, Dir, [{"aout.txt"}|Acc]);
    "m" ->
      g_repo:setmaxmem(Value),
      admin_list(T, Dir, [{"mem"}|Acc]);
    "s" ->
      top_sup:stop()
  end.



