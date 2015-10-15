%%%-------------------------------------------------------------------
%%% @doc mipush connection process
%%%-------------------------------------------------------------------
-module(mipush_connection).
-author('zhongwencool@gmail.com').

-behaviour(gen_server).

%% API
-export([build_request/2]).
-export([urlencode/1]).

%% API
-export([stop/1]).
-export([send_message/3]).

%% gen_server callback
-export([start_link/1, start_link/2, init/1, init/2, handle_call/3,
  handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(TIMEOUT, 10*1000).

-spec stop(pid()) -> ok.
stop(ConnId) -> gen_server:cast(ConnId, stop).

-spec send_message(pid(), mipush:push_msg(), return|no_return) -> ok.
send_message(ConnId, Msg, no_return) -> gen_server:cast(ConnId, Msg);
send_message(ConnId, Msg, return) -> gen_server:call(ConnId, Msg).

-spec start_link(atom(), apns:connection()) -> {ok, pid()} | {error, {already_started, pid()}}.
start_link(Name, Connection) ->
  gen_server:start_link({local, Name}, ?MODULE, [Name, Connection], []).
%% @hidden
-spec start_link(apns:connection()) -> {ok, pid()}.
start_link(Connection) ->
  gen_server:start_link(?MODULE, Connection, []).

%% @hidden
-spec init(mipush:connection()) -> {ok, mipush:connection()} | {stop, term()}.
init([Name, Connection]) -> init(Name, Connection);
init(Connection) -> init(?MODULE, Connection).

%% @hidden
-spec init(atom(), mipush:connection()) -> {ok, mipush:connection() | {stop, term()}}.
init(Name, #{host := Host, port := Port, timeout :=  Timeout,
  expires := Expires, ssl_opts := SSLOpts} = Connection) ->
  try
    {ok, Socket} = ssl:connect(Host, Port, ssl_opts(SSLOpts), Timeout),
    {ok, Connection#{socket => Socket, name => Name, expires_conn => epoch(Expires)}}
  catch
    _: ErrReason -> {stop, ErrReason}
  end.

%% @hidden
ssl_opts(SSLOpts) -> [{mode, binary} | SSLOpts].

%% @hidden
-spec handle_call(X, reference(), mipush:connection()) ->
  {stop, {unknown_request, X}, {unknown_request, X}, mipush:connection()}.
handle_call(Msg, From, State = #{socket := undefined, expires := Expires, timeout := Timeout,
  host := Host, port := Port, ssl_opts := SSLOpts}) ->
  try
    {ok, Socket} = ssl:connect(Host, Port, ssl_opts(SSLOpts), Timeout),
    handle_call(Msg, From, State#{socket => Socket, expires_conn => epoch(Expires)})
  catch
    _: ErrReason -> {stop, ErrReason}
  end;

handle_call({Method, Query} = Req, From, State = #{socket := Socket, host := Host, auth_key := Auth,
  expires := Expires, expires_conn := ExpiresConn}) ->
  case ExpiresConn =< epoch(0) of
    true ->
      ssl:close(Socket),
      handle_call(Req, From, State#{socket => undefined});
    false ->
      case do_send_recv_data(Socket, Method, Query, Host, Auth) of
        {reply, Data} ->
          NewData = re:replace(Data, <<"\r\n\.+\r\n">>, <<"">>, [global, {return, binary}]),
          DataList = binary:split(NewData,[<<",">>],[global]),
          {reply, DataList, State#{expires_conn => epoch(Expires)}};
        {error, Reason} -> {reply, {error, Reason}, State}
      end
  end;
handle_call(Request, _From, State) ->
  {stop, {unknown_request, Request}, {unknown_request, Request}, State}.

%% @hidden
-spec handle_cast(stop | mipush:msg(), mipush:connection()) ->
  {noreply, mipush:connection()} | {stop, normal | {error, term()}, mipush:connection()}.
handle_cast(Msg, State = #{socket := undefined, expires := Expires, timeout := Timeout,
  host := Host, port := Port, ssl_opts := SSLOpts}) ->
  try
    {ok, Socket} = ssl:connect(Host, Port, ssl_opts(SSLOpts), Timeout),
    handle_cast(Msg, State#{socket => Socket, expires_conn => epoch(Expires)})
  catch
    _: ErrReason -> {stop, ErrReason}
  end;
handle_cast(stop, State) -> {stop, normal, State};
handle_cast({Method, Query}, State = #{socket := Socket, host := Host, auth_key := Auth,
  expires := Expires, expires_conn := ExpiresConn})  ->
  case ExpiresConn =< epoch(0) of
    true ->
      ssl:close(Socket),
      handle_cast(Query,State#{socket => undefined});
    false ->
      Msg = joint_req(Method, Query, Auth, Host),
      case ssl:send(Socket, Msg) of
        ok ->
          {noreply, State#{expires_conn => epoch(Expires)}};
        {error, Reason} -> {stop, {error, Reason}, State}
      end
  end.

%% @hidden
-spec handle_info({ssl, tuple(), binary()} | {ssl_closed, tuple()} | X, mipush:connection()) ->
  {noreply, mipush:connection()} | {stop, ssl_closed | {unknown_request, X}, mipush:connection()}.
handle_info({ssl, SslSocket, Data},#{socket := SslSocket, buffer := <<>>, err_callback := ErrCallback} = State) ->
  case binary:split(Data, [<<"\r\n">>], [trim]) of
    [<<"HTTP/1.1 200 OK">>, RestBin] ->
      case check_result(RestBin, ErrCallback) of
        ok -> {noreply, State};
        stop -> {stop, {reply_error, Data}, State}
      end;
    _ -> {stop, {reply_error, Data}, State}
  end;

handle_info({ssl_closed, SslSocket}, State = #{socket := SslSocket}) ->
  {noreply, State#{socket => undefined}};

handle_info(Request, State) -> {stop, {unknown_request, Request}, State}.

%% @hidden
-spec terminate(term(), mipush:connection()) -> ok.
terminate(_Reason, _State) -> ok.

%% @hidden
-spec code_change(term(), mipush:connection(), term()) -> {ok, mipush:connection()}.
code_change(_OldVsn, State, _Extra) ->  {ok, State}.

check_result(RestBin, ErrorFun) ->
  ResultList = binary:split(RestBin, [<<"\r\n">>], [global]),
  case lists:keyfind(<<"result">>, 1, jsx:decode(lists:last(ResultList))) of
    {_, <<"ok">>} -> ok;
    _ -> ErrorFun(ResultList)
  end.

build_request(Path, []) -> Path;
build_request(Path, QueryParameters = #{}) ->
  build_request(Path, transform_map_to_list(QueryParameters));
build_request(Path, QueryParameters) ->
  QueryString = urlencode(QueryParameters),
  Path ++ "?" ++ QueryString.

%% ===================================================================
%% INTERNAL FUNCTION
%% ===================================================================

joint_req(Method, Query, Auth, Host) ->
  [Method, " ", Query, " ", "HTTP/1.1", "\r\n",
    [["Authorization",": ", Auth, "\r\n"],
      ["Host", ": ", Host, "\r\n"],
      ["Content-Length",": ","0","\r\n"]],
    "\r\n"].

do_send_recv_data(Socket, Method, Query, Host, Auth) ->
  Msg = joint_req(Method, Query, Auth, Host),
  ssl:setopts(Socket, [{active, false}]),
  case ssl:send(Socket, Msg) of
    ok ->
      case ssl:recv(Socket, 0, ?TIMEOUT) of
        {ok, Data} ->
          Result =
            case binary:split(Data, [<<"\r\n">>], [global]) of
              [<<"HTTP/1.1 200 OK">>|Rests] ->
                case lists:member(<<"Transfer-Encoding: chunked">>, Rests) of
                  false -> {reply, lists:last(Rests)};
                  true ->
                    Rest = receive_chunked_data(Socket, <<>>),
                    {reply, <<(lists:last(Rests))/binary, Rest/binary>>}
                end;
              Err -> {error, Err}
            end,
          ssl:setopts(Socket, [{active, true}]),
          Result;
        {error, _Reason} = Err ->
          ssl:setopts(Socket, [{active, true}]),
          Err
      end;
    {error, _Reason} = Err ->
      ssl:setopts(Socket, [{active, true}]),
      Err
  end.

receive_chunked_data(Socket, Acc) ->
  case ssl:recv(Socket, 0 , ?TIMEOUT) of
    {ok, SocketData} ->
      List = binary:split(SocketData, [<<"\r\n">>], [global]),
      case lists:member(<<"0">>, List) of
        true ->
          [Result |_] = binary:split(SocketData, [<<"]}">>]),
          <<Acc/binary, Result/binary>>;
        false -> receive_chunked_data(Socket, <<Acc/binary, SocketData/binary>>)
      end;
    {error, _Error} = Err -> Err
  end.

%% second
epoch(ExpireTime) ->
  {M,S,_} = os:timestamp(),
  M * 1000000 + S + ExpireTime.

-define(PERCENT, 37).  % $\%
-define(FULLSTOP, 46). % $\.
-define(IS_HEX(C), ((C >= $0 andalso C =< $9) orelse
  (C >= $a andalso C =< $f) orelse
  (C >= $A andalso C =< $F))).
-define(QS_SAFE(C), ((C >= $a andalso C =< $z) orelse
  (C >= $A andalso C =< $Z) orelse
  (C >= $0 andalso C =< $9) orelse
  (C =:= ?FULLSTOP orelse C =:= $- orelse C =:= $~ orelse
    C =:= $_))).

-define(MIN_EXP, -1074).
-define(FLOAT_BIAS, 1022).
-define(BIG_POW, 4503599627370496).

transform_map_to_list([Map|_Rest] = Maps)when is_map(Map) ->
  [begin transform_map_to_list(MapTmp) end|| MapTmp <-Maps];
transform_map_to_list([NotMap|Rest]) ->
  [NotMap] ++ [begin transform_map_to_list(MapTmp) end|| MapTmp <-Rest];
transform_map_to_list(#{} = Map) ->
  lists:foldl(fun({Key, Value}, Acc) ->
  case is_map(Value) orelse is_list(Value) of
    true -> [{Key, transform_map_to_list(Value)}|Acc];
    false -> [{Key, Value}| Acc]
  end end, [], maps:to_list(Map));
transform_map_to_list(Value) ->
  Value.

urlencode(Props) ->
  Pairs = lists:foldr(
    fun ({K, V}, Acc) ->
      [quote_plus(K) ++ "=" ++ quote_plus(V) | Acc]
    end, [], Props),
  string:join(Pairs, "&").

quote_plus(Atom) when is_atom(Atom) ->
  quote_plus(atom_to_list(Atom));
quote_plus(Int) when is_integer(Int) ->
  quote_plus(integer_to_list(Int));
quote_plus(Binary) when is_binary(Binary) ->
  quote_plus(binary_to_list(Binary));
quote_plus(Float) when is_float(Float) ->
  quote_plus(digits(Float));
quote_plus(String) ->
  quote_plus(String, []).

quote_plus([], Acc) ->
  lists:reverse(Acc);
quote_plus([C | Rest], Acc) when ?QS_SAFE(C) ->
  quote_plus(Rest, [C | Acc]);
quote_plus([$\s | Rest], Acc) ->
  quote_plus(Rest, [$+ | Acc]);
quote_plus([C | Rest], Acc) ->
  <<Hi:4, Lo:4>> = <<C>>,
  quote_plus(Rest, [hexdigit(Lo), hexdigit(Hi), ?PERCENT | Acc]).

digits(N) when is_integer(N) -> integer_to_list(N);
digits(0.0) -> "0.0";
digits(Float) ->
  {Frac1, Exp1} = frexp_int(Float),
  [Place0 | Digits0] = digits1(Float, Exp1, Frac1),
  {Place, Digits} = transform_digits(Place0, Digits0),
  R = insert_decimal(Place, Digits),
  case Float < 0 of
    true -> [$- | R];
    _ -> R
  end.

transform_digits(Place, [0 | Rest]) -> transform_digits(Place, Rest);
transform_digits(Place, Digits) -> {Place, [$0 + D || D <- Digits]}.

insert_decimal(0, S) -> "0." ++ S;
insert_decimal(Place, S) when Place > 0 ->
  L = length(S),
  case Place - L of
    0 -> S ++ ".0";
    N when N < 0 ->
      {S0, S1} = lists:split(L + N, S),
      S0 ++ "." ++ S1;
    N when N < 6 -> S ++ lists:duplicate(N, $0) ++ ".0";
    _ -> insert_decimal_exp(Place, S)
  end;
insert_decimal(Place, S) when Place > -6 -> "0." ++ lists:duplicate(abs(Place), $0) ++ S;
insert_decimal(Place, S) -> insert_decimal_exp(Place, S).

insert_decimal_exp(Place, S) ->
  [C | S0] = S,
  S1 = case S0 of
         [] -> "0";
         _ -> S0
       end,
  Exp = case Place < 0 of
          true -> "e-";
          false -> "e+"
        end,
  [C] ++ "." ++ S1 ++ Exp ++ integer_to_list(abs(Place - 1)).

hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $A + (C - 10).

digits1(Float, Exp, Frac) ->
  Round = ((Frac band 1) =:= 0),
  case Exp >= 0 of
    true ->
      BExp = 1 bsl Exp,
      case (Frac =/= ?BIG_POW) of
        true -> scale((Frac * BExp * 2), 2, BExp, BExp, Round, Round, Float);
        false -> scale((Frac * BExp * 4), 4, (BExp * 2), BExp, Round, Round, Float)
      end;
    false ->
      case (Exp =:= ?MIN_EXP) orelse (Frac =/= ?BIG_POW) of
        true -> scale((Frac * 2), 1 bsl (1 - Exp), 1, 1, Round, Round, Float);
        false -> scale((Frac * 4), 1 bsl (2 - Exp), 2, 1, Round, Round, Float)
      end
  end.

scale(R, S, MPlus, MMinus, LowOk, HighOk, Float) ->
  Est = int_ceil(math:log10(abs(Float)) - 1.0e-10),
  case Est >= 0 of
    true -> fixup(R, S * int_pow(10, Est), MPlus, MMinus, Est, LowOk, HighOk);
    false ->
      Scale = int_pow(10, -Est),
      fixup(R * Scale, S, MPlus * Scale, MMinus * Scale, Est, LowOk, HighOk)
  end.

int_pow(_X, 0) -> 1;
int_pow(X, N) when N > 0 -> int_pow(X, N, 1).

int_pow(X, N, R) when N < 2 -> R * X;
int_pow(X, N, R) -> int_pow(X * X, N bsr 1, case N band 1 of 1 -> R * X; 0 -> R end).

int_ceil(X) ->
  T = trunc(X),
  case (X - T) of
    Pos when Pos > 0 -> T + 1;
    _ -> T
  end.

fixup(R, S, MPlus, MMinus, K, LowOk, HighOk) ->
  TooLow = case HighOk of
             true -> (R + MPlus) >= S;
             false -> (R + MPlus) > S
           end,
  case TooLow of
    true -> [(K + 1) | generate(R, S, MPlus, MMinus, LowOk, HighOk)];
    false -> [K | generate(R * 10, S, MPlus * 10, MMinus * 10, LowOk, HighOk)]
  end.

generate(R0, S, MPlus, MMinus, LowOk, HighOk) ->
  D = R0 div S,
  R = R0 rem S,
  TC1 = case LowOk of
          true -> R =< MMinus;
          false -> R < MMinus
        end,
  TC2 = case HighOk of
          true -> (R + MPlus) >= S;
          false -> (R + MPlus) > S
        end,
  case TC1 of
    false ->
      case TC2 of
        false -> [D | generate(R * 10, S, MPlus * 10, MMinus * 10, LowOk, HighOk)];
        true -> [D + 1]
      end;
    true ->
      case TC2 andalso R * 2 >= S of
        false -> [D];
        true -> [D + 1]
      end
  end.

frexp_int(F) ->
  case unpack(F) of
    {_Sign, 0, Frac} -> {Frac, ?MIN_EXP};
    {_Sign, Exp, Frac} -> {Frac + (1 bsl 52), Exp - 53 - ?FLOAT_BIAS}
  end.

unpack(Float) ->
  <<Sign:1, Exp:11, Frac:52>> = <<Float:64/float>>,
  {Sign, Exp, Frac}.
