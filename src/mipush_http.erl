%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
-module(mipush_http).
-author("zhongwencool@gmail.com").

%% API
-export([get/3]).
-export([get/4]).
-export([post/5]).
-export([request/6]).
-export([request/7]).
-export([urlencode/1]).

%%{ok, Status, ResponseHeaders, ResponseBody} | {ibrowse_req_id, req_id() } | {error, Reason}
-type response() :: {ok, binary(), binary(), list()}|{ibrowse_req_id, non_neg_integer() }|{error, string()}.

-spec get(list()|binary(), list()|map(), integer()) -> response().
get(Url, Querys, Timeout) ->
  request(get, Url, [], Querys, [], Timeout).

-spec get(list()|binary(), list(), list()|map(), integer()) -> response().
get(Url, Header, Querys, Timeout) ->
  request(get, Url, Header, Querys, [], Timeout).

-spec post(list()|binary(), list(), map()|list(), list(), integer()) -> response().
post(Url, Headers, Querys, Bodys, Timeout) ->
  request(post, Url, Headers, Querys, Bodys, [], Timeout).

-spec request(get|post, list()|binary(), list(), map()|list(), list(), integer()) -> response().
request(Method, Url, Headers, Querys, Bodys, Timeout) when is_binary(Url) ->
  request(Method, unicode_characters_to_list(Url), Headers, Querys, Bodys, Timeout);
request(Method, Url, Headers, Querys, Bodys, Timeout) ->
  request(Method, Url, Headers, Querys, Bodys, [], Timeout).

-spec request(get|post, list()|binary(), list(), map()|list(), list(), list(), pos_integer()) -> response().
request(Method, Url, Headers, Querys, Bodys, Options, Timeout) when is_binary(Url) ->
  request(Method, unicode_characters_to_list(Url), Headers, Querys, Bodys, Options, Timeout);
request(Method, Url, Headers, Querys, Bodys, Options, Timeout) ->
  FinalUrl = attach_query_parameters(Url, Querys),  
  %io:format("~p~n", [[{url, FinalUrl}, {header, Headers}, {method, Method},
  %%% {body, Bodys}, {options, Options}, {timeout, Timeout}]]),
  Response = (catch ibrowse:send_req(FinalUrl, Headers, Method, Bodys,
    [{socket_options, [{reuseaddr, true}]}| Options], Timeout)),  
  Response.

%%-------------------------------------------------------------------
%% INTERNAL FUNCTION
%%-------------------------------------------------------------------
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

unicode_characters_to_list(Text) ->
  case unicode:characters_to_list(Text, unicode) of
    {error, _, _} -> unicode:characters_to_list(Text, latin1);
    {incomplete, L, B} -> L ++ unicode:characters_to_list(B, latin1);
    V -> V
  end.

attach_query_parameters(Url, []) -> Url;
attach_query_parameters(Url, QueryParameters = #{}) ->
  attach_query_parameters(Url, transform_map_to_list(QueryParameters));
attach_query_parameters(Url, QueryParameters) ->
  QueryString = urlencode(QueryParameters),
  Url ++ "?" ++ QueryString.

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
