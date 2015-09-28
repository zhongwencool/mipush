%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
-module(mipush).
-author("zhongwencool@gmail.com").

-include("mipush.hrl").

-define(MAX_TOPIC_LEN, 5).

%% API
%% 推送单条消息
-export([push_to_regid/3]).
-export([push_to_alias/3]).
-export([push_to_account/3]).
-export([push_to_topic/3]).
-export([push_to_multi_topic/4]).
-export([push_to_all/2]).

%%推送多条消息
-export([multi_msg_to_regids/3]).
-export([multi_msg_to_alias/3]).
-export([multi_msg_to_account/3]).

%%消息的状态数据
-export([get_msg_count_info/4]).
-export([get_msg_status/3]).
-export([get_msgs_status/3]).
-export([get_invalid_regids/1]).

%%订阅topic alias
-export([subscribe_topic/4]).
-export([unsubscribe_topic/3]).
-export([get_all_topic/3]).
-export([unsubscribe_alias/4]).
-export([subscribe_alias/4]).
-export([get_all_alias/3]).

%% Job 操作
-export([check_schedule_job_exist/2]).
-export([del_schedule_job/2]).

%% Util
-export([milliseconds_utc_since_1970/1]).

%%-------------------------------------------------------------------
%%推送单条消息
%%-------------------------------------------------------------------
%% @doc 向某个regid或一组regid列表推送某条消息
-spec push_to_regid(api_key(), [registration_id(), ...], push_msg()) -> term().
push_to_regid(APIKey, RegIDs = [_|_], PushMsg) ->
  Query = PushMsg#{registration_id => join(RegIDs, ", ")},
  Response = mipush_http:post(?REGID_PUSH_URL, ?AUTH(APIKey), Query, [], ?PUSH_TIMEOUT),
  simplify_response(Response).

%%  @doc 向某个alias或一组alias列表推送某条消息
-spec push_to_alias(api_key(), [alias(), ...], push_msg()) -> term().
push_to_alias(APIKey, Alias = [_|_], PushMsg) ->
  Query = PushMsg#{alias => join(Alias, ", ")},
  Response = mipush_http:post(?ALIAS_PUSH_URL, ?AUTH(APIKey), Query, [], ?PUSH_TIMEOUT),
  simplify_response(Response).

%% @doc 向某个account或一组account列表推送某条消息
-spec push_to_account(api_key(), [account(), ...], push_msg()) -> term().
push_to_account(APIKey, Accounts = [_|_], PushMsg) ->
  Query = PushMsg#{user_account => join(Accounts, ", ")},
  Response = mipush_http:post(?ACCOUNTS_PUSH_URL, ?AUTH(APIKey), Query, [], ?PUSH_TIMEOUT),
  simplify_response(Response).

%% @doc 向某个topic推送某条消息
-spec push_to_topic(api_key(), nonempty_string(), push_msg()) -> term().
push_to_topic(APIKey, Topic, PushMsg) ->
  Query = PushMsg#{topic => Topic},
  Response = mipush_http:post(?TOPIC_PUSH_URL, ?AUTH(APIKey), Query, [], ?PUSH_TIMEOUT),
  simplify_response(Response).

%% @doc 向所有设备推送某条消息
-spec push_to_all(api_key(), push_msg()) -> term().
push_to_all(APIKey, Msg) ->
  Response = mipush_http:post(?ALL_PUSH_URL, ?AUTH(APIKey), Msg, [], ?PUSH_TIMEOUT),
  simplify_response(Response).

%% @doc 向多个topic推送单条消息
-spec push_to_multi_topic(api_key(), [string(), ...], string(), push_msg()) -> term().
push_to_multi_topic(APIKey, Topics, OP, PushMsg)->
  case check_topic(Topics, OP) of
    ok ->
      Query = PushMsg#{topics => join(Topics, ":$")},
      Response = mipush_http:post(?MULTI_TOPIC_PUSH_URL, ?AUTH(APIKey), Query, [], ?PUSH_TIMEOUT),
      simplify_response(Response);
    {error, Reason} -> {error, Reason}
  end.

%%-------------------------------------------------------------------
%%推送多条消息
%%-------------------------------------------------------------------
%% @doc 针对不同的regid推送不同的消息
-spec multi_msg_to_regids(api_key(), [{registration_id(), push_msg()}, ...], non_neg_integer()) -> term().
multi_msg_to_regids(APIKey, Msgs, TimeToSend)when is_integer(TimeToSend) ->
  Query =
    case TimeToSend == 0 of
      true -> #{messages => jsx:encode(transform_extra(Msgs))};
      false -> #{messages => jsx:encode(transform_extra(Msgs)), time_to_send => TimeToSend}
    end,
  Response = mipush_http:post(?REGIDS_MSGS_PUSH_URL, ?AUTH(APIKey), Query, [], ?PUSH_TIMEOUT),
  simplify_response(Response).

%% @doc 针对不同的alias推送不同的消息
-spec multi_msg_to_alias(api_key(), [{alias(), push_msg()}, ...], non_neg_integer()) -> term().
multi_msg_to_alias(APIKey, Msgs, TimeToSend) when is_integer(TimeToSend) ->
  Query =
    case TimeToSend == 0 of
      true -> #{messages => jsx:encode(transform_extra(Msgs))};
      false -> #{messages => jsx:encode(transform_extra(Msgs)), time_to_send => TimeToSend}
    end,
  Response = mipush_http:post(?ALIAS_MSGS_PUSH_URL, ?AUTH(APIKey), Query, [], ?PUSH_TIMEOUT),
  simplify_response(Response).

%% @doc 针对不同的userAccount推送不同的消息
-spec multi_msg_to_account(api_key(), [{account(), push_msg()}, ...], non_neg_integer()) -> term().
multi_msg_to_account(APIKey, Msgs, TimeToSend)when is_integer(TimeToSend) ->
  Query =
    case TimeToSend == 0 of
      true -> #{message => jsx:encode(transform_extra(Msgs))};
      false -> #{message => jsx:encode(transform_extra(Msgs)), time_to_send => TimeToSend}
    end,
  Response = mipush_http:post(?ACCOUNT_MSGS_PUSH_URL, ?AUTH(APIKey), Query, [], ?PUSH_TIMEOUT),
  simplify_response(Response).

%%-------------------------------------------------------------------
%%消息的状态数据
%%-------------------------------------------------------------------
%% @doc 获取消息的统计数据
-spec get_msg_count_info(api_key(), date(), date(), string()) -> term().
get_msg_count_info(APIKey, StartDate, EndDate, APPName) ->
  Query = #{start_date => format_date(StartDate), end_date => format_date(EndDate),
    restricted_package_name => APPName},
  Response = mipush_http:get(?MSG_COUNTER_URL, ?AUTH(APIKey), Query, ?PUSH_TIMEOUT),
  simplify_response(Response).

%% @doc 追踪消息的状态
-spec get_msg_status(api_key(), 'msg_id'|'job_key', string()) -> term().
get_msg_status(APIKey, Type, Value) ->
  Query = maps:put(Type, Value, #{}),
  Response = mipush_http:get(?MSG_STATUS, ?AUTH(APIKey), Query, ?PUSH_TIMEOUT),
  simplify_response(Response).


-spec get_msgs_status(api_key(), non_neg_integer(), non_neg_integer()) -> term().
get_msgs_status(APIKey, BeginTime, EndTime) ->
  Query = #{begin_time => BeginTime, end_time => EndTime},
  Response = mipush_http:get(?MSGS_STATUS, ?AUTH(APIKey), Query, ?PUSH_TIMEOUT),
  simplify_response(Response).

%% @doc 获取失效的regId列表
%%获取失效的regId列表，每次请求最多返回1000个regId。
%%每次请求之后，成功返回的失效的regId将会从MiPush数据库删除。
-spec get_invalid_regids(api_key()) -> term().
get_invalid_regids(APIKey) ->
  mipush_http:get(?INVALID_REGIDS_URL, ?AUTH(APIKey), [], ?PUSH_TIMEOUT).

%%-------------------------------------------------------------------
%%订阅 topic/alias
%%-------------------------------------------------------------------
%% @doc 订阅RegId的标签
-spec subscribe_topic(api_key(), registration_id(), string(), 'undefined'|string()) -> term().
subscribe_topic(APIKey, RegisterID, Topic, Category) ->
  Querys =
    case Category of
      undefined -> #{registration_id => RegisterID, topic => Topic};
      _ -> #{registration_id => RegisterID, topic => Topic, category => Category}
    end,
  Response = mipush_http:post(?SUB_TOPIC_URL, ?AUTH(APIKey), Querys, [], ?PUSH_TIMEOUT),
  simplify_response(Response).

%% @doc 取消订阅RegId的标签
-spec unsubscribe_topic(api_key(), registration_id(), string()) -> term().
unsubscribe_topic(APIKey, RegisterID, Topic) ->
  Querys = #{registration_id => RegisterID, topic => Topic},
  Response = mipush_http:post(?UNSUB_TOPIC_URL, ?AUTH(APIKey), Querys, [], ?PUSH_TIMEOUT),
  simplify_response(Response).

%% @doc 获取一个应用的某个用户目前订阅的所有Topic
-spec get_all_topic(api_key(), registration_id(), string()) -> term().
get_all_topic(APIKey, RegisterID, APPName) ->
  Querys = #{registration_id => RegisterID, regestricted_package_name => APPName},
  Response = mipush_http:get(?TOPIC_ALL, ?AUTH(APIKey), Querys, ?PUSH_TIMEOUT),
  simplify_response(Response).

%% @doc  订阅Regid的Aliases
-spec subscribe_alias(api_key(), registration_id(), string(), [alias()]) -> term().
subscribe_alias(APIKey, RegisterID, Topic, Aliases) ->
  Querys = #{registration_id => RegisterID, topic => Topic,
    aliases => Aliases, category => <<"global_push">>},
  Response = mipush_http:post(?SUB_ALIAS_URL, ?AUTH(APIKey), Querys, [], ?PUSH_TIMEOUT),
  simplify_response(Response).

%% @doc  取消订阅RegId的Aliases
-spec unsubscribe_alias(api_key(), registration_id(), string(), [alias()]) -> term().
unsubscribe_alias(APIKey, RegisterID, Topic, Aliases) ->
  Querys = #{registration_id => RegisterID, topic => Topic, aliases => Aliases},
  Response = mipush_http:post(?UNSUB_ALIAS_URL, ?AUTH(APIKey), Querys, [], ?PUSH_TIMEOUT),
  simplify_response(Response).

%% @doc  获取一个应用的某个用户目前设置的所有Alias
-spec get_all_alias(api_key(), registration_id(), string()) -> term().
get_all_alias(APIKey, RegID, APPName) ->
  Querys = #{registration_id => RegID, regestricted_package_name => APPName},
  Response = mipush_http:get(?ALIAS_ALL, ?AUTH(APIKey), Querys, ?PUSH_TIMEOUT),
  simplify_response(Response).

%%-------------------------------------------------------------------
%%JOB 操作
%%-------------------------------------------------------------------
%% @doc 检测定时任务是否存在
-spec check_schedule_job_exist(api_key(), string()) -> term().
check_schedule_job_exist(APIKey, JobID) ->
  Querys = #{job_id => JobID},
  Response = mipush_http:get(?JOB_EXIST, ?AUTH(APIKey), Querys, ?PUSH_TIMEOUT),
  simplify_response(Response).

%% @doc 删除定时任务
-spec del_schedule_job(api_key(), string()) -> term().
del_schedule_job(APIKey, JobID) ->
  Querys = #{job_id => JobID},
  Response = mipush_http:get(?JOB_DELETE, ?AUTH(APIKey), Querys, ?PUSH_TIMEOUT),
  simplify_response(Response).

%% @doc 自1970年来的UTC毫秒数（国际时间，不是local_time, local_time中国区比universal_time快8小时）
-spec milliseconds_utc_since_1970({{year(), month(), day()}, {hour(), minute(), second()}}) -> milliseconds().
milliseconds_utc_since_1970({{_Year, _Month, _Day}, {_Hour, _Min, _Sec}} = Time) ->
  [UTCTime] = calendar:local_time_to_universal_time_dst(Time),
  (calendar:datetime_to_gregorian_seconds(UTCTime) -
    calendar:datetime_to_gregorian_seconds({{1970, 01, 01}, {0, 0, 0 }})) * 1000.

%%-------------------------------------------------------------------
%% INTERNAL FUNCTION
%%-------------------------------------------------------------------
simplify_response({ok, "200", _, Res}) ->
  ResponseList = jsx:decode(list_to_binary(Res)),
  case proplists:get_value(<<"result">>, ResponseList) of
    <<"ok">> -> {ok, ResponseList};
    <<"error">> -> {error, ResponseList}
  end;
simplify_response(Error) -> {error, Error}.

%% EXCEPT 差集 INTERSECTION 交集 UNION 并集
check_topic(Topics, OP)when OP == "UNION" orelse OP == "INTERSECTION" orelse OP == "EXCEPT" ->
  case  erlang:length(Topics) > ?MAX_TOPIC_LEN of
    true -> {error, {"topic should =<", ?MAX_TOPIC_LEN}};
    false -> ok
  end;
check_topic(Topic, OP) -> {error, {"topic operation can't be", {Topic, OP}}}.

join([ID| _RestIDs] = IDs, Sep) when is_binary(ID) ->
  join([binary:bin_to_list(IDtmp) ||IDtmp <- IDs], Sep);
join([ID| RestIDs], Sep) ->
  ID ++ lists:append([Sep ++ X || X <- RestIDs]).

transform_extra([]) -> [];
transform_extra([{Target, Message}|RestMsgs]) ->
  [#{target => list_to_binary(Target), message => transform_message(Message)}|transform_extra(RestMsgs)].

transform_message(Message) ->
  NewMessage = maps:without(?EXTRA_LIST, Message),
  ExtraList =
    lists:foldl(fun(Key, Acc) ->
      case maps:get(Key, Message, undefined) of
        undefined -> Acc;
        Value -> maps:put(list_to_binary(atom_to_list(Key) -- "extra."), to_binary(Value), Acc)
      end end, #{}, ?EXTRA_LIST),
  NewMessage#{extra => ExtraList}.

format_date({Year, Month, Day})->
  MonthStr =
    case Month < 10 of
      false -> erlang:integer_to_list(Month);
      true -> "0" ++ erlang:integer_to_list(Month)
    end,
  DayStr =
    case Day < 10 of
      true -> "0" ++ erlang:integer_to_list(Day);
      false -> erlang:integer_to_list(Day)
    end,
  erlang:integer_to_list(Year) ++ MonthStr ++ DayStr.

to_binary(Value)when is_list(Value) -> list_to_binary(Value);
to_binary(Value)when is_integer(Value) -> integer_to_binary(Value);
to_binary(Value)when is_atom(Value) -> atom_to_binary(Value, latin1);
to_binary(Value)when is_binary(Value) -> Value.
