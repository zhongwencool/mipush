%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
-module(mipush).
-author('zhongwencool@gmail.com').

-include("mipush.hrl").

-define(MAX_TOPIC_LEN, 5).

%% API

-export([connect/1]).

-export([disconnect/1]).

%% 推送单条消息
-export([push_to_regid/4]).
-export([push_to_alias/4]).
-export([push_to_account/4]).
-export([push_to_topic/4]).
-export([push_to_multi_topic/5]).
-export([push_to_all/3]).

%%推送多条消息
-export([multi_msg_to_regids/5]).
-export([multi_msg_to_alias/5]).
-export([multi_msg_to_account/5]).

%%消息的状态数据
-export([get_msg_count_info/5]).
-export([get_msg_status/4]).
-export([get_msgs_status/4]).
-export([get_invalid_regids/2]).

%%订阅topic alias
-export([subscribe_topic/6]).
-export([unsubscribe_topic/5]).
-export([get_all_topic/4]).
-export([unsubscribe_alias/6]).
-export([subscribe_alias/6]).
-export([get_all_alias/4]).

%% Job 操作
-export([check_schedule_job_exist/3]).
-export([del_schedule_job/3]).

%% Util
-export([milliseconds_utc_since_1970/1]).

-type url() :: nonempty_string().

-type year()     ::2000..10000.
-type month()    :: 1..12.
-type day()      :: 1..31.
-type hour()     :: 1..24.
-type minute()   :: 0..59.
-type second()   :: 0..59.
-type milliseconds() :: non_neg_integer().
-type date()     :: {year(), month(), day()}.
-export_type([hour/0, minute/0, second/0, milliseconds/0, date/0]).

-type registration_id() :: binary()| string().
-type alias() :: string()| binary().
-type account() :: string()| binary().
-export_type([account/0, alias/0]).

-export_type([registration_id/0]).

-type push_msg() :: android_push_msg() | ios_push_msg().
-export_type([push_msg/0]).

-type android_push_msg()  ::
#{payload => nonempty_string(), %%消息的内容.（注意：需要对payload字符串做urlencode处理）
regestricted_package_name => string(), %%App的包名, packageName必须和开发者网站上申请的结果一致
pass_through => 0 | 1, %%0 表示通知栏消息 1 表示透传消息
title => string(), %%通知栏展示的通知的标题
description => string(), %%通知栏展示的通知的描述
notify_type => string(), %%可以是DEFAULT_ALL或者以下其他几种的OR组合

%% 以上为必填项, 以下为可选项
%%可选项, 如果用户离线, 设置消息在服务器保存的时间, 单位:ms. 服务器默认最长保留两周:1209600000
time_to_live => 1..1209600000,
%%可选项, 定时发送消息. 仅支持七天内的定时消息, 用自1970年1月1日以来00:00:00.0 UTC时间表示（毫秒）
time_to_send => pos_integer(),
%%可选项, 默认情况下, 通知栏只显示一条推送消息.
%%如果通知栏要显示多条推送消息, 需要针对不同的消息设置不同的notify_id
%%相同notify_id的通知栏消息会覆盖之前的
notify_id => pos_integer(),
%%可选项, 自定义通知栏消息铃声. extra.sound_uri的值设置为铃声的URI, 不要加铃声文件的后缀,
%%如:"android.resource://com.xiaomi.mipushdemo/raw/shaking"铃声只能使用当前app内的资源,
%%URI格式满足 android.resource://your packagename/XXX/XXX, 铃声文件放在Android app的raw目录下
'extra.sound_uri' => nonempty_string(),
%%可选项, 开启通知消息在状态栏滚动显示, 如: "我是滚动的文字"
'extra.ticker' => string(),
%%可选项, 开启/关闭app在前台时的通知弹出.
%%当extra.notify_foreground值为”1″时, app会弹出通知栏消息;
%%当extra.notify_foreground值为”0″时, app不会弹出通知栏消息。注: 默认情况下会弹出通知栏消息
'extra.notify_foreground' => string(),
%%可选项, 预定义通知栏消息的点击行为. 通过设置extra.notify_effect的值以得到不同的预定义点击行为
%%"1"通知栏点击后打开app的Launcher Activity
%%"2":通知栏点击后打开app的任一Activity（还需要传入extra.intent_uri)
%%"3":通知栏点击后打开网页（还需要传入extra.web_uri）
'extra.notify_effect' => string(),
%% 可选项"intent:#Intent;component=com.yourpackage/.YourActivity;end"
'extra.intent_uri' => string(),
%%可选项 "http://www.google.com"
'extra.web_uri' => url(),
%%  可选项, 控制是否需要进行平缓发送（qps less 1000/second）默认不支持 0 表示不支持平缓发送 1 表示支持平缓发送
'extra.flow_control' => integer(),
%% 可选项, 自定义通知栏样式, 设置为客户端要展示的layout文件名 如"custom_notify"
'extra.layout_name' => nonempty_string(),
%%可选项, 自定义通知栏样式, 必须与layout_name一同使用,
%%指定layout中各控件展示的内容 如"{\"text\": {\"titleText\":\"标题\"}, \"image\": {\"iconImage\": \"icon\"}"
'extra.layout_value' => nonempty_string(),
%%可选项, 使用推送批次（JobKey）功能聚合消息. 客户端会按照jobkey去重,
%%即相同jobkey的消息只展示第一条, 其他的消息会被忽略. 合法的jobkey由数字（[0-9]）
%%大小写字母（[a-zA-Z]), 下划线（_）和中划线（-）组成, 长度不大于8个字符
'extra.jobkey' => nonempty_string(),
%%可选项, 开启消息送达和点击回执. 将extra.callback的值设置为第三方接收回执的http接口
%%小米推送服务器每隔1s将已送达或已点击的消息ID和对应设备的regid或alias
%%通过调用第三方http接口传给开发者.
%%每次调用后, 小米推送服务器会清空这些数据.
%%下次传给开发者将是新一拨数据。注：消息的送达回执只支持单发消息.
'extra.callback' => url(),
%%可选项, 可以接收消息的设备的语言范围, 用逗号分隔, 如:中国大陆用"zh_CN"
'extra.locale' => nonempty_string(),
%%可选项, 无法收到消息的设备的语言范围, 逗号分隔
'extra.locale_not_in' => nonempty_string(),
%%可选项, 对应不同品牌的手机或手机价格范畴
'extra.model' => nonempty_string(),
%%可选项, 无法收到消息的设备的机型范围, 逗号分隔
'extra.model_not_in' => nonempty_string(),
%%可选项, 可以接收消息的app版本号, 用逗号分割,
%%安卓app版本号来源于manifest文件中的”android:versionName”的值.
%%注: 目前支持MiPush_SDK_Client_2_2_12_sdk.jar（及以后）的版本.
'extra.app_version' => nonempty_string(),
%%可选项, 无法接收消息的app版本号, 用逗号分割
'extra.app_version_not_in' => nonempty_string(),
%%可选项, 指定在特定的网络环境下才能接收到消息 目前仅支持指定”wifi”
'extra.connpt' => nonempty_string()
}.

-type ios_push_msg() ::
#{description => nonempty_string(), %%通知栏展示的通知的
%% 以上为必填项, 以下为可选项
%%可选项, 如果用户离线, 设置消息在服务器保存的时间, 单位:ms.服务器默认最长保留两周
time_to_live => non_neg_integer(),
%%可选项, 定时发送消息. 用自1970年1月1日以来00:00:00.0 UTC时间表示(以毫秒为单位的时间).
%%注: 仅支持七天内的定时消息
time_to_send => non_neg_integer(),
%%可选项, 自定义消息铃声. 当值为空时为无声, default为系统默认声音
'extra.sound_url' => string(),
%%可选项.通知角标
'extra.badge' => non_neg_integer(),
%%可选项. iOS8推送消息快速回复类别
'extra.category' => non_neg_integer()
}.

-type connection() ::
#{host => nonempty_string(),
port => pos_integer(),
ssl_opts => list(),
timeout =>  pos_integer(),
expires => pos_integer(),
expires_conn => pos_integer(),
socket => any(),
err_callback => fun((binary()) -> stop | _)
}.
-export_type([connection/0]).

%% <<"code">>|<<"data">>|<<"description">>|<<"info">>|<<"result">>|<<"trace_id">>
-type result() :: #{binary() => any()}.

-export_type([result/0]).

-spec connect(connection()) -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::term()}.
connect(Connection = #{})  ->
  mipush_sup:start_connection(merge_connection(Connection)).

-spec disconnect(pid()) -> ok|result().
disconnect(ConnId) -> mipush_connection:stop(ConnId).

%% ===================================================================
%%推送单条消息
%% ===================================================================

%% @doc 向某个regid或一组regid列表推送某条消息
-spec push_to_regid(pid(), [registration_id(), ...], push_msg(), return|no_return)-> ok|result().
push_to_regid(ConnID, RegIDs = [_|_], PushMsg, ReturnType) ->
  NewPushMsg = maps:remove(type, PushMsg),
  MsgType = maps:get(type, PushMsg),
  Query = NewPushMsg#{registration_id => join(RegIDs, ", ")},
  Req = {"POST", MsgType, mipush_connection:build_request(?REGID_PUSH_URL, Query)},
  Result = mipush_connection:send_message(ConnID, Req, ReturnType),
  simplify_to_result(Result).

%% @doc 向某个alias或一组alias列表推送某条消息
-spec push_to_alias(pid(), [alias(), ...], push_msg(), return|no_return) -> ok|result().
push_to_alias(ConnID, Alias = [_|_], PushMsg, ReturnType) ->
  NewPushMsg = maps:remove(type, PushMsg),
  MsgType = maps:get(type, PushMsg),
  Query = NewPushMsg#{alias => join(Alias, ", ")},
  Req = {"POST", MsgType, mipush_connection:build_request(?ALIAS_PUSH_URL, Query)},
  Result = mipush_connection:send_message(ConnID, Req, ReturnType),
  simplify_to_result(Result).

%% @doc 向某个account或一组account列表推送某条消息 restapi没有提供设置account的接口，所以只能通过客户端做
-spec push_to_account(pid(), [account(), ...], push_msg(), return|no_return) -> ok|result().
push_to_account(ConnID, Accounts = [_|_], PushMsg, ReturnType) ->
  NewPushMsg = maps:remove(type, PushMsg),
  MsgType = maps:get(type, PushMsg),
  Query = NewPushMsg#{user_account => join(Accounts, ", ")},
  Req = {"POST", MsgType, mipush_connection:build_request(?ACCOUNTS_PUSH_URL, Query)},
  Result = mipush_connection:send_message(ConnID, Req, ReturnType),
  simplify_to_result(Result).

%% @doc 向某个topic推送某条消息
-spec push_to_topic(pid(), nonempty_string(), push_msg(), return|no_return) -> ok|result().
push_to_topic(ConnID, Topic, PushMsg, ReturnType) ->
  NewPushMsg = maps:remove(type, PushMsg),
  MsgType = maps:get(type, PushMsg),
  Query = NewPushMsg#{topic => Topic},
  Req = {"POST", MsgType, mipush_connection:build_request(?TOPIC_PUSH_URL, Query)},
  Result = mipush_connection:send_message(ConnID, Req, ReturnType),
  simplify_to_result(Result).

%% @doc 向所有设备推送某条消息
-spec push_to_all(pid(), push_msg(), return|no_return) -> ok|result().
push_to_all(ConnID, PushMsg, ReturnType) ->
  NewPushMsg = maps:remove(type, PushMsg),
  MsgType = maps:get(type, PushMsg),
  Req = {"POST", MsgType, mipush_connection:build_request(?ALL_PUSH_URL, NewPushMsg)},
  Result = mipush_connection:send_message(ConnID, Req, ReturnType),
  simplify_to_result(Result).

%% @doc 向多个topic推送单条消息
-spec push_to_multi_topic(pid(), [string(), ...], string(), push_msg(), return|no_return) -> ok|{error, any()}.
push_to_multi_topic(ConnID, Topics, OP, PushMsg, ReturnType)->
  case check_topic(Topics, OP) of
    ok ->
      NewPushMsg = maps:remove(type, PushMsg),
      MsgType = maps:get(type, PushMsg),
      Query = NewPushMsg#{topics => join(Topics, ":$")},
      Req = {"POST", MsgType, mipush_connection:build_request(?MULTI_TOPIC_PUSH_URL, Query)},
      Result = mipush_connection:send_message(ConnID, Req, ReturnType),
      simplify_to_result(Result);
    {error, Reason} -> {error, Reason}
  end.

%% ===================================================================
%%推送多条消息
%% ===================================================================

%% @doc 针对不同的regid推送不同的消息
-spec multi_msg_to_regids(pid(), ios|android, [{registration_id(), push_msg()}, ...], non_neg_integer(), return|no_return) -> ok|result().
multi_msg_to_regids(ConnID, MsgType, Msgs, TimeToSend, ReturnType)when is_integer(TimeToSend) ->
  Query =
    case TimeToSend == 0 of
      true -> #{messages => jsx:encode(transform_extra(Msgs))};
      false -> #{messages => jsx:encode(transform_extra(Msgs)), time_to_send => TimeToSend}
    end,
  Req = {"POST", MsgType, mipush_connection:build_request(?REGIDS_MSGS_PUSH_URL, Query)},
  Result = mipush_connection:send_message(ConnID, Req, ReturnType),
  simplify_to_result(Result).

%% @doc 针对不同的alias推送不同的消息
-spec multi_msg_to_alias(pid(), ios|android, [{alias(), push_msg()}, ...], non_neg_integer(), return|no_return) -> ok|result().
multi_msg_to_alias(ConnID, MsgType, Msgs, TimeToSend, ReturnType) when is_integer(TimeToSend) ->
  Query =
    case TimeToSend == 0 of
      true -> #{messages => jsx:encode(transform_extra(Msgs))};
      false -> #{messages => jsx:encode(transform_extra(Msgs)), time_to_send => TimeToSend}
    end,
  Req = {"POST", MsgType, mipush_connection:build_request(?ALIAS_MSGS_PUSH_URL, Query)},
  Result = mipush_connection:send_message(ConnID, Req, ReturnType),
  simplify_to_result(Result).

%% @doc 针对不同的userAccount推送不同的消息
-spec multi_msg_to_account(pid(), ios|android, [{account(), push_msg()}, ...], non_neg_integer(), return|no_return) -> ok|result().
multi_msg_to_account(ConnID, MsgType, Msgs, TimeToSend, ReturnType)when is_integer(TimeToSend) ->
  Query =
    case TimeToSend == 0 of
      true -> #{message => jsx:encode(transform_extra(Msgs))};
      false -> #{message => jsx:encode(transform_extra(Msgs)), time_to_send => TimeToSend}
    end,
  Req = {"POST", MsgType, mipush_connection:build_request(?ACCOUNT_MSGS_PUSH_URL, Query)},
  Result = mipush_connection:send_message(ConnID, Req, ReturnType),
  simplify_to_result(Result).

%% ===================================================================
%%消息的状态数据
%% ===================================================================
%% @doc 获取消息的统计数据
-spec get_msg_count_info(pid(), ios|android, date(), date(), string()) -> result().
get_msg_count_info(ConnID, MsgType, StartDate, EndDate, APPName) ->
  Query = #{start_date => format_date(StartDate), end_date => format_date(EndDate),
    restricted_package_name => APPName},
  Req = {"GET", MsgType, mipush_connection:build_request(?MSG_COUNTER_URL, Query)},
  Result = mipush_connection:send_message(ConnID, Req, return),
  simplify_to_result(Result).

%% @doc 追踪消息的状态
-spec get_msg_status(pid(), ios|android, 'msg_id'|'job_key', string()) -> result().
get_msg_status(ConnID, MsgType, Type, Value) ->
  Query = maps:put(Type, Value, #{}),
  Req = {"GET", MsgType, mipush_connection:build_request(?MSG_STATUS, Query)},
  Result = mipush_connection:send_message(ConnID, Req, return),
  simplify_to_result(Result).

-spec get_msgs_status(pid(), ios|android, non_neg_integer(), non_neg_integer()) -> result().
get_msgs_status(ConnID, MsgType, BeginTime, EndTime) ->
  Query = #{begin_time => BeginTime, end_time => EndTime},
  Req = {"GET", MsgType, mipush_connection:build_request(?MSGS_STATUS, Query)},
  Result = mipush_connection:send_message(ConnID, Req, return),
  simplify_to_result(Result).

%% @doc 获取失效的regId列表
%%获取失效的regId列表，每次请求最多返回1000个regId。
%%每次请求之后，成功返回的失效的regId将会从MiPush数据库删除。
-spec get_invalid_regids(pid(), ios|android) -> list().
get_invalid_regids(ConnID, MsgType) ->
  Req = {"GET", MsgType, mipush_connection:build_request(?INVALID_REGIDS_URL, [])},
  mipush_connection:send_message(ConnID, Req, return).

%% ===================================================================
%%订阅 topic/alias
%% ===================================================================

%% @doc 订阅RegId的标签
-spec subscribe_topic(pid(), ios|android, registration_id(), string(),
    'undefined'|string(), return|no_return) -> ok|result().
subscribe_topic(ConnID, MsgType, RegisterID, Topic, Category, ReturnType) ->
  Querys =
    case Category of
      undefined -> #{registration_id => RegisterID, topic => Topic};
      _ -> #{registration_id => RegisterID, topic => Topic, category => Category}
    end,
  Req = {"POST", MsgType, mipush_connection:build_request(?SUB_TOPIC_URL, Querys)},
  Result = mipush_connection:send_message(ConnID, Req, ReturnType),
  simplify_to_result(Result).

%% @doc 取消订阅RegId的标签,
-spec unsubscribe_topic(pid(), ios|android, registration_id(), string(), return|no_return) -> ok|result().
unsubscribe_topic(ConnID, MsgType, RegisterID, Topic, ReturnType) ->
  Querys = #{registration_id => RegisterID, topic => Topic},
  Req = {"POST", MsgType, mipush_connection:build_request(?UNSUB_TOPIC_URL, Querys)},
  Result = mipush_connection:send_message(ConnID, Req, ReturnType),
  simplify_to_result(Result).

%% @doc 获取一个应用的某个用户目前订阅的所有Topic
-spec get_all_topic(pid(), ios|android, registration_id(), string()) -> ok|result().
get_all_topic(ConnID, MsgType, RegisterID, APPName) ->
  Querys = #{registration_id => RegisterID, regestricted_package_name => APPName},
  Req = {"GET", MsgType, mipush_connection:build_request(?TOPIC_ALL, Querys)},
  Result = mipush_connection:send_message(ConnID, Req, return),
  simplify_to_result(Result).

%% @doc  订阅Regid的Aliases restapi没有提供设置alias的接口，所以只能通过客户端做
-spec subscribe_alias(pid(), ios|android, registration_id(), string(), [alias()], return|no_return) -> ok|result().
subscribe_alias(ConnID, MsgType, RegisterID, Topic, Aliases, ReturnType) ->
  Querys = #{registration_id => RegisterID, topic => Topic, aliases => Aliases},
  Req = {"POST", MsgType, mipush_connection:build_request(?SUB_ALIAS_URL, Querys)},
  Result = mipush_connection:send_message(ConnID, Req, ReturnType),
  simplify_to_result(Result).

%% @doc  取消订阅RegId的Aliases
-spec unsubscribe_alias(pid(), ios|android, registration_id(), string(), [alias()], return|no_return) -> ok|result().
unsubscribe_alias(ConnID, MsgType, RegisterID, Topic, Aliases, ReturnType) ->
  Querys = #{registration_id => RegisterID, topic => Topic, aliases => Aliases},
  Req = {"POST", MsgType, mipush_connection:build_request(?UNSUB_ALIAS_URL, Querys)},
  Result = mipush_connection:send_message(ConnID, Req, ReturnType),
  simplify_to_result(Result).

%% @doc  获取一个应用的某个用户目前设置的所有Alias
-spec get_all_alias(pid(), ios|android, registration_id(), string()) -> result().
get_all_alias(ConnID, MsgType, RegID, APPName) ->
  Querys = #{registration_id => RegID, regestricted_package_name => APPName},
  Req = {"GET", MsgType, mipush_connection:build_request(?ALIAS_ALL, Querys)},
  Result = mipush_connection:send_message(ConnID, Req, return),
  simplify_to_result(Result).

%% ===================================================================
%%JOB 操作
%% ===================================================================

%% @doc 检测定时任务是否存在
-spec check_schedule_job_exist(pid(), ios|android, string()) -> result().
check_schedule_job_exist(ConnID, MsgType, JobID) ->
  Querys = #{job_id => JobID},
  Req = {"GET", MsgType, mipush_connection:build_request(?JOB_EXIST, Querys)},
  Result = mipush_connection:send_message(ConnID, Req, return),
  simplify_to_result(Result).


%% @doc 删除定时任务
-spec del_schedule_job(pid(), ios|android, string()) -> result().
del_schedule_job(ConnID, MsgType, JobID) ->
  Querys = #{job_id => JobID},
  Req = {"GET", MsgType, mipush_connection:build_request(?JOB_DELETE, Querys)},
  Result = mipush_connection:send_message(ConnID, Req, return),
  simplify_to_result(Result).

%% @doc 自1970年来的UTC毫秒数(国际时间:不是local_time:local_time中国区比universal_time快8小时)
-spec milliseconds_utc_since_1970({{year(), month(), day()}, {hour(), minute(), second()}}) -> milliseconds().
milliseconds_utc_since_1970({{_Year, _Month, _Day}, {_Hour, _Min, _Sec}} = Time) ->
  [UTCTime] = calendar:local_time_to_universal_time_dst(Time),
  (calendar:datetime_to_gregorian_seconds(UTCTime) -
    calendar:datetime_to_gregorian_seconds({{1970, 01, 01}, {0, 0, 0 }})) * 1000.

%% ===================================================================
%% INTERNAL FUNCTION
%% ===================================================================

merge_connection(Connection) ->
  Default = #{host => "sandbox.xmpush.xiaomi.com",
    name => undefined,
    port => 443,
    android_auth_key => "please_config_android_api_key",
    android_reg_package_name => "please_config_android_package_name",
    ios_auth_key => "please_config_ios_api_key",
    ios_bundle_id => "please_config_ios_bundle_id",
    ssl_opts => [{nodelay, true}, {reuseaddr, true}],
    timeout =>  30000, %% ms
    expires => 300, %% s
    expires_conn => 0,
    socket => undefined,
    err_callback => fun(T) -> io:format("~p~n", [T]) end
  },
  maps:merge(Default, Connection).

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

simplify_to_result(ok) -> ok;
simplify_to_result([First|Rest]) ->
  jsx:decode(lists:foldl(fun(B, Acc) -> <<Acc/binary, ", ", B/binary>> end, First, Rest), [return_maps]).

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
