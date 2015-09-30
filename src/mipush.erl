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
-type api_key() :: binary()| string().
-export_type([api_key/0]).

-type url() :: nonempty_string().

-type year()     ::2000..10000.
-type month()    :: 1..12.
-type day()      :: 1..31.
-type hour()     :: 1..24.
-type minute()   :: 0..59.
-type second()   :: 0..59.
-type milliseconds() :: non_neg_integer().
-type date()     :: {year(),month(),day()}.
-export_type([hour/0, minute/0, second/0, milliseconds/0, date/0]).

-type registration_id() :: binary()| string().
-type alias() :: string()| binary().
-type account() :: string()| binary().
-export_type([account/0, alias/0]).

-export_type([registration_id/0]).

-type push_msg() :: android_push_msg() | ios_push_msg().
-export_type([push_msg/0]).

-type android_push_msg()  ::
#{payload => nonempty_string(),%%消息的内容。（注意：需要对payload字符串做urlencode处理）
regestricted_package_name => string(), %%App的包名,packageName必须和开发者网站上申请的结果一致
pass_through => 0 | 1,%%0 表示通知栏消息 1 表示透传消息
title => string(), %%通知栏展示的通知的标题
description	 => string(), %%通知栏展示的通知的描述
notify_type => string(), %%可以是DEFAULT_ALL或者以下其他几种的OR组合
%% 以上为必填项,以下为可选项
time_to_live => 1..1209600000,%%可选项,如果用户离线, 设置消息在服务器保存的时间，单位：ms。服务器默认最长保留两周:1209600000
time_to_send => pos_integer(), %%可选项, 定时发送消息。仅支持七天内的定时消息,用自1970年1月1日以来00:00:00.0 UTC时间表示（以毫秒为单位的时间）
notify_id => pos_integer(), %%可选项,默认情况下，通知栏只显示一条推送消息。如果通知栏要显示多条推送消息，需要针对不同的消息设置不同的notify_id（相同notify_id的通知栏消息会覆盖之前的）
'extra.sound_uri' => nonempty_string(), %%可选项,自定义通知栏消息铃声。extra.sound_uri的值设置为铃声的URI,不要加铃声文件的后缀,如:"android.resource://com.xiaomi.mipushdemo/raw/shaking"铃声只能使用当前app内的资源，URI格式满足 android.resource://your packagename/XXX/XXX,铃声文件放在Android app的raw目录下
'extra.ticker' => string(), %%可选项,开启通知消息在状态栏滚动显示,如:"我是滚动的文字"
'extra.notify_foreground' => string(), %%可选项，开启/关闭app在前台时的通知弹出。当extra.notify_foreground值为”1″时，app会弹出通知栏消息；当extra.notify_foreground值为”0″时，app不会弹出通知栏消息。注：默认情况下会弹出通知栏消息
'extra.notify_effect' => string(), %%可选项，预定义通知栏消息的点击行为。通过设置extra.notify_effect的值以得到不同的预定义点击行为 "1"通知栏点击后打开app的Launcher Activity "2":通知栏点击后打开app的任一Activity（还需要传入extra.intent_uri) "3":通知栏点击后打开网页（还需要传入extra.web_uri）
'extra.intent_uri' => string(), %% 可选项"intent:#Intent;component=com.yourpackage/.YourActivity;end"
'extra.web_uri' => url(), %%可选项 "http://www.google.com"
'extra.flow_control' => integer(),%%  可选项,控制是否需要进行平缓发送（qps less 1000/second）默认不支持 0 表示不支持平缓发送 1 表示支持平缓发送
'extra.layout_name' => nonempty_string(), %% 可选项,自定义通知栏样式，设置为客户端要展示的layout文件名 如"custom_notify"
'extra.layout_value' => nonempty_string(), %%可选项,自定义通知栏样式，必须与layout_name一同使用，指定layout中各控件展示的内容 如"{\"text\":{\"titleText\":\"标题\"},\"image\": {\"iconImage\": \"icon\"}"
'extra.jobkey' => nonempty_string(), %%可选项,使用推送批次（JobKey）功能聚合消息。客户端会按照jobkey去重，即相同jobkey的消息只展示第一条，其他的消息会被忽略。合法的jobkey由数字（[0-9]），大小写字母（[a-zA-Z]），下划线（_）和中划线（-）组成，长度不大于8个字符
'extra.callback' => url(), %%可选项,开启消息送达和点击回执。将extra.callback的值设置为第三方接收回执的http接口 小米推送服务器每隔1s将已送达或已点击的消息ID和对应设备的regid或alias通过调用第三方http接口传给开发者。（每次调用后，小米推送服务器会清空这些数据，下次传给开发者将是新一拨数据。）注：消息的送达回执只支持单发消息。
'extra.locale' => nonempty_string(), %%可选项,可以接收消息的设备的语言范围，用逗号分隔, 如:中国大陆用"zh_CN"
'extra.locale_not_in' => nonempty_string(), %%可选项，无法收到消息的设备的语言范围,逗号分隔
'extra.model' => nonempty_string(), %%可选项,对应不同品牌的手机或手机价格范畴
'extra.model_not_in' => nonempty_string(), %%可选项,无法收到消息的设备的机型范围,逗号分隔
'extra.app_version' => nonempty_string(), %%可选项, 可以接收消息的app版本号，用逗号分割,安卓app版本号来源于manifest文件中的”android:versionName”的值。注：目前支持MiPush_SDK_Client_2_2_12_sdk.jar（及以后）的版本。
'extra.app_version_not_in' => nonempty_string(), %%可选项, 无法接收消息的app版本号，用逗号分割
'extra.connpt' => nonempty_string() %%可选项,指定在特定的网络环境下才能接收到消息 目前仅支持指定”wifi”
}.

-type ios_push_msg() ::
#{description => nonempty_string(), %%通知栏展示的通知的
%% 以上为必填项,以下为可选项
time_to_live => non_neg_integer(),	%%可选项,如果用户离线,设置消息在服务器保存的时间,单位:ms.服务器默认最长保留两周
time_to_send => non_neg_integer(),%%可选项。定时发送消息.用自1970年1月1日以来00:00:00.0 UTC时间表示(以毫秒为单位的时间).注:仅支持七天内的定时消息
'extra.sound_url' => string(), %%可选项,自定义消息铃声.当值为空时为无声,default为系统默认声音
'extra.badge' => non_neg_integer(), %%	可选项.通知角标
'extra.category' => non_neg_integer() %% 可选项.iOS8推送消息快速回复类别
}.

-type push_result() :: {ok, result()}|{error, result()}.

%%#{<<"code">> => 0,
%%<<"data">> => #{<<"id">> => <<"slm41b60443521684347Uj">>},
%%<<"description">> => <<230,136,144,229,138,159>>,
%%<<"info">> => <<"Received push messages for 1 regid">>,
%%<<"result">> => <<"ok">>,
%%<<"trace_id">> => <<"Xlm41b60443521684343pc">>

%% <<"code">>|<<"data">>|<<"description">>|<<"info">>|<<"result">>|<<"trace_id">>
-type result() :: #{binary() => any()}.

-export_type([push_result/0, result/0]).

%%-------------------------------------------------------------------
%%推送单条消息
%%-------------------------------------------------------------------
%% @doc 向某个regid或一组regid列表推送某条消息
-spec push_to_regid(api_key(), [registration_id(), ...], push_msg())-> push_result().
push_to_regid(APIKey, RegIDs = [_|_], PushMsg) ->
  Query = PushMsg#{registration_id => join(RegIDs, ", ")},
  Response = mipush_http:post(?REGID_PUSH_URL, ?AUTH(APIKey), Query, [], ?PUSH_TIMEOUT),
  simplify_response(Response).

%% @doc 向某个alias或一组alias列表推送某条消息
-spec push_to_alias(api_key(), [alias(), ...], push_msg()) -> push_result().
push_to_alias(APIKey, Alias = [_|_], PushMsg) ->
  Query = PushMsg#{alias => join(Alias, ", ")},
  Response = mipush_http:post(?ALIAS_PUSH_URL, ?AUTH(APIKey), Query, [], ?PUSH_TIMEOUT),
  simplify_response(Response).

%% @doc 向某个account或一组account列表推送某条消息
-spec push_to_account(api_key(), [account(), ...], push_msg()) -> push_result().
push_to_account(APIKey, Accounts = [_|_], PushMsg) ->
  Query = PushMsg#{user_account => join(Accounts, ", ")},
  Response = mipush_http:post(?ACCOUNTS_PUSH_URL, ?AUTH(APIKey), Query, [], ?PUSH_TIMEOUT),
  simplify_response(Response).

%% @doc 向某个topic推送某条消息
-spec push_to_topic(api_key(), nonempty_string(), push_msg()) -> push_result().
push_to_topic(APIKey, Topic, PushMsg) ->
  Query = PushMsg#{topic => Topic},
  Response = mipush_http:post(?TOPIC_PUSH_URL, ?AUTH(APIKey), Query, [], ?PUSH_TIMEOUT),
  simplify_response(Response).

%% @doc 向所有设备推送某条消息
-spec push_to_all(api_key(), push_msg()) -> push_result().
push_to_all(APIKey, Msg) ->
  Response = mipush_http:post(?ALL_PUSH_URL, ?AUTH(APIKey), Msg, [], ?PUSH_TIMEOUT),
  simplify_response(Response).

%% @doc 向多个topic推送单条消息
-spec push_to_multi_topic(api_key(), [string(), ...], string(), push_msg()) -> push_result().
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
-spec multi_msg_to_regids(api_key(), [{registration_id(), push_msg()}, ...], non_neg_integer()) -> push_result().
multi_msg_to_regids(APIKey, Msgs, TimeToSend)when is_integer(TimeToSend) ->
  Query =
    case TimeToSend == 0 of
      true -> #{messages => jsx:encode(transform_extra(Msgs))};
      false -> #{messages => jsx:encode(transform_extra(Msgs)), time_to_send => TimeToSend}
    end,
  Response = mipush_http:post(?REGIDS_MSGS_PUSH_URL, ?AUTH(APIKey), Query, [], ?PUSH_TIMEOUT),
  simplify_response(Response).

%% @doc 针对不同的alias推送不同的消息
-spec multi_msg_to_alias(api_key(), [{alias(), push_msg()}, ...], non_neg_integer()) -> push_result().
multi_msg_to_alias(APIKey, Msgs, TimeToSend) when is_integer(TimeToSend) ->
  Query =
    case TimeToSend == 0 of
      true -> #{messages => jsx:encode(transform_extra(Msgs))};
      false -> #{messages => jsx:encode(transform_extra(Msgs)), time_to_send => TimeToSend}
    end,
  Response = mipush_http:post(?ALIAS_MSGS_PUSH_URL, ?AUTH(APIKey), Query, [], ?PUSH_TIMEOUT),
  simplify_response(Response).

%% @doc 针对不同的userAccount推送不同的消息
-spec multi_msg_to_account(api_key(), [{account(), push_msg()}, ...], non_neg_integer()) -> push_result().
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
-spec get_msg_count_info(api_key(), date(), date(), string()) -> push_result().
get_msg_count_info(APIKey, StartDate, EndDate, APPName) ->
  Query = #{start_date => format_date(StartDate), end_date => format_date(EndDate),
    restricted_package_name => APPName},
  Response = mipush_http:get(?MSG_COUNTER_URL, ?AUTH(APIKey), Query, ?PUSH_TIMEOUT),
  simplify_response(Response).

%% @doc 追踪消息的状态
-spec get_msg_status(api_key(), 'msg_id'|'job_key', string()) -> push_result().
get_msg_status(APIKey, Type, Value) ->
  Query = maps:put(Type, Value, #{}),
  Response = mipush_http:get(?MSG_STATUS, ?AUTH(APIKey), Query, ?PUSH_TIMEOUT),
  simplify_response(Response).


-spec get_msgs_status(api_key(), non_neg_integer(), non_neg_integer()) -> push_result().
get_msgs_status(APIKey, BeginTime, EndTime) ->
  Query = #{begin_time => BeginTime, end_time => EndTime},
  Response = mipush_http:get(?MSGS_STATUS, ?AUTH(APIKey), Query, ?PUSH_TIMEOUT),
  simplify_response(Response).

%% @doc 获取失效的regId列表
%%获取失效的regId列表，每次请求最多返回1000个regId。
%%每次请求之后，成功返回的失效的regId将会从MiPush数据库删除。
-spec get_invalid_regids(api_key()) -> push_result().
get_invalid_regids(APIKey) ->
  mipush_http:get(?INVALID_REGIDS_URL, ?AUTH(APIKey), [], ?PUSH_TIMEOUT).

%%-------------------------------------------------------------------
%%订阅 topic/alias
%%-------------------------------------------------------------------
%% @doc 订阅RegId的标签
-spec subscribe_topic(api_key(), registration_id(), string(), 'undefined'|string()) -> push_result().
subscribe_topic(APIKey, RegisterID, Topic, Category) ->
  Querys =
    case Category of
      undefined -> #{registration_id => RegisterID, topic => Topic};
      _ -> #{registration_id => RegisterID, topic => Topic, category => Category}
    end,
  Response = mipush_http:post(?SUB_TOPIC_URL, ?AUTH(APIKey), Querys, [], ?PUSH_TIMEOUT),
  simplify_response(Response).

%% @doc 取消订阅RegId的标签
-spec unsubscribe_topic(api_key(), registration_id(), string()) -> push_result().
unsubscribe_topic(APIKey, RegisterID, Topic) ->
  Querys = #{registration_id => RegisterID, topic => Topic},
  Response = mipush_http:post(?UNSUB_TOPIC_URL, ?AUTH(APIKey), Querys, [], ?PUSH_TIMEOUT),
  simplify_response(Response).

%% @doc 获取一个应用的某个用户目前订阅的所有Topic
-spec get_all_topic(api_key(), registration_id(), string()) -> push_result().
get_all_topic(APIKey, RegisterID, APPName) ->
  Querys = #{registration_id => RegisterID, regestricted_package_name => APPName},
  Response = mipush_http:get(?TOPIC_ALL, ?AUTH(APIKey), Querys, ?PUSH_TIMEOUT),
  simplify_response(Response).

%% @doc  订阅Regid的Aliases
-spec subscribe_alias(api_key(), registration_id(), string(), [alias()]) -> push_result().
subscribe_alias(APIKey, RegisterID, Topic, Aliases) ->
  Querys = #{registration_id => RegisterID, topic => Topic,
    aliases => Aliases, category => <<"global_push">>},
  Response = mipush_http:post(?SUB_ALIAS_URL, ?AUTH(APIKey), Querys, [], ?PUSH_TIMEOUT),
  simplify_response(Response).

%% @doc  取消订阅RegId的Aliases
-spec unsubscribe_alias(api_key(), registration_id(), string(), [alias()]) -> push_result().
unsubscribe_alias(APIKey, RegisterID, Topic, Aliases) ->
  Querys = #{registration_id => RegisterID, topic => Topic, aliases => Aliases},
  Response = mipush_http:post(?UNSUB_ALIAS_URL, ?AUTH(APIKey), Querys, [], ?PUSH_TIMEOUT),
  simplify_response(Response).

%% @doc  获取一个应用的某个用户目前设置的所有Alias
-spec get_all_alias(api_key(), registration_id(), string()) -> push_result().
get_all_alias(APIKey, RegID, APPName) ->
  Querys = #{registration_id => RegID, regestricted_package_name => APPName},
  Response = mipush_http:get(?ALIAS_ALL, ?AUTH(APIKey), Querys, ?PUSH_TIMEOUT),
  simplify_response(Response).

%%-------------------------------------------------------------------
%%JOB 操作
%%-------------------------------------------------------------------
%% @doc 检测定时任务是否存在
-spec check_schedule_job_exist(api_key(), string()) -> push_result().
check_schedule_job_exist(APIKey, JobID) ->
  Querys = #{job_id => JobID},
  Response = mipush_http:get(?JOB_EXIST, ?AUTH(APIKey), Querys, ?PUSH_TIMEOUT),
  simplify_response(Response).

%% @doc 删除定时任务
-spec del_schedule_job(api_key(), string()) -> push_result().
del_schedule_job(APIKey, JobID) ->
  Querys = #{job_id => JobID},
  Response = mipush_http:get(?JOB_DELETE, ?AUTH(APIKey), Querys, ?PUSH_TIMEOUT),
  simplify_response(Response).

%% @doc 自1970年来的UTC毫秒数(国际时间:不是local_time:local_time中国区比universal_time快8小时)
-spec milliseconds_utc_since_1970({{year(), month(), day()}, {hour(), minute(), second()}}) -> milliseconds().
milliseconds_utc_since_1970({{_Year, _Month, _Day}, {_Hour, _Min, _Sec}} = Time) ->
  [UTCTime] = calendar:local_time_to_universal_time_dst(Time),
  (calendar:datetime_to_gregorian_seconds(UTCTime) -
    calendar:datetime_to_gregorian_seconds({{1970, 01, 01}, {0, 0, 0 }})) * 1000.

%%-------------------------------------------------------------------
%% INTERNAL FUNCTION
%%-------------------------------------------------------------------
simplify_response({ok, "200", _, Res}) ->
  ResMap  = #{<<"result">> := Result} = jsx:decode(list_to_binary(Res), [return_maps]),
  case Result of
    <<"ok">> -> {ok, ResMap};
    <<"error">> -> {error, ResMap}
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
