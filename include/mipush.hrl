%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
-author("zhongwencool@gmail.com").

%%todo test API

-define(PUSH_TIMEOUT, 10000).

-define(AUTH(APIKey), [{'Authorization', string:concat("key=", APIKey)}]).


%% pass_through 1为透传,0为(通知栏信息)不透传
-define(PASS_THROUGH_YES, 1).
-define(PASS_THROUGH_NO,0).

%%notify_type的值可以是DEFAULT_ALL或者以下其他几种的OR组合：
-define(DEFAULT_ALL, -1).
-define(DEFAULT_SOUND,  1).  %% 使用默认提示音提示；
-define(DEFAULT_VIBRATE, 2).  %% 使用默认震动提示；
-define(DEFAULT_LIGHTS,  4).  %% 使用默认led灯光提示；

%%可选项，开启/关闭app在前台时的通知弹出。当extra.notify_foreground值为”1″时，app会弹出通知栏消息；当extra.notify_foreground值为”0″时，app不会弹出通知栏消息。注：默认情况下会弹出通知栏消息
-define(NOTIFY_FOREGROUND_ENABLED, "1").
-define(NOTIFY_FOREGROUND_DISABLED, "0").

%%notify_effect 可选项，预定义通知栏消息的点击行为。通过设置extra.notify_effect的值以得到不同的预定义点击行为。
%%"1"通知栏点击后打开app的Launcher Activity
-define(NOTIFY_EFFECT_LANUCHER, "1").
%%"2":通知栏点击后打开app的任一Activity（开发者还需要传入extra.intent_uri)
-define(NOTIFY_EFFECT_INTENT_URL, "2").
%%"3":通知栏点击后打开网页（开发者还需要传入extra.web_uri）
-define(NOTIFY_EFFECT_WEB_URL, "2").

-define(EXTRA_LIST,  ['extra.sound_uri','extra.ticker','extra.notify_foreground','extra.notify_effect',
'extra.intent_uri','extra.web_uri','extra.flow_control','extra.layout_name',
'extra.layout_value','extra.jobkey','extra.callback','extra.locale','extra.locale_not_in',
'extra.model','extra.model_not_in','extra.app_version',
'extra.app_version_not_in','extra.connpt']).
%%-------------------------------------------------------------------
%%单条消息内容
%%-------------------------------------------------------------------

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

-type registration_id() :: binary()| string().
-type alias() :: string()| binary().
-type account() :: string()| binary().

-export_type([registration_id/0]).

-type push_msg() :: android_push_msg() | ios_push_msg().

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
-type result() :: #{result_key() => result_value()}.

-type result_key() :: binary(). %% <<"code">>|<<"data">>|<<"description">>|<<"info">>|<<"result">>|<<"trace_id">>
-type result_value() :: any().


%%-------------------------------------------------------------------
%%推送单条消息
%%-------------------------------------------------------------------
%%向某个regid或一组regid列表推送某条消息
-define(REGID_PUSH_URL, "https://api.xmpush.xiaomi.com/v2/message/regid").

%%向某个alias或一组alias列表推送某条消息
-define(ALIAS_PUSH_URL, "https://api.xmpush.xiaomi.com/v2/message/alias").

%%向某个account或一组account列表推送某条消息
-define(ACCOUNTS_PUSH_URL, "https://api.xmpush.xiaomi.com/v2/message/user_account").

%%向某个topic推送某条消息
-define(TOPIC_PUSH_URL, "https://api.xmpush.xiaomi.com/v2/message/topic").

%%向多个topic推送单条消息
-define(MULTI_TOPIC_PUSH_URL, "https://api.xmpush.xiaomi.com/v2/message/multi_topic").

%%向所有设备推送某条消息
-define(ALL_PUSH_URL, "https://api.xmpush.xiaomi.com/v2/message/all").

%%-------------------------------------------------------------------
%%推送多条消息
%%-------------------------------------------------------------------
%%针对不同的regid推送不同的消息
-define(REGIDS_MSGS_PUSH_URL, "https://api.xmpush.xiaomi.com/v2/multi_messages/regids").

%%针对不同的alias推送不同的消息
-define(ALIAS_MSGS_PUSH_URL, "https://api.xmpush.xiaomi.com/v2/multi_messages/aliases").

%%针对不同的userAccount推送不同的消息
-define(ACCOUNT_MSGS_PUSH_URL, "https://api.xmpush.xiaomi.com/v2/multi_messages/user_accounts").

%%-------------------------------------------------------------------
%%消息的状态数据
%%-------------------------------------------------------------------
%%获取消息的统计数据
-define(MSG_COUNTER_URL, "https://api.xmpush.xiaomi.com/v1/stats/message/counters").

%%追踪消息状态
-define(MSG_STATUS,"https://api.xmpush.xiaomi.com/v1/trace/message/status").

%%追踪某个时间区域内的消息
-define(MSGS_STATUS, "https://api.xmpush.xiaomi.com/v1/trace/messages/status").

%%-------------------------------------------------------------------
%%订阅/取消订阅标签
%%-------------------------------------------------------------------
%%订阅RegId的标签
-define(SUB_TOPIC_URL, "https://api.xmpush.xiaomi.com/v2/topic/subscribe").

%%取消订阅RegId的标签
-define(UNSUB_TOPIC_URL, "https://api.xmpush.xiaomi.com/v2/topic/unsubscribe").

%%订阅alias的标签
-define(SUB_ALIAS_URL, "https://api.xmpush.xiaomi.com/v2/topic/subscribe/alias").

%%取消订阅alias的标签
-define(UNSUB_ALIAS_URL, "https://api.xmpush.xiaomi.com/v2/topic/unsubscribe/alias").

%%获取一个应用的某个用户目前订阅的所有Topic
-define(TOPIC_ALL, "https://api.xmpush.xiaomi.com/v1/topic/all").

%%获取失效的regId列表
-define(INVALID_REGIDS_URL, "https://feedback.xmpush.xiaomi.com/v1/feedback/fetch_invalid_regids").

%%获取一个应用的某个用户目前设置的所有Alias
-define(ALIAS_ALL, "https://api.xmpush.xiaomi.com/v1/alias/all").

%%检测定时任务是否存在
-define(JOB_EXIST, "https://api.xmpush.xiaomi.com/v2/schedule_job/exist").

%%删除定时任务
-define(JOB_DELETE, "https://api.xmpush.xiaomi.com/v2/schedule_job/delete").