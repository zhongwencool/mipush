%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
-author('zhongwencool@gmail.com').

%%todo test API

-define(PUSH_TIMEOUT, 10000).

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

-define(MI_URL, "api.xmpush.xiaomi.com").
%% ===================================================================
%%单条消息内容
%% ===================================================================

%%-------------------------------------------------------------------
%%推送单条消息
%%-------------------------------------------------------------------
%%向某个regid或一组regid列表推送某条消息
-define(REGID_PUSH_URL, "/v2/message/regid").

%%向某个alias或一组alias列表推送某条消息
-define(ALIAS_PUSH_URL, "/v2/message/alias").

%%向某个account或一组account列表推送某条消息
-define(ACCOUNTS_PUSH_URL, "/v2/message/user_account").

%%向某个topic推送某条消息
-define(TOPIC_PUSH_URL, "/v2/message/topic").

%%向多个topic推送单条消息
-define(MULTI_TOPIC_PUSH_URL, "/v2/message/multi_topic").

%%向所有设备推送某条消息
-define(ALL_PUSH_URL, "/v2/message/all").

%%-------------------------------------------------------------------
%%推送多条消息
%%-------------------------------------------------------------------
%%针对不同的regid推送不同的消息
-define(REGIDS_MSGS_PUSH_URL, "/v2/multi_messages/regids").

%%针对不同的alias推送不同的消息
-define(ALIAS_MSGS_PUSH_URL, "/v2/multi_messages/aliases").

%%针对不同的userAccount推送不同的消息
-define(ACCOUNT_MSGS_PUSH_URL, "/v2/multi_messages/user_accounts").

%% ===================================================================
%%消息的状态数据
%% ===================================================================

%%获取消息的统计数据
-define(MSG_COUNTER_URL, "/v1/stats/message/counters").

%%追踪消息状态
-define(MSG_STATUS,"/v1/trace/message/status").

%%追踪某个时间区域内的消息
-define(MSGS_STATUS, "/v1/trace/messages/status").

%% ===================================================================
%%订阅/取消订阅标签
%% ===================================================================

%%订阅RegId的标签
-define(SUB_TOPIC_URL, "/v2/topic/subscribe").

%%取消订阅RegId的标签
-define(UNSUB_TOPIC_URL, "/v2/topic/unsubscribe").

%%订阅alias的标签
-define(SUB_ALIAS_URL, "/v2/topic/subscribe/alias").

%%取消订阅alias的标签
-define(UNSUB_ALIAS_URL, "/v2/topic/unsubscribe/alias").

%%获取一个应用的某个用户目前订阅的所有Topic
-define(TOPIC_ALL, "/v1/topic/all").

%%获取失效的regId列表
-define(INVALID_REGIDS_URL, "https://feedback.xmpush.xiaomi.com/v1/feedback/fetch_invalid_regids").

%%获取一个应用的某个用户目前设置的所有Alias
-define(ALIAS_ALL, "/v1/alias/all").

%%检测定时任务是否存在
-define(JOB_EXIST, "/v2/schedule_job/exist").

%%删除定时任务
-define(JOB_DELETE, "/v2/schedule_job/delete").