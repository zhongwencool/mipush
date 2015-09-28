%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
-module(mipush_example).
-author("zhongwencool@gmail.com").

-compile(export_all).

-include("mipush.hrl").

%% todo 访问小米推送官网查看如何获取api_key
-define(APIKEY, "APIKEY").
-define(APPNAME, "APPNAME").
-define(INTENET_URL, <<"&?from_push=1#Intent;scheme=yourapp;action=android.intent.action.VIEW;end">>).
-define(INTENT_URI(Id), <<"intent://yourdeeplink/", Id/binary, "?wid=", Id/binary, ?INTENET_URL/binary>>).

-define(BOB_REGID, "BobRegidBase64").
-define(ALICE_REGID,"AliceRegidBase64").
-define(BOB_ACCOUNT, "bob_account").
-define(ALICE_ACCOUNT, "alice_account").

-define(TOPIC_1, "test_topic_1").
-define(TOPIC_2, "test_topic_2").
-define(ALIAS_1, "test_alias_1").
-define(ALIAS_2, "test_alias_2").

-define(TEST_PUSH_MSG_1, #{payload => "I'm payload 1",
  regestricted_package_name => ?APPNAME,
  pass_through => ?PASS_THROUGH_NO,
  title => <<"1通知栏展示的通知的标题1"/utf8>>,
  description => <<"1通知栏展示的通知的描述1"/utf8>>,
  notify_type => ?DEFAULT_VIBRATE bor ?DEFAULT_LIGHTS,
  time_to_live => 10001,
  time_to_send => 0,
  notify_id => 10,
  'extra.notify_foreground' => ?NOTIFY_FOREGROUND_ENABLED,
  'extra.notify_effect' => "2",
  'extra.intent_uri' => ?INTENT_URI(<<"0520b3b6c4c0cd135821dc36cfcbe8602619e8">>)
}).

-define(TEST_PUSH_MSG_2, #{payload => "I'm payload 2",
  regestricted_package_name => ?APPNAME,
  pass_through => ?PASS_THROUGH_NO,
  title => <<"2通知栏展示的通知的标题2"/utf8>>,
  description => <<"2通知栏展示的通知的描述2"/utf8>>,
  notify_type => ?DEFAULT_VIBRATE bor ?DEFAULT_LIGHTS,
  time_to_live => 10001,
  time_to_send => 0,
  notify_id => 10,
  'extra.notify_foreground' => ?NOTIFY_FOREGROUND_ENABLED,
  'extra.notify_effect' => "2",
  'extra.intent_uri' => ?INTENT_URI(<<"0520b3b6c4c0cd135821dc36cfcbe8602619e8">>)
}).

%% 订阅Topic
subscribe_topic_test() ->
  R0 = mipush:unsubscribe_topic(?APIKEY, ?BOB_REGID, ?TOPIC_1),
  io:format("mipush:unsubscribe_topic(?APIKEY, ?BOB_REGID, ?TOPIC_1).~n~p~n", [R0]),
  R1 = mipush:subscribe_topic(?APIKEY, ?BOB_REGID, ?TOPIC_1, undefined),
  io:format("mipush:subscribe_topic(?APIKEY, ?BOB_REGID, ?TOPIC_1, undefined).~n~p~n", [R1]),
  R2 = mipush:get_all_topic(?APIKEY, ?BOB_REGID, ?APPNAME),
  io:format("mipush:get_all_topic(?APIKEY, ?BOB_REGID, ?TOPIC_1).~n~p~n", [R2]),
  R3 = mipush:unsubscribe_topic(?APIKEY, ?BOB_REGID, ?TOPIC_1),
  io:format("mipush:unsubscribe_topic(?APIKEY, ?BOB_REGID, ?TOPIC_1).~n~p~n", [R3]),
  R4 = mipush:get_all_topic(?APIKEY, ?BOB_REGID, ?APPNAME),
  io:format("mipush:get_all_topic(?APIKEY, ?BOB_REGID, ?TOPIC_1).~n~p~n", [R4]),
  ok.

%%订阅ALias
subscribe_alias_test() ->
  R0 = mipush:get_all_alias(?APIKEY, ?BOB_REGID, ?APPNAME),
  io:format("mipush:get_all_alias(?APIKEY, ?BOB_REGID, ?APPNAME).~n~p~n", [R0]),
  R1 = mipush:subscribe_alias(?APIKEY, ?BOB_REGID, ?TOPIC_1, ?ALIAS_1),
  io:format("mipush:subscribe_topic(?APIKEY, ?BOB_REGID, ?TOPIC_1, ?ALIAS_1, undefined).~n~p~n", [R1]),
  R2 = mipush:get_all_alias(?APIKEY, ?BOB_REGID, ?APPNAME),
  io:format("mipush:get_all_alias(?APIKEY, ?BOB_REGID, ?APPNAME).~n~p~n", [R2]),
  R3 = mipush:unsubscribe_alias(?APIKEY, ?BOB_REGID, ?TOPIC_1, ?ALIAS_1),
  io:format("mipush:unsubscribe_alias(?APIKEY, ?BOB_REGID, \"?TOPIC_1\", \"test_alias1\").~n~p~n", [R3]),
  R4 = mipush:get_all_alias(?APIKEY, ?BOB_REGID, ?APPNAME),
  io:format("mipush:get_all_topic(?APIKEY, ?BOB_REGID, ?APPNAME).~n~p~n", [R4]),
  ok.

%%推送单条消息
push_one_msg_test() ->
  R1 = mipush:push_to_regid(?APIKEY, [?BOB_REGID, ?ALICE_REGID], ?TEST_PUSH_MSG_1),
  io:format("mipush:push_to_regid(?APIKEY, [?BOB_REGID, ?ALICE_REGID], ?TEST_PUSH_MSG_1).~n~p~n", [R1]),
  R2 = mipush:push_to_alias(?APIKEY, [?BOB_REGID, ?ALICE_REGID], ?TEST_PUSH_MSG_1),
  io:format("mipush:push_to_alias(?APIKEY, [?BOB_REGID, ?ALICE_REGID], ?TEST_PUSH_MSG_1).~n~p~n", [R2]),
  R3 = mipush:push_to_account(?APIKEY, ["account1", "account2"], ?TEST_PUSH_MSG_1),
  io:format("mipush:push_to_account(?APIKEY, [?BOB_ACCOUNT, ?ALICE_ACCOUNT], ?TEST_PUSH_MSG_1).~n~p~n", [R3]),
  R4 = mipush:push_to_topic(?APIKEY, ?TOPIC_1, ?TEST_PUSH_MSG_1),
  io:format("mipush:push_to_topic(?APIKEY, ?TOPIC_1, ?TEST_PUSH_MSG_1).~n~p~n", [R4]),

  %%%R5 = mipush:push_to_all(?APIKEY, ?TEST_PUSH_MSG_1),
  %%%io:format("mipush:push_to_all(?APIKEY, ?TEST_PUSH_MSG_1).~n~p~n", [R5]),

  R6 = mipush:push_to_multi_topic(?APIKEY, [?TOPIC_1, ?TOPIC_2], "UNION", ?TEST_PUSH_MSG_1),
  io:format("mipush:push_to_multi_topic(?APIKEY, [?TOPIC_1, ?TOPIC_2] \"UNION\", ?TEST_PUSH_MSG_1).~n~p~n", [R6]),
  ok.

%%推送多条消息
push_multi_msg_test() ->
  %%定时5分钟后推送
  SendTime = calendar:gregorian_seconds_to_datetime(
    calendar:datetime_to_gregorian_seconds(calendar:local_time()) + 5*60),
  Time = mipush:milliseconds_utc_since_1970(SendTime),
  R1 = mipush:multi_msg_to_regids(?APIKEY, [{?BOB_REGID, ?TEST_PUSH_MSG_1}, {?ALICE_REGID, ?TEST_PUSH_MSG_2}], Time),
  io:format("mipush:multi_msg_to_regids(?APIKEY, [{?MY_REGID, ?TEST_PUSH_MSG_1}, {?TEST_REGID, ?TEST_PUSH_MSG_2}], 0)
  ~n ~p~n", [R1]),
  R2 = mipush:multi_msg_to_alias(?APIKEY, [{?ALIAS_1, ?TEST_PUSH_MSG_1}, {?ALIAS_2, ?TEST_PUSH_MSG_2}], 0),
  io:format("mipush:multi_msg_to_alias(?APIKEY, [{?ALIAS_1, ?TEST_PUSH_MSG_1}, {?ALIAS_2, ?TEST_PUSH_MSG_2}], 0).
  ~n~p~n", [R2]),
  R3 = mipush:multi_msg_to_account(?APIKEY, [{?BOB_ACCOUNT, ?TEST_PUSH_MSG_1}, {?ALICE_ACCOUNT, ?TEST_PUSH_MSG_2}], 0),
  io:format(
    "mipush:multi_msg_to_account(?APIKEY, [{?BOB_ACCOUNT, ?TEST_PUSH_MSG_1}, {?ALICE_ACCOUNT, ?TEST_PUSH_MSG_2}], 0).
    ~n~p~n", [R3]),
  ok.

%%查看消息的状态
msg_state_test() ->
  EndDate = {Y, M , D} = erlang:date(),
  StartDate = case D of 1 -> EndDate ; _ -> {Y, M, D - 1} end,
  R1 = mipush:get_msg_count_info(?APIKEY, StartDate, EndDate, ?APPNAME),
  io:format("mipush:get_msg_count_info(?APIKEY, StartDate, EndDate, ?APPNAME).~n~p~n", [R1]),
  R2 = mipush:get_msg_status(?APIKEY, 'msg_id', "slm47b58443354878982Qf"),
  io:format("mipush:get_msg_status(?APIKEY, 'msg_id', \"slm47b58443354878982Qf\").~n~p~n", [R2]),
  BeginTime = calendar:gregorian_seconds_to_datetime(
    calendar:datetime_to_gregorian_seconds(calendar:local_time()) -3600*24),
  EndTime = calendar:local_time(),
  R3 = mipush:get_msgs_status(?APIKEY,
    mipush:milliseconds_utc_since_1970(BeginTime),
    mipush:milliseconds_utc_since_1970(EndTime)),
  io:format("mipush:get_msgs_status(APIKey, BeginTime, EndTime).~n~p~n", [R3]),
  %%%R4 = mipush:get_invalid_regids(?APIKEY),
  %%%io:format("mipush:get_invalid_regids(?APIKEY).~n~p~n", [R4]),
  ok.

%%定时任务查看和删除
check_schedule_test() ->
  JobID = "jobID",
  R1 = mipush:check_schedule_job_exist(?APIKEY, JobID),
  io:format("mipush:check_schedule_job_exist(?APIKEY, jobID).~n~p~n", [R1]),
  R2 = mipush:del_schedule_job(?APIKEY, JobID),
  io:format("mipush:del_schedule_job(?APIKEY, JobID).~n~p~n", [R2]),
  ok.
