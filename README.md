##小米推送服务器 API

[小米推送官网](http://dev.xiaomi.com/doc/?p=533)


使用方法：
=========

```erlang

       PushMsg = #{payload => <<"">>,
           regestricted_package_name => "app.package_name",
           pass_through => 0,
           title => <<"小米App"/utf8>>,
           description => <<"测试描述"/utf8>>,
           notify_type => 1 bor 4,
           time_to_live => 1209500000,
           notify_id => 2,
           'extra.notify_effect' => "2",
           'extra.notify_foreground' => "1",
           'extra.sound_uri' => <<"android.resource://yourpath/raw/sound_file">>,
           'extra.intent_uri' => <<"intent://your_url#Intent;scheme=http;action=android.intent.action.VIEW;end">>
         },
       RegID1 = "your_regid_base64",
       RegID2 = "your_regid_base64",
       APIKey = "your_APIKey",
      {ok, Pid} = mipush:connect(#{auth_key => APIKey, name => push_one_msg_test}),
      R1 = mipush:push_to_regid(Pid, [RegID1, RegID2], Push, return),
      mipush:disconnect(Pid).

```

其它方法说明可见mipush.erl, 例子可见mipush_example.erl

内部实现流程图：
============
TODO







