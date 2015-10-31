##小米推送服务器 API

小米推送 Restful API Erlang实现

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

内部实现：
============
####异步发送大量请求流程：
1. 使用{active, true}连接小米推送服务器(ssl:connect/3);
2. 拼凑符合小米官方的restful API接口request请求格式;
3. 使ssl:send/2发送步骤2拼凑的消息所直接返回ok;
4. 重复步骤2,3;
5. 小米服务器会主动对步骤3中的每一个消息都异步的repsonse,
   如果repsonse error, 则使用ssl:connect/3中定义的err_callback函数处理;
6. 发送完毕后，可以手动使用ssl:close/1 关闭连接（小米服务器如果没有一段时间没有收到请求，也会在自动发ssl_closed主动要求关闭连接).

####同步发送请求流程:
1. 使用{active, false}连接小米推送服务器(ssl:connect/3);
2. 拼凑符合小米官方的restful API接口request请求格式;
3. 使ssl:send/2发送步骤2拼凑的消息;
4. 使用ssl:recv/3来阻塞等待小米服务器返回步骤3中请求的repsonse;
5. 直接返回repsonse给调用者(不会使用err_callback处理错误)
5. 重复步骤3,4,5;
6. 发送完毕后，可以手动使用ssl:close/1 关闭连接（小米服务器如果没有一段时间没有收到请求，也会在自动发ssl_closed主动要求关闭连接).

####Tip
1. 使用mipush:connect/1建立的连接会根据expires(秒)来定期重连小米服务器.

CheckList
================
- [X]  It's checked with [Elvis](https://github.com/inaka/elvis)
- [X]  Readme中添加example或[wiki](https://github.com/zhongwencool/mipush/wiki)
- [ ]  添加MIT证书 
- [ ]  功能测试(目前是手动测试，有空就mock server)
- [ ]  性能测试分析(benchmark)
- [ ]  如何推广



