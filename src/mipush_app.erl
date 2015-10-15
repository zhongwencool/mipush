-module(mipush_app).

-behaviour(application).

-export([start/0, stop/0]).

%% Application callbacks
-export([start/2, stop/1]).

-spec start() ->  {'ok', Started} | {'error', Reason} when
  Started :: [atom()],
  Reason :: term().
start() -> application:ensure_all_started(mipush).
-spec stop() -> 'ok' | {'error', Reason} when
  Reason :: term().
stop() -> application:stop(mipush).

%% ===================================================================
%% Application callbacks
%% ===================================================================
-spec start(normal | {takeover, node()} | {failover, node()}, term()) ->
  {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
  mipush_sup:start_link().

-spec stop(any()) -> ok.
stop(_State) -> ok.
