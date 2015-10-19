%%%-------------------------------------------------------------------
%%% @doc mipush main supervisor
%%% @end
%%%-------------------------------------------------------------------
-module(mipush_sup).
-author('zhongwencool@gmail.com').

-behaviour(supervisor).

-export([start_link/0, start_connection/1]).
%% API callback
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================
%% @hidden
-spec start_link() ->
  {ok, pid()} | ignore | {error, {already_started, pid()} | shutdown | term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @hidden
-spec start_connection(mipush:connection()) -> {ok, pid()} | {error, term()}.
start_connection(Connection) ->
  supervisor:start_child(?MODULE, [Connection]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
%% @hidden
-spec init(_) ->
  {ok,
   {{simple_one_for_one, 5, 10},
    [{connection, {mipush_connection, start_link, []},
      transient, 5000, worker, [mipush_connection]}]}}.
init(_) ->
  {ok,
   {{simple_one_for_one, 5, 10},
    [{connection, {mipush_connection, start_link, []},
      transient, 5000, worker, [mipush_connection]}]}}.