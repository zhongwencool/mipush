-module(mipush_app).

-behaviour(application).

%% reload config
-export([import_config/0]).
-export([start/0]).
%% Application callbacks
-export([start/2, stop/1]).

-spec start() -> ok.
start() -> application:ensure_all_started(mipush).
%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  ok = import_config(),
  {ok, self()}.

stop(_State) -> ok.

import_config() ->
  case code:priv_dir(mipush) of
    {error, Reason} -> {error, Reason};
    PrivDir ->
      MiPushFilename = filename:join(PrivDir, "mipush_ibrowse.conf"),
      IbrowseFilename = filename:join(code:priv_dir(ibrowse), "ibrowse.conf"),
      Term = get_new_term(MiPushFilename, IbrowseFilename),
      Explain = "%% {dest, Hostname, Portnumber, MaxSessions, MaxPipelineSize, Options}.\n",
      Content = [begin io_lib:fwrite("~p.\n", [Line]) end|| Line <- Term],
      ok = file:write_file(MiPushFilename, lists:flatten(Explain, Content)),
      ibrowse:rescan_config(MiPushFilename),
      ok
  end.

get_new_term(MiPush, Ibrowse) ->
  {ok, MiPushTerm} = file:consult(MiPush),
  {ok, IbrowseTerm} = file:consult(Ibrowse),
  lists:foldl(fun(Term, Acc) ->
    case lists:member(Term, Acc) of
      true -> Acc;
      false -> [Term|Acc]
    end  end, MiPushTerm, IbrowseTerm).


