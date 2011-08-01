-module(restcp_app).

-behaviour(application).
-export([start/2,stop/1]).

-spec start(any(), any()) -> any().
start(_Type, _StartArgs) ->
  deps(),
  restcp_sup:start_link().

deps() ->
  Ebin = filename:dirname(code:where_is_file("restcp_app.beam")),
  Deps = filelib:wildcard(Ebin ++ "/../deps/*/"),
  DepsEbins = filelib:wildcard(Ebin ++ "/../deps/*/{ebin,priv}/"),
  [code:add_pathz(P) || P <- Deps],
  [code:add_pathz(P) || P <- DepsEbins],
  ok.

-spec stop(any()) -> any().
stop(_State) ->
  ok.
