-module(restcp).

-include("types.hrl").

-compile(export_all).

%%%----------------------------------------------------------------------
%%% Route API
%%%----------------------------------------------------------------------

-spec routes(app()) -> [route_desc()].
routes(App) ->
  gen_server:call(restcp_server, {routes, App}).

-spec route_add(app()) -> route_init_desc().
route_add(App) ->
  gen_server:call(restcp_server, {route_add, App}).

-spec route_delete(app(), route()) -> boolean().
route_delete(App, Route) ->
  gen_server:call(restcp_server, {route_delete, App, Route}).

%%%----------------------------------------------------------------------
%%% Backend API
%%%----------------------------------------------------------------------

-spec backends(app(), route()) -> [backend_desc()].
backends(App, Route) ->
  gen_server:call(restcp_server, {backends, App, Route}).

-spec backend_add(app(), route(), ip(), tcp_port()) -> backend().
backend_add(App, Route, IP, Port) ->
  gen_server:call(restcp_server, {backend_add, App, Route, IP, Port}).

-spec backend_update(app(), route(), backend(), ip(), tcp_port()) -> backend().
backend_update(App, Route, Backend, IP, Port) ->
  gen_server:call(restcp_server,
    {backend_update, App, Route, Backend, IP, Port}).

-spec backend_delete(app(), route(), backend()) -> boolean().
backend_delete(App, Route, Backend) ->
  gen_server:call(restcp_server, {backend_delete, App, Route, Backend}).
