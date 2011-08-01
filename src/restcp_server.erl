-module(restcp_server).
-behaviour(gen_server).

-include("types.hrl").

-compile(export_all).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%% api callbacks
-export([start_link/0]).

-record(route, {app_route    :: app_route(),
                frontend     :: ip_port(),
                proxy        :: pid()
               }).

-record(backend, {app_route   :: app_route(),
                  backend_id  :: backend(),
                  backend     :: {ip(), tcp_port()}
                 }).

-record(state, {routes         :: tid(),
                backends       :: tid(),
                counters       :: tid()
               }).

%%%--------------------------------------------------------------------
%%% api callbacks
%%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%--------------------------------------------------------------------
%%% gen_server callbacks
%%%--------------------------------------------------------------------

init([]) ->
  Routes = ets:new(routes, [set, {keypos, 2}]),
  Backends = ets:new(backends, [duplicate_bag, {keypos, 2}]),
  Counters = ets:new(counters, [set]),

  % Let's start frontend ports at 10000.  9999 + first increment = 10000:
  ets:insert(Counters, {frontend_ports, 9999}),

  {ok, #state{routes = Routes,
              backends = Backends,
              counters = Counters}}.

-spec handle_call({routes, app()}, any(), #state{}) ->
                   {reply, [route_desc()], any()};
                 ({route_add, app()}, any(), #state{}) ->
                   {reply, route_init_desc(), any()};
                 ({route_delete, app(), route()}, any(), #state{}) ->
                   {reply, boolean(), any()};
                 ({backends, app(), route()}, any(), #state{}) ->
                   {reply, [backend_desc()], any()};
                 ({backend_add, app(), route(),
                   ip(), tcp_port()}, any(), #state{}) ->
                     {reply, backend(), any()};
                 ({backend_update, app(), route(), backend(),
                   ip(), tcp_port()}, any(), #state{}) ->
                     {reply, backend(), any()};
                 ({backend_delete, app(), route(), backend()},any(),#state{}) ->
                   {reply, boolean(), any()}.

handle_call({routes, App}, _From, #state{routes = Routes,
                                         backends = Backends} = State) ->
  AllRoutes = ets:match(Routes, #route{app_route = {App, '$1'},
                                       frontend = '$2',
                                       proxy = '_'}),
  AppRouteBackends =
  [{App, Route, Frontend,
    backends(Backends, App, Route)} || [Route, Frontend] <- AllRoutes],
  {reply, AppRouteBackends, State};

handle_call({route_add, App}, _From,
    #state{routes = Routes,
           counters = Counters} = State) ->
  NewRouteId = increment(Counters, {app, App}),
  AssignedPort = increment(Counters, frontend_ports),
  AssignedIP = "127.0.0.1",
  Frontend = {AssignedIP, AssignedPort},
  FrontendPid = spawn_frontend({App, NewRouteId}, Frontend),
  add_route(Routes, App, NewRouteId, Frontend, FrontendPid),
  {reply, {NewRouteId, AssignedIP, AssignedPort}, State};

handle_call({route_delete, App, Route}, _From,
    #state{routes=Routes,
           backends = Backends} = State) ->
  delete_route(Routes, Backends, App, Route),
  {reply, true, State};

handle_call({backends, App, Route}, _From,
    #state{backends = Backends} = State) ->
  FoundBackends = backends(Backends, App, Route),
  {reply, FoundBackends, State};

handle_call({backend_add, App, Route, IP, Port},
    _From, #state{backends = Backends, counters = Counters} = State) ->
  CounterKey = {backend, App, Route},
  NewBackendId = increment(Counters, CounterKey),
  add_backend(Backends, App, Route, NewBackendId, IP, Port),
  {reply, NewBackendId, State};

handle_call({backend_update, App, Route, Backend, IP, Port},
    _From, #state{backends = Backends} = State) ->
  delete_backend(Backends, App, Route, Backend),
  add_backend(Backends, App, Route, Backend, IP, Port),
  {reply, Backend, State};

handle_call({backend_delete, App, Route, Backend},
    _From, #state{backends = Backends} = State) ->
  delete_backend(Backends, App, Route, Backend),
  {reply, true, State}.


handle_cast(_What, State) ->
  {noreply, State}.

handle_info(Info, State) ->
  io:format("Other info: ~p with state ~p~n", [Info, State]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%--------------------------------------------------------------------
%%% Internal API
%%%--------------------------------------------------------------------

app_route(App, Route) ->
  {App, Route}.

add_route(Routes, App, RouteId, Frontend, FrontendPid) ->
  ets:insert(Routes, #route{app_route = {App, RouteId},
                            frontend = Frontend,
                            proxy = FrontendPid}).

delete_route(Routes, Backends, App, Route) ->
  RouteKey = app_route(App, Route),
  case ets:lookup(Routes, RouteKey) of
    [#route{proxy = ProxyPid}] -> exit(ProxyPid, shutdown);
                             _ -> ok
  end,
  ets:delete(Routes, RouteKey),
  ets:delete(Backends, RouteKey).

add_backend(Backends, App, Route, BackendId, IP, Port) ->
  ets:insert(Backends, #backend{app_route = {App, Route},
                                backend_id = BackendId,
                                backend = {IP, Port}}).

backends(Backends, App, Route) ->
  [B#backend.backend || B <- ets:lookup(Backends, app_route(App, Route))].

delete_backend(Backends, App, Route, BackendId) ->
  ets:match_delete(Backends, #backend{app_route = {App, Route},
                                      backend_id = BackendId,
                                      backend = '_'}).

increment(CounterTable, CounterKey) ->
  % update_counter only updates existing keys.  If key doesn't exist, create.
  case ets:member(CounterTable, CounterKey) of
    false -> ets:insert(CounterTable, {CounterKey, 1}),
             1;
     true -> ets:update_counter(CounterTable, CounterKey, 1)
  end.

spawn_frontend(RouteId, {ListenIP, ListenPort}) ->
  {ok, Pid} = restcp_proxy:start(RouteId, ListenIP, ListenPort),
  gen_server:cast(Pid, listen),
  Pid.
