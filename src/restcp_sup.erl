-module(restcp_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% Paths
% /apps/123/routes   <- PUT new route, GET all routes
% /apps/123/routes/1 <- DELETE route
% /apps/123/routes/1/backends   <- PUT new backend, GET all backends
% /apps/123/routes/1/backends/1 <- POST updated backend, DELETE backend
dispatch() ->
  [{["apps", app, "routes", route, "backends"],
     restcp_webmachine_backends_resource, []},
   {["apps", app, "routes", route, "backends", backend],
     restcp_webmachine_backends_resource, []},
   {["apps", app, "routes"],
     restcp_webmachine_routes_resource, []},
   {["apps", app, "routes", route],
     restcp_webmachine_routes_resource, []},
   {["tracer", '*'],
     wmtrace_resource, [{trace_dir, "./traces"}]}].

env(What, Default) ->
  case application:get_env(restcp, What) of
    undefined -> Default;
      {ok, X} -> X 
  end.

init([]) ->
  IP     = env(web_ip, "127.0.0.1"),
  Port   = env(web_port, "8001"),
  LogDir = env(log_dir, "./logs"),

  Dispatch = dispatch(),

  WebConfig = [{ip, IP}, {port, Port}, {log_dir, LogDir},
               {dispatch, Dispatch}],

  OneshotSup = {oneshot_sup,
                {oneshot_sup, start_link, []},
                permanent, 5000, supervisor, dynamic},

  RESTCP = {restcp_server,
            {restcp_server, start_link, []},
            permanent, 5000, worker, dynamic},

  Web = {webmachine_mochiweb,
        {webmachine_mochiweb, start, [WebConfig]},
         permanent, 5000, worker, dynamic},

  Processes = [OneshotSup,
               RESTCP,
               Web],

  Strategy = {one_for_one, 10, 10},
  {ok,
   {Strategy, Processes}}.
