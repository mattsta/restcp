-module(restcp_webmachine_routes_resource).

-export([init/1,
         content_types_provided/2,
         content_types_accepted/2,
         get_routes/2,
         new_route/2,
         is_conflict/2,
         resource_exists/2,
         allowed_methods/2,
         delete_resource/2]).

-compile(export_all).

-include_lib("webmachine/include/webmachine.hrl").

%%%--------------------------------------------------------------------
%%% api callbacks
%%%--------------------------------------------------------------------

init(_Config) ->
  {ok, nil}.
%  {{trace, "./traces"}, nil}.

allowed_methods(ReqData, Context) ->
  {['GET', 'PUT', 'DELETE'], ReqData, Context}.

resource_exists(ReqData, Context) ->
  {true, ReqData, Context}.

% PUT requires conflict override.  We're making new resources off a base URI.
is_conflict(ReqData, Context) ->
  {false, ReqData, Context}.

% GET requests
content_types_provided(ReqData, Context) ->
  {[{"application/json", get_routes}], ReqData, Context}.

% PUT requests
content_types_accepted(ReqData, Context) ->
  % Accept anything.  We only care about the method, not the content.
  {Type, _} = webmachine_util:media_type_to_detail("application/octet-stream"),
  {[{Type, new_route}], ReqData, Context}.

% DELETE requests
delete_resource(ReqData, Context) ->
  App = app(ReqData),
  Route = route(ReqData),
  restcp:route_delete(App, Route),
  {true, ReqData, Context}.

% GET
get_routes(ReqData, Context) ->
  % dump all routes and their backends
  App = app(ReqData),
  Routes = restcp:routes(App),
  Json = [{struct,
           [{<<"id">>, RouteId},
            {<<"url">>, url(Host, FEPort)},
            {<<"backends">>,
              [{struct,
                 [{<<"ip">>, IP},
                  {<<"port">>, BEPort}]} || {IP, BEPort} <- Backends]}
         ]} || {_AppId, RouteId, {Host, FEPort}, Backends} <- Routes],
  {mochijson2:encode(Json), ReqData, Context}.

% PUT
new_route(ReqData, Context) ->
  App = app(ReqData),
  {RouteId, AssignedIP, AssignedPort} = restcp:route_add(App),
  Json = {struct,
           [{<<"id">>, RouteId},
            {<<"url">>, url(AssignedIP, AssignedPort)}
         ]},
  NewData = wrq:set_resp_body(mochijson2:encode(Json), ReqData),
  {true, NewData, Context}.

%%%--------------------------------------------------------------------
%%% internals
%%%--------------------------------------------------------------------

app(ReqData) -> list_to_integer(wrq:path_info(app, ReqData)).
route(ReqData) -> list_to_integer(wrq:path_info(route, ReqData)).

url(Host, Port) when is_list(Host) andalso is_integer(Port) ->
  list_to_binary("tcp://" ++ Host ++ ":" ++ integer_to_list(Port) ++ "/").
