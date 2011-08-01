-module(restcp_webmachine_backends_resource).

-export([init/1,
         content_types_provided/2,
         content_types_accepted/2,
         get_backends/2,
         new_backend/2,
         update_backend/2,
         is_conflict/2,
         process_post/2,
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
  {['GET', 'PUT', 'POST', 'DELETE'], ReqData, Context}.

resource_exists(ReqData, Context) ->
  {true, ReqData, Context}.

% PUT requires conflict override.  We're making new resources off a base URI.
is_conflict(ReqData, Context) ->
  {false, ReqData, Context}.

% GET requests
content_types_provided(ReqData, Context) ->
  {[{"application/json", get_backends}], ReqData, Context}.

% PUT requests
content_types_accepted(ReqData, Context) ->
  % Accept anything.  We only care about the method, not the content.
  {Type, _} = webmachine_util:media_type_to_detail("application/octet-stream"),
  {[{Type, new_backend}], ReqData, Context}.

% DELETE requests
delete_resource(ReqData, Context) ->
  App = app(ReqData),
  Route = route(ReqData),
  Backend = backend(ReqData),
  restcp:backend_delete(App, Route, Backend),
  {true, ReqData, Context}.

% POST
process_post(ReqData, Context) ->
  update_backend(ReqData, Context),
  {true, ReqData, Context}.

% GET
get_backends(ReqData, Context) ->
  % dump all routes and their backends
  App = app(ReqData),
  Route = route(ReqData),
  Backends = restcp:backends(App, Route),
  Json = [{struct,
           [{<<"ip">>, IP},
           {<<"port">>, BEPort}]} || {IP, BEPort} <- Backends],
  {mochijson2:encode(Json), ReqData, Context}.

% PUT
new_backend(ReqData, Context) ->
  App = app(ReqData),
  Route = route(ReqData),
  {IP, Port} = ip_port(wrq:req_body(ReqData)),
  Backend = restcp:backend_add(App, Route, IP, Port),
  Json = {struct,
           [{<<"id">>, Backend}]},
  NewData = wrq:set_resp_body(mochijson2:encode(Json), ReqData),
  {true, NewData, Context}.

update_backend(ReqData, _Context) ->
  App = app(ReqData),
  Route = route(ReqData),
  Backend = backend(ReqData),
  {IP, Port} = ip_port(wrq:req_body(ReqData)),
  restcp:backend_update(App, Route, Backend, IP, Port).

%%%--------------------------------------------------------------------
%%% internals
%%%--------------------------------------------------------------------

app(ReqData) -> list_to_integer(wrq:path_info(app, ReqData)).
route(ReqData) -> list_to_integer(wrq:path_info(route, ReqData)).
backend(ReqData) -> list_to_integer(wrq:path_info(backend, ReqData)).

ip_port(Json) ->
  {struct, JsonProps} = mochijson2:decode(Json),
  IP = proplists:get_value(<<"ip">>, JsonProps),
  Port = proplists:get_value(<<"port">>, JsonProps),
  {IP, Port}.
