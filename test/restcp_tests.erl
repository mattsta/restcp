-module(restcp_tests).
-include_lib("eunit/include/eunit.hrl").

-define(E(A, B), ?assertEqual(A, B)).
-define(_E(A, B), ?_assertEqual(A, B)).

restcp_setup_clean() ->
  application:start(restcp),
  ibrowse:start().

restcp_cleanup(_) ->
  ibrowse:stop(),
  application:stop(restcp).

restcp_direct_setup_then_teardown_test_() ->
  {setup,
    fun restcp_setup_clean/0,
    fun restcp_cleanup/1,
    fun(_C) ->
      [
        ?_E({1, "127.0.0.1", 10000}, restcp:route_add(123)),
        ?_E(1, restcp:backend_add(123, 1, "127.0.0.1", 9999)),
        ?_E([{"127.0.0.1", 9999}], restcp:backends(123, 1)),
        ?_E([{123, 1, {"127.0.0.1", 10000}, [{"127.0.0.1", 9999}]}],
              restcp:routes(123)),
        ?_E(true, restcp:backend_delete(123, 1, 1)),
        ?_E([], restcp:backends(123, 1)),
        ?_E([{123, 1, {"127.0.0.1", 10000}, []}], restcp:routes(123)),
        ?_E(true, restcp:route_delete(123, 1))
      ]
    end
  }.
  
restcp_direct_connectivity_test_() ->
  {setup,
    fun restcp_setup_clean/0,
    fun restcp_cleanup/1,
    fun(_C) ->
      SendRecvData = case file:read_file("/usr/share/dict/words") of
                       {ok, Words} -> Words;
                                 _ -> <<"datamatch">>
                     end,
      BackendProvider = fun(Port, Data) ->
                          fun() -> 
                            {ok, Listen} = gen_tcp:listen(Port,
                                             [{active, false},
                                              {reuseaddr, true}]),
                            {ok, Client} = gen_tcp:accept(Listen),
                            gen_tcp:send(Client, Data),
                            gen_tcp:close(Client),
                            gen_tcp:close(Listen)
                          end
                        end,
      spawn(BackendProvider(9999, SendRecvData)),
      [
        % Make sure nothing is listening on port 10000
        ?_E({error, econnrefused}, gen_tcp:connect("127.0.0.1", 10000, [])),
        % Add route, get first listen port of 10000
        ?_E({1, "127.0.0.1", 10000}, restcp:route_add(123)),
        % Verify we are listening on port 10000
        ?_E(connected,
          fun() ->
            case gen_tcp:connect("127.0.0.1", 10000, [{active, false}]) of
              {ok, Sock} -> gen_tcp:close(Sock),
                            connected;
              {error, _} -> failed
            end
          end()),
        % Add a backend
        ?_E(1, restcp:backend_add(123, 1, "127.0.0.1", 9999)),
        % Verify connecting to port 10000 receives data from port 9999
        ?_E(SendRecvData,
          fun() ->
            case gen_tcp:connect("127.0.0.1", 10000,
                                 [binary, {active, false}]) of
              {ok, Sock} -> Data = recv_all(Sock, []),
                            gen_tcp:close(Sock),
                            Data;
              {error, _} -> failed
            end
          end()),
        ?_E([{"127.0.0.1", 9999}], restcp:backends(123, 1)),
        ?_E([{123, 1, {"127.0.0.1", 10000}, [{"127.0.0.1", 9999}]}],
              restcp:routes(123)),
        % delete backend
        ?_E(true, restcp:backend_delete(123, 1, 1)),
        % Verify the backend is deleted
        ?_E([], restcp:backends(123, 1)),
        ?_E([{123, 1, {"127.0.0.1", 10000}, []}], restcp:routes(123)),
        ?_E(true, restcp:route_delete(123, 1)),
        % Verify the route is deletd
        ?_E([], restcp:routes(123)),
        % backend is deleted.  Nothing should be listening on port 10k anymore.
        ?_E({error, econnrefused}, gen_tcp:connect("127.0.0.1", 10000, []))
      ]
    end
  }.


restcp_rest_api_test_() ->
  {setup,
    fun restcp_setup_clean/0,
    fun restcp_cleanup/1,
    fun(_C) ->
      Routes = "http://localhost:8001/apps/123/routes",
      Route = Routes ++ "/1",
      Backends = Route ++ "/backends",
      Backend = Backends ++ "/1",
      FirstOneRouteBackendJson = route_backend_json(9999),
      UpdatedOneRouteBackendJson = route_backend_json(8888),
      [
        ?_E({1, <<"tcp://127.0.0.1:10000/">>},
          fun() ->
            R = http_put(Routes),
            JR = mochijson2:decode(R),
            {id(JR), url(JR)}
          end()),
        ?_E(1,
          fun() ->
            Json = mochijson2:encode({struct, [{<<"ip">>, <<"127.0.0.1">>},
                                              {<<"port">>, 9999}]}),
            R = http_put(Backends, Json),
            JR = mochijson2:decode(R),
            id(JR)
          end()),
        ?_E([{<<"127.0.0.1">>, 9999}],
          fun() ->
            R = http_get(Backends),
            JR = mochijson2:decode(R),
            [{ip(JR), port(JR)}]
          end()),
        % Verify the route and backend exists as expected:
        ?_E(FirstOneRouteBackendJson, http_get(Routes)),
        % Replace backend 9999 with backend 8888:
        ?_E(ok,
          fun() ->
            Json = mochijson2:encode({struct, [{<<"ip">>, <<"127.0.0.1">>},
                                              {<<"port">>, 8888}]}),
            http_post(Backend, Json),
            ok
          end()),
        % Verify the update took place:
        ?_E(UpdatedOneRouteBackendJson, http_get(Routes)),
        ?_E("", http_delete(Backend)),
        ?_E("[]", http_get(Backends)),
        ?_E("", http_delete(Route)),
        ?_E("[]", http_get(Routes)),
        ?_E({error, econnrefused}, gen_tcp:connect("127.0.0.1", 10000, []))
      ]
    end
  }.


% Helpers
id(Json) ->
  json_extract(id, Json).

ip(Json) ->
  json_extract(ip, Json).

port(Json) ->
  json_extract(port, Json).

url(Json) ->
  json_extract(url, Json).

backends(Json) ->
  json_extract(backends, Json).

json_extract(What, {struct, PropList}) ->
  json_extract(What, PropList);
json_extract(What, [{struct, PropList}]) ->
  json_extract(What, PropList);
json_extract(What, PropList) when is_atom(What) andalso is_list(PropList) ->
  proplists:get_value(list_to_binary(atom_to_list(What)), PropList).
  
http_get(Url) ->
  {ok, _RC, _ResponseHeaders, ResponseBody} = ibrowse:send_req(Url, [], get),
  ResponseBody.

http_put(Url) ->
  http_put(Url, []).

http_put(Url, Body) ->
  {ok, _RC, _ResponseHeaders, ResponseBody} =
    ibrowse:send_req(Url, [], put, Body),
  ResponseBody.

http_post(Url, RequestBody) ->
  {ok, _RC, _ResponseHeaders, ResponseBody} =
    ibrowse:send_req(Url,
      [{"Content-Type", "application/json"}], post, RequestBody),
  ResponseBody.

http_delete(Url) ->
  {ok, _RC, _ResponseHeaders, ResponseBody} = ibrowse:send_req(Url, [], delete),
  ResponseBody.

route_backend_json(Port) when is_integer(Port) ->
  J = mochijson2:encode([{struct, 
        [{<<"id">>, 1},
         {<<"url">>, <<"tcp://127.0.0.1:10000/">>},
         {<<"backends">>, [{struct, [{<<"ip">>, <<"127.0.0.1">>},
                                     {<<"port">>, Port}]}]}]}]),
  binary_to_list(iolist_to_binary(J)).

recv_all(Sock, Accum) ->
  case gen_tcp:recv(Sock, 0) of
    {ok, Data} -> recv_all(Sock, [Data | Accum]);
             _ -> iolist_to_binary(lists:reverse(Accum))
  end.
  
