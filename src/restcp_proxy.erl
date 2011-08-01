-module(restcp_proxy).
-behaviour(gen_server).

-include("types.hrl").

-compile(export_all).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%% api callbacks
-export([start/3]).

-record(state, {app_route      :: app_route(),
                sock           :: term()  % opaque socket term
               }).

%%%--------------------------------------------------------------------
%%% api callbacks
%%%--------------------------------------------------------------------
start(AppRoute, ListenIP, ListenPort) when is_integer(ListenPort) ->
  gen_server:start(?MODULE, [AppRoute, ListenIP, ListenPort], []).

%%%--------------------------------------------------------------------
%%% gen_server callbacks
%%%--------------------------------------------------------------------
init([AppRoute, ListenIP, ListenPort]) ->
  UseIP = case inet:getaddr(ListenIP, inet) of
            {ok, Addr} -> Addr;
            {error, _} -> throw({?MODULE, invalid_ip})
          end,

  case gen_tcp:listen(ListenPort, [{ip, UseIP}, binary, {backlog, 511},
                                   {active, false},
                                   {reuseaddr, true}]) of
    {ok, Sock} -> {ok, #state{app_route = AppRoute,
                              sock = Sock}};
    {error, Err} -> {stop, {Err, {AppRoute, ListenIP, ListenPort}}}
  end.

handle_call(Call, _From, State) ->
  {reply, Call, State}.

handle_cast(listen, State) ->
  listen(State),
  {noreply, State}.

handle_info(Info, State) ->
  io:format("Other info: ~p with state ~p~n", [Info, State]),
  {noreply, State}.

terminate(_Reason, #state{sock = Sock}) ->
  gen_tcp:close(Sock).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%--------------------------------------------------------------------
%%% Internal
%%%--------------------------------------------------------------------

listen(#state{app_route = AppRoute,
              sock = ListenSock} = State) ->
  case gen_tcp:accept(ListenSock) of
    {ok, ClientSock} -> case restcp_pipe:start(AppRoute) of
                          {ok, Pipe} ->
                              gen_tcp:controlling_process(ClientSock, Pipe),
                              gen_server:cast(Pipe, {connect, ClientSock});
                          ignore -> gen_tcp:close(ClientSock)
                        end;
        {error, Err} -> lager:error("Accept Error: ~p", [Err])
  end,
  listen(State).
