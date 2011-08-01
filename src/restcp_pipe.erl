-module(restcp_pipe).
-behaviour(gen_server).

-include("types.hrl").

-compile(export_all).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%% api callbacks
-export([start/1]).

-record(state, {backend       :: ip_port(),
                backend_sock  :: socket(),
                frontend_sock :: socket()
               }).

%%%--------------------------------------------------------------------
%%% api callbacks
%%%--------------------------------------------------------------------
start(AppRoute) ->
  gen_server:start(?MODULE, [AppRoute], []).

%%%--------------------------------------------------------------------
%%% gen_server callbacks
%%%--------------------------------------------------------------------
init([{App, Route}]) ->
  case restcp:backends(App, Route) of
          [] -> ignore;
    Backends -> RandomPosition = crypto:rand_uniform(1, length(Backends) + 1),
                {IP, Port} = lists:nth(RandomPosition, Backends),
                {ok, #state{backend = {IP, Port}}}
  end.

handle_call(Call, _From, State) ->
  {reply, Call, State}.

handle_cast({connect, FrontendSock}, State) ->
  BackendSock = init_backend(State),
  % {active, true} feels dirty, but we are just a pipe. who needs flow control?
  inet:setopts(FrontendSock, [{active, true}]),
  inet:setopts(BackendSock, [{active, true}]),
  {noreply, State#state{backend_sock = BackendSock,
                        frontend_sock = FrontendSock}}.

handle_info({tcp, FrontendSock, Data},
    #state{frontend_sock = FrontendSock,
           backend_sock = BackendSock} = State) ->
  gen_tcp:send(BackendSock, Data),
  % If we are reciving on {active, once}, reset once here.
  {noreply, State};

handle_info({tcp, BackendSock, Data},
    #state{frontend_sock = FrontendSock,
           backend_sock = BackendSock} = State) ->
  gen_tcp:send(FrontendSock, Data),
  % If we are reciving on {active, once}, reset once here.
  {noreply, State};

handle_info({tcp_closed, _},
    #state{frontend_sock = FrontendSock,
           backend_sock = BackendSock} = State) ->
  gen_tcp:close(FrontendSock),
  gen_tcp:close(BackendSock),
  {stop, normal, State};

handle_info(Info, State) ->
  io:format("Other info: ~p with state ~p~n", [Info, State]),
  {noreply, State}.

terminate(_Reason, #state{frontend_sock = FrontendSock,
                          backend_sock = BackendSock}) ->
  gen_tcp:close(FrontendSock),
  gen_tcp:close(BackendSock).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%--------------------------------------------------------------------
%%% Internal
%%%--------------------------------------------------------------------

init_backend(#state{backend = {IP, Port}}) ->
  case gen_tcp:connect(IP, Port, [binary]) of
    {ok, BackendSock} -> BackendSock;
         {error, Err} -> lager:error("Accept Error: ~p", [Err]),
                         exit(self(), shutdown)
  end.
