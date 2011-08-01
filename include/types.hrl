-type app() :: integer().
-type route() :: integer().
-type backend() :: integer().

-type app_route() :: {app(), route()}.
-type route_backend() :: {app_route(), backend()}.

-type ip() :: string().
-type tcp_port() :: 0..65535.
-type ip_port() :: {ip(), tcp_port()}.

-type backend_desc() :: [ip_port()].
-type route_desc() :: {app(), route(), ip_port(), backend_desc()}.
-type route_init_desc() :: {route(), ip(), tcp_port()}.

-type tid() :: pos_integer().  % ets table ID
-type socket() :: port().      % sockets are Erlang Ports
