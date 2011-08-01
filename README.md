restcp: rest-driven tcp proxy
=============================

## REST API

### Create a route

    curl -XPUT http://localhost:8001/apps/123/routes
     -> '{"id": 1, "url": "tcp://localhost:10000/"}'

### Attach a backend to a route

    curl -XPUT -d '{"ip": "127.0.0.1", "port": 9999}' http://localhost:8001/apps/123/routes/1/backends
     -> '{"id": 1}'

### Detach a backend from a route

    curl -XDELETE http://localhost:8001/apps/123/routes/1/backends/1

### Update a backend attached to a route

    curl -XPOST -d '{"ip": "127.0.0.1", "port": 8888}' http://localhost:8001/apps/123/routes/1/backends/1

### Get backends

    curl -XGET http://localhost:8001/apps/123/routes/1/backends
     -> '[{"ip": "127.0.0.1", "port": 8888}]'

### Get routes

    curl -XGET http://localhost:8001/apps/123/routes
     -> '{"id": 1, "url": "tcp://localhost:10000/", "backends": [{"ip": "127.0.0.1", "port": 8888}]}'

### Destroy a route

    curl -XDELETE http://localhost:8001/apps/123/routes/1

## Example

    $ curl -XPUT http://localhost:8001/apps/123/routes
    {"id": 1, "url": "tcp://localhost:10000/"}
    $ curl -XPUT -d '{"ip": "127.0.0.1", "port": 8888}' http://localhost:8001/apps/123/routes/1/backends
    {"id": 1}
    $ ncat -l -p 8888

    * separate tab *
    $ telnet 127.0.0.1 10000
    Trying 127.0.0.1...
    Connected to localhost.
    Escape character is '^]'.


Code Overview
-------------

`restcp_server` holds ets tables containing the configuration of routes
and backends.  External services call into `restcp_server` to list routes,
provision routes, add backends, modify backends, and remove routes.

Webmachine manages all restcp services by using only the interface defined
in `restcp`.

When a new route is provisioned, it gets set up on 127.0.0.1 with port numbers
starting at 10000 and incrementing with each new route.  A route consists of
an entry in the `routes` ets table and a spawned `restcp_proxy` process.

The `restcp_proxy` process handles listening on the assigned IP and port
for new client connections.  When a new client connects, a new `restcp_pipe`
is spawned to handle choosing a backend and relaying traffic between the
backend and the client.

When a client disconnects, its associated `restcp_pipe` terminates.  When a
route is deleted, its associated `restcp_proxy` terminates and all new
incoming connections to the old IP and port will be refused.  When a backend
is deleted, it gets removed from the pool of addresses `restcp_pipe` looks at
when picking new backends for clients.


Tests
-----
     rebar eunit suite=restcp_tests skip_deps=true


Turning restcp into a production-ready service
----------------------------------------------

### Health Checks

Backends are currently not checked for reachability except for when one
is picked to serve a request.   We close a client connection if the chosen
backend is down or unreachable.

Testing reachability of backends would allow us to have a known-good set of
backends so clients have much lower chances of connecting to a down backend.

Health checks could be implemented by having each `restcp_proxy` server spawn
a `restcp_health_checker` process whose job is to maintain up/down flags on
backends belonging to the proxy.  Then, when a new client connects, only the
list of reachable backends has to be consulted to complete the connection.


### Load Balancing / Clustering / Stable Configuration / Multiple Frontend IPs

NB: This section assumes the REST front-end provisioning is happening on
any node in the cluster.  The node you send provisioning requests to is not
necessarily the same node where your proxy will exist.

`restcp` currently exists to be configured and accessed on one erlang node only.
Making `restcp` usable in a production environment means making the mapping of
routes (consisting of a frontend IP, frontend port, app id, and route id),
backends (consisting of a list of backend ips and ports, app id, route id,
and backend id) and available front ends (consisting of IP addresses and ports
where we can offer route services) accessible from any provisioning or
proxying node.

As an example, a redis-based data model could store configs as:

    redis type          example key             example value
    --------------------------------------------------------------
    integer             route_counter           1
    list                frontend_ips            [127.0.0.1, 66.23.249.241]
    list                ip:127.0.0.1:routes     [route:1, route:7, ...]
    hash                route:1                 {ip, port}
    integer             route:1:backend_counter 13
    list                route:1:backends_live   [1, 2, 5, 7, 12]
    list                route:1:backends_dead   [4, 9, 10]
    hash                route:1:backend:1       {ip, port}

Provisioning becomes an exercise in distributed notifications when
dealing with live infrastructure middleware.  The node
handling REST requests probably will not be the same node where the proxy
lives.  The provisioning server can get along fine having no concept of
nodes as long as the responsible node gets notified of configuration changes
and tells the provisioning server of the final decisions (allocated front
end port number, for instance).

One way to decouple provisioning REST endpoint vs. proxies is by using
redis pubsub to notify proxy nodes about new provisions.  For example,
server 127.0.0.1 subscribes to channel `provision:127.0.0.1`,
then the provisioning server calls `publish provision:127.0.0.1 appid:routeid`
to notify the proxy node of a new route.  Server 127.0.0.1 provisions
appid:routeid, assigns a port number, then publishes the result back to
any interested provisioning server with
`publish provision-success:appid:routeid provisionedPortNumber`.  Something
similar would happen for updates and deletions.

If a node reboots and needs to resume its route responsibilities, it can
load the key having its assigned routes (e.g. `ip:127.0.0.1:routes`).  The
proxy can look up each route, retrieve the old IP and port allocation, then
re-initialize all assigned proxy frontends as they were previously allocated.
