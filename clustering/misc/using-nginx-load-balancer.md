# AllegroGraph Replication using NGINX Load Balancer

[TL;DR: Working configuration example](#allegrograph-and-nginx-configuration-changes)


## Load balancing for AllegroGraph cluster

AllegroGraph supports a distributed deployment scheme called
[Multi-master Replication (MMR)][mmr], which is useful in cases when
increased availability and data safety is required. As with other
distributed deployment schemes, an MMR cluster consists of multiple AG
server instances that are available for client access. Naturally, the
access load can be distributed among these instances using a load
balancer, but doing it correctly requires some understanding of the
AllegroGraph architecture.

From a high-level point of view, the problem with using AllegroGraph
behind any kind of proxy is that AllegroGraph clients encourage
stateful, session-based workflow.

AllegroGraph sessions are dedicated connections from client agents
(either Java, Python or Lisp client libraries) to separate processes
within the running AG server. This approach improves the access
performance by allocating dedicated resources for each client's
operations, but the problem with session-based workflow is that
sessions are inherently stateful: both client and server maintain
information about a particular session used for interacting, and while
the client simply supplies this information as part of every request,
AG server actually allocates certain resources (e.g. separate process
and port number), so this state cannot be moved or replicated across
AG server instances.

It is clear that in order to resolve this problem in general, the application
using AllegroGraph client libraries must adopt a stateless approach to
interacting with AG deployments. This may require significant changes
and imposes certain restrictions on application side. For example,
a Java application interacting with an AG server via [Java AllegroGraph
client][agraph-java] should not use transactions (more specifically,
`RepositoryConnection.begin()` method), since the Java AllegroGraph client
implicitly requests a dedicated session whenever a transaction is
started.

In this document, we start with a two-node MMR cluster and a
simplistic NGINX load balancer setup that doesn't work as expected,
and develop a working solution, providing an explanation for each
step, so that user is able to choose a different approach for
bypassing the underlying problem.


## Problems with the simplistic load balancer setup

Assume we have two AG servers organized into an MMR cluster. The
instances are running on `back1:10035` and `back2:10035`. Now let's
take a look at the following simplistic NGINX configuration:

```
upstream agraph-backends {
    server back1:10035;
    server back2:10035;
}

server {
    listen 80;
    server_name front;

    root /usr/share/nginx/html/;

    location / {
        proxy_pass http://agraph-backends;
        proxy_http_version 1.1;
    }
}
```

Here we are creating a load balancer proxy listening on `front:80` and
redirecting all requests to AG servers `back1:10035` and
`back2:10035` in a round-robin manner.

Whenever an AG client opens a connection to `front:80`, the instance
that is selected by load balancer to respond will assume its host name
is the one specified in the request's `Host` header and will respond
with a dedicated session URL `http://agraph-backends:<session
port>/<session details>`. In order to use the dedicated session, the
client now must be able to open a direct connection at this location
and perform all the operations over it.

The first problem that arises is that the `agraph-backends` hostname
cannot be resolved from the client. In order to fix this, AG servers
can be configured to provide fixed hostnames in session-specific URLs,
using the `SessionHost` configuration option:

```
# back1: agraph.cfg
SessionHost back1

# back2: agraph.cfg
SessionHost back2
```

This will make AG servers provide correct URLs for sessions, but
several other problems arise:
- `back1` and `back2` may be internal hosts and it may be undesirable
  to allow connections to them;
- even if `back1` and `back2` can be accessed from outside, under this
  setup only the session opening requests will go through the load
  balancer, and all consequent operations will bypass the load
  balancer altogether, so this will break the assumption about the load
  balancer being the entrypoint into the cluster;
- (this applies only to the Java client) credentials specified for
  `front:80` will be limited in scope to the `front` hostname, and
  won't be used with `back1` and `back2` hosts, which will break
  session connections completely.


## How to make it work

In order to resolve all these problems at once, we will force all
sessions to go through the main port:

```
# agraph.cfg
UseMainPortForSessions true

# Note that SessionHost option must be removed.
# SessionHost back1
```

We also need AG servers to think that their host name is the one of
the load balancer, so we add this line to the `server.location`
section in the NGINX configuration:

```
server {
    ...
    location / {
        ...
        proxy_set_header Host $host;
    }
}
```

Now all session URLs will be of the form `http://front:10035/<session
details>`. The problem is that the load balancer listens on
`front:80`, so we need to change the port to be able to connect to
the session-specific locations (in other words, both load balancer
port and AG server ports must be the same) :

```
server {
    listen 10035;
    ...
}
```


Finally, round-robin load balancing won't work in this case. When a
session-opening requests hits one of the AG servers (say,
`back1:10035`), the new session is created, but the other instance
(`back2:10035`) doesn't know about it, so the next request, that will
go to `back2:10035`, will just fail. In order to fix this, we can use
session (HTTP session, not AG session in this case) persistence
methods available in the NGINX Plus. Otherwise, we can use IP hash
load balancing: all request from the same IP will go to the same
instance, so once the AG session is open, all requests from the
corresponding client will go the same host.


## AllegroGraph and NGINX configuration changes

Here are all AllegroGraph and NGINX configuration changes required for
a two-node MMR cluster behind an NIGNX load balancer:

- AllegroGraph configuration change: add this line to each AG server's
  `agraph.cfg` file:
  ```
  UseMainPortForSessions true
  ```

- NIGNX configuration: add file `agraph.conf` to the NGINX site
  configuration directory (usually `/etc/nginx/conf.d`, assuming the
  `nginx.conf` contains a `include /etc/nginx/conf.d/*.conf;` line):
  ```
  upstream agraph-backends {
      ip_hash;
      server back1:10035;
      server back2:10035;
  }

  server {
      listen 10035;
      server_name front;

      root /usr/share/nginx/html/;

      location / {
          proxy_pass http://agraph-backends;
          proxy_http_version 1.1;
          proxy_set_header Host $host;
      }
  }
  ```

[mmr]: https://franz.com/agraph/support/documentation/current/multi-master.html
[agraph-java]: https://github.com/franzinc/agraph-java-client
