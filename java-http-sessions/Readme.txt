			Session Pool and Load Balancer Example

 There are many client APIs for AllegroGraph.  Here we use the HTTP API
which forms the basis for the Java and Python APIs.

 The HTTP API is useful if you wish to issue  sparql queries to the repository
and can process the query result form returned by the server.  There are many
possible result formats but in this code we just accept the default result format.

 This code demonstrates how you can can create a session pool using the HTTP API.
Using a session pool allows you create a session backend with the repository
open and send queries that backend without going through the main AllegroGraph
front end (assuming that you haven't set  "UseMainForForSessions true" in your
agraph.cfg file).

 We then implement a load balancer on top of the session pool.  This isn't
a typical load balancer that proxies requests to a set of back ends.  Instead
this Load Balancer just allocates sessions from a set of session pools.
Once the session is returned the client talks directly to the session backend
and the load balancer is not used again.


To use this:

edit com.franz.sessionpool.LbDemo
and change the setup code in main to specify repositories on your server.
If you specify more than one repository they should have identical
content (either via MMR or warm standby replication).


Edit com.franz.sessionpool.SampleQuery
and put in a query you want to execute.

type
% make compile

then with the server running

% make run

You can specify e parameters to the 'make run' call, e..g

% make threads=10 queries=20 lifetime=60 initial=10 worker=5 run

with the meaning of each parameter

threads - start these many threads to run queries
queries - each thread will run the query this many times.  Each time it
  runs a query it get a session out of the pool through the load balancer
  and puts the session back when the query is over
lifetime - when a session  is started it is given an idle time and if the
  session is idle for more than this many seconds the session process may be killed
initial - when the session pools is created this many sessions are created
  immediately
worker - this is just a number which is printed to identify which worker
  is printing a message.

you may with to run this program simultaneously many times. Here'a
a way to run it 15 times. 

% for i in {1..15} ; do (make queries=200 threads=20 lifetime=500 initial=20 worker=$i run &)  \
; done


In this case you have 15 java programs, each starting 20 threads (so 15*20 = 300 threads)
and each thread running 200 queries (so 300*200 = 60,000 queries).
You start the session pool with 20 sessions which means the 20 threads will
each find a pre-allocated session to use saving a bit of time creating a
new session).



Example:

jkf@epic java]$ make compile
javac -cp "lib/*" com/franz/sessionpool/*.java

[jkf@epic java]$ make run
java -cp "lib/*:." com.franz.sessionpool.LbDemo --threads 4\
           --queries 50 --lifetime 32 --initial 7 --worker 9999
4 threads, 50 queries, 32 lifetime, 7 initial sessions
Thread 9999.0 Started
Thread 9999.1 Started
Thread 9999.2 Started
Thread 9999.3 Started
Thread 9999.0 Finished
Thread 9999.3 Finished
Thread 9999.1 Finished
Thread 9999.2 Finished
[jkf@epic java]$ 



