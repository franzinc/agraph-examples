
package com.franz.sessionpool;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author jkf
 *  The LoadBalancer class holds a set of SessionPool objects which 
 * should reference repos with the same contents (e.g. through MMR).
 * This class will return sessions from one of the SessionPools distributing
 * the sessions between the pools in a round robin fashion.
 * Unlike a typical load balancer this class does not proxy the request
 * and results.  It simply returns a session object and from them on
 * the client and server are directly connected.
 */
public class LoadBalancer {

    ArrayList<SessionPool> servers;
    int poolindex = 0;
    int poolmax;

    

    /*
     This constructor assumes that the same catalog:repo is to be
     accessed on all machines given in the urls
     The urls are like  http://machinex:234234
     just specifying the scheme, host and port.
     
     to use this:
       LoadBalancer lb = new LoadBalancer( ... )  // choose a constructor
      then when a session is needed:
       Session sess = lb.getSessionFromPool();
      and when you no longer need the session
       sess.returnToPool();
    */
    
    LoadBalancer(List<String> urls, String catalog, String reponame,
                String username, String password, int lifetime,
                int initialSessions) throws Exception
    {
        servers = new ArrayList<>();
        
        for (String url : urls) {
            servers.add(SessionPool.setupSessionPool(url, catalog, reponame, username,
                                   password, lifetime, initialSessions));
        }
        
        poolindex = 0;
        poolmax = urls.size();
    }
    
    /*
    this constructor allows you to load balance against repos and
    specify different names and authentication for each repo
    */
    LoadBalancer(ArrayList<ServerSpec> sss, int lifetime, int initialSessions)
            throws Exception
    {
        servers = new ArrayList<>();
        
        for (ServerSpec ss :sss) {
            servers.add(SessionPool.setupSessionPool(ss.url, ss.catalog,
                    ss.reponame, ss.username, ss.password, lifetime,
                    initialSessions));
        }
        
        poolindex = 0;
        poolmax = sss.size();
    }
    
    SessionPool.Session getSessionFromLoadBalancer()
            throws Exception {

        int useindex;
        
        for (int count = 0; count < 3*poolmax; count++) {
            /* try three times to get a session before giving up */
            
            synchronized (this) {
                poolindex = (poolindex + 1) % poolmax;
                useindex = poolindex;
            }

            try {
                return servers.get(useindex).getSessionFromPool();
            } catch (Exception e) {
            }

        }

        /* all servers are down */
        throw new Exception("All servers cannot create a session");
    }

    
}

