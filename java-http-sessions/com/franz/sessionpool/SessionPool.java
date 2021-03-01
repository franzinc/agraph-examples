package com.franz.sessionpool;


import java.util.*;
import java.util.concurrent.*;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.auth.AuthScope;
import org.apache.http.auth.UsernamePasswordCredentials;
import org.apache.http.client.HttpClient;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpUriRequest;
import org.apache.http.client.protocol.HttpClientContext;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.BasicCredentialsProvider;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.util.EntityUtils;

/*
 *
 * @author jkf
 *
 * The SessionPool class holds a set of sesssion urls for a single repository
 * on a single AllegroGraph server.
 *
 * setupSessionPool creates the ServerPool object and starts an initial set
 * of sessions on the server.  More sessions will be created as needed.
 * Sessions on the server will go away if they are idle for a specfied
 * number of seconds.
 *
 * The sessions are represented as strings which are the urls that directly
 * send a message to the session backend.  This use of sessions is distinct
 * from the classes used by the AllegroGraph Java Client code.  The sessions
 * returned here have to be used with the HTTP API of AllegroGraph.
 *
 * This is designed to work with the LoadBalancer class which is useful if
 * the same repository is replicated on multiple servers and you wish to
 * distribute the load between servers.  The Session class holds the
 * session url as well as the ServerPool object.
 *
 * To use this SessionPool class if you're not using the LoadBalancer
 * class:
 *    SessionPool sp = setupSessionPool( ....)
 *  then when you need a session
 *    Session sess = sp.getSessionFromPool();
 *  and when you're done with the session
 *    sess.returnToPool();
 */

// doc about classes.
// https://docs.oracle.com/javase/8/docs/api/index.html?java/util/concurrent/package-summary.html
// https://www.vogella.com/tutorials/ApacheHttpClient/article.html


public class SessionPool {
    
    static final boolean debug = false; /* set to true for printing some actions */
    
    String serverurl;
    String repopath;
    
    ConcurrentLinkedDeque<String> sessionurls;
    int lifetime;
    String username;
    String password;
    
    public final static int method_get = 1;
    public final static int method_post = 2;
    
    public class Session {
        public String url;
        public SessionPool sessionPool;
        
        Session(String url, SessionPool serverPool) 
        {
            this.url = url;
            this.sessionPool = serverPool;
        }  
        
        public void returnToPool() 
        {
            sessionPool.returnSessionUrlToPool(url);
        }       
    }
    
    
    SessionPool(String serverurl, String catalog,
            String repo, String username, String password,
            int lifetime) 
    {
        String catstring = "";
        
        if (catalog != null) {
            catstring = "/catalogs/" + catalog;
        }
        
        this.serverurl = serverurl;
        this.repopath = catstring + "/repositories/" + repo;
        this.sessionurls = new ConcurrentLinkedDeque<>();
        this.lifetime = lifetime;
        this.username = username;
        this.password = password;
    }
    
    public static SessionPool setupSessionPool(String serverurl, String catalog,
            String repo, String username, String password,
            int lifetime, int number_sessions) throws Exception 
    {
        
        SessionPool sp = new SessionPool(serverurl, catalog, repo,
        username, password, lifetime);
        
        for (int i = 0 ; i < number_sessions ; i++ ) {
            sp.sessionurls.addFirst(sp.newSession());
        }
           
        return sp;
    }
    
    public Session getSessionFromPool() throws Exception
    {
        String url;

        while (true) {
           
            url = sessionurls.pollFirst();
            
            if (url != null) {
                /* test to see if the session is still alive */

                try {
                    httpCall(url, "/rollback", method_post,
                            new ArrayList<NameValuePair>());
                    /* no failure, so ok */
                    return new Session(url, this);
                } catch (Exception e) {
                    if ((e != null) || debug) {
                        System.out.println("url " + url + " failed with error "
                                + e.toString());
                    }
                }
            } else {
                /* must create a new session */
                url = newSession();
                
                return new Session (url, this);
            }
        }
    }
    
    void returnSessionUrlToPool(String url) 
    {
        sessionurls.addFirst(url);
    }
    
    
    String newSession() throws Exception
    {
        List<NameValuePair> pairs;
        pairs = new ArrayList<>(2);
        
        pairs.add(new BasicNameValuePair("autoCommit", "false"));
        pairs.add(new BasicNameValuePair("lifetime", lifetime + ""));
        
        return httpCall(null, "/session", method_post, pairs);
    }
    
    String httpCall(String overrideUrl, String suffix,
            int method, List<NameValuePair> query) 
            throws Exception
    {
        BasicCredentialsProvider credp = new BasicCredentialsProvider();
        credp.setCredentials(
            AuthScope.ANY,
            new UsernamePasswordCredentials(username, password));
        HttpClientContext context = HttpClientContext.create();
        
        context.setCredentialsProvider(credp);
        
        String fullurl;
        
        if (null == overrideUrl) {   
            fullurl = serverurl + repopath + suffix;
        } else {
            fullurl = overrideUrl + suffix;
        }
        
        if (debug) System.out.println("full url is " + fullurl);
        HttpUriRequest req;
        
        switch (method) {
            case method_get: 
                URIBuilder urib = new URIBuilder(fullurl);
                for (NameValuePair nvp : query) {
                    urib = urib.addParameter(nvp.getName(), nvp.getValue());
                }
                req = new HttpGet(urib.build());
                break;
            case method_post:
                req = new HttpPost(fullurl);
                ((HttpPost)req).setEntity(new UrlEncodedFormEntity(query));
                break;
            default:
                throw new Exception("bad method");
        }
       
        String body = "";
        CloseableHttpClient client;

        client = HttpClients.createDefault();
        try {
            CloseableHttpResponse response = client.execute(req, context);
            try {
                HttpEntity entity = response.getEntity();

                if (entity != null) {
                    body = EntityUtils.toString(entity);
                }

                int retcode = response.getStatusLine().getStatusCode();
                if (retcode < 200 || retcode > 299) {
                    throw new Exception("http call return status " + retcode
                            + " and body " + body);

                }
                if (debug) {
                    System.out.println(" response is " + body);
                }
            } finally {
                response.close();
            }

        } finally {
            client.close();
        }

        return body;


    }
}

