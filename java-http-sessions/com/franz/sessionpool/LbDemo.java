/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.franz.sessionpool;


import java.util.ArrayList;
import java.util.List;
import org.apache.http.NameValuePair;
import org.apache.http.message.BasicNameValuePair;

/**
 *
 * @author jkf
 */
public class LbDemo {

    
    public static void main(String[] args) throws Exception {
        
        int threads = 5;
        int queries = 10;
        int lifetime = 30;
        int initial = 5;  //initial sessions
        int worker = 0;  // for debugging

        for (int i = 0; i < args.length; i++) {
 
            if (args[i].equals("--threads")) {
                threads = Integer.parseInt(args[++i]);
            } else if (args[i].equals("--queries")) {
                queries = Integer.parseInt(args[++i]);
            } else if (args[i].equals("--lifetime")) {
                lifetime = Integer.parseInt(args[++i]);
            } else if (args[i].equals("--initial")) {
                initial = Integer.parseInt(args[++i]);
            } else if (args[i].equals("--worker")) {
                worker = Integer.parseInt(args[++i]);
            }
        }

        System.out.println(threads + " threads, " + queries + " queries, " +
                lifetime + " lifetime, " + initial + " initial sessions");

        // create load balancer using ServerSpec class
        
//        ArrayList<ServerSpec> sss = new ArrayList<>();
//        sss.add(new ServerSpec("http://localhost:10035",
//                null, "demorepo", "test", "xyzzy"));
//        sss.add(new ServerSpec("http://localhost:10035",
//                null, "secondrepo", "test", "xyzzy"));
//
//        LoadBalancer lb = new LoadBalancer(sss, lifetime, initial);
//   

        
        // create load balancer using array of one server urls
	// thus using a single session pool
        
        ArrayList<String> aaa = new ArrayList<>();
        aaa.add("http://localhost:10035");
        
        LoadBalancer lb = new LoadBalancer(aaa,null,"sp2",
					   "test", "xyzzy",
					   lifetime, initial);
        



        // prepare the query argument for the POST command we'll execute
        List<NameValuePair> pairs;
        pairs = new ArrayList<>(1);

        pairs.add(new BasicNameValuePair("query", SampleQuery.sampleQueryString));

       
     
    
        
        for (int i = 0; i < threads; i++) {
            Thread thread;
            final int qcount = queries;
            final int qi = i;
            final int qworker = worker;
            thread = new Thread() {
                public void run() {
                    try {
                        for (int j = 0; j < qcount; j++) {
                            SessionPool.Session sess = lb.getSessionFromLoadBalancer();
                            sess.sessionPool.httpCall(sess.url,
                                    "/sparql", SessionPool.method_post, pairs);
                            sess.returnToPool();
                        }
                        
                        System.out.println("Thread " + qworker + "." + qi + " Finished");
                        
                    } catch (Exception e) {
                        System.out.println("query thread got exception " + e);
                    }
                }
            };
            thread.start();
            System.out.println("Thread " + worker + "." + i + " Started");
        }
    }

}
