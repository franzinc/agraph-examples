
package com.franz.sessionpool;

/*
 *
 * @author jkf
 *  The information needed to specify a repository on a server
 *  and the credentials to access that repo.
 */

public class ServerSpec {

        String url;
        String catalog;
        String reponame;
        String username;
        String password;

        public ServerSpec(String url,
                String catalog,
                String reponame,
                String username,
                String password) {
            this.url = url;
            this.catalog = catalog;
            this.reponame = reponame;
            this.username = username;
            this.password = password;

        }

    }