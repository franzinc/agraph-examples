#!/bin/bash
#
## to start ag and then create or join a cluster
##

cd /app/agraph/scripts

set -x
. ./accounts.sh
. ./vars.sh

agtool=/app/agraph/bin/agtool

echo ip is $myip


# move the copy of user with our login to the newly mounted volume
# if this is the first time we've run agraph on this volume
if [ ! -e /app/agraph/data/rootcatalog/$reponame ] ; then
    cp -rp /app/agraph/data/user/* /app/agraph/data/settings/user
fi  

# due to volume mounts /app/agraph/data could be owned by root
# so we have to take back ownership
chown -R agraph.agraph /app/agraph/data


## start agraph
/app/agraph/bin/agraph-control --config /app/agraph/lib/agraph.cfg start

term_handler() {
    # this signal is delivered when the pod is
    # about to be killed.  We remove ourselves
    # from the cluster.
   echo got term signal
   /bin/bash ./remove-instance.sh
   exit
}

sleepforever() {
    # This unusual way of sleeping allows
    # a TERM signal sent when the pod is to
    # die to then cause the shell to invoke
    # the term_handler function above.
    date
    while true
    do
        sleep 99999 & wait ${!}
    done
}    

if [ -e /app/agraph/data/rootcatalog/$reponame ] ; then
    echo repository $reponame already exists in this persistent volume
    sleepforever
fi    

controllinghost=controlling

controllingspec=$authuser:$authpassword@$controllinghost:$port/$reponame

if [ x$Controlling == "xyes" ] ;
then
   # It may take a little time for the dns record for 'controlling' to be present
   # and we need that record because the agtool program below will use it
   until host controlling ; do  echo controlling not in DNS yet; sleep 5 ; done
    
   ## create first and controlling cluster instance
   $agtool repl create-cluster $controllingspec controlling
    

else
    # wait for the controlling ag server to be running
    until curl -s http://$authuser:$authpassword@$controllinghost:$port/version ; do echo wait for controlling ; sleep 5; done

    # wait for server in this container to be running
    until curl -s http://$authuser:$authpassword@$myip:$port/version ; do echo wait for local server ; sleep 5; done

    
   # wait for cluster repo on the controlling instance to be present
   until $agtool repl status $controllingspec > /dev/null ; do echo wait for repo ; sleep 5; done
    
   
   myiname=i-$myip
   echo $myiname > instance-name.txt

   # construct the remove-instance.sh shell script to remove this instance
   # from the cluster when the instance is terminated.
   echo $agtool repl remove $controllingspec $myiname > remove-instance.sh
   chmod 755 remove-instance.sh
   #

   # note that
   #  % docker kill container
   # will send a SIGKILL signal by default  we can't trap on  SIGKILL.
   # so
   #  % docker kill -s TERM container
   # in order to test this handler
   
   trap term_handler SIGTERM SIGHUP SIGUSR1
   trap -p
   echo this pid is $$

   # join the cluster
   echo joining the cluster
   $agtool repl grow-cluster $controllingspec $authuser:$authpassword@$myip:$port/$reponame $myiname
   
fi
sleepforever
