#!/bin/bash
#
## This starts in a container where the AllegroGraph installation
## files are present.
## The agraph server is installed and then either the controlling
## instance of an MMR cluster is created or the MMR cluster
## is grown by adding an instance on this server.
## 
##

echo secrets
ls /app/secrets

secretsroot=/app/secrets

port=$(cat $secretsroot/port)
reponame=$(cat $secretsroot/reponame)
authuser=$(cat $secretsroot/user)
authpassword=$(cat $secretsroot/password)

myip=$(hostname -I | sed -e 's/ .*$//')

## put useful information into the console log
## to diagnose startup problems
echo port is $port
echo reponame is $reponame
echo authuser is $authuser
echo authpassword is not shown

echo myip is $myip

## install the agraph server
(cd agraph-${agversion} ;  ./install-agraph /app/agraph -- --non-interactive \
		--runas-user agraph \
		--super-user $authuser \
		--super-password $authpassword ) 

## insert dns patch so we cache dns entries for a very short time
## so that if the controlling instance pod died and is restarted we
## learn about the new IP address of the controlling instance quickly.
cp /app/misc/dnspatch.cl /app/agraph/lib/patches/dnspatch.cl

# personalize the agraph.cfg file with the port selected by
# the user found in the secrets file.  Also add the agraph license
# to the agraph.cfg file.
sed -e s/Config_Port/$port/  /app/misc/agraph.cfg.in > /app/agraph/lib/agraph.cfg
cat /app/secrets/license >> /app/agraph/lib/agraph.cfg


chown -R agraph.agraph /app/agraph /app/rootcatalog


agtool=/app/agraph/bin/agtool


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

## the DNS name of the controlling host is specified in the yaml file
## starting this container
controllinghost=${ControllingHost}

controllingspec=$authuser:$authpassword@$controllinghost:$port/$reponame

if [ x$Controlling == "xyes" ] ;
then
   # It may take a little time for the DNS record for $controllinghost to be present
   # and we need that DNS lookup to work because the agtool program below will use it
   until host $controllinghost ; do  echo $controllinghost not in DNS yet; sleep 5 ; done
    
   ## create the first cluster instance which is the controlling instance
   $agtool repl create-cluster $controllingspec controlling
    

else
   # wait for the controlling ag server to be running
   until curl -s http://$authuser:$authpassword@$controllinghost:$port/version ; do echo wait for controlling ; sleep 5; done
   echo
   
   # wait for server in this container to be running
   until curl -s http://$authuser:$authpassword@$myip:$port/version ; do echo wait for local server ; sleep 5; done
   echo
    
   # wait for cluster repo on the controlling instance to be present
   until $agtool repl status $controllingspec > /dev/null ; do echo wait for repo ; sleep 5; done
   echo
   
   ## give the instance to be created a unique name within the cluster
   myiname=i-$myip
   echo $myiname > instance-name.txt

   # construct the remove-instance.sh shell script to remove this instance
   # from the cluster when the instance is terminated.
   echo $agtool repl remove $controllingspec $myiname > remove-instance.sh
   chmod 755 remove-instance.sh
   #

   # note that
   #  % docker kill container
   # will send a SIGKILL signal by default.  we can't trap on  SIGKILL.
   # so use
   #  % docker kill -s TERM container
   # in order to test the container's reaction to being commanded to shut down.
   
   trap term_handler SIGTERM SIGHUP SIGUSR1
   trap -p
   echo this pid is $$

   # join the cluster
   echo joining the cluster
   $agtool repl grow-cluster $controllingspec $authuser:$authpassword@$myip:$port/$reponame $myiname
   
fi
sleepforever
