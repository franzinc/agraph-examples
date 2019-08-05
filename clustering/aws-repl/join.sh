#!/bin/bash
# join the cluster
# This assumes the cwd is the directory holding this file.
#
# This is invoked by the joincluster script in /etc/rc/init.d
#
set -x
. ./vars.sh
#
#

echo ip is $myip

if [ $myip == "$controlling" ] ; then ./create.sh ; exit 0; fi

# wait for repo we'll copy on the controlling instance to be present
until curl -s http://$authuser:$authpassword@$controlling:$port/repositories/$reponame/size ; do echo wait for controlling ; sleep 15; done

myiname=i-$myip
echo $myiname > instance-name.txt

# construct the remove-instance.sh shell script to remove this instance
# from the cluster when the instance is terminated.
echo curl -X PUT -u $authuser:$authpassword "http://$controlling:$port/repositories/$reponame/repl/remove?instanceName=$myiname" > remove-instance.sh
chmod 755 remove-instance.sh
#
# join the cluster
curl -X PUT -u $authuser:$authpassword "http://$controlling:$port/repositories/$reponame/repl/growCluster?host=$myip&port=$port&name=$reponame&user=$authuser&password=$authpassword&instanceName=$myiname" 

