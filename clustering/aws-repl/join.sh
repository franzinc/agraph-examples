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

controllingspec=$authuser:$authpassword@$controlling:$port/$reponame

if [ $myip == "$controlling" ] ; then ./create.sh ; exit 0; fi

# wait for the controlling ag server to be running
until curl -s http://$authuser:$authpassword@$controlling:$port/version ; do echo wait for controlling ; sleep 5; done

# wait for server on this machine to be running
until curl -s http://$authuser:$authpassword@127.0.0.1:$port/version ; do echo wait for local server ; sleep 5; done

# wait for cluster repo on the controlling instance to be present
until $agtool repl status $controllingspec > /dev/null ; do echo wait for repo ; sleep 5; done
    

myiname=i-$myip
echo $myiname > instance-name.txt

# construct the remove-instance.sh shell script to remove this instance
# from the cluster when the instance is terminated.
echo $agtool repl remove $controllingspec $myiname > remove-instance.sh
chmod 755 remove-instance.sh
#
# join the cluster
$agtool repl grow-cluster $controllingspec $authuser:$authpassword@$myip:$port/$reponame $myiname
