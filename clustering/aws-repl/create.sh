#!/bin/bash
#
# create the initial cluster repo for the controlling instance
#
. ./vars.sh

curl -X PUT -u $authuser:$authpassword "http://127.0.0.1:$port/repositories/$reponame/repl/createCluster?host=$myip&port=$port&user=$authuser&password=$authpassword&instanceName=controlling"

exit 0
