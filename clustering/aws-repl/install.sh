#!/bin/bash
set -x
cp joincluster /etc/rc.d/init.d
chkconfig --add joincluster
