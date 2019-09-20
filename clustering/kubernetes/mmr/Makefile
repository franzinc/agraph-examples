#
# Before working in the subdirectories you must specify a dockerhub
# account name.  The images built must be stored on dockerhub
# so they can be retrieved by the kubernetes system when it needs
# to run the image in a container.
#
# Create a dockerhub account if you don't have one and make sure
# that you can "docker image push" to that account without needing
# a password.
#
# Then choose a username and password for the initial account created
# when the agraph server is installed.   If you don't specify a user or
# password value then the value of user defaults to 'test' and the
# value of password defaults to 'xyzzy'
#
# run the following (again, specifying user and password is optional)
# 
#   % make account=accountname user=username password=password
#

## defaults  (the account default will definitely not work so you must not use the default)
account=unspecified
user=test
password=xyzzy

setup:
	@echo
	@echo you did: make account=$(account) user=$(user) password=$(password)
	@echo
	@echo "# This file was automatically generated" > accounts.sh
	@echo DockerAccount=$(account) >> accounts.sh
	@echo authuser=$(user)         >> accounts.sh
	@echo authpassword=$(password) >> accounts.sh
	@echo accounts.sh updated


