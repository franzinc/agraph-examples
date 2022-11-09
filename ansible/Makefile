#
# Ansible control of AllegroGraph
#
# This is an example of the use of Ansible https://ansible.com  to
# install and control AllegroGraph
#
# Ansible is very flexible and we just show one of the
# many ways it can be used to control AllegroGraph.
# Consider these playbooks just an example of how it
# can be done and feel free to edit them to work in your
# particular location
#
# In particular you'll want to edit vars.yaml before runnning
# this script and you'll need to download a version of AllegroGraph
# in tar.gz form and store it in a location accessible to all servers
# (and put that location vars.yaml).
#
# You'll want to edit inventory.yaml to specify the machines on
# which to install AllegroGraph
#

default:
	@echo make rules are start, stop and install
	@echo will ping your servers now:
	@echo
	ansible -i inventory.txt -m ping agservers


start:
	ansible-playbook -v -i inventory.txt start.yaml

stop:
	ansible-playbook -v -i inventory.txt stop.yaml


install:
	ansible-playbook -v -i inventory.txt install.yaml

clean-install:
	ansible-playbook -v -i inventory.txt clean-install.yaml

# create the agraph shell script that is then stored in /etc/init.d
agraph: agraph.init vars.yaml
	ansible-playbook -v agraph.yaml



