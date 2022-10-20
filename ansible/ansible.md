# Using Ansible with AllegroGraph

 [Ansible](http://ansible.com) is an open source automatation tool
that can be used to install and control [AllegroGraph](http://allegrograph.com) servers.

We have an example of the use of Ansible in this directory.
Consider it as a starting point for creating your own
Ansible playbooks.

The Makefile contains rules to **install**, **start** and **stop**
AllegroGraph servers.  You will need to edit `vars.yaml`
and `inventory.yaml` for your specific site in order for
the Makefile rules to work.



