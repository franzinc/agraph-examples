# Using AllegroGraph clustering

These files contain examples of building clustered databases using AllegroGraph.

## aws-repl

These are support files for setting up a multi-master replication cluster on AWS. It's best used in conjunction with terraform-elb.

## terraform-elb

A terraform script for creating a Multi-Master replication cluster with a load balancer distributing work to the instances.

## kubernetes/mmr

Examples of using AllegroGraph Multi-Master Replication with [Kubernetes](https://kubernetes.io) and [Docker Swarm](https://docs.docker.com/engine/swarm/)

## misc

Generic clustering-related examples (load balancing with NGINX, etc).
