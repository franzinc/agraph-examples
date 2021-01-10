# Development/Release Notes

Author:     Robert Wydler </br>
LinkedIn:   [Profile](https://www.linkedin.com/in/robwyd/) 

## Introduction 


### Based on ###
 This is an extension of the product "AllegroGraph for Multi-Master Replication" and is based on https://github.com/franzinc/agraph-examples.

### Goals ###
1. Creating a docker image like [Docker and AllegroGraph 7.0.4](https://franz.com/agraph/support/documentation/current/docker.html) but for [Multi-Master Replication in AllegroGraph 7.0.4](https://franz.com/agraph/support/documentation/current/multi-master.html) based on [Welcome to AllegroGraph examples](https://github.com/franzinc/agraph-examples).

1. Run standardized Multi-master Replication in AllegroGraph 7.0.4 using [Google Kubernetes Engine, GKE](https://cloud.google.com/kubernetes-engine).

1. Make it available for semantic AI research studies with help of [Google's Free Tier ](https://cloud.google.com/free) and [AllegroGraph's Free Editon ](https://allegrograph.com/) and to learn more about both products.

1. Setup a Kubernetes namespace `agraph-ns`,  Kubernetes `Update-Strategy` and the `Horizontal Pod AutoScaler, HPA` e.g. using `Google's Cluster Autoscaler`. 


### Affected Parts ###

Therefore, only the clustering parts of **docker** (folders: ``clustering/kubernetes/mmr/agmmr/*``), **kubernetes** and **helm** (folders: ``clustering/kubernetes/mmr/helm/*``) has been changed. 

## Important Constraints !!! ##
Currently ...
* ... it's only a **developer study** version and it's **not ready for production!** 
* ... the prototype was only running in a **very small (free)** Google GKE **test environment**.
* ... **fundamental changes** had been made (e.g. from ``StateFull`` copy read only ``pods`` to ``Deployment`` copy read only ``pods``) and Kubernetes' ``Horizontal Pod Autoscaler`` had been defined added (``hpa.yaml``).

## Known Issue ##
After deployment, sometimes it may happen, that the repositories are not reachable by the `agraph-mmr-copy-loadbalancer`. That's because the `agraph-mmr-controlling` stateful set has to run   before the `agraph-mmr-copy` deployment. I haven't figured out yet, how to arrange a sequential start in Helm. 

You can **circumvent** the issue by deleting the `agraph-mmr-copy` pods in the Google Cloud Console or you can perform a graceful pod deletion with the following command:

`kubectl delete pods \<pod>`

The `agraph-mmr-copy` pods will be recreated automatically by the deploment.  

---
## ToDo's for a Reliable Release
1. Change Helm to always start the stateful set `agraph-mmr-controlling` before the  `agraph-mmr-copy` deployment.  

For a reliable release of the Docker Image 7.0.4 together with Kubernetes and Helm, it's important to validate and test ...
1. ... the functionality of `copy-ssl.yaml` pods after the change from `StatefulSet` to Kubernetes `Deployment` pods.
1. ... the reliability and horizontal scalability of the pods (`hpa.yaml`) and of the Kubernetes cluster (e.g. GKE) in an operational environment with high query loads.
1. ... the interaction of the state-oriented controller pods with the stateless copy pods.
1. ... etc.
---
## Overview of Changes 
### Affected Parts ###
Only the clustering parts of **docker** (folders under: ``clustering/kubernetes/mmr/agmmr/*``), **kubernetes** and **helm** (folders under: ``clustering/kubernetes/mmr/helm/*``) were changed. 

### Docker Image, Version 7.0.4
*.../mmr/agmmr/Dockerfile:*
* Changed to AllegroGraph version to 7.0.4
* Testrelease removed
* Changed access to `chmod +x /app/misc/runrepl.sh`

*.../mmr/agmmr/Makefile:*
* Changed to AllegroGraph version to 7.0.4
* Testrelease removed

### Helm 
*.../mmr/helm/Makefile:*
* Given application name `agraph-mmr`
* Kubenretes namespace added `agraph-ns` for all components
* Changed to AllegroGraph version to 7.0.4
* Changed to AllegroGraph app version to 7.0.4

*.../mmr/helm/agraphmmr/Chart.yaml:*
* Changed to AllegroGraph version to 7.0.4
* Kept the maintainers to "Franz Inc."

*.../mmr/helm/agraphmmr/values.yaml:*
* CPU limits added
* Update behaviour added
* Horizontal Pod Autoscaler criteria added

### Kubernetes templates
*.../mmr/helm/agraphmmr/templates/controlling-ss.yaml:*
* Required `selector`added
* Update strategy added
*  CPU limits for the Horizontal Pod Autoscaler, HPA added 

*.../mmr/helm/agraphmmr/templates/copy-ss.yaml:*
* Because of read-only, stateless pods: Changed the  `kind` from `StatefulSet` to `Deploment`
* Constant `replica` set to `1`; it's under HPA control
* `matchLabels` and `app`added
* Update strategy added
* Values added under `Resources` for HPA

*.../mmr/helm/agraphmmr/templates/hpa.yaml:*
New file added for Horizontal Pod Autoscaler, HPA concerning ... 
* the controllers (controlling-ss.yaml)
* the workers (copy-ss.yaml) 

