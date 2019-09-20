-include ../accounts.sh

#
# This creates an MMR cluster using Docker Swarm
# To use this Makefile you must first have done
# % cd .. ; make account=mydockeraccount 
# % cd ../ag ; make build push
# % cd ../agrepl ; make build push
#
# and then have a Docker Swarm network running (which means
# you
# % docker init
# on one node and follow the printed directions for adding
# other nodes
# )
# 
#
# start the controlling instance
# % make controlling
#
# and then start the other instances
# % make copy

controlling: controlling.yaml network.created 
	docker run -it -d --shm-size=1g --net replnet -p 10035:10035 --name controlling -e Controlling=yes $(DockerAccount)/agrepl:latest; 


controlling.yaml: setaccounts controlling.yaml.in
	sed -e s/DockerAccount/$(DockerAccount)/ controlling.yaml.in > $@

copy:  copy.yaml  network.created
	docker stack deploy copy -c copy.yaml

copy.yaml: setaccounts copy.yaml.in
	sed -e s/DockerAccount/$(DockerAccount)/ copy.yaml.in > $@

-include ../setaccounts.make


# just do this once
network.created:
	-docker network create replnet -d overlay --attachable
	touch network.created




# interesting application to show what's running in the swarm
visualizer:
	docker service create \
		-p 8080:8080 \
		--constraint=node.role==manager \
		--mount=type=bind,src=/var/run/docker.sock,dst=/var/run/docker.sock \
		dockersamples/visualizer


# Kill and remove all running docker containers and services
# *** Don't do this if there are other docker containers running on this machine ***

cleandocker:
	-docker service rm `docker service ls -q`
	-docker container kill `docker container ls -q`
	-docker container prune --force


