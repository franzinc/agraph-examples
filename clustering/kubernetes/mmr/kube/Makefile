-include ../accounts.sh

Namespace=testns


# After creating the cluster you can access the controlling node via
# % kubectl port-forward sts/controlling 10035
#  or to forward local port 30035 to 10035 on the pod do
# % kubectl port-forward sts/controlling 30035:10035
#
# These port-forward ports are not designed for a heavy load. Use
# the load balancers for that.
#
# You can access the copy nodes this way to but when you do the
# port forward it will forward to only one pod forever.. and
# should that pod die the forward will end
#

doall:  controlling.yaml copy.yaml
	kubectl create -f controlling-service.yaml
	kubectl create -f copy-service.yaml
	kubectl create -f controlling.yaml
	kubectl create -f copy.yaml
	kubectl create -f controlling-lb.yaml
	kubectl create -f copy-lb.yaml
	kubectl get svc
	kubectl get po


dosome: controlling.yaml copy.yaml
	kubectl create -f controlling.yaml
	kubectl create -f copy.yaml

# clean up processes but not the services for dns
cleansome:
	-kubectl delete sts --all
	-kubectl delete pvc --all

cleanall:
	make cleansome
	-kubectl delete svc copy
	-kubectl delete svc controlling
	-kubectl delete svc controlling-loadbalancer
	-kubectl delete svc copy-loadbalancer


controlling.yaml: setaccounts controlling.yaml.in ../accounts.sh
	sed -e s/DockerAccount/$(DockerAccount)/ controlling.yaml.in > $@

copy.yaml: setaccounts copy.yaml.in ../accounts.sh
	sed -e s/DockerAccount/$(DockerAccount)/ copy.yaml.in > $@


# create a quick port forward of port 10035 into the contolling instance
# This port forward is only good for small requests. For serious
# traffic use a load balancer
forward-controlling:
	kubectl port-forward --address 0.0.0.0 sts/controlling 10035

# once you've 'make forward-controlling' this will show you
# the replication status from the controlling instance's view
controlstatus:
	curl http://test:xyzzy@localhost:10035/repositories/myrepl/repl/status



## remove all resources
reset:
	-kubectl delete namespace ${Namespace}
	kubectl create namespace ${Namespace}
	kubectl config set-context `kubectl config current-context` --namespace ${Namespace}


-include ../setaccounts.make
