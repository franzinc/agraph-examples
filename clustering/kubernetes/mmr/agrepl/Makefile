-include ../accounts.sh

build:  setaccounts
	cp ../accounts.sh .   # must be in this directory for Dockerfile to access it
	docker build --build-arg DockerAccount=$(DockerAccount) -t $(DockerAccount)/agrepl:latest .

push:
	docker push $(DockerAccount)/agrepl:latest


-include ../setaccounts.make

