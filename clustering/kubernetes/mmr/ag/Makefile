-include ../accounts.sh

build:  setaccounts
	docker build --build-arg user=$(authuser) --build-arg password=$(authpassword) -t  $(DockerAccount)/ag:latest .

push:   setaccounts
	docker image push $(DockerAccount)/ag:latest

-include ../setaccounts.make

