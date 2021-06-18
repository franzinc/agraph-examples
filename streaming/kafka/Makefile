##
# Basic streaming with Kafka
#
# @file
# @version 0.1

DOCKER=docker
DOCKER_COMPOSE=docker-compose
AG_CONTAINER=ag-kafka-basic_ag
AG_KAFKA_BASIC_IMAGE=ag-kafka-basic_main
PRODUCER_CONTAINER=ag-kafka-basic_producer
AGENT_CONTAINER=ag-kafka-basic_agent

AGRAPH_VERSION=7.1.0
AGRAPH_HOST=localhost
AGRAPH_PORT=10055
AGRAPH_USER=test
AGRAPH_PASSWORD=xyzzy
AGRAPH_REPO=kafka-basic

NUM_POSTS=10000

build:
	@$(DOCKER) build -t $(AG_KAFKA_BASIC_IMAGE) -f Dockerfile .

kafka:
	@$(DOCKER_COMPOSE) up --detach --remove-orphans

stop_kafka:
	@$(DOCKER_COMPOSE) down

ag: build
	@$(DOCKER) pull franzinc/agraph:v$(AGRAPH_VERSION)
	@$(DOCKER) container inspect $(AG_CONTAINER) >/dev/null 2>&1 || ($(DOCKER) run --detach --rm \
		-p $(AGRAPH_PORT):10035 --shm-size 1g \
		--name $(AG_CONTAINER) \
		--env AGRAPH_SUPER_USER=$(AGRAPH_USER) \
		--env AGRAPH_SUPER_PASSWORD=$(AGRAPH_PASSWORD) \
		franzinc/agraph:v$(AGRAPH_VERSION) && \
	$(DOCKER) run --rm --network host \
		--env AGRAPH_HOST=$(AGRAPH_HOST) \
		--env AGRAPH_PORT=$(AGRAPH_PORT) \
		--env AGRAPH_USER=$(AGRAPH_USER) \
		--env AGRAPH_PASSWORD=$(AGRAPH_PASSWORD) \
		--env AGRAPH_REPO=$(AGRAPH_REPO) \
		$(AG_KAFKA_BASIC_IMAGE) init_repo)

stop_ag:
	@$(DOCKER) container inspect -f '{{.State.Running}}' $(AG_CONTAINER) && $(DOCKER) stop $(AG_CONTAINER) -t 0

producer: build
	@$(DOCKER) run --detach --rm --env NUM_POSTS=$(NUM_POSTS) --network host \
		--name $(PRODUCER_CONTAINER) $(AG_KAFKA_BASIC_IMAGE) producer

stop_producer:
	@$(DOCKER) stop $(PRODUCER_CONTAINER) -t 0

agent: build
	@$(DOCKER) run --detach --rm \
		--env AGRAPH_HOST=$(AGRAPH_HOST) \
		--env AGRAPH_PORT=$(AGRAPH_PORT) \
		--env AGRAPH_USER=$(AGRAPH_USER) \
		--env AGRAPH_PASSWORD=$(AGRAPH_PASSWORD) \
		--env AGRAPH_REPO=$(AGRAPH_REPO) \
		--network host --name $(AGENT_CONTAINER) $(AG_KAFKA_BASIC_IMAGE) agent

stop_agent:
	@$(DOCKER) stop $(AGENT_CONTAINER) -t 0

stop: stop_kafka stop_ag stop_agent stop_producer

.PHONY: build kafka stop_kafka ag stop_ag producer stop_producer agent stop_agent stop

# end
