##
# Makefile for starting and stopping the whole demo ...
# including a Spark cluster, an AllegroGraph instance and a Jupyter Notebook server
#
# @file
# @version 0.1

AG_CONTAINER_NAME=ag-spark-demo-ag
JUPYTER_CONTAINER_NAME=ag-spark-demo-jupyter

.DEFAULT_GOAL: demo

.PHONY: demo
demo:
	@docker-compose --file .docker/docker-compose.yml up --build --detach --remove-orphans
	@echo "Importing kennedy.ntriples ..."
	@docker cp .docker/kennedy.ntriples $(AG_CONTAINER_NAME):/tmp
	@docker exec $(AG_CONTAINER_NAME) sh -c "agtool load kennedy /tmp/kennedy.ntriples && rm /tmp/kennedy.ntriples"
	@echo "Importing jupyter notebook data ..."
	@docker cp AGSpark.ipynb $(JUPYTER_CONTAINER_NAME):/root/work
	@docker exec $(JUPYTER_CONTAINER_NAME) sh -c "mkdir -p /root/work/img"
	@docker cp img/kennedy.png $(JUPYTER_CONTAINER_NAME):/root/work/img

.PHONY: stop
stop:
	@docker-compose --file .docker/docker-compose.yml down --timeout 0

# end
